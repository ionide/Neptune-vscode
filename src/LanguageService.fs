module LanguageService
open Fable.Import.Node
open Fable.Import
open Utils
open vscode
open System
open Fable.Core.JsInterop
open Ionide.VSCode.Helpers
open Model

let ax =  Globals.require.Invoke "axios" |> unbox<Axios.AxiosStatic>

let devMode = false

[<RequireQualifiedAccess>]
type LogConfigSetting = None | Output | DevConsole | Both
let logLanguageServiceRequestsConfigSetting =
    try
        match "FSharp.logLanguageServiceRequests" |> Configuration.get "output" with
        | "devconsole" -> LogConfigSetting.DevConsole
        | "output" -> LogConfigSetting.Output
        | "both" -> LogConfigSetting.Both
        | _ -> LogConfigSetting.Output
    with
    | _ -> LogConfigSetting.Output

// note: always log to the loggers, and let it decide where/if to write the message
let createConfiguredLoggers source channelName =

    let logLanguageServiceRequestsOutputWindowLevel () =
        try
            match "FSharp.logLanguageServiceRequestsOutputWindowLevel" |> Configuration.get "INFO" with
            | "DEBUG" -> Level.DEBUG
            | "INFO" -> Level.INFO
            | "WARN" -> Level.WARN
            | "ERROR" -> Level.ERROR
            | _ -> Level.INFO
        with
        | _ -> Level.INFO

    let channel, logRequestsToConsole =
        match logLanguageServiceRequestsConfigSetting with
        | LogConfigSetting.None -> None, false
        | LogConfigSetting.Both -> Some (window.createOutputChannel channelName), true
        | LogConfigSetting.DevConsole -> None, true
        | LogConfigSetting.Output -> Some (window.createOutputChannel channelName), false

    let logLevel = logLanguageServiceRequestsOutputWindowLevel ()
    let editorSideLogger = ConsoleAndOutputChannelLogger(Some source, logLevel, channel, Some logLevel)

    let showCurrentLevel level =
        if level <> Level.DEBUG then
            editorSideLogger.Info ("Logging to output at level %s. If you want detailed messages, try level DEBUG.", (level.ToString()))

    editorSideLogger.ChanMinLevel |> showCurrentLevel

    workspace.onDidChangeConfiguration
    |> Event.invoke (fun () ->
        editorSideLogger.ChanMinLevel <- logLanguageServiceRequestsOutputWindowLevel ()
        editorSideLogger.ChanMinLevel |> showCurrentLevel )
    |> ignore

    // show the stdout data printed from FSAC in a separate channel
    let fsacStdOutWriter =
        if logRequestsToConsole then
            let chan = window.createOutputChannel (channelName + " (server)")
            chan.append
        else
            ignore

    editorSideLogger, fsacStdOutWriter

let log, fsacStdoutWriter = createConfiguredLoggers "IONIDE-FSAC" "F# Language Service"



let genPort () =
    let r = JS.Math.random ()
    let r' = r * (8999. - 8100.) + 8100.
    r'.ToString().Substring(0,4)

let port = if devMode then "8088" else genPort ()
let private url fsacAction requestId = (sprintf "http://127.0.0.1:%s/%s?requestId=%i" port fsacAction requestId)
let mutable private service : ChildProcess.ChildProcess option =  None

let private platformPathSeparator = if Process.isMono () then "/" else "\\"
let private makeRequestId =
    let mutable requestId = 0
    fun () -> (requestId <- requestId + 1); requestId
let private relativePathForDisplay (path: string) =
    path.Replace(vscode.workspace.rootPath + platformPathSeparator, "~" + platformPathSeparator)
let private makeOutgoingLogPrefix (requestId:int) = String.Format("REQ ({0:000}) ->", requestId)
let private makeIncomingLogPrefix (requestId:int) = String.Format("RES ({0:000}) <-", requestId)

let private logOutgoingRequest requestId (fsacAction:string) obj =
    // At the INFO level, it's nice to see only the key data to get an overview of
    // what's happening, without being bombarded with too much detail
    log.Debug (makeOutgoingLogPrefix(requestId) + " {%s}\nData=%j", fsacAction, obj)

let private logIncomingResponse requestId fsacAction (started: DateTime) (r: Axios.AxiosXHR<_>) (res: _ option) (ex: exn option) =
    let elapsed = DateTime.Now - started
    match res, ex with
    | Some res, None ->
        log.Debug(makeIncomingLogPrefix(requestId) + " {%s} in %s ms:\nData=%j", fsacAction, elapsed.TotalMilliseconds, res?Data)
    | None, Some ex ->
        log.Error (makeIncomingLogPrefix(requestId) + " {%s} ERROR in %s ms: {%j}, Data=%j", fsacAction, elapsed.TotalMilliseconds, ex.ToString(), obj)
    | _, _ -> log.Error(makeIncomingLogPrefix(requestId) + " {%s} ERROR in %s ms: %j, %j, %j", fsacAction, elapsed.TotalMilliseconds, res, ex.ToString(), obj)

let private logIncomingResponseError requestId fsacAction (started: DateTime) (r: obj) =
    let elapsed = DateTime.Now - started
    log.Error (makeIncomingLogPrefix(requestId) + " {%s} ERROR in %s ms: %s Data=%j",
                fsacAction, elapsed.TotalMilliseconds, r.ToString(), obj)

let private request<'a, 'b> (action: string) (obj : 'a) =
    let started = DateTime.Now
    let requestId = makeRequestId()
    let fullRequestUrl = url action requestId
    logOutgoingRequest requestId action obj
    let options =
        createObj [
            "proxy" ==> false
        ]

    ax.post (fullRequestUrl, obj, unbox options)
    |> Promise.onFail (fun r ->
        // The outgoing request was not made
        logIncomingResponseError requestId action started r
        null |> unbox
    )
    |> Promise.map(fun r ->
        // the outgoing request was made
        try
            r.data |> unbox<string> |> ofJson<'b>
        with
        | ex ->
            logIncomingResponse requestId action started r None (Some ex)
            null |> unbox
    )

let parseRequest = request<ParseRequest, ParseResponse> "parse"
let projectRequest = request<ProjectRequest, ProjectResponse> "project"


let start' serverExe (args : string list) =
    Promise.create (fun resolve reject ->
        let child =
            let spawnLogged path (args: string list) =
                fsacStdoutWriter (sprintf "Running: %s %s\n" path (args |> String.concat " "))
                ChildProcess.spawn(path, args |> ResizeArray)
            spawnLogged serverExe
                [ yield! args
                  yield port ]

        let mutable isResolvedAsStarted = false
        child
        |> Process.onOutput (fun buffer ->
            let outputString = buffer.toString()
            // Wait until Neptune server sends the 'listener started' magic string until
            // we inform the caller that it's ready to accept requests.
            let isStartedMessage = outputString.Contains "listener started in"
            if isStartedMessage then
                fsacStdoutWriter ("Resolving startup promise because Neptune printed the 'listener started' message")
                fsacStdoutWriter "\n"
                service <- Some child
                resolve child
                isResolvedAsStarted <- true

            fsacStdoutWriter outputString
        )
        |> Process.onError (fun e ->
            let error = unbox<JS.Error> e
            fsacStdoutWriter (error.message)
            if not isResolvedAsStarted then
                reject (error.message)
        )
        |> Process.onErrorOutput (fun n ->
            let buffer = unbox<Buffer.Buffer> n
            fsacStdoutWriter (buffer.toString())
            if not isResolvedAsStarted then
                reject (buffer.toString())
        )
        |> ignore
    )
    |> Promise.onFail (fun err ->
        log.Error("Failed to start Neptune server. %s", err)
        if Process.isMono () then
            "Failed to start Neptune server. Please check if mono is in PATH"
            |> vscode.window.showErrorMessage
            |> ignore)

type [<RequireQualifiedAccess>] TargetRuntime = NET | NetcoreFdd

let startServer () =
    let fsacTargetRuntime =
        match "Neptune.serverRuntime" |> Configuration.get "net" with
        | "netcore" -> TargetRuntime.NetcoreFdd
        | "net" | _ -> TargetRuntime.NET

    let pluginPath =
        try
            (VSCode.getPluginPath "Ionide.neptune")
        with
        | _ -> (VSCode.getPluginPath "Ionide.Neptune")

    match fsacTargetRuntime with
    | TargetRuntime.NET ->
        let path = pluginPath + "/bin/Neptune.exe"
        let exe, args =
            if Process.isMono () then
                let mono = "Neptune.monoPath" |> Configuration.get "mono"
                mono, [ yield path ]
            else
                path, []
        start' exe args
    | TargetRuntime.NetcoreFdd ->
        let path = pluginPath + "/bin_netcore/Neptune.dll"
        start' "dotnet" [ path ]

let start () =
    if devMode then Promise.empty else startServer ()
    |> Promise.map (ignore)

let stop () =
    service |> Option.iter (fun n -> n.kill "SIGKILL")
    service <- None
    ()