module LanguageService
open Fable.Import.Node
open Fable.Import
open Utils
open vscode
open System
open Fable.Core.JsInterop
open Ionide.VSCode.Helpers

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

let private request<'a, 'b> (fsacAction: string) requestId (obj : 'a) =
    let started = DateTime.Now
    let fullRequestUrl = url fsacAction requestId
    logOutgoingRequest requestId fsacAction obj
    let options =
        createObj [
            "proxy" ==> false
        ]

    ax.post (fullRequestUrl, obj, unbox options)
    |> Promise.onFail (fun r ->
        // The outgoing request was not made
        logIncomingResponseError requestId fsacAction started r
        null |> unbox
    )
    |> Promise.map(fun r ->
        // the outgoing request was made
        try
            r.data |> unbox<string> |> ofJson<'b>
        with
        | ex ->
            logIncomingResponse requestId fsacAction started r None (Some ex)
            null |> unbox
    )