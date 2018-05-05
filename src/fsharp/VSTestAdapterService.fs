module VSTestAdapterService

open Fable.Import.Node
open Fable.Import
open Fable.Core.JsInterop
open Ionide.VSCode.Helpers
open System

let private ax =  Globals.require.Invoke "axios" |> unbox<Axios.AxiosStatic>

let private devMode = false
let log = createConfiguredLoggers "NEPTUNE" "Neptune (F# - VSTest Adapter)"


let private genPort () =
    let r = JS.Math.random ()
    let r' = r * (8999. - 8100.) + 8100.
    r'.ToString().Substring(0,4)

let private port = if devMode then "8088" else genPort ()
let private url action requestId = (sprintf "http://127.0.0.1:%s/%s?requestId=%i" port action requestId)
let mutable private service : ChildProcess.ChildProcess option =  None
let mutable private vstest : ChildProcess.ChildProcess option =  None

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

let private logIncomingResponse requestId fsacAction (started: DateTime) (res: _ option) (ex: exn option) =
    let elapsed = DateTime.Now - started
    match res, ex with
    | Some res, None ->
        log.Debug(makeIncomingLogPrefix(requestId) + " {%s} in %s ms:\nData=%j", fsacAction, elapsed.TotalMilliseconds, res)
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
            logIncomingResponse requestId action started (r.data) None
            r.data |> unbox<'b>
        with
        | ex ->
            logIncomingResponse requestId action started None (Some ex)
            null |> unbox
    )

let getPortRequest = request<string, string> "getPort"
let handshakeRequest = request<string, string> "handshake"

let vstestRequest req =
    promise {
        let! res = request<string, string []> "request" req
        if res.Length > 0 then
            return res |> Array.map (JS.JSON.parse)
        else
            return null
    }


let private start' serverExe (args : string list) =
    Promise.create (fun resolve reject ->
        let child =
            let spawnLogged path (args: string list) =
                ChildProcess.spawn(path, args |> ResizeArray)
            spawnLogged serverExe
                [ yield! args
                  yield port ]

        let mutable isResolvedAsStarted = false
        child
        |> Process.onOutput (fun buffer ->
            let outputString = buffer.toString()
            printfn "[VSTEST ADAPTER]: %s" outputString
            // Wait until Neptune server sends the 'listener started' magic string until
            // we inform the caller that it's ready to accept requests.
            let isStartedMessage = outputString.Contains "listener started in"
            if isStartedMessage then
                service <- Some child
                resolve child
                isResolvedAsStarted <- true

        )
        |> Process.onError (fun e ->
            let error = unbox<JS.Error> e
            if not isResolvedAsStarted then
                reject (error.message)
        )
        |> Process.onErrorOutput (fun n ->
            let buffer = unbox<Buffer.Buffer> n
            if not isResolvedAsStarted then
                reject (buffer.toString())
        )
        |> ignore
    )
    |> Promise.onFail (fun err ->
        "Failed to start VSTest Adapter server. Please check if dotnet is in PATH"
        |> vscode.window.showErrorMessage
        |> ignore)


let private startServer () =
    let path = pluginPath + "/bin_vstest/Neptun.VSTestAdapter.dll"
    start' "dotnet" [ path ]

let start () =
    if devMode then Promise.empty else startServer ()
    |> Promise.map (ignore)
        |> Promise.bind (fun _ ->
        getPortRequest ""
    )
    |> Promise.bind (fun port ->
        let p = handshakeRequest ""
        vstest <- Some (Process.spawn "dotnet" "" (sprintf "vstest --port:%s" port))
        p
    )
    |> Promise.bind (fun _ ->
        vstestRequest """{"MessageType": "ProtocolVersion","Payload": 1}"""
    )

let stop () =
    service |> Option.iter (fun n -> n.kill "SIGKILL")
    service <- None
    vstest |> Option.iter (fun n -> n.kill "SIGKILL")
    vstest <- None
    ()