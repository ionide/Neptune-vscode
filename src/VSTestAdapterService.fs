module VSTestAdapterService
open Fable.Import.Node
open Fable.Import
open Fable.Core.JsInterop
open Ionide.VSCode.Helpers

let ax =  Globals.require.Invoke "axios" |> unbox<Axios.AxiosStatic>

let devMode = false

let genPort () =
    let r = JS.Math.random ()
    let r' = r * (8999. - 8100.) + 8100.
    r'.ToString().Substring(0,4)

let port = if devMode then "8088" else genPort ()
let private url action = (sprintf "http://127.0.0.1:%s/%s" port action)
let mutable private service : ChildProcess.ChildProcess option =  None

let private request<'a, 'b> (action: string) (obj : 'a) =
    let fullRequestUrl = url action
    let options =
        createObj [
            "proxy" ==> false
        ]

    ax.post (fullRequestUrl, obj, unbox options)
    |> Promise.onFail (fun r ->
        // The outgoing request was not made
        null |> unbox
    )
    |> Promise.map(fun r ->
        // the outgoing request was made
        try
            r.data |> unbox<'b>
        with
        | _ ->
            null |> unbox
    )

let getPortRequest = request<string, string> "getPort"
let projectRequest : obj -> JS.Promise<obj> = request<obj, obj> "project"


let start' serverExe (args : string list) =
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


let startServer () =
    let pluginPath =
        try
            (VSCode.getPluginPath "Ionide.neptune")
        with
        | _ -> (VSCode.getPluginPath "Ionide.Neptune")

    let path = pluginPath + "/bin_vstest/Neptune.VSTestAdapter.dll"
    start' "dotnet" [ path ]

let start () =
    if devMode then Promise.empty else startServer ()
    |> Promise.map (ignore)

let stop () =
    service |> Option.iter (fun n -> n.kill "SIGKILL")
    service <- None
    ()