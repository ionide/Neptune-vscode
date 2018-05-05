module Utils
open Fable.Import.vscode

[<AutoOpen>]
module StatusMsgs =
    let startingMsg = "Starting tests"
    let completedMsg = "Tests finished"
    let failedRunMsg = "Running tests failed"
    let buildingMsg = "Building tests"
    let runningMsg = "Running tests"

[<AutoOpen>]
module Utils =
    let inline undefined<'a> = unbox<'a> ()

    let withProgress (f : Progress<ProgressMessage> -> 'a) =
        let progressOpts = Fable.Core.JsInterop.createEmpty<ProgressOptions>
        progressOpts.location <- ProgressLocation.Window
        window.withProgress(progressOpts, f >> unbox)
        |> unbox

    let report msg (handler: Progress<ProgressMessage>) =
        let pm = Fable.Core.JsInterop.createEmpty<ProgressMessage>
        pm.message <- msg
        handler.report pm

[<RequireQualifiedAccess>]
module String =
    let trim (s: string) = s.Trim()
    let replace (oldVal: string) (newVal: string) (str: string) : string =
        match str with
        | null -> null
        | _ -> str.Replace (oldVal, newVal)
    let split seperator (s : string) = s.Split seperator

    let endWith ending (s : string) = s.EndsWith ending

    let startWith ending (s : string) = s.StartsWith ending


[<RequireQualifiedAccess>]
module Document =

    let (|FSharp|CSharp|VB|Other|) (document : TextDocument) =
        if document.languageId = "fsharp" then FSharp
        else if document.languageId = "csharp" then CSharp
        else if document.languageId = "vb" then VB
        else Other

[<RequireQualifiedAccess>]
module Configuration =
    let get defaultValue key =
        workspace.getConfiguration().get(key, defaultValue)

module Event =

    let invoke (listener: 'T -> _) (event: Fable.Import.vscode.Event<'T>) =
        event.Invoke(unbox<System.Func<_, _>>(listener))

module NodeUtil =
    open System
    open Fable.Core

    type IExports =
        abstract member format: format: string * [<ParamArray>] args: obj[] -> string

    [<Import("*", "util")>]
    let Util: IExports = jsNative

[<AutoOpen>]
module Object =
    open System
    open Fable.Core


    type Object with
        [<Emit("$0[$1]")>]
        member __.Item n : obj = jsNative

module Promise =
    open Ionide.VSCode.Helpers

    let collect f xs =
        xs |> Seq.fold (fun acc e ->
            promise {
                let! acc = acc
                let! res = f e
                return List.append acc res
            }
        ) (Promise.lift [])