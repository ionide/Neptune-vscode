module Utils
open Fable.Import.vscode

[<AutoOpen>]
module Utils =
    let inline undefined<'a> = unbox<'a> ()

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