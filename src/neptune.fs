module Neptune

open Fable.Core
open Fable.Import
open vscode
open JsInterop
open Ionide.VSCode.Helpers



let activate (context : vscode.ExtensionContext) =
    let df = createEmpty<DocumentFilter>
    df.language <- Some "fsharp"
    let df' : DocumentSelector = df |> U3.Case2

    let ext = vscode.extensions.getExtension<Model.Api> "Ionide.Ionide-fsharp"

    LanguageService.start ()
    |> Promise.onSuccess (fun _ ->
        TestExplorer.activate df' context ext.exports)
    |> ignore
