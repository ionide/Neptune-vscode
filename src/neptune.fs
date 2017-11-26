module Neptune

open Fable.Core
open Fable.Import
open vscode
open JsInterop

let activate (context : vscode.ExtensionContext) =
    let df = createEmpty<DocumentFilter>
    df.language <- Some "fsharp"
    let df' : DocumentSelector = df |> U3.Case2


    TestExplorer.activate df' context
