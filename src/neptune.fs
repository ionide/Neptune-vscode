module Neptune

open Fable.Core
open Fable.Import
open vscode
open JsInterop
open Ionide.VSCode.Helpers
open Model
open Fable.Import.Node

// vscode-extension-telemetry

[<Import("default", from="vscode-extension-telemetry")>]
let reporterConstr : obj = jsNative

[<Emit("new $0($1,$2,$3,$4)")>]
let createReporter(o, a, b,c,d) : IReporter = jsNative

let activate (context : vscode.ExtensionContext) =
    let reporter : IReporter = createReporter(reporterConstr, "Neptune", "0.1.0", "9ed427d0-bc3a-4660-bea5-645012b626d5", "Neptune")
    reporter.sendTelemetryEvent "Activate" Globals.undefined Globals.undefined
    let df = createEmpty<DocumentFilter>
    df.language <- Some "fsharp"
    let df' : DocumentSelector = df |> U3.Case2

    TestExplorer.activate df' context reporter

    FSharpTestDetector.activate context
    |> Promise.onSuccess (fun (api, storagePath) ->
        ExpectoRunner.activate api
        VSTestRunner.activate api storagePath.Value
    )
    |> ignore
