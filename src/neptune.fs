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

let notifyTelemetry (context : vscode.ExtensionContext) =
    match context.globalState.get "telemetryInfo" with
    | Some s when s -> ()
    | _ ->
        context.globalState.update("telemetryInfo", true)
        |> ignore
        vscode.window.showInformationMessage("Neptune is collecting anonymous telemetry data that allows us to make project better. You can disable it by changing `Neptune.enableTelemetry` setting", "Open settings")
        |> Promise.onSuccess (fun n ->
            if n = "Open settings" then
                commands.executeCommand("workbench.action.openGlobalSettings") |> ignore
        )
        |> ignore


let activate (context : vscode.ExtensionContext) =
    let reporter : IReporter = createReporter(reporterConstr, "Neptune", "0.1.0", "9ed427d0-bc3a-4660-bea5-645012b626d5", "Neptune")
    reporter.sendTelemetryEvent "Activate" Globals.undefined Globals.undefined
    let df = createEmpty<DocumentFilter>
    df.language <- Some "fsharp"
    let df' : DocumentSelector = df |> U3.Case2

    notifyTelemetry context |> ignore
    TestExplorer.activate df' context reporter

    FSharpTestDetector.activate context
    |> Promise.onSuccess (fun (api, storagePath) ->
        ExpectoRunner.activate api
        VSTestRunner.activate api storagePath.Value
    )
    |> ignore
