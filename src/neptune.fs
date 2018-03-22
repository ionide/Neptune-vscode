module Neptune

open Fable.Core
open Fable.Import
open vscode
open JsInterop
open Ionide.VSCode.Helpers
open Model
open Fable.Import.Node
open System
open Utils

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

let checkKey (context : vscode.ExtensionContext) =
    match context.globalState.get "productKey" with
    | Some s -> Promise.lift true
    | _ ->
        let date =
            match context.globalState.get "productFirstUse" with
            | Some s -> DateTime.Parse s
            | None ->
                let d = DateTime.Now
                context.globalState.update("productFirstUse", d.ToShortDateString()) |> ignore
                d
        let remaining = Math.Ceiling (date.AddDays(7.) - DateTime.Now).TotalDays
        let rd = remaining.ToString()
        let uri = vscode.Uri.parse "http://google.com"
        if remaining > 0. then
            let msg = sprintf "Neptune is paid extension. Your free trail has started on %s, and you have %s days left." (date.ToShortDateString()) rd
            vscode.window.showWarningMessage(msg, "Buy Neptune")
            |> Promise.onSuccess (fun n ->
                if n = "Buy Neptune" then
                    vscode.commands.executeCommand("vscode.open", uri)
                    |> ignore
            )
            |> ignore
            Promise.lift true
        else
            let msg = "Neptune is paid extension. Your free trail has ended."
            vscode.window.showWarningMessage(msg, "Buy Neptune")
            |> Promise.onSuccess (fun n ->
                if n = "Buy Neptune" then
                    vscode.commands.executeCommand("vscode.open", uri)
                    |> ignore
            )
            |> ignore
            Promise.lift false

let activate (context : vscode.ExtensionContext) =
    let reporter : IReporter = createReporter(reporterConstr, "Neptune", "0.1.0", "9ed427d0-bc3a-4660-bea5-645012b626d5", "Neptune")
    reporter.sendTelemetryEvent "Activate" Globals.undefined Globals.undefined
    let df = createEmpty<DocumentFilter>
    df.language <- Some "fsharp"
    let df' : DocumentSelector = df |> U3.Case2

    notifyTelemetry context |> ignore
    checkKey context
    |> Promise.onSuccess (fun n ->
        if n then
            TestExplorer.activate df' context reporter
            FSharpTestDetector.activate context
            |> Promise.onSuccess (fun (api, storagePath) ->
                ExpectoRunner.activate api
                VSTestRunner.activate api storagePath.Value
            )
            |> ignore
    )
    |> ignore
