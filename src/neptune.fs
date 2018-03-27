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


let private ax =  Globals.require.Invoke "axios" |> unbox<Axios.AxiosStatic>

let authorizeKey (context : vscode.ExtensionContext) key =
    let args =
        createObj [
            "product_permalink" ==> "NeptunePlugin"
            "license_key" ==> key
        ]

    ax.post("https://api.gumroad.com/v2/licenses/verify", args)
    |> Promise.map (fun n ->
        let res = n.data
        let dataS = unbox<string>res?purchase?created_at
        let data = dataS.Substring(0,10)
        context.globalState.update("productDate", data)
        data
    )

let checkKey (context : vscode.ExtensionContext) =
    let uri = vscode.Uri.parse "https://gumroad.com/l/NeptunePlugin"
    match context.globalState.get "productKey" with
    | Some key ->
        match context.globalState.get "productDate" with
        | Some s ->
            let d = DateTime.Parse s
            let shouldCheck = DateTime.Now >= d.AddDays(1.)
            if shouldCheck then
                authorizeKey context key
                |> Promise.either
                    (fun data ->
                        let ds = DateTime.Parse data
                        if DateTime.Now <= ds.AddDays(1.) then
                            Promise.lift true
                        else
                            vscode.window.showWarningMessage("Neptune is paid extension. Your subscription has expired." , "Buy Neptune")
                            |> Promise.onSuccess (fun n ->
                                if n = "Buy Neptune" then
                                    vscode.commands.executeCommand("vscode.open", uri)
                                    |> ignore
                            )
                            |> ignore
                            Promise.lift false)
                    (fun _ ->
                        vscode.window.showWarningMessage("Neptune is paid extension. Your subscription has expired." , "Buy Neptune")
                        |> Promise.onSuccess (fun n ->
                            if n = "Buy Neptune" then
                                vscode.commands.executeCommand("vscode.open", uri)
                                |> ignore
                        )
                        |> ignore
                        Promise.lift false)
            else
                authorizeKey context key
                Promise.lift true
        | None ->
            authorizeKey context key
            |> Promise.either
                (fun data ->
                    let ds = DateTime.Parse data
                    if DateTime.Now <= ds.AddDays(1.) then
                        Promise.lift true
                    else
                        vscode.window.showWarningMessage("Neptune is paid extension. Your subscription has expired." , "Buy Neptune")
                        |> Promise.onSuccess (fun n ->
                            if n = "Buy Neptune" then
                                vscode.commands.executeCommand("vscode.open", uri)
                                |> ignore
                        )
                        |> ignore
                        Promise.lift false)
                (fun _ ->
                    vscode.window.showWarningMessage("Neptune is paid extension. Your subscription has expired." , "Buy Neptune")
                    |> Promise.onSuccess (fun n ->
                        if n = "Buy Neptune" then
                            vscode.commands.executeCommand("vscode.open", uri)
                            |> ignore
                    )
                    |> ignore
                    Promise.lift false)
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
        if remaining > 0. then
            let msg = sprintf "Neptune is paid extension. Your free trail has started on %s, and you have %s days left." (date.ToShortDateString()) rd
            vscode.window.showWarningMessage(msg, "Buy Neptune", "Enter License")
            |> Promise.onSuccess (fun n ->
                if n = "Buy Neptune" then
                    vscode.commands.executeCommand("vscode.open", uri)
                    |> ignore
                elif n = "Enter License" then
                    let opts = createEmpty<InputBoxOptions>
                    opts.prompt <- Some "License Key"
                    vscode.window.showInputBox(opts)
                    |> Promise.onSuccess (fun n ->
                        if JS.isDefined n then
                            context.globalState.update("productDate", n)
                            |> ignore

                    ) |> ignore
            )
            |> ignore
            Promise.lift true
        else
            let msg = "Neptune is paid extension. Your free trail has ended."
            vscode.window.showWarningMessage(msg, "Buy Neptune", "Enter License")
            |> Promise.onSuccess (fun n ->
                if n = "Buy Neptune" then
                    vscode.commands.executeCommand("vscode.open", uri)
                    |> ignore
                elif n = "Enter License" then
                    let opts = createEmpty<InputBoxOptions>
                    opts.prompt <- Some "License Key"
                    vscode.window.showInputBox(opts)
                    |> Promise.onSuccess (fun n ->
                        if JS.isDefined n then
                            context.globalState.update("productDate", n)
                            |> ignore

                    ) |> ignore
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
