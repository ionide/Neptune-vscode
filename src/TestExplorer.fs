module TestExplorer

open System
open Fable.Core
open JsInterop
open Fable.Import
open vscode
open Fable.Import.Node

open Ionide.VSCode.Helpers
open System.Collections.Generic
open Utils
open Model

type TreeModel = {
    Name: string
    FullName: string
    Range: Range
    FileName: string
    mutable State: TestState
    mutable Timer: string
    mutable ErrorMessage: string
    Childs: TreeModel []
    List: bool
    Id : int
}

let emptyModel = {
    Name = ""
    FullName = ""
    Range = {StartColumn = 0; StartLine = 0; EndColumn = 0; EndLine = 0}
    FileName = ""
    State = NotRun
    Childs = [||]
    Timer = ""
    ErrorMessage = ""
    List = false
    Id = -1
}

let rec ofTestEntry fileName state prefix (oldTests: TreeModel list)  (input: TestEntry) =
    let fullname = prefix + "/" + input.Name
    let rangeEquals (o : Range) (i : Range) =
        if JS.isDefined o && JS.isDefined i then
            o.StartLine = i.StartLine &&
            o.StartColumn = i.StartColumn &&
            o.EndLine = i.EndLine &&
            o.EndColumn = i.EndColumn
        else false


    let state =
        match oldTests |> List.tryFind (fun o -> rangeEquals o.Range input.Range || (o.FullName = fullname && o.FileName = fileName)  ) with
        | Some o -> o.State
        | None -> state
    let timer =
        match oldTests |> List.tryFind (fun o -> rangeEquals o.Range input.Range || (o.FullName = fullname && o.FileName = fileName)  ) with
        | Some o -> o.Timer
        | None -> ""
    let error =
        match oldTests |> List.tryFind (fun o -> rangeEquals o.Range input.Range || (o.FullName = fullname && o.FileName = fileName)  ) with
        | Some o -> o.ErrorMessage
        | None -> ""
    {
        Name = input.Name
        FullName = fullname
        Range = input.Range
        FileName = fileName
        State = state
        Timer = timer
        ErrorMessage = error
        Childs = input.Childs |> Array.map (ofTestEntry fileName state fullname oldTests)
        List = input.List
        Id = input.Id
    }

let rec updateState (name : string list) state (model : TreeModel) =
    match name with
    | [] -> ()
    | [x] ->
        match model.Childs |> Seq.tryFind (fun n -> n.Name.Trim() = (x.Trim('"', ' '))) with
        | None -> ()
        | Some s ->
            s.State <- state
            s.Timer <- ""
            s.ErrorMessage <- ""
    | x::xs ->
        match model.Childs |> Seq.tryFind (fun n -> n.Name.Trim() = (x.Trim('"', ' ')) ) with
        | None -> ()
        | Some s -> updateState xs state s

let rec updateTimer (name : string list) timer (model : TreeModel) =
    match name with
    | [] -> ()
    | [x] ->
        match model.Childs |> Seq.tryFind (fun n -> n.Name.Trim() = (x.Trim('"', ' '))) with
        | None -> ()
        | Some s ->
            s.Timer <- timer
    | x::xs ->
        match model.Childs |> Seq.tryFind (fun n -> n.Name.Trim() = (x.Trim('"', ' ')) ) with
        | None -> ()
        | Some s -> updateTimer xs timer s

let rec updateError (name : string list) error (model : TreeModel) =
    match name with
    | [] -> ()
    | [x] ->
        match model.Childs |> Seq.tryFind (fun n -> n.Name.Trim() = (x.Trim('"', ' '))) with
        | None -> ()
        | Some s ->
            s.ErrorMessage <- error
    | x::xs ->
        match model.Childs |> Seq.tryFind (fun n -> n.Name.Trim() = (x.Trim('"', ' ')) ) with
        | None -> ()
        | Some s -> updateError xs error s
let mutable display = 0

let private getIconPath light dark =
    let plugPath =
        try
            (VSCode.getPluginPath "Ionide.neptune")
        with
        | _ ->  (VSCode.getPluginPath "Ionide.Neptune")

    let p = createEmpty<TreeIconPath>
    p.dark <- Path.join(plugPath, "images", dark)
    p.light <- Path.join(plugPath, "images", light)
    p
let tests = Dictionary<string, TreeModel []>()

let flattedTests () =
    let rec flatten model =
        model::(model.Childs |> Array.toList |> List.collect flatten)
    tests
    |> Seq.map(|KeyValue|)
    |> Seq.collect snd
    |> Seq.toList
    |> List.collect flatten

let getTests state =
    flattedTests ()
    |> List.filter (fun n -> n.State = state)

let private failedDecorationType =
    let opt = createEmpty<DecorationRenderOptions>
    let file = "testFailed.png"
    let path =
        try
            (VSCode.getPluginPath "Ionide.neptune") + "/images/" + file |> Uri.file
        with
        | _ ->  (VSCode.getPluginPath "Ionide.Neptune") + "/images/" + file |> Uri.file
    opt.gutterIconPath <- unbox path
    opt.overviewRulerLane <- Some OverviewRulerLane.Full
    opt.overviewRulerColor <- Some (U2.Case1 "rgba(224, 64, 6, 0.7)")
    window.createTextEditorDecorationType opt

let private passedDecorationType =
    let opt = createEmpty<DecorationRenderOptions>
    let file = "testPassed.png"
    let path =
        try
            (VSCode.getPluginPath "Ionide.neptune") + "/images/" + file |> Uri.file
        with
        | _ ->  (VSCode.getPluginPath "Ionide.Neptune") + "/images/" + file |> Uri.file
    opt.gutterIconPath <- unbox path
    opt.overviewRulerLane <- Some OverviewRulerLane.Full
    opt.overviewRulerColor <- Some (U2.Case1 "rgba(166, 215, 133, 0.7)")
    window.createTextEditorDecorationType opt

let private ignoredDecorationType =
    let opt = createEmpty<DecorationRenderOptions>
    let file = "testIgnored.png"
    let path =
        try
            (VSCode.getPluginPath "Ionide.neptune") + "/images/" + file |> Uri.file
        with
        | _ ->  (VSCode.getPluginPath "Ionide.Neptune") + "/images/" + file |> Uri.file
    opt.gutterIconPath <- unbox path
    opt.overviewRulerLane <- Some OverviewRulerLane.Full
    opt.overviewRulerColor <- Some (U2.Case1 "rgba(255, 188, 64, 0.7)")
    window.createTextEditorDecorationType opt

let private notRunDecorationType =
    let opt = createEmpty<DecorationRenderOptions>
    let file = "testNotRun.png"
    let path =
        try
            (VSCode.getPluginPath "Ionide.neptune") + "/images/" + file |> Uri.file
        with
        | _ ->  (VSCode.getPluginPath "Ionide.Neptune") + "/images/" + file |> Uri.file
    opt.gutterIconPath <- unbox path
    window.createTextEditorDecorationType opt


let private setDecorations () =
    let failed fn =
        getTests Failed
        |> List.filter (fun n -> n.FileName = fn && n.Childs.Length = 0)
        |> List.map (fun n -> Range.ToCodeRange n.Range)
        |> ResizeArray

    let passed fn =
        getTests Passed
        |> List.filter (fun n -> n.FileName = fn && n.Childs.Length = 0)
        |> List.map (fun n -> Range.ToCodeRange n.Range )
        |> ResizeArray

    let ignored fn =
        getTests Ignored
        |> List.filter (fun n -> n.FileName = fn && n.Childs.Length = 0)
        |> List.map (fun n -> Range.ToCodeRange n.Range )
        |> ResizeArray

    let notRun fn =
        getTests NotRun
        |> List.filter (fun n -> n.FileName = fn && n.Childs.Length = 0)
        |> List.map (fun n -> Range.ToCodeRange n.Range)
        |> ResizeArray

    window.visibleTextEditors
    |> Seq.iter (fun te ->
        match te.document with
        | Document.FSharp ->
            let fld = failed te.document.fileName
            te.setDecorations(failedDecorationType, U2.Case1 fld)

            let psd = passed te.document.fileName
            te.setDecorations(passedDecorationType, unbox psd)

            let ign = ignored te.document.fileName
            te.setDecorations(ignoredDecorationType, unbox ign)

            let nr = notRun te.document.fileName
            te.setDecorations(notRunDecorationType, unbox nr)
        | _ -> ()
    )

    ()

let refresh = EventEmitter<TreeModel> ()

let diagnostcs = languages.createDiagnosticCollection()

let handle (input : ParseResponse) =
    if input.Tests.Length > 0 then
        let oldTests = flattedTests ()
        tests.[input.FileName] <- input.Tests |> Array.map (ofTestEntry input.FileName NotRun "" oldTests)
        refresh.fire undefined

let parseTextDocument document =
    match document with
    | Document.FSharp ->
        let txt = document.getText()
        let request = {ParseRequest.Content = txt; FileName = document.fileName }
        LanguageService.parseRequest request
        |> Promise.onSuccess (handle)
        |> unbox
    | _ -> undefined


let testResultHandler (state : TestState, names: string []) =
    names
    |> Seq.iter (fun name ->
        let indents = name.Split ('/') |> Seq.toList
        let tsts = flattedTests ()
        let mdl = {emptyModel with Childs = tsts |> List.toArray}
        updateState indents state mdl
    )
    refresh.fire undefined
    ()

let testTimerHandler (changes: (string * string) []) =
    changes
    |> Seq.iter (fun (name, timer) ->
        let indents = name.Split ('/') |> Seq.toList
        let tsts = flattedTests ()
        let mdl = {emptyModel with Childs = tsts |> List.toArray}
        updateTimer indents timer mdl
    )
    refresh.fire undefined
    ()

let testErrorHandler (changes: (string * string) []) =
    printfn "CHANGES: %A" changes
    changes
    |> Seq.iter (fun (name, error) ->
        let indents = name.Split ('/') |> Seq.toList
        let tsts = flattedTests ()
        let mdl = {emptyModel with Childs = tsts |> List.toArray}
        updateError indents error mdl
    )
    diagnostcs.clear()
    getTests Failed
    |> List.groupBy(fun n -> n.FileName )
    |> List.map (fun (fn, values) ->
        let diags =
            values
            |> List.map (fun v -> Diagnostic(Range.ToCodeRange v.Range, v.ErrorMessage, DiagnosticSeverity.Error))
            |> ResizeArray

        Uri.file fn, diags )
    |> ResizeArray
    |> diagnostcs.set


let createTreeProvider () : TreeDataProvider<TreeModel> =
    { new TreeDataProvider<TreeModel>
      with
        member __.onDidChangeTreeData =
            refresh.event

        member __.getChildren(node) =
            if JS.isDefined node then
                if display = 0 then
                    node.Childs
                    |> ResizeArray
                elif display = 1 && (node.Name = "Passed" || node.Name = "Ignored" || node.Name = "Failed" || node.Name = "Not Run") then
                    node.Childs
                    |> ResizeArray
                else
                    [] |> ResizeArray
            else
                if display = 0 then
                    tests
                    |> Seq.map (|KeyValue|)
                    |> Seq.collect snd
                    |> ResizeArray
                else
                    [
                        {emptyModel with Name = "Passed"; Childs = getTests Passed |> Array.ofList; List = true }
                        {emptyModel with Name = "Failed"; Childs = getTests Failed |> Array.ofList; List = true }
                        {emptyModel with Name = "Ignored"; Childs = getTests Ignored |> Array.ofList; List = true }
                        {emptyModel with Name = "Not Run"; Childs = getTests NotRun |> Array.ofList; List = true }
                    ]
                    |> ResizeArray

        member __.getTreeItem(node) =
            let ti = createEmpty<TreeItem>
            ti.label <- node.Name + (if node.Timer <> "" then sprintf " (%s)" node.Timer else "")
            ti.collapsibleState <-
                if node.List then
                    Some TreeItemCollapsibleState.Expanded
                else
                    None


            ti.iconPath <-

                if node.List then
                    Some <| getIconPath "icon-module-light.svg" "icon-module-dark.svg"
                else
                    match node.State with
                    | NotRun -> Some <| getIconPath "testNotRun.png" "testNotRun.png"
                    | Passed -> Some <| getIconPath "testPassed.png" "testPassed.png"
                    | Ignored -> Some <| getIconPath "testIgnored.png" "testIgnored.png"
                    | Failed -> Some <| getIconPath "testFailed.png" "testFailed.png"

            ti.contextValue <-
                if node.List then
                    Some "neptune.testExplorer.group"
                else
                    Some "neptune.testExplorer.test"


            let c = createEmpty<Command>
            c.command <- "neptune.testExplorer.goTo"
            c.title <- "open"
            c.arguments <- Some (ResizeArray [| unbox node|])
            ti.command <- Some c

            ti
    }

let createCodeLensesProvider () =
    { new CodeLensProvider with
        member __.provideCodeLenses(doc, _) =
            flattedTests ()
            |> Seq.where (fun t -> t.FileName = doc.fileName)
            |> Seq.collect (fun t ->
                let range = Range.ToCodeRange t.Range
                let commandRun = createEmpty<Command>
                commandRun.title <- if t.Childs.Length > 0 then "Run Tests" else "Run Test"
                commandRun.command <- if t.Childs.Length > 0 then "neptune.runList" else "neptune.runTest"
                commandRun.arguments <- Some <| ResizeArray [| box t.FullName |]
                let commandDebug = createEmpty<Command>
                commandDebug.title <- if t.Childs.Length > 0 then "Debug Tests" else "Debug Test"
                commandDebug.command <- if t.Childs.Length > 0 then "neptune.debugList" else "neptune.debugTest"
                commandDebug.arguments <- Some <| ResizeArray [| box t.FullName |]
                [ CodeLens(range, commandRun); CodeLens(range, commandDebug) ]

            )
            |> ResizeArray
            |> U2.Case1

        member __.resolveCodeLens(codeLens, _) =
            codeLens
            |> U2.Case1

        member __.onDidChangeCodeLenses = unbox refresh.event

    }



let activate selector (context: ExtensionContext) =


    // Webhooks.testCallback <- handle
    // Expecto.testResultChanged.Publish.Add testResultHandler
    // Expecto.testTimerChanged.Publish.Add testTimerHandler
    // Expecto.testErrorMessageChanged.Publish.Add testErrorHandler

    workspace.onDidChangeTextDocument.Invoke(fun te -> parseTextDocument te.document) |> context.subscriptions.Add
    window.visibleTextEditors |> Seq.iter (fun te -> parseTextDocument te.document)

    commands.registerCommand("neptune.testExplorer.goTo", Func<obj, obj>(fun n ->
        let entry = unbox<TreeModel> n
        let line = entry.Range.StartLine - 1
        let uri = Uri.file entry.FileName
        workspace.openTextDocument(uri)
        |> Promise.map (fun td ->
            window.showTextDocument td
            |> Promise.map (fun te ->
                te.revealRange (Range(float line, 0., float line, 0.), TextEditorRevealType.InCenter)))
        |> unbox
    )) |> context.subscriptions.Add

    commands.registerCommand("neptune.runList", Func<obj, obj>(fun m ->
        // let m = unbox<TreeModel> m
        // Expecto.runTestsList m.FullName
        () |> unbox
    )) |> context.subscriptions.Add

    commands.registerCommand("neptune.runTest", Func<obj, obj>(fun m ->
        // let m = unbox<TreeModel> m
        // Expecto.runSingleTest m.FullName
        () |> unbox
    )) |> context.subscriptions.Add

    commands.registerCommand("neptune.runAll", Func<obj, obj>(fun _ ->
        // Expecto.runAllTests ()
        () |> unbox
    )) |> context.subscriptions.Add

    commands.registerCommand("neptune.runFailed", Func<obj, obj>(fun _ ->
        // getTests Failed
        // |> List.map (fun n -> n.FullName.TrimStart('/') )
        // |> List.toArray
        // |> Expecto.runMultipleTests
        () |> unbox
    )) |> context.subscriptions.Add

    commands.registerCommand("neptune.changeDisplayMode", Func<obj, obj>(fun _ ->
        if display = 0 then display <- 1 else display <- 0
        refresh.fire undefined
        |> unbox
    )) |> context.subscriptions.Add


    window.registerTreeDataProvider("neptune.testExplorer", createTreeProvider () )
    |> context.subscriptions.Add

    languages.registerCodeLensProvider(selector, createCodeLensesProvider ())
    |> context.subscriptions.Add

    refresh.event.Invoke(unbox setDecorations)
    |> context.subscriptions.Add

    window.onDidChangeVisibleTextEditors.Invoke(unbox setDecorations)
    |> context.subscriptions.Add