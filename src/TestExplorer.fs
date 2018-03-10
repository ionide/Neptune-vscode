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

type private TreeModel = {
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
    Type : string
}

let private emptyModel = {
    Name = ""
    FullName = ""
    Range = {StartColumn = 0; StartLine = 0; EndColumn = 0; EndLine = 0}
    FileName = ""
    State = TestState.NotRun
    Childs = [||]
    Timer = ""
    ErrorMessage = ""
    List = false
    Id = -1
    Type = ""
}

let private runnerRegister = Dictionary<string, ITestRunner>()
let private detectorRegister = Dictionary<string, ITestDetector>()
let private refresh = EventEmitter<TreeModel option> ()
let private diagnostcs = languages.createDiagnosticCollection()

let private getProjectList () =
    detectorRegister.Values
    |> Seq.collect (fun dt ->
        dt.GetProjectList () )
    |> Seq.toList

let private getProjectForFile (fn : SourceFilePath) =
    detectorRegister.Values
    |> Seq.tryPick (fun dt ->
        if dt.ShouldHandleFilePath fn then
            dt.GetProjectForFile fn
        else None

    )

let registerTestRunner id (runner: ITestRunner) = runnerRegister.[id] <- runner
let registerTestDetector id (runner: ITestDetector) = detectorRegister.[id] <- runner

let rec private ofTestEntry fileName state prefix (oldTests: TreeModel list)  (input: TestEntry) =
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
        Type = if input.Type = "NUnit" || input.Type = "XUnit" then "VSTest" else input.Type //TODO: Hack
    }
let mutable private display = 0

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
let private tests = Dictionary<string, TreeModel []>()

let private flattedTests () =
    let rec flatten model =
        model::(model.Childs |> Array.toList |> List.collect flatten)
    tests
    |> Seq.map(|KeyValue|)
    |> Seq.collect snd
    |> Seq.toList
    |> List.collect flatten

let private getTests state =
    flattedTests ()
    |> List.filter (fun n -> n.State = state && not n.List)

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
        getTests TestState.Failed
        |> List.filter (fun n -> n.FileName = fn && n.Childs.Length = 0)
        |> List.map (fun n -> Range.ToCodeRange n.Range)
        |> ResizeArray

    let passed fn =
        getTests TestState.Passed
        |> List.filter (fun n -> n.FileName = fn && n.Childs.Length = 0)
        |> List.map (fun n -> Range.ToCodeRange n.Range )
        |> ResizeArray

    let ignored fn =
        getTests TestState.Ignored
        |> List.filter (fun n -> n.FileName = fn && n.Childs.Length = 0)
        |> List.map (fun n -> Range.ToCodeRange n.Range )
        |> ResizeArray

    let notRun fn =
        getTests TestState.NotRun
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

let private handle (input : ParseResponse) =
    if input.Tests.Length > 0 then
        let oldTests = flattedTests ()
        tests.[input.FileName] <- input.Tests |> Array.map (ofTestEntry input.FileName TestState.NotRun "" oldTests)
        refresh.fire undefined

let parseTextDocument (document : TextDocument) =
    detectorRegister.Values
    |> Seq.tryFind (fun dt -> dt.ShouldHandleFile document)
    |> Option.iter (fun dt ->
        dt.GetTestsForFile document
        |> Promise.onSuccess (handle)
        |> ignore
    )

let parseProject (project : Project) =
    detectorRegister.Values
    |> Seq.tryFind (fun dt -> dt.ShouldHandleProject project)
    |> Option.iter (fun dt ->
        dt.GetTestsForProject project
        |> Promise.onSuccess (Seq.iter handle)
        |> ignore
    )

let private handleTestResults (results: TestResult list) =
    let tsts = flattedTests ()
    results
    |> Seq.iter (fun n ->
        let name = n.FullName.Trim( '"', ' ', '\\', '/')
        let name =
            if n.Runner = "VSTest" then
               name.Replace('/', '.').Replace('\\', '.')
            else
                name
        match tsts |> Seq.tryFind (fun t ->
            let tName = t.FullName.Trim( '"', ' ', '\\', '/')
            let tName =
                if n.Runner = "VSTest" then
                    tName.Replace('/', '.').Replace('\\', '.').Replace("this.", "") //TODO: THIS IS HACK, SHOULD BE HANDLED BY THE DETECTION SERVER
                else
                    tName
            tName = name ) with
        | None -> ()
        | Some tst ->
            tst.State <- n.State
            tst.Timer <- n.Timer
            tst.ErrorMessage <- n.ErrorMessage
    )
    refresh.fire undefined

    diagnostcs.clear()
    getTests TestState.Failed
    |> List.groupBy(fun n -> n.FileName )
    |> List.map (fun (fn, values) ->
        let diags =
            values
            |> List.map (fun v -> Diagnostic(Range.ToCodeRange v.Range, v.ErrorMessage, DiagnosticSeverity.Error))
            |> ResizeArray
        Uri.file fn, diags )
    |> ResizeArray
    |> diagnostcs.set

let private createTreeProvider () : TreeDataProvider<TreeModel> =
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
                        {emptyModel with Name = "Passed"; Childs = getTests TestState.Passed |> Array.ofList; List = true }
                        {emptyModel with Name = "Failed"; Childs = getTests TestState.Failed |> Array.ofList; List = true }
                        {emptyModel with Name = "Ignored"; Childs = getTests TestState.Ignored |> Array.ofList; List = true }
                        {emptyModel with Name = "Not Run"; Childs = getTests TestState.NotRun |> Array.ofList; List = true }
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
                    | TestState.NotRun -> Some <| getIconPath "testNotRun.png" "testNotRun.png"
                    | TestState.Passed -> Some <| getIconPath "testPassed.png" "testPassed.png"
                    | TestState.Ignored -> Some <| getIconPath "testIgnored.png" "testIgnored.png"
                    | TestState.Failed -> Some <| getIconPath "testFailed.png" "testFailed.png"

            ti.contextValue <-
                let runner = runnerRegister.[node.Type]
                match getProjectForFile node.FileName with
                | None -> None
                | Some project ->
                if node.List then
                    let x = "neptune.testExplorer.group"
                    let x = if (runner.Capabilities project |> List.contains CanRunList) then x + "Run" else x
                    let x = if (runner.Capabilities project |> List.contains CanDebugList) then x + "Debug" else x
                    Some x
                else
                    let x = "neptune.testExplorer.test"
                    let x = if (runner.Capabilities project |> List.contains CanRunSingle) then x + "Run" else x
                    let x = if (runner.Capabilities project |> List.contains CanDebugSingle) then x + "Debug" else x
                    Some x


            let c = createEmpty<Command>
            c.command <- "neptune.testExplorer.goTo"
            c.title <- "open"
            c.arguments <- Some (ResizeArray [| unbox node|])
            ti.command <- Some c

            ti
    }

let private createCodeLensesProvider () =
    { new CodeLensProvider with
        member __.provideCodeLenses(doc, _) =
            flattedTests ()
            |> Seq.where (fun t -> t.FileName = doc.fileName)
            |> Seq.collect (fun t ->
                let runner = runnerRegister.[t.Type]
                match getProjectForFile t.FileName with
                | None -> []
                | Some project ->
                let range = Range.ToCodeRange t.Range
                [
                    if t.List && (runner.Capabilities project |> List.contains CanRunList) then
                        let command = createEmpty<Command>
                        command.title <- "Run Tests"
                        command.command <- "neptune.runList"
                        command.arguments <- Some <| ResizeArray [| box t |]
                        yield CodeLens(range, command)
                    if (not t.List) && (runner.Capabilities project |> List.contains CanRunSingle) then
                        let command = createEmpty<Command>
                        command.title <- "Run Test"
                        command.command <- "neptune.runTest"
                        command.arguments <- Some <| ResizeArray [| box t |]
                        yield CodeLens(range, command)
                    if t.List && (runner.Capabilities project |> List.contains CanDebugList) then
                        let command = createEmpty<Command>
                        command.title <- "Debug Tests"
                        command.command <- "neptune.debugList"
                        command.arguments <- Some <| ResizeArray [| box t |]
                        yield CodeLens(range, command)
                    if (not t.List) && (runner.Capabilities project |> List.contains CanDebugSingle) then
                        let command = createEmpty<Command>
                        command.title <- "Debug Test"
                        command.command <- "neptune.debugTest"
                        command.arguments <- Some <| ResizeArray [| box t |]
                        yield CodeLens(range, command)
                ]
            )
            |> ResizeArray
            |> U2.Case1

        member __.resolveCodeLens(codeLens, _) =
            codeLens
            |> U2.Case1

        member __.onDidChangeCodeLenses = unbox refresh.event

    }

let activate selector (context: ExtensionContext) =
    workspace.onDidChangeTextDocument.Invoke(fun te -> parseTextDocument te.document |> unbox) |> context.subscriptions.Add

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
        let m =
            if JS.isDefined m then
                Promise.lift <| unbox<TreeModel> m
            else
                let tests =
                    flattedTests ()
                    |> Seq.filter (fun n -> n.List)
                    |> Seq.map (fun n ->
                        let qpi = createEmpty<QuickPickItem>
                        qpi.label <- n.Name
                        qpi?data <- n
                        qpi
                    )
                    |> ResizeArray

                window.showQuickPick(U2.Case1 tests)
                |> Promise.map (fun n -> n?data |> unbox<TreeModel>)
        m |> Promise.map (fun m ->
            match getProjectForFile m.FileName with
            | None -> undefined
            | Some prj ->
                runnerRegister.Values
                |> Seq.choose (fun r ->
                    if r.ShouldProjectBeRun prj then
                        Some (r.RunList (prj, m.FullName.Trim( '"', ' ', '\\', '/')))
                    else
                        None
                )
                |> Promise.all
                |> Promise.onSuccess (fun n ->
                    n
                    |> Seq.toList
                    |> List.collect id
                    |> handleTestResults
                )
        ) |> unbox
    )) |> context.subscriptions.Add

    commands.registerCommand("neptune.debugList", Func<obj, obj>(fun m ->
        let m =
            if JS.isDefined m then
                Promise.lift <| unbox<TreeModel> m
            else
                let tests =
                    flattedTests ()
                    |> Seq.filter (fun n -> n.List)
                    |> Seq.map (fun n ->
                        let qpi = createEmpty<QuickPickItem>
                        qpi.label <- n.Name
                        qpi?data <- n
                        qpi
                    )
                    |> ResizeArray

                window.showQuickPick(U2.Case1 tests)
                |> Promise.map (fun n -> n?data |> unbox<TreeModel>)
        m |> Promise.map (fun m ->
            match getProjectForFile m.FileName with
            | None -> undefined
            | Some prj ->
                runnerRegister.Values
                |> Seq.choose (fun r ->
                    if r.ShouldProjectBeRun prj then
                        Some (r.DebugList (prj, m.FullName.Trim( '"', ' ', '\\', '/')))
                    else
                        None
                )
                |> Promise.all
                |> Promise.onSuccess (fun n ->
                    n
                    |> Seq.toList
                    |> List.collect id
                    |> handleTestResults
                )
        ) |> unbox
    )) |> context.subscriptions.Add

    commands.registerCommand("neptune.runTest", Func<obj, obj>(fun m ->
        let m =
            if JS.isDefined m then
                Promise.lift <| unbox<TreeModel> m
            else
                let tests =
                    flattedTests ()
                    |> Seq.filter (fun n -> not n.List)
                    |> Seq.map (fun n ->
                        let qpi = createEmpty<QuickPickItem>
                        qpi.label <- n.Name
                        qpi?data <- n
                        qpi
                    )
                    |> ResizeArray
                window.showQuickPick(U2.Case1 tests)
                |> Promise.map (fun n -> n?data |> unbox<TreeModel>)
        m |> Promise.map (fun m ->
            match getProjectForFile m.FileName with
            | None -> undefined
            | Some prj ->
                let projectsWithTests = [prj, [m.FullName.Trim( '"', ' ', '\\', '/') ] ]
                runnerRegister.Values
                |> Seq.map (fun r ->
                    let prjsWithTsts = projectsWithTests |> List.filter (fun (p,_) -> r.ShouldProjectBeRun p)
                    match prjsWithTsts with
                    | [] -> Promise.lift []
                    | xs ->  r.RunTests xs
                )
                |> Promise.all
                |> Promise.onSuccess (fun n ->
                    n
                    |> Seq.toList
                    |> List.collect id
                    |> handleTestResults
                )
        ) |> unbox
    )) |> context.subscriptions.Add

    commands.registerCommand("neptune.debugTest", Func<obj, obj>(fun m ->
        let m =
            if JS.isDefined m then
                Promise.lift <| unbox<TreeModel> m
            else
                let tests =
                    flattedTests ()
                    |> Seq.filter (fun n -> not n.List)
                    |> Seq.map (fun n ->
                        let qpi = createEmpty<QuickPickItem>
                        qpi.label <- n.Name
                        qpi?data <- n
                        qpi
                    )
                    |> ResizeArray
                window.showQuickPick(U2.Case1 tests)
                |> Promise.map (fun n -> n?data |> unbox<TreeModel>)
        m |> Promise.map (fun m ->
            match getProjectForFile m.FileName with
            | None -> undefined
            | Some prj ->
                let projectsWithTests = [prj, [m.FullName.Trim( '"', ' ', '\\', '/') ] ]
                runnerRegister.Values
                |> Seq.map (fun r ->
                    let prjsWithTsts = projectsWithTests |> List.filter (fun (p,_) -> r.ShouldProjectBeRun p)
                    match prjsWithTsts with
                    | [] -> Promise.lift []
                    | xs ->  r.DebugTests xs
                )
                |> Promise.all
                |> Promise.onSuccess (fun n ->
                    n
                    |> Seq.toList
                    |> List.collect id
                    |> handleTestResults
                )
        ) |> unbox
    )) |> context.subscriptions.Add


    commands.registerCommand("neptune.runAll", Func<obj, obj>(fun _ ->
        let projects = getProjectList ()
        runnerRegister.Values
        |> Seq.map (fun r ->
            let prjs = projects |> List.filter r.ShouldProjectBeRun
            r.RunAll prjs )
        |> Promise.all
        |> Promise.onSuccess (fun n ->
            n
            |> Seq.toList
            |> List.collect id
            |> handleTestResults
        ) |> unbox
    )) |> context.subscriptions.Add

    commands.registerCommand("neptune.debugAll", Func<obj, obj>(fun _ ->
        let projects = getProjectList ()
        runnerRegister.Values
        |> Seq.map (fun r ->
            let prjs = projects |> List.filter r.ShouldProjectBeRun
            r.DebugAll prjs )
        |> Promise.all
        |> Promise.onSuccess (fun n ->
            n
            |> Seq.toList
            |> List.collect id
            |> handleTestResults
        ) |> unbox
    )) |> context.subscriptions.Add

    commands.registerCommand("neptune.runFailed", Func<obj, obj>(fun _ ->
        let projectsWithTests =
            getTests TestState.Failed
            |> List.choose (fun t -> getProjectForFile t.FileName |> Option.map (fun p -> p, t))
            |> List.groupBy fst
            |> List.map (fun (p, lst) -> p, (lst |> List.map (fun (_, test) -> test.FullName.Trim( '"', ' ', '\\', '/') )) )

        runnerRegister.Values
        |> Seq.map (fun r ->
            let prjsWithTsts = projectsWithTests |> List.filter (fun (p,_) -> r.ShouldProjectBeRun p)
            match prjsWithTsts with
            | [] -> Promise.lift []
            | xs ->  r.RunTests xs
        )
        |> Promise.all
        |> Promise.onSuccess (fun n ->
            n
            |> Seq.toList
            |> List.collect id
            |> handleTestResults
        ) |> unbox
    )) |> context.subscriptions.Add

    commands.registerCommand("neptune.debugFailed", Func<obj, obj>(fun _ ->
        let projectsWithTests =
            getTests TestState.Failed
            |> List.choose (fun t -> getProjectForFile t.FileName |> Option.map (fun p -> p, t))
            |> List.groupBy fst
            |> List.map (fun (p, lst) -> p, (lst |> List.map (fun (_, test) -> test.FullName.Trim( '"', ' ', '\\', '/') )) )

        runnerRegister.Values
        |> Seq.map (fun r ->
            let prjsWithTsts = projectsWithTests |> List.filter (fun (p,_) -> r.ShouldProjectBeRun p)
            match prjsWithTsts with
            | [] -> Promise.lift []
            | xs ->  r.DebugTests xs
        )
        |> Promise.all
        |> Promise.onSuccess (fun n ->
            n
            |> Seq.toList
            |> List.collect id
            |> handleTestResults
        ) |> unbox
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

