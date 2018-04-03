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
open System.Text.RegularExpressions

type private TreeModel = {
    Name: string
    FullName: string
    Range: Range
    FileName: string
    mutable State: TestState
    mutable Timer: string
    mutable ErrorMessage: string
    mutable Childs: TreeModel []
    List: bool
    Id : int
    Type : string
    mutable HasMultipleCases: bool
    IsSubCase: bool
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
    HasMultipleCases = false
    IsSubCase = false
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
    let childs =
        match oldTests |> List.tryFind (fun o -> rangeEquals o.Range input.Range || (o.FullName = fullname && o.FileName = fileName)  ) with
        | Some o -> if o.HasMultipleCases then o.Childs else input.Childs |> Array.map (ofTestEntry fileName state fullname oldTests)
        | None -> input.Childs |> Array.map (ofTestEntry fileName state fullname oldTests)
    let hasMultipleCases =
        match oldTests |> List.tryFind (fun o -> rangeEquals o.Range input.Range || (o.FullName = fullname && o.FileName = fileName)  ) with
        | Some o -> o.HasMultipleCases
        | None -> false

    {
        Name = input.Name
        FullName = fullname
        Range = input.Range
        FileName = fileName
        State = state
        Timer = timer
        ErrorMessage = error
        Childs = childs
        List = input.List
        Id = input.Id
        Type = if input.Type = "NUnit" || input.Type = "XUnit" then "VSTest" else input.Type //TODO: Hack
        HasMultipleCases = hasMultipleCases
        IsSubCase = false
    }
let mutable private display = 0

let private getIconPath light dark =
    let plugPath =
        try
            (VSCode.getPluginPath "LambdaFactory.neptune")
        with
        | _ ->  (VSCode.getPluginPath "LambdaFactory.Neptune")

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
    |> List.filter (fun n -> not n.IsSubCase)

let private getTests state =
    flattedTests ()
    |> List.filter (fun n -> n.State = state && not n.List && not n.IsSubCase)

let private failedDecorationType =
    let opt = createEmpty<DecorationRenderOptions>
    let file = "testFailed.png"
    let path =
        try
            (VSCode.getPluginPath "LambdaFactory.neptune") + "/images/" + file |> Uri.file
        with
        | _ ->  (VSCode.getPluginPath "LambdaFactory.Neptune") + "/images/" + file |> Uri.file
    opt.gutterIconPath <- unbox path
    opt.overviewRulerLane <- Some OverviewRulerLane.Full
    opt.overviewRulerColor <- Some (U2.Case1 "rgba(224, 64, 6, 0.7)")
    window.createTextEditorDecorationType opt

let private passedDecorationType =
    let opt = createEmpty<DecorationRenderOptions>
    let file = "testPassed.png"
    let path =
        try
            (VSCode.getPluginPath "LambdaFactory.neptune") + "/images/" + file |> Uri.file
        with
        | _ ->  (VSCode.getPluginPath "LambdaFactory.Neptune") + "/images/" + file |> Uri.file
    opt.gutterIconPath <- unbox path
    opt.overviewRulerLane <- Some OverviewRulerLane.Full
    opt.overviewRulerColor <- Some (U2.Case1 "rgba(166, 215, 133, 0.7)")
    window.createTextEditorDecorationType opt

let private ignoredDecorationType =
    let opt = createEmpty<DecorationRenderOptions>
    let file = "testIgnored.png"
    let path =
        try
            (VSCode.getPluginPath "LambdaFactory.neptune") + "/images/" + file |> Uri.file
        with
        | _ ->  (VSCode.getPluginPath "LambdaFactory.Neptune") + "/images/" + file |> Uri.file
    opt.gutterIconPath <- unbox path
    opt.overviewRulerLane <- Some OverviewRulerLane.Full
    opt.overviewRulerColor <- Some (U2.Case1 "rgba(255, 188, 64, 0.7)")
    window.createTextEditorDecorationType opt

let private notRunDecorationType =
    let opt = createEmpty<DecorationRenderOptions>
    let file = "testNotRun.png"
    let path =
        try
            (VSCode.getPluginPath "LambdaFactory.neptune") + "/images/" + file |> Uri.file
        with
        | _ ->  (VSCode.getPluginPath "LambdaFactory.Neptune") + "/images/" + file |> Uri.file
    opt.gutterIconPath <- unbox path
    window.createTextEditorDecorationType opt


let private setDecorations () =
    let failed fn =
        getTests TestState.Failed
        |> List.filter (fun n -> n.FileName = fn && not n.List)
        |> List.map (fun n -> Range.ToCodeRange n.Range)
        |> ResizeArray

    let passed fn =
        getTests TestState.Passed
        |> List.filter (fun n -> n.FileName = fn && not n.List)
        |> List.map (fun n -> Range.ToCodeRange n.Range )
        |> ResizeArray

    let ignored fn =
        getTests TestState.Ignored
        |> List.filter (fun n -> n.FileName = fn && not n.List)
        |> List.map (fun n -> Range.ToCodeRange n.Range )
        |> ResizeArray

    let notRun fn =
        getTests TestState.NotRun
        |> List.filter (fun n -> n.FileName = fn && not n.List)
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
    |> Seq.tryFind (fun dt -> dt.ShouldHandleFile document && dt.RunOnFileEdit document)
    |> Option.iter (fun dt ->
        dt.GetTestsForFile document
        |> Promise.onSuccess (handle)
        |> ignore
    )

let parseTextDocumentSave (document : TextDocument) =
    detectorRegister.Values
    |> Seq.tryFind (fun dt -> dt.ShouldHandleFile document && dt.RunOnFileSave document)
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
    |> Seq.toArray
    |> Array.groupBy (fun testFromResults ->
        let name = testFromResults.FullName.Trim( '"', ' ', '\\', '/')
        let name =
            if testFromResults.Runner = "VSTest" then
               name.Replace('/', '.').Replace('\\', '.').Replace('+', '.')
            else
                name

        if testFromResults.Runner = "VSTest" then
            let m = Regex.Match(name, "(.*)<.*>(\(.*\))")
            if m.Success then
                m.Groups.[1].Value
            else
                name
        else
            name
    )
    |> Array.iter (fun (name,tests) ->
        let foundTests =
            tsts
            |> Seq.where (fun testFromList ->
                let t = tests |> Seq.head

                let tName = testFromList.FullName.Trim( '"', ' ', '\\', '/')
                let tName =
                    if t.Runner = "VSTest" then
                        tName.Replace('/', '.').Replace('\\', '.')
                    else
                        tName
                match t.FileName with
                | None -> tName = name
                | Some fn ->
                    tName = name && fn = testFromList.FileName ) |> Seq.toArray

        match foundTests with
        | [||] -> ()
        | xs ->
            xs
            |> Array.iter (fun tst ->
                if tests.Length > 1 then
                    let state =
                        if tests |> Array.exists (fun t -> t.State = TestState.Failed) then TestState.Failed
                        elif tests |> Array.exists (fun t -> t.State = TestState.Ignored) then TestState.Ignored
                        elif tests |> Array.exists (fun t -> t.State = TestState.NotRun) then TestState.NotRun
                        else TestState.Passed

                    tst.State <- state
                    tst.Timer <- ""
                    tst.ErrorMessage <-
                        tests
                        |> Array.where  (fun t -> t.State = TestState.Failed)
                        |> Array.map (fun t -> t.ErrorMessage)
                        |> String.concat "\n---\n"
                    tst.HasMultipleCases <- true
                    let chlds =
                        tests
                        |> Array.map (fun n ->
                            let name =
                                let m = Regex.Match(n.FullName, "(.*)<.*>(\(.*\))")
                                tst.Name + " " + m.Groups.[2].Value
                            {
                                Name = name
                                FullName = n.FullName
                                FileName = tst.FileName
                                ErrorMessage = n.ErrorMessage
                                Childs = [||]
                                List = false
                                Id = tst.Id + 3000000
                                Type = tst.Type
                                HasMultipleCases = false
                                Range = tst.Range
                                State = n.State
                                Timer = n.Timer
                                IsSubCase = true
                            })
                    tst.Childs <- chlds
                else
                    let t = tests.[0]
                    tst.State <- t.State
                    tst.Timer <- t.Timer
                    tst.ErrorMessage <- t.ErrorMessage
                    tst.HasMultipleCases <- false
            )
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
                elif display = 1 && node.HasMultipleCases then
                    node.Childs
                    |> ResizeArray
                elif display = 2 then
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

                elif display = 1 then
                    [
                        {emptyModel with Name = "Passed"; Childs = getTests TestState.Passed |> Array.ofList; List = true }
                        {emptyModel with Name = "Failed"; Childs = getTests TestState.Failed |> Array.ofList; List = true }
                        {emptyModel with Name = "Ignored"; Childs = getTests TestState.Ignored |> Array.ofList; List = true }
                        {emptyModel with Name = "Not Run"; Childs = getTests TestState.NotRun |> Array.ofList; List = true }
                    ]
                    |> ResizeArray
                else
                    detectorRegister.Values
                    |> Seq.collect (fun n -> n.GetProjectList () )
                    |> Seq.map(fun proj ->
                        let childs =
                            proj.Files
                            |> Seq.choose (fun n ->
                                match tests.TryGetValue(n) with
                                | true, models -> Some (n,models)
                                | _ -> None
                            )
                            |> Seq.map (fun (name, tests) ->
                                let name = Path.basename name
                                {emptyModel with Name = name; List = true; Childs = tests}
                            )
                            |> Seq.toArray
                        let name = Path.basename proj.Project
                        { emptyModel with Name = name; List = true; Childs = childs}


                    )
                    |> ResizeArray



        member __.getTreeItem(node) =
            let ti = createEmpty<TreeItem>
            ti.label <- node.Name + (if node.Timer <> "" then sprintf " (%s)" node.Timer else "")
            ti.collapsibleState <-
                if node.List || node.HasMultipleCases then
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
                elif node.IsSubCase then
                    None
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
            |> Seq.where (fun t -> t.FileName = doc.fileName && not t.IsSubCase)
            |> Seq.collect (fun t ->
                let runner = runnerRegister.[t.Type]
                let detector = detectorRegister.[doc.languageId]
                match detector.RunOnFileEdit doc with
                | false -> []
                | true ->
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



let activate selector (context: ExtensionContext) (reporter : IReporter) =
    workspace.onDidChangeTextDocument.Invoke(fun te -> parseTextDocument te.document |> unbox) |> context.subscriptions.Add
    workspace.onDidSaveTextDocument.Invoke(fun te -> parseTextDocumentSave te |> unbox) |> context.subscriptions.Add

    commands.registerCommand("neptune.testExplorer.goTo", Func<obj, obj>(fun n ->
        let entry =
            if JS.isDefined n then
                reporter.sendTelemetryEvent "GoTo" undefined undefined
                Promise.lift <| unbox<TreeModel> n
            else
                reporter.sendTelemetryEvent "GoTo/Palette" undefined undefined
                let tests =
                    flattedTests ()
                    |> Seq.map (fun n ->
                        let qpi = createEmpty<QuickPickItem>
                        qpi.label <- n.Name
                        qpi?data <- n
                        qpi
                    )
                    |> ResizeArray

                window.showQuickPick(U2.Case1 tests)
                |> Promise.map (fun n -> n?data |> unbox<TreeModel>)
        entry
        |> Promise.map(fun entry ->
            let line = entry.Range.StartLine - 1
            let uri = Uri.file entry.FileName
            workspace.openTextDocument(uri)
            |> Promise.map (fun td ->
                window.showTextDocument td
                |> Promise.map (fun te ->
                    te.revealRange (Range(float line, 0., float line, 0.), TextEditorRevealType.InCenter)))
        )
        |> unbox

    )) |> context.subscriptions.Add

    commands.registerCommand("neptune.runList", Func<obj, obj>(fun m ->
        withProgress (fun msgHandler ->
            let m =
                if JS.isDefined m then
                    reporter.sendTelemetryEvent "RunList/Activate" undefined undefined
                    Promise.lift <| unbox<TreeModel> m
                else
                    reporter.sendTelemetryEvent "RunList/Activate/Palette" undefined undefined

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
                    msgHandler |> report startingMsg
                    runnerRegister.Values
                    |> Seq.where (fun r -> r.ShouldProjectBeRun prj)
                    |> Promise.collect (fun r ->
                        r.RunList msgHandler (prj, m.FullName.Trim( '"', ' ', '\\', '/'))
                    )
                    |> Promise.onSuccess (fun n ->
                        msgHandler |> report completedMsg
                        handleTestResults n
                    )
                    |> Promise.onFail (fun n ->
                        msgHandler |> report failedRunMsg
                        window.showErrorMessage (sprintf "Running test failed - %O" n)
                        |> ignore

                        ()
                    )
            )
        )
    )) |> context.subscriptions.Add

    commands.registerCommand("neptune.debugList", Func<obj, obj>(fun m ->
        withProgress (fun msgHandler ->
            let m =
                if JS.isDefined m then
                    reporter.sendTelemetryEvent "DebugList/Activate" undefined undefined
                    Promise.lift <| unbox<TreeModel> m
                else
                    reporter.sendTelemetryEvent "DebugList/Activate/Palette" undefined undefined
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
                    |> Seq.where (fun r -> r.ShouldProjectBeRun prj)
                    |> Promise.collect (fun r ->
                        r.DebugList msgHandler (prj, m.FullName.Trim( '"', ' ', '\\', '/'))
                    )
                    |> Promise.onSuccess (fun n ->
                        msgHandler |> report completedMsg
                        handleTestResults n
                    )
                    |> Promise.onFail (fun n ->
                        msgHandler |> report failedRunMsg
                        window.showErrorMessage (sprintf "Running test failed - %O" n)
                        |> ignore

                        ()
                    )
            )
        )
    )) |> context.subscriptions.Add

    commands.registerCommand("neptune.runTest", Func<obj, obj>(fun m ->
        withProgress (fun msgHandler ->
            let m =
                if JS.isDefined m then
                    reporter.sendTelemetryEvent "RunTest/Activate" undefined undefined
                    Promise.lift <| unbox<TreeModel> m
                else
                    reporter.sendTelemetryEvent "RunTest/Activate/Palette" undefined undefined
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
                    msgHandler |> report startingMsg
                    let projectsWithTests = [prj, [m.FullName.Trim( '"', ' ', '\\', '/') ] ]
                    runnerRegister.Values
                    |> Promise.collect (fun r ->
                        let prjsWithTsts = projectsWithTests |> List.filter (fun (p,_) -> r.ShouldProjectBeRun p)
                        match prjsWithTsts with
                        | [] -> Promise.lift []
                        | xs ->  r.RunTests msgHandler xs
                    )
                    |> Promise.onSuccess (fun n ->
                        msgHandler |> report completedMsg
                        handleTestResults n
                    )
                    |> Promise.onFail (fun n ->
                        msgHandler |> report failedRunMsg
                        window.showErrorMessage (sprintf "Running test failed - %O" n)
                        |> ignore

                        ()
                    )
            )
        )
    )) |> context.subscriptions.Add

    commands.registerCommand("neptune.debugTest", Func<obj, obj>(fun m ->
        withProgress (fun msgHandler ->
            let m =
                if JS.isDefined m then
                    reporter.sendTelemetryEvent "DebugTest/Activate" undefined undefined
                    Promise.lift <| unbox<TreeModel> m
                else
                    reporter.sendTelemetryEvent "DebugTest/Activate/Palette" undefined undefined
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
                msgHandler |> report startingMsg
                match getProjectForFile m.FileName with
                | None -> undefined
                | Some prj ->
                    let projectsWithTests = [prj, [m.FullName.Trim( '"', ' ', '\\', '/') ] ]
                    runnerRegister.Values
                    |> Promise.collect (fun r ->
                        let prjsWithTsts = projectsWithTests |> List.filter (fun (p,_) -> r.ShouldProjectBeRun p)
                        match prjsWithTsts with
                        | [] -> Promise.lift []
                        | xs ->  r.DebugTests msgHandler xs
                    )
                    |> Promise.onSuccess (fun n ->
                        msgHandler |> report completedMsg
                        handleTestResults n
                    )
                    |> Promise.onFail (fun n ->
                        msgHandler |> report failedRunMsg
                        window.showErrorMessage (sprintf "Running test failed - %O" n)
                        |> ignore

                        ()
                    )
            )
        )
    )) |> context.subscriptions.Add


    commands.registerCommand("neptune.runAll", Func<obj, obj>(fun _ ->
        reporter.sendTelemetryEvent "RunAll/Activate" undefined undefined
        withProgress (fun msgHandler ->
            let projects = getProjectList ()
            msgHandler |> report startingMsg
            runnerRegister.Values
            |> Promise.collect (fun r ->
                let prjs = projects |> List.filter r.ShouldProjectBeRun
                match prjs with
                | [] -> Promise.lift []
                | prjs -> r.RunAll msgHandler prjs )
            |> Promise.onSuccess (fun n ->
                msgHandler |> report completedMsg
                handleTestResults n
            )
            |> Promise.onFail (fun n ->
                msgHandler |> report failedRunMsg
                window.showErrorMessage (sprintf "Running test failed - %O" n)
                |> ignore

                ()
            )
        )
    )) |> context.subscriptions.Add

    commands.registerCommand("neptune.debugAll", Func<obj, obj>(fun _ ->
        reporter.sendTelemetryEvent "DebugAll/Activate" undefined undefined
        withProgress (fun msgHandler ->
            let projects = getProjectList ()
            msgHandler |> report startingMsg
            runnerRegister.Values
            |> Promise.collect (fun r ->
                let prjs = projects |> List.filter r.ShouldProjectBeRun
                match prjs with
                | [] -> Promise.lift []
                | prjs -> r.DebugAll msgHandler prjs )
            |> Promise.onSuccess (fun n ->
                msgHandler |> report completedMsg
                handleTestResults n
            )
            |> Promise.onFail (fun n ->
                msgHandler |> report failedRunMsg
                window.showErrorMessage (sprintf "Running test failed - %O" n)
                |> ignore

                ()
            )
        )
    )) |> context.subscriptions.Add

    commands.registerCommand("neptune.runFailed", Func<obj, obj>(fun _ ->
        reporter.sendTelemetryEvent "RunFailed/Activate" undefined undefined
        withProgress (fun msgHandler ->
            let projectsWithTests =
                getTests TestState.Failed
                |> List.choose (fun t -> getProjectForFile t.FileName |> Option.map (fun p -> p, t))
                |> List.groupBy fst
                |> List.map (fun (p, lst) -> p, (lst |> List.map (fun (_, test) -> test.FullName.Trim( '"', ' ', '\\', '/') )) )

            msgHandler |> report startingMsg
            runnerRegister.Values
            |> Promise.collect (fun r ->
                let prjsWithTsts = projectsWithTests |> List.filter (fun (p,_) -> r.ShouldProjectBeRun p)
                match prjsWithTsts with
                | [] -> Promise.lift []
                | xs ->  r.RunTests msgHandler xs
            )
            |> Promise.onSuccess (fun n ->
                msgHandler |> report completedMsg
                handleTestResults n
            )
            |> Promise.onFail (fun n ->
                msgHandler |> report failedRunMsg
                window.showErrorMessage (sprintf "Running test failed - %O" n)
                |> ignore

                ()
            )
        )
    )) |> context.subscriptions.Add

    commands.registerCommand("neptune.debugFailed", Func<obj, obj>(fun _ ->
        reporter.sendTelemetryEvent "DebugFailed/Activate" undefined undefined
        withProgress (fun msgHandler ->
            let projectsWithTests =
                getTests TestState.Failed
                |> List.choose (fun t -> getProjectForFile t.FileName |> Option.map (fun p -> p, t))
                |> List.groupBy fst
                |> List.map (fun (p, lst) -> p, (lst |> List.map (fun (_, test) -> test.FullName.Trim( '"', ' ', '\\', '/') )) )

            msgHandler |> report startingMsg
            runnerRegister.Values
            |> Promise.collect (fun r ->
                let prjsWithTsts = projectsWithTests |> List.filter (fun (p,_) -> r.ShouldProjectBeRun p)
                match prjsWithTsts with
                | [] -> Promise.lift []
                | xs ->  r.DebugTests msgHandler xs
            )
            |> Promise.onSuccess (fun n ->
                msgHandler |> report completedMsg
                handleTestResults n
            )
            |> Promise.onFail (fun n ->
                msgHandler |> report failedRunMsg
                window.showErrorMessage (sprintf "Running test failed - %O" n)
                |> ignore

                ()
            )
        )
    )) |> context.subscriptions.Add

    commands.registerCommand("neptune.changeDisplayMode", Func<obj, obj>(fun _ ->
        reporter.sendTelemetryEvent "ChangeDisplayMode" undefined undefined
        if display = 2 then display <- 0 else display <- display + 1
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

