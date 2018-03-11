module VSTestRunner

open Model
open TestExplorer
open Ionide.VSCode.Helpers
open Fable.Core.JsInterop
open Fable
open Fable.Core
open Fable.Import.vscode
open Fable.Import.Node
open Utils

type [<Pojo>] RequestLaunch =
    { name: string
      ``type``: string
      request: string
      preLaunchTask: string option
      program: string
      args: string array
      cwd: string
      console: string
      stopAtEntry: bool }

let mutable storagePath = ""

let convert =  Globals.require.Invoke "xml-js" |> unbox<obj>

let buildProjs api projs =
    projs
    |> List.fold (fun p proj -> p |> Promise.bind (fun _ -> api.BuildProject proj)) Promise.empty

let getXml (projs : Project[]) =
    let p = projs.[0]
    match p.Info with
    | ProjectResponseInfo.DotnetSdk z ->
        sprintf """<?xml version="1.0" encoding="utf-8"?>
        <RunSettings>
            <RunConfiguration>
              <TargetFrameworkVersion>%s,Version=%s</TargetFrameworkVersion>
            </RunConfiguration>
        </RunSettings>""" z.TargetFrameworkIdentifier z.TargetFrameworkVersion
    | _ -> null

let findOldRunner (pr : Project) =
    let rec findFile file dir =
        let files = Fs.readdirSync (U2.Case1 dir)
        files
        |> Seq.toList
        |> List.collect(fun s' ->
            try
                let s = dir + Path.sep + s'
                if Fs.statSync(U2.Case1 s).isDirectory () then
                    findFile file (s)
                else
                   if s.EndsWith file then [ s ] else []
            with
            | _ -> []
    )

    pr.References
    |> List.tryFind (fun r -> r.EndsWith "nunit.framework.dll" || r.EndsWith "xunit.assert.dll" )
    |> Option.bind (fun r ->
        let isNunit = r.EndsWith "nunit.framework.dll"
        let dir = Path.dirname r
        let pcksIndex =
            let p1 = dir.IndexOf "Packages"
            if p1 > 0 then p1 + 8
            else dir.IndexOf "packages" + 8
        let pcksPath = dir.Substring(0, pcksIndex)
        let runner =
            if isNunit then findFile "nunit3-console.exe" pcksPath
            else findFile "xunit.console.exe" pcksPath
        List.tryHead runner
        |> Option.map (fun n -> isNunit, n)
    )

let findId (testCase : obj) : string * obj =
    let prop =
        let psp = !!testCase?Properties
        psp |> Array.find (fun (n :obj) -> !!(n?Key?Id) = "TestCase.FullyQualifiedName")
    !!prop?Value, testCase

let toTestResult (testCase: obj) : TestResult =
    let name =
        let psp = !!testCase?TestCase?Properties
        psp |> Array.find (fun (n :obj) -> !!(n?Key?Id) = "TestCase.FullyQualifiedName") |> fun n -> !!n?Value

    let timer =
        let psp = !!testCase?Properties
        psp |> Array.find (fun (n :obj) -> !!(n?Key?Id) = "TestResult.Duration") |> fun n -> !!n?Value

    let state : int =
        let psp = !!testCase?Properties
        psp |> Array.find (fun (n :obj) -> !!(n?Key?Id) = "TestResult.Outcome") |> fun n -> !!n?Value

    let error =
        let psp = !!testCase?Properties
        psp |> Array.find (fun (n :obj) -> !!(n?Key?Id) = "TestResult.ErrorMessage")
        |> fun n ->
            let res = !!n?Value
            if res = null then
                ""
            else res
    let state =
        match state with
        | 0 -> TestState.NotRun
        | 1 -> TestState.Passed
        | 2 -> TestState.Failed
        | 3 -> TestState.Ignored
        | _ -> TestState.NotRun

    {FullName = name; ErrorMessage = error; State = state; Timer = timer; Runner = "VSTest" }


let discoverTests (projs : Project[]) initial =
    let xml = getXml projs
    let o =
        createObj [
            "MessageType" ==> "TestDiscovery.Start"
            "Payload" ==>
                createObj [
                    "Sources" ==> (projs |> Array.map (fun p -> p.Output))
                    "RunSettings" ==> xml
                ]
        ]
    let msg = Fable.Import.JS.JSON.stringify o
    promise {
        let! res = VSTestAdapterService.vstestRequest msg
        let res =
            res
            |> Array.collect (fun n ->
                if (!!n?MessageType = "TestDiscovery.Completed") && !!n?Payload?LastDiscoveredTests <> null then
                    !!n?Payload?LastDiscoveredTests
                    |> Array.map findId
                elif !!n?MessageType = "TestDiscovery.TestFound" then
                    !!n?Payload
                    |> Array.map findId
                else
                    [||]
            )
        return Array.append initial res
    }

let runAllTests (projs: Project[]) initial =
    let xml = getXml projs
    let o =
        createObj [
            "MessageType" ==> "TestExecution.RunAllWithDefaultHost"
            "Payload" ==>
                createObj [
                    "Sources" ==> (projs |> Array.map (fun p -> p.Output))
                    "RunSettings" ==> xml
                ]
        ]
    let msg = Fable.Import.JS.JSON.stringify o
    promise {
        let! res = VSTestAdapterService.vstestRequest msg
        let res =
            res
            |> Array.collect (fun n ->
                if !!n?MessageType = "TestExecution.Completed" && !!n?Payload?LastRunTests <> null then
                    !!n?Payload?LastRunTests?NewTestResults
                    |> Array.map toTestResult
                elif !!n?MessageType = "TestExecution.StatsChange" then
                    !!n?Payload?NewTestResults
                    |> Array.map toTestResult
                else
                    [||]
            )
        return Array.append initial res
    }


let runSomeTests (projs: Project[]) (tests: obj[]) initial =
    if tests.Length > 0 then
        let xml = getXml projs
        let o =
            createObj [
                "MessageType" ==> "TestExecution.RunAllWithDefaultHost"
                "Payload" ==>
                    createObj [
                        "Sources" ==> null
                        "TestCases" ==> tests
                        "RunSettings" ==> xml
                    ]
            ]
        let msg = Fable.Import.JS.JSON.stringify o
        promise {
            let! res = VSTestAdapterService.vstestRequest msg
            let res =
                res
                |> Array.collect (fun n ->
                    if ((!!n?MessageType = "TestExecution.Completed") && (!!n?Payload?LastRunTests <> null)) then
                        !!n?Payload?LastRunTests?NewTestResults
                        |> Array.map toTestResult
                    elif !!n?MessageType = "TestExecution.StatsChange" then
                        !!n?Payload?NewTestResults
                        |> Array.map toTestResult
                    else
                        [||]
                )
            return Array.append initial res
        }
    else
        Promise.lift [||]

let getNUnitResults () =
    let fn = Path.join(workspace.rootPath, "TestResult.xml")
    let buf = Fs.readFileSync(fn)
    let xmlCnt = buf.toString()
    let opts =
        createObj [
            "compact" ==> true
            "alwaysArray" ==> true
        ]
    let rec collectTestCases o =
        let current = if !!o?``test-case`` <> Utils.undefined then unbox<obj[]>(o?``test-case``) else [||]
        let testSuites = if !!o?``test-suite`` <> Utils.undefined then unbox<obj[]>(o?``test-suite``) else [||]
        Array.append current (testSuites |> Array.collect collectTestCases)

    let res = convert?xml2js(xmlCnt, opts)
    let res = unbox<obj[]>(res?``test-run``).[0]
    collectTestCases res

let nUnitOldResultToTestResult obj =
    let name = !!obj?_attributes?fullname
    let timer = !!obj?_attributes?duration
    let result = !!obj?_attributes?result
    let state =
        match result with
        | "Passed" -> TestState.Passed
        | "Failed" | "Inconclusive" -> TestState.Failed
        | "Skipped" -> TestState.Ignored
        | _ -> TestState.NotRun
    let error =
        try
            let error = (unbox<obj[]>obj?failure).[0]
            let r = (unbox<obj[]>error?message).[0]
            !!(unbox<obj[]> r?``_cdata``).[0]
        with
        | _ -> ""
    {FullName = name; ErrorMessage = error; State = state; Timer = timer; Runner = "VSTest" }



let runAllTestsWithOldRunner (proj: Project) initial =
    match findOldRunner proj with
    | None -> Promise.lift initial
    | Some (isNUnit, runner) ->
        let args =
            if isNUnit then "\"" + proj.Output + "\""
            else
                "\"" + proj.Output + "\""
                + " -nunit"
        Process.spawn runner "mono" args
        |> Process.toPromise
        |> Promise.map (fun _ ->
            let res = getNUnitResults ()
            let res = res |> Array.map nUnitOldResultToTestResult
            Array.append initial res
        )


let runSomeTestsWithOldRunner (proj: Project) (tests: string[]) initial : Fable.Import.JS.Promise<TestResult[]> =
    match findOldRunner proj with
    | None -> Promise.lift initial
    | Some (isNUnit, runner) ->
        let args =
            if isNUnit then
                "\"" + proj.Output + "\""
                + " --test=" + (tests |> Array.map (fun n -> n.Trim( '"', ' ', '\\', '/').Replace('/', '.').Replace('\\', '.').Replace("this.", "")) |> String.concat ",")
            else
                "\"" + proj.Output  + "\""
                + " " + (tests |> Array.map (fun n -> "-method \"" + n.Trim( '"', ' ', '\\', '/').Replace('/', '.').Replace('\\', '.').Replace("this.", "") + "\"") |> String.concat " ")
                + " -nunit"
        Process.spawn runner "mono" args
        |> Process.toPromise
        |> Promise.map (fun _ ->
            let res = getNUnitResults ()
            let res = res |> Array.map nUnitOldResultToTestResult
            Array.append initial res
        )

let runListTestsWithOldRunner (proj: Project) (list : string) initial =
    match findOldRunner proj with
    | None -> Promise.lift initial
    | Some (isNUnit, runner) ->
        let list = list.Trim( '"', ' ', '\\', '/').Replace('/', '.').Replace('\\', '.').Replace("this.", "")
        let args =
            if isNUnit then
                "\"" + proj.Output + "\""
                + " --where \"class == " + list + " || namespace == " + list + "\""
            else
                "\"" + proj.Output + "\""
                + " -namespace \"" + list + "\"" + " -class \"" + list + "\""
                + " -nunit"
        Process.spawn runner "mono" args
        |> Process.toPromise
        |> Promise.map (fun _ ->
            let res = getNUnitResults ()
            let res = res |> Array.map nUnitOldResultToTestResult
            Array.append initial res
        )

let debugAllTests (projs: Project[]) initial =

    let xml = getXml projs
    let o =
        createObj [
            "MessageType" ==> "TestExecution.GetTestRunnerProcessStartInfoForRunAll"
            "Payload" ==>
                createObj [
                    "Sources" ==> (projs |> Array.map (fun p -> p.Output))
                    "RunSettings" ==> xml
                    "DebuggingEnabled" ==> true
                ]
        ]
    let msg = Fable.Import.JS.JSON.stringify o
    promise {
        let! res = VSTestAdapterService.vstestRequest msg
        let res = res.[0]
        let fn = !!res?Payload?FileName
        let args = !!res?Payload?Arguments
        let cwd = !!res?Payload?WorkingDirectory
        let debugerCnf =
            {
                name = "VSTest Debug"
                ``type`` = "coreclr"
                request = "launch"
                preLaunchTask = None
                program = fn
                args = args
                cwd = cwd
                console = "integratedTerminal"
                stopAtEntry = false
            }
        let folder = workspace.workspaceFolders.[0]
        let! _ = debug.startDebugging(folder, !!debugerCnf)
        let! id =
            Promise.create(fun resolve error ->
                debug.onDidReceiveDebugSessionCustomEvent.Invoke(!!(fun a ->
                    if !!(a?body?systemProcessId) <> null then
                        resolve a?body?systemProcessId
                |> ignore
                ))|> ignore )
        let o =
            createObj [
                "MessageType" ==> "TestExecution.CustomTestHostLaunchCallback"
                "Payload" ==>
                    createObj [
                        "HostProcessId" ==> id
                        "ErrorMessage" ==> ""
                    ]
            ]
        let msg = Fable.Import.JS.JSON.stringify o
        let! res = VSTestAdapterService.vstestRequest msg

        let res =
            res
            |> Array.collect (fun n ->
                if ((!!n?MessageType = "TestExecution.Completed") && (!!n?Payload?LastRunTests <> null)) then
                    !!n?Payload?LastRunTests?NewTestResults
                    |> Array.map toTestResult
                elif !!n?MessageType = "TestExecution.StatsChange" then
                    !!n?Payload?NewTestResults
                    |> Array.map toTestResult
                else
                    [||]
            )
        return Array.append initial res
    }

let debugSomeTests (projs: Project[]) tests initial =

    let xml = getXml projs
    let o =
        createObj [
            "MessageType" ==> "TestExecution.GetTestRunnerProcessStartInfoForRunAll"
            "Payload" ==>
                createObj [
                    "Sources" ==> null
                    "TestCases" ==> tests
                    "RunSettings" ==> xml
                    "DebuggingEnabled" ==> true
                ]
        ]
    let msg = Fable.Import.JS.JSON.stringify o
    promise {
        let! res = VSTestAdapterService.vstestRequest msg
        let res = res.[0]
        let fn = !!res?Payload?FileName
        let args = !!res?Payload?Arguments
        let cwd = !!res?Payload?WorkingDirectory
        let debugerCnf =
            {
                name = "VSTest Debug"
                ``type`` = "coreclr"
                request = "launch"
                preLaunchTask = None
                program = fn
                args = args
                cwd = cwd
                console = "integratedTerminal"
                stopAtEntry = false
            }
        let folder = workspace.workspaceFolders.[0]
        let! _ = debug.startDebugging(folder, !!debugerCnf)
        let! id =
            Promise.create(fun resolve error ->
                debug.onDidReceiveDebugSessionCustomEvent.Invoke(!!(fun a ->
                    if !!(a?body?systemProcessId) <> null then
                        resolve a?body?systemProcessId
                |> ignore
                ))|> ignore )
        let o =
            createObj [
                "MessageType" ==> "TestExecution.CustomTestHostLaunchCallback"
                "Payload" ==>
                    createObj [
                        "HostProcessId" ==> id
                        "ErrorMessage" ==> ""
                    ]
            ]
        let msg = Fable.Import.JS.JSON.stringify o
        let! res = VSTestAdapterService.vstestRequest msg

        let res =
            res
            |> Array.collect (fun n ->
                if ((!!n?MessageType = "TestExecution.Completed") && (!!n?Payload?LastRunTests <> null)) then
                    !!n?Payload?LastRunTests?NewTestResults
                    |> Array.map toTestResult
                elif !!n?MessageType = "TestExecution.StatsChange" then
                    !!n?Payload?NewTestResults
                    |> Array.map toTestResult
                else
                    [||]
            )
        return Array.append initial res
    }

let createRunner (api : Api) =
    { new ITestRunner with
        member __.GetTypeName() = "VSTest"
        member __.ShouldProjectBeRun proj = proj.References |> List.exists (fun r -> r.EndsWith "nunit.framework.dll" || r.EndsWith "xunit.assert.dll" )
        member __.RunAll projs =
            projs
            |> buildProjs api
            |> Promise.bind (fun _ ->
                let newProjs, oldProjs =
                    projs
                    |> Seq.toArray
                    |> Array.partition (fun p -> match p.Info with | ProjectResponseInfo.DotnetSdk _ -> true | _ -> false)
                let newResult =
                    match newProjs with
                    | [||] -> Promise.lift [||]
                    | newProjs ->
                        let outs =
                            newProjs
                            |> Array.groupBy (fun p -> match p.Info with | ProjectResponseInfo.DotnetSdk z -> z.TargetFramework | _ -> "" )
                            |> Array.toList
                        let results =
                            match outs with
                            | [] -> Promise.empty
                            | [ (_, projs) ] -> runAllTests projs [||]
                            | (_, projs)::xs ->
                                xs |> List.fold (fun acc (_,e) -> acc |> Promise.bind (fun acc -> runAllTests e acc)) (runAllTests projs [||])

                        results
                let results =
                    match oldProjs with
                    | [||] -> newResult
                    | oldProjs ->
                        oldProjs |> Array.fold (fun acc e -> acc |> Promise.bind (fun acc -> runAllTestsWithOldRunner e acc)) newResult

                results
                |> Promise.map (Array.toList)
            )

        member __.RunTests testsByProj =
            let proj =
                testsByProj
                |> List.map fst

            proj
            |> buildProjs api
            |> Promise.bind (fun _ ->
                let newProjs, oldProjs =
                    proj
                    |> Seq.toArray
                    |> Array.partition (fun p -> match p.Info with | ProjectResponseInfo.DotnetSdk _ -> true | _ -> false)

                let newResult =
                    match newProjs with
                    | [||] -> Promise.lift [||]
                    | newProjs ->
                        let outs =
                            newProjs
                            |> Array.groupBy (fun p -> match p.Info with | ProjectResponseInfo.DotnetSdk z -> z.TargetFramework | _ -> "" )
                            |> Array.toList

                        let vstest =
                            match outs with
                            | [] -> Promise.empty
                            | [ (_, projs) ] -> discoverTests projs [||]
                            | (_, projs)::xs ->
                                xs |> List.fold (fun acc (_,e) -> acc |> Promise.bind (fun acc -> discoverTests e acc)) (discoverTests projs [||])
                        vstest
                        |> Promise.bind (fun n ->
                            let results =
                                let getTests (ps : Project []) =
                                    let names =
                                        testsByProj
                                        |> List.collect (fun (p, ts) -> if ps |> Array.contains p then ts else [])
                                        |> List.map (fun t -> t.Trim( '"', ' ', '\\', '/').Replace('/', '.').Replace('\\', '.').Replace("this.", "") )

                                    n
                                    |> Array.filter (fun (n, _) ->
                                        let n = n.Trim( '"', ' ', '\\', '/').Replace('/', '.').Replace('\\', '.')
                                        names |> List.contains n
                                    )
                                    |> Array.map snd

                                match outs with
                                | [] -> Promise.empty
                                | [ (_, projs) ] ->
                                    let tests = getTests projs
                                    runSomeTests projs tests [||]
                                | (_, projs)::xs ->
                                    xs |> List.fold (fun acc (_,e) -> acc |> Promise.bind (fun acc -> runSomeTests e (getTests e) acc)) (runSomeTests projs (getTests projs) [||])

                            results)

                let results =
                    match oldProjs with
                    | [||] -> newResult
                    | oldProjs ->
                        oldProjs |> Array.fold (fun acc e ->
                            let (_,tests) = testsByProj |> Seq.find (fun (p,_) -> p.Project = e.Project )
                            let tests = tests |> List.toArray
                            acc |> Promise.bind (fun acc -> runSomeTestsWithOldRunner e tests acc)) newResult

                results
                |> Promise.map (Array.toList)
            )

        member __.RunList projAndList =
            let (proj, list) = projAndList

            [proj]
            |> buildProjs api
            |> Promise.bind (fun _ ->
                let newProjs, oldProjs =
                    [proj]
                    |> Seq.toArray
                    |> Array.partition (fun p -> match p.Info with | ProjectResponseInfo.DotnetSdk _ -> true | _ -> false)

                let newResult =
                    match newProjs with
                    | [||] -> Promise.lift [||]
                    | newProjs ->
                        let outs =
                            newProjs
                            |> Array.groupBy (fun p -> match p.Info with | ProjectResponseInfo.DotnetSdk z -> z.TargetFramework | _ -> "" )
                            |> Array.toList

                        let vstest =
                            match outs with
                            | [] -> Promise.empty
                            | [ (_, projs) ] -> discoverTests projs [||]
                            | (_, projs)::xs ->
                                xs |> List.fold (fun acc (_,e) -> acc |> Promise.bind (fun acc -> discoverTests e acc)) (discoverTests projs [||])
                        vstest
                        |> Promise.bind (fun n ->
                            let results =
                                let tests =
                                    n
                                    |> Array.filter (fun (n, _) ->
                                        let n = n.Trim( '"', ' ', '\\', '/').Replace('/', '.').Replace('\\', '.')
                                        let l = list.Trim( '"', ' ', '\\', '/').Replace('/', '.').Replace('\\', '.') + "."
                                        n.StartsWith l
                                    )
                                    |> Array.map snd

                                match outs with
                                | [] -> Promise.empty
                                | [ (_, projs) ] ->
                                    runSomeTests projs tests [||]
                                | (_, projs)::xs ->
                                    xs |> List.fold (fun acc (_,e) -> acc |> Promise.bind (fun acc -> runSomeTests e (tests) acc)) (runSomeTests projs (tests) [||])

                            results)

                let results =
                    match oldProjs with
                    | [||] -> newResult
                    | oldProjs ->
                        oldProjs |> Array.fold (fun acc e -> acc |> Promise.bind (fun acc -> runListTestsWithOldRunner e list acc)) newResult

                results
                |> Promise.map (Array.toList)
            )

        member __.DebugList projAndList =
            let (proj, list) = projAndList

            [proj]
            |> buildProjs api
            |> Promise.bind (fun _ ->
                let outs =
                    [proj]
                    |> Seq.toArray
                    |> Array.filter (fun p -> match p.Info with | ProjectResponseInfo.DotnetSdk _ -> true | _ -> false)
                    |> Array.groupBy (fun p -> match p.Info with | ProjectResponseInfo.DotnetSdk z -> z.TargetFramework | _ -> "" )
                    |> Array.toList

                let vstest =
                    match outs with
                    | [] -> Promise.empty
                    | [ (_, projs) ] -> discoverTests projs [||]
                    | (_, projs)::xs ->
                        xs |> List.fold (fun acc (_,e) -> acc |> Promise.bind (fun acc -> discoverTests e acc)) (discoverTests projs [||])
                vstest
                |> Promise.bind (fun n ->
                    let results =
                        let tests =
                            n
                            |> Array.filter (fun (n, _) ->
                                let n = n.Trim( '"', ' ', '\\', '/').Replace('/', '.').Replace('\\', '.')
                                let l = list.Trim( '"', ' ', '\\', '/').Replace('/', '.').Replace('\\', '.') + "."
                                n.StartsWith l
                            )
                            |> Array.map snd

                        match outs with
                        | [] -> Promise.empty
                        | [ (_, projs) ] ->
                            debugSomeTests projs tests [||]
                        | (_, projs)::xs ->
                            xs |> List.fold (fun acc (_,e) -> acc |> Promise.bind (fun acc -> debugSomeTests e (tests) acc)) (debugSomeTests projs (tests) [||])

                    results
                    |> Promise.map (Array.toList)
                )
            )

        member __.DebugAll projs =
            projs
            |> buildProjs api
            |> Promise.bind (fun _ ->
                let outs =
                    projs
                    |> Seq.toArray
                    |> Array.filter (fun p -> match p.Info with | ProjectResponseInfo.DotnetSdk _ -> true | _ -> false)
                    |> Array.groupBy (fun p -> match p.Info with | ProjectResponseInfo.DotnetSdk z -> z.TargetFramework | _ -> "" )
                    |> Array.toList
                let results =
                    match outs with
                    | [] -> Promise.empty
                    | [ (_, projs) ] -> debugAllTests projs [||]
                    | (_, projs)::xs ->
                        xs |> List.fold (fun acc (_,e) -> acc |> Promise.bind (fun acc -> debugAllTests e acc)) (debugAllTests projs [||])

                results
                |> Promise.map (Array.toList)
            )

        member __.DebugTests testsByProj =
            let proj =
                testsByProj
                |> List.map fst

            proj
            |> buildProjs api
            |> Promise.bind (fun _ ->
                let outs =
                    proj
                    |> Seq.toArray
                    |> Array.filter (fun p -> match p.Info with | ProjectResponseInfo.DotnetSdk _ -> true | _ -> false)
                    |> Array.groupBy (fun p -> match p.Info with | ProjectResponseInfo.DotnetSdk z -> z.TargetFramework | _ -> "" )
                    |> Array.toList

                let vstest =
                    match outs with
                    | [] -> Promise.empty
                    | [ (_, projs) ] -> discoverTests projs [||]
                    | (_, projs)::xs ->
                        xs |> List.fold (fun acc (_,e) -> acc |> Promise.bind (fun acc -> discoverTests e acc)) (discoverTests projs [||])
                vstest
                |> Promise.bind (fun n ->
                    let results =
                        let getTests (ps : Project []) =
                            let names =
                                testsByProj
                                |> List.collect (fun (p, ts) -> if ps |> Array.contains p then ts else [])
                                |> List.map (fun t -> t.Trim( '"', ' ', '\\', '/').Replace('/', '.').Replace('\\', '.').Replace("this.", "") )

                            n
                            |> Array.filter (fun (n, _) ->
                                let n = n.Trim( '"', ' ', '\\', '/').Replace('/', '.').Replace('\\', '.')
                                names |> List.contains n
                            )
                            |> Array.map snd

                        match outs with
                        | [] -> Promise.empty
                        | [ (_, projs) ] ->
                            let tests = getTests projs
                            debugSomeTests projs tests [||]
                        | (_, projs)::xs ->
                            xs |> List.fold (fun acc (_,e) -> acc |> Promise.bind (fun acc -> debugSomeTests e (getTests e) acc)) (debugSomeTests projs (getTests projs) [||])

                    results
                    |> Promise.map (Array.toList)
                )
            )
        member __.Capabilities proj =
            match proj.Info with
            | ProjectResponseInfo.DotnetSdk z when z.TargetFrameworkIdentifier = ".NETFramework" ->
                [Capability.CanRunAll; Capability.CanRunList; Capability.CanRunSingle]
            | ProjectResponseInfo.DotnetSdk _ ->
                [Capability.CanDebugAll; Capability.CanDebugList; Capability.CanDebugSingle; Capability.CanRunAll; Capability.CanRunList; Capability.CanRunSingle]
            | _ ->
                [Capability.CanRunAll; Capability.CanRunList; Capability.CanRunSingle]
    }

let activate api sPath =
    storagePath <- sPath
    if not (Fs.existsSync(!!sPath)) then
        Fs.mkdirSync(!!sPath)

    VSTestAdapterService.start() |> ignore
    registerTestRunner "VSTest" (createRunner api)