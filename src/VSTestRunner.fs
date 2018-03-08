module VSTestRunner

open Model
open TestExplorer
open Ionide.VSCode.Helpers
open Fable.Core.JsInterop
open Fable
open Fable.Core
open Fable.Import.vscode

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
    { new TestExplorer.ITestRunner with
        member __.GetTypeName() = "VSTest"
        member __.ShouldProjectBeRun proj = proj.References |> List.exists (fun r -> r.EndsWith "nunit.framework.dll" || r.EndsWith "xunit.assert.dll" )
        member __.RunAll projs =
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
                    | [ (_, projs) ] -> runAllTests projs [||]
                    | (_, projs)::xs ->
                        xs |> List.fold (fun acc (_,e) -> acc |> Promise.bind (fun acc -> runAllTests e acc)) (runAllTests projs [||])

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
                            runSomeTests projs tests [||]
                        | (_, projs)::xs ->
                            xs |> List.fold (fun acc (_,e) -> acc |> Promise.bind (fun acc -> runSomeTests e (getTests e) acc)) (runSomeTests projs (getTests projs) [||])

                    results
                    |> Promise.map (Array.toList)
                )
            )

        member __.RunList projAndList =
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
                            runSomeTests projs tests [||]
                        | (_, projs)::xs ->
                            xs |> List.fold (fun acc (_,e) -> acc |> Promise.bind (fun acc -> runSomeTests e (tests) acc)) (runSomeTests projs (tests) [||])

                    results
                    |> Promise.map (Array.toList)
                )
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

let activate api =
    registerTestRunner "VSTest" (createRunner api)