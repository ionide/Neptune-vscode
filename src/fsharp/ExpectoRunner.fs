module ExpectoRunner

open System
open Utils
open Model
open TestExplorer
open Ionide.VSCode.Helpers
open System.Text.RegularExpressions
open Fable.Import.Node
open Fable.Core.JsInterop
open Fable.Import
open Utils

let private lastOutput = Collections.Generic.Dictionary<string,string>()
let private outputChannel = Fable.Import.vscode.window.createOutputChannel "Neptune (F# - Expecto Adapter)"

let private trySkip count seq =
    let mutable index = 0
    seq
    |> Seq.skipWhile (fun _ ->
        index <- index + 1
        index <= count)

let private parseTestSummaryRecord (n : string) =
    let split = n.Split ('[')
    let loc = split.[1] |> String.replace "]" ""
    split.[0], loc

let private getFailed () =
    try
        lastOutput
        |> Seq.collect (fun kv ->
            kv.Value.Split('\n')
            |> Seq.map(String.trim)
            |> Seq.skipWhile (not << String.startWith "Failed:")
            |> Seq.filter (not << String.startWith "Failed:")
            |> Seq.filter (not << String.startWith "Errored:")
            |> Seq.filter (not << String.IsNullOrWhiteSpace)
            |> Seq.map (parseTestSummaryRecord)
        )
        |> Seq.map(fun (n,_loc) -> if n.Contains " " then sprintf "\"%s\"" n else n)
        |> Seq.toArray
    with
    | _ -> [||]

let private getPassed () =
    try
        lastOutput
        |> Seq.collect (fun kv ->
            kv.Value.Split('\n')
            |> Seq.map(String.trim)
            |> Seq.skipWhile (not << String.startWith "Passed:")
            |> Seq.takeWhile (not << String.startWith "Ignored:")
            |> trySkip 1
            |> Seq.map (parseTestSummaryRecord)
        )
        |> Seq.map(fun (n,_loc) -> if n.Contains " " then sprintf "\"%s\"" n else n)
        |> Seq.toArray
    with
    | _ -> [||]

let private getIgnored () =
    try
        lastOutput
        |> Seq.collect (fun kv ->
            kv.Value.Split('\n')
            |> Seq.map(String.trim)
            |> Seq.skipWhile (not << String.startWith "Ignored:")
            |> Seq.takeWhile (not << String.startWith "Failed:")
            |> trySkip 1
            |> Seq.map (parseTestSummaryRecord)
        )
        |> Seq.map(fun (n,_loc) -> if n.Contains " " then sprintf "\"%s\"" n else n)
        |> Seq.toArray
    with
    | _ -> [||]

let private getTimers () =
    try
        lastOutput
        |> Seq.collect (fun kv ->
            kv.Value.Split('\n')
            |> Seq.choose (fun l ->
                let m = Regex.Match(l, """\[.*\] (.*) (passed|failed) in (.*)\.""")
                if m.Success then
                    Some (m.Groups.[1].Value, m.Groups.[3].Value)
                else
                    None
                ))
        |> Seq.toArray

    with
    | _ -> [||]

let getErrors () =
    try
        lastOutput
        |> Seq.collect (fun kv ->

            let output = kv.Value.Split('\n')
            let errors =
                output
                |> Seq.mapi (fun id n -> (id,n))
                |> Seq.choose (fun (id, l) ->
                    let m = Regex.Match(l, """\[.*\ ERR] (.*) failed""")
                    if m.Success then
                        Some (id, m.Groups.[1].Value)
                    else
                        None
                    )
                |> Seq.toArray
            errors
            |> Seq.map (fun (id, name) ->
                let error =
                    output
                    |> Seq.skip (id + 1)
                    |> Seq.takeWhile (fun n -> not (String.IsNullOrWhiteSpace n || n.StartsWith "[") )
                    |> Seq.toArray
                let error =
                    error
                    |> Seq.take ((error |> Seq.length) - 1)
                    |> String.concat "\n"

                name, error
            )
        )
        |> Seq.toArray
    with
    | _ -> [||]

let buildProjs api projs =
    projs
    |> List.iter (fun n ->
        match n.Info with
        | ProjectResponseInfo.DotnetSdk z when z.TargetFrameworkIdentifier <> ".NETFramework" ->
            let name = Path.basename(n.Project)
            let targPath = Path.join(Path.dirname n.Project, "obj", name + ".neptune.targets")
            let content = targetFileContent (Path.join(pluginPath, "bin_coverlet"))
            Fs.writeFileSync(targPath, content)

        | _ ->()
    )

    projs
    |> List.fold (fun p proj ->  p |> Promise.bind (fun code ->
        if code = "1" then Promise.reject "Build failed"
        else
            let setting = Configuration.get false "Neptune.fastBuild"
            if setting then api.BuildProjectFast proj else api.BuildProject proj
    )) (Promise.lift "")
    |> Promise.bind (fun code ->
        if code = "1" then Promise.reject "Build failed"
        else Promise.lift ""
    )
    |> Promise.onSuccess(fun _ ->
        projs
        |> List.iter (fun n ->
            match n.Info with
            | ProjectResponseInfo.DotnetSdk z when z.TargetFrameworkIdentifier <> ".NETFramework" ->
                let name = Path.basename(n.Project)
                let targPath = Path.join(Path.dirname n.Project, "obj", name + ".neptune.targets")
                Fs.unlinkSync(!!targPath)
            | _ -> ()
        )
    )
    |> Promise.onFail(fun _ ->
        projs
        |> List.iter (fun n ->
            match n.Info with
            | ProjectResponseInfo.DotnetSdk z when z.TargetFrameworkIdentifier <> ".NETFramework" ->
                let name = Path.basename(n.Project)
                let targPath = Path.join(Path.dirname n.Project, "obj", name + ".neptune.targets")
                Fs.unlinkSync(!!targPath)
            | _ -> ()
        )
    )

let runExpectoProject (api : Api) project args =
    match api.GetProjectLauncher outputChannel project with
    | None -> Promise.lift ""
    | Some launcher ->
        promise {
            let exe = project.Output
            let! childProcess = launcher args
            return!
                childProcess
                |> Process.onOutput (fun out -> lastOutput.[exe] <- lastOutput.[exe] + out.toString () )
                |> Process.toPromise
                |> Promise.map (fun n ->
                    string (defaultArg n.Code 0)
                )
        }


let runProjs api projsWithArgs =
    outputChannel.clear ()
    lastOutput.Clear()
    projsWithArgs
    |> Seq.map (fun (proj, arg) ->
        runExpectoProject api proj arg)
    |> Promise.all
    |> Promise.map (fun _ ->
        let timers = getTimers ()
        let errors = getErrors ()
        let tryGetTime (name : string) =
            let x = timers |> Seq.tryPick (fun (n,t) -> if n.Trim( '"', ' ', '\\', '/') = name.Trim( '"', ' ', '\\', '/') then Some (t.TrimEnd('0')) else None )
            defaultArg x ""
        let tryGetError (name : string) =
            let x = errors |> Seq.tryPick (fun (n,t) -> if n.Trim( '"', ' ', '\\', '/') = name.Trim( '"', ' ', '\\', '/') then Some t else None )
            defaultArg x ""


        let failed =
            getFailed ()
            |> Seq.map (fun name ->
                { FullName = name;
                  State = TestState.Failed
                  Timer = tryGetTime name
                  ErrorMessage = tryGetError name
                  Runner = "Expecto"
                  FileName = None } )
        let ignored =
            getIgnored ()
            |> Seq.map (fun name ->
                { FullName = name;
                  State = TestState.Ignored
                  Timer = ""
                  ErrorMessage = ""
                  Runner = "Expecto"
                  FileName = None  } )
        let passed =
            getPassed ()
            |> Seq.map (fun name ->
                { FullName = name;
                  State = TestState.Passed
                  Timer = tryGetTime name
                  ErrorMessage = ""
                  Runner = "Expecto"
                  FileName = None } )
        [ yield! failed; yield! ignored; yield! passed]

    )


let createRunner (api : Api) =
    { new ITestRunner with
        member __.GetTypeName() = "Expecto"
        member __.ShouldProjectBeRun proj = proj.References |> List.exists (fun r -> r.EndsWith "Expecto.dll" )
        member __.RunAll msgHandler projs =
            msgHandler |> report buildingMsg

            projs
            |> buildProjs api
            |> Promise.bind (fun _ ->
                msgHandler |> report runningMsg

                projs |> Seq.map (fun p -> p, "--summary-location --debug")
                |> runProjs api
            )
        member __.RunTests msgHandler testsByProj =
            msgHandler |> report buildingMsg

            testsByProj
            |> List.map fst
            |> buildProjs api
            |> Promise.bind (fun _ ->
                msgHandler |> report runningMsg

                testsByProj
                |> Seq.map (fun (p, tests) ->
                    let tst =
                        tests
                        |> Seq.map (fun t -> sprintf "\"%s\"" t )
                        |> String.concat " "
                    p, sprintf "--summary-location --debug --run %s" tst
                )
                |> runProjs api
            )

        member __.RunList msgHandler projAndList =
            msgHandler |> report buildingMsg

            let (proj, list) = projAndList
            [proj]
            |> buildProjs api
            |> Promise.bind (fun _ ->
                msgHandler |> report runningMsg

                let projAndArgs = [proj, sprintf "--summary-location --debug --filter \"%s\"" list ]
                projAndArgs |> List.toSeq |> runProjs api
            )
        member __.DebugList msgHandler projAndList =
            msgHandler |> report buildingMsg

            let (proj, list) = projAndList
            [proj]
            |> buildProjs api
            |> Promise.bind (fun _ ->
                msgHandler |> report runningMsg

                let args = [| "--summary-location" ; "--debug"; "--filter"; list|]
                api.DebugProject proj args)
            |> Promise.map (fun _ -> [])

        member __.DebugTests msgHandler testsByProj =
            msgHandler |> report buildingMsg

            testsByProj
            |> List.map fst
            |> buildProjs api
            |> Promise.bind (fun _ ->
                msgHandler |> report runningMsg

                testsByProj
                |> Seq.map (fun (p, tests) ->
                    let tst =
                        tests
                        |> Seq.map (fun t -> sprintf "\"%s\"" t )
                        |> String.concat " "
                    api.DebugProject p [| "--summary-location" ; "--debug"; "--run"; tst|]
                )
                |> Seq.toArray
                |> Promise.all
            )
            |> Promise.map (fun _ -> [])

        member __.DebugAll msgHandler projs =
            msgHandler |> report buildingMsg

            projs
            |> buildProjs api
            |> Promise.bind (fun _ ->
                msgHandler |> report runningMsg

                projs
                |> Seq.map (fun p ->
                    api.DebugProject p [| "--summary-location" ; "--debug"; |]
                )
                |> Seq.toArray
                |> Promise.all
            )
            |> Promise.map (fun _ -> [])

        member __.GenerateCoverage proj =
            let generatorPath = Path.join(pluginPath, "bin_coverlet", "coverlet.generator.dll")
            let instrPath = Path.join(Path.dirname proj.Output, "Instr.json")
            let args = sprintf "\"%s\" \"%s\"" generatorPath instrPath
            Process.exec "dotnet" "" args
            |> Promise.map(fun _ ->
                let coverPath = Path.join(Path.dirname proj.Output, "CovResult.json")
                let content = Fs.readFileSync(coverPath).toString()
                let json = JS.JSON.parse content
                let libs = !!JS.Object.keys(json)

                libs
                |> Array.collect (fun n ->
                    let o = json.Item n

                    let files = !!JS.Object.keys(o)

                    files
                    |> Array.collect (fun f ->
                        let o = o.Item f
                        let modules = !!JS.Object.keys(o)

                        modules
                        |> Array.collect (fun m ->
                            let o = o.Item m
                            let funcs = !!JS.Object.keys(o)

                            funcs
                            |> Array.collect (fun fn ->
                                let o = o.Item fn
                                let lines = !!JS.Object.keys(o)

                                lines
                                |> Array.map (fun l ->
                                    let o = o.Item l
                                    (f, l, o?Hits)
                                )
                            )
                        )

                    ) )

            )
            |> Promise.map (!!)

        member __.Capabilities proj =
            match proj.Info with
            | ProjectResponseInfo.DotnetSdk z when z.TargetFrameworkIdentifier = ".NETFramework" ->
                [Capability.CanRunAll; Capability.CanRunList; Capability.CanRunSingle]
            | ProjectResponseInfo.DotnetSdk _ ->
                [Capability.CanDebugAll; Capability.CanDebugList; Capability.CanDebugSingle; Capability.CanRunAll; Capability.CanRunList; Capability.CanRunSingle; Capability.CanCodeCoverage]
            | _ ->
                [Capability.CanRunAll; Capability.CanRunList; Capability.CanRunSingle]
    }

let activate api =
    registerTestRunner "Expecto" (createRunner api)