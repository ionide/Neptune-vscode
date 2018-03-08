module ExpectoRunner

open System
open Utils
open Model
open TestExplorer
open Ionide.VSCode.Helpers
open System.Text.RegularExpressions

let private lastOutput = Collections.Generic.Dictionary<string,string>()
let private outputChannel = Fable.Import.vscode.window.createOutputChannel "Expecto"

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
            errors
            |> Seq.map (fun (id, name) ->
                let error =
                    output
                    |> Seq.skip (id + 1)
                    |> Seq.takeWhile (String.IsNullOrWhiteSpace >> not)
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
    |> List.fold (fun p proj -> p |> Promise.bind (fun _ -> api.BuildProject proj)) Promise.empty

let runExpectoProject (api : Api) project args =
    match api.GetProjectLauncher outputChannel project with
    | None -> Promise.empty
    | Some launcher ->
        promise {
            let exe = project.Output
            let! childProcess = launcher args
            return!
                childProcess
                |> Process.onOutput (fun out -> lastOutput.[exe] <- lastOutput.[exe] + out.toString () )
                |> Process.toPromise
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
                  Runner = "Expecto" } )
        let ignored =
            getIgnored ()
            |> Seq.map (fun name ->
                { FullName = name;
                  State = TestState.Ignored
                  Timer = ""
                  ErrorMessage = ""
                  Runner = "Expecto"  } )
        let passed =
            getPassed ()
            |> Seq.map (fun name ->
                { FullName = name;
                  State = TestState.Passed
                  Timer = tryGetTime name
                  ErrorMessage = ""
                  Runner = "Expecto" } )
        [ yield! failed; yield! ignored; yield! passed]

    )


let createRunner (api : Api) =
    { new TestExplorer.ITestRunner with
        member __.GetTypeName() = "Expecto"
        member __.ShouldProjectBeRun proj = proj.References |> List.exists (fun r -> r.EndsWith "Expecto.dll" )
        member __.RunAll projs =
            projs
            |> buildProjs api
            |> Promise.bind (fun _ ->
                projs |> Seq.map (fun p -> p, "--summary-location --debug")
                |> runProjs api
            )
        member __.RunTests testsByProj =
            testsByProj
            |> List.map fst
            |> buildProjs api
            |> Promise.bind (fun _ ->
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

        member __.RunList projAndList =
            let (proj, list) = projAndList
            [proj]
            |> buildProjs api
            |> Promise.bind (fun _ ->
                let projAndArgs = [proj, sprintf "--summary-location --debug --filter \"%s\"" list ]
                projAndArgs |> List.toSeq |> runProjs api
            )
        member __.DebugList projAndList =
            let (proj, list) = projAndList
            [proj]
            |> buildProjs api
            |> Promise.bind (fun _ ->
                let args = [| "--summary-location" ; "--debug"; "--filter"; list|]
                api.DebugProject proj args)
            |> Promise.map (fun _ -> [])

        member __.DebugTests testsByProj =
            testsByProj
            |> List.map fst
            |> buildProjs api
            |> Promise.bind (fun _ ->
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

        member __.DebugAll projs =
            Promise.lift []

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
    registerTestRunner "Expecto" (createRunner api)