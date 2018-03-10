module FSharpTestDetector

open Fable.Import.vscode
open Fable.Import.Node
open System.Collections.Generic
open Model
open Ionide.VSCode.Helpers
open Utils

let state = Dictionary<string, Project>()

let detector =
    { new ITestDetector with
        member __.ShouldHandleFile (doc: TextDocument) =
            match doc with
            | Document.FSharp -> true
            | _ -> false

        member __.ShouldHandleFilePath (path : string) =
            let ext = Path.extname path
            ext = ".fs" || ext = ".fsx"

        member __.ShouldHandleProject pr =
            Path.extname pr.Project = ".fsproj"

        member __.GetProjectList () =
            state.Values |> Seq.toList

        member __.GetProjectForFile path =
            state.Values
            |> Seq.tryFind (fun pr -> pr.Files |> List.contains path)

        member __.GetTestsForFile doc =
            let txt = doc.getText()
            let request = {ParseRequest.Content = txt; FileName = doc.fileName }
            LanguageService.parseRequest request

        member __.GetTestsForProject pr =
            let request = {ProjectRequest.FileName = pr.Project; Files = List.toArray pr.Files }
            LanguageService.projectRequest request
            |> Promise.map (fun n -> n.Data)
    }

let activate (context: ExtensionContext)  =
    let ext = extensions.getExtension<Api> "Ionide.Ionide-fsharp"

    LanguageService.start ()
    |> Promise.map (fun _ ->
        let api = ext.exports
        api.ProjectLoadedEvent.Invoke(fun pr ->
            state.[pr.Project] <- pr
            TestExplorer.parseProject pr
            |> unbox) |> context.subscriptions.Add
        TestExplorer.registerTestDetector "fsharp" detector
        api
    )