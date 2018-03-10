[<AutoOpen>]
module FSharpCommon
open Model

//API for communication with Ionide
type Api = {
    ProjectLoadedEvent: Fable.Import.vscode.Event<Project>
    BuildProject: Project -> Fable.Import.JS.Promise<string>
    GetProjectLauncher: Fable.Import.vscode.OutputChannel -> Project -> (string -> Fable.Import.JS.Promise<Fable.Import.Node.ChildProcess.ChildProcess>) option
    DebugProject: Project -> string [] -> Fable.Import.JS.Promise<unit>
}