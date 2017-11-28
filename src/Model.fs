module Model

type TestState =
        | Passed
        | Ignored
        | Failed
        | NotRun

type CodeRange = Fable.Import.vscode.Range

type Range = {
    StartColumn: int
    StartLine: int
    EndColumn: int
    EndLine: int
}
with
    static member ToCodeRange (range: Range) =
        CodeRange (float range.StartLine - 1.,
                   float range.StartColumn - 1.,
                   float range.EndLine - 1.,
                   float range.EndColumn - 1.)

type TestEntry = {
    Name : string
    Range : Range
    Childs : TestEntry []
    Id : int
    List: bool
    Type: string
}

type ParseRequest = {
    FileName: string
    Content: string
}

type ProjectRequest = {
    FileName: string
    Files: string []
}

type ParseResponse = {
    FileName: string
    Tests: TestEntry []
}

type ProjectResponse = {
    Data: ParseResponse []
}

//Ionide's Project model

type ProjectFilePath = string
type SourceFilePath = string
type ProjectReferencePath = string


[<RequireQualifiedAccess>]
type ProjectResponseInfo =
    | DotnetSdk of ProjectResponseInfoDotnetSdk
    | Verbose
    | ProjectJson
and ProjectResponseInfoDotnetSdk = {
    IsTestProject: bool
    Configuration: string
    IsPackable: bool
    TargetFramework: string
    TargetFrameworkIdentifier: string
    TargetFrameworkVersion: string
    RestoreSuccess: bool
    TargetFrameworks: string list
    RunCmd: RunCmd option
    IsPublishable: bool option }
and [<RequireQualifiedAccess>] RunCmd = { Command: string; Arguments: string }

type Project = {
    Project: ProjectFilePath
    Files: SourceFilePath list
    Output: string
    References: ProjectReferencePath list
    Logs: Map<string, string>
    OutputType: string
    Info: ProjectResponseInfo
    AdditionalInfo: Map<string, string>
}

//API for communication with Ionide

type Api = {
    ProjectLoadedEvent: Fable.Import.vscode.Event<Project>
    BuildProject: Project -> Fable.Import.JS.Promise<string>
}