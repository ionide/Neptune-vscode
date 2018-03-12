module Model

open Fable.Import
open Fable.Import.vscode

[<RequireQualifiedAccess>]
type TestState =
    | Passed
    | Ignored
    | Failed
    | NotRun

type TestResult = {
    FullName: string
    State: TestState
    Timer: string
    ErrorMessage: string
    Runner: string
}

type Capability =
    | CanDebugSingle
    | CanDebugList
    | CanDebugAll
    | CanRunSingle
    | CanRunList
    | CanRunAll



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
    | None
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

// API for test explorer abstraction

type ITestRunner =
    abstract member GetTypeName : unit -> string
    abstract member ShouldProjectBeRun: Project -> bool
    abstract member RunAll: Progress<ProgressMessage> -> Project list -> JS.Promise<TestResult list>
    abstract member RunTests: Progress<ProgressMessage> -> (Project * string list) list -> JS.Promise<TestResult list>
    abstract member RunList: Progress<ProgressMessage> -> (Project * string) -> JS.Promise<TestResult list>
    abstract member DebugAll: Progress<ProgressMessage> -> Project list -> JS.Promise<TestResult list>
    abstract member DebugTests: Progress<ProgressMessage> -> (Project * string list) list -> JS.Promise<TestResult list>
    abstract member DebugList: Progress<ProgressMessage> -> (Project * string) -> JS.Promise<TestResult list>
    abstract member Capabilities: Project -> Capability list

type ITestDetector =
    abstract member ShouldHandleFile : TextDocument -> bool
    abstract member ShouldHandleFilePath : SourceFilePath -> bool
    abstract member ShouldHandleProject : Project -> bool
    abstract member GetProjectList : unit -> Project list
    abstract member GetProjectForFile : SourceFilePath -> Project option
    abstract member GetTestsForFile : TextDocument -> JS.Promise<ParseResponse>
    abstract member GetTestsForProject : Project -> JS.Promise<ParseResponse []>
