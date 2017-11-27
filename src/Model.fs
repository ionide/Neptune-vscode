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