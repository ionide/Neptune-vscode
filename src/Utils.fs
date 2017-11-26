module Utils

[<AutoOpen>]
module Utils =
    let inline undefined<'a> = unbox<'a> ()

[<RequireQualifiedAccess>]
module Document =
    let (|FSharp|CSharp|VB|Other|) (document : TextDocument) =
        if document.languageId = "fsharp" then FSharp
        else if document.languageId = "csharp" then CSharp
        else if document.languageId = "vb" then VB
        else Other
