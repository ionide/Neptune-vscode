open System.IO
// --------------------------------------------------------------------------------------
// FAKE build script
// --------------------------------------------------------------------------------------

#I "packages/build/FAKE/tools"
#r "FakeLib.dll"
open Fake
open Fake.YarnHelper


// --------------------------------------------------------------------------------------
// Build the Generator project and run it
// --------------------------------------------------------------------------------------

Target "Clean" (fun _ ->
    CleanDir "./temp"
)

Target "YarnInstall" <| fun () ->
    Yarn (fun p -> { p with Command = Install Standard })

Target "DotNetRestore" <| fun () ->
    DotNetCli.Restore (fun p -> { p with WorkingDir = "src" } )

let releaseBin      = "release/bin"
let neptuneBin         = "paket-files/github.com/Krzysztof-Cieslak/Neptune/build"

Target "CopyNeptune" (fun _ ->
    ensureDirectory releaseBin
    CleanDir releaseBin

    !! (neptuneBin + "/*")
    |> CopyFiles releaseBin

    CopyDir (releaseBin </> "runtimes") (neptuneBin </> "runtimes") (fun _ -> true)
)

let releaseVSTestBin      = "release/bin_vstest"
let neptuneVSTestBin         = "paket-files/github.com/Krzysztof-Cieslak/Neptune-vstest/build"

Target "CopyNeptuneVSTest" (fun _ ->
    ensureDirectory releaseVSTestBin
    CleanDir releaseVSTestBin

    !! (neptuneVSTestBin + "/*")
    |> CopyFiles releaseVSTestBin

    CopyDir (releaseVSTestBin </> "runtimes") (neptuneVSTestBin </> "runtimes") (fun _ -> true)
)
let runFable additionalArgs =
    let path = Path.Combine(__SOURCE_DIRECTORY__, "webpack.config.js")
    let cmd = sprintf "fable webpack -- --config %s %s" path additionalArgs
    DotNetCli.RunCommand (fun p -> { p with WorkingDir = "src" } ) cmd

Target "RunScript" (fun _ ->
    // Ideally we would want a production (minized) build but UglifyJS fail on PerMessageDeflate.js as it contains non-ES6 javascript.
    runFable ""
)

Target "Watch" (fun _ ->
    runFable "--watch"
)


// --------------------------------------------------------------------------------------
// Run generator by default. Invoke 'build <Target>' to override
// --------------------------------------------------------------------------------------

Target "Default" DoNothing

"YarnInstall" ?=> "RunScript"
"DotNetRestore" ?=> "RunScript"

"Clean"
==> "CopyNeptune"
==> "CopyNeptuneVSTest"
==> "RunScript"
==> "Default"

RunTargetOrDefault "Default"
