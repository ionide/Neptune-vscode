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

let targetFileContent path =
    sprintf """<?xml version="1.0" encoding="utf-8" standalone="no"?>
<Project ToolsVersion="14.0" xmlns="http://schemas.microsoft.com/developer/msbuild/2003">
  <PropertyGroup>
        <CollectCoverage>true</CollectCoverage>
        <CoverletOutputFormat>opencover</CoverletOutputFormat>
        <CoverletInstrPath>$(OutputPath)/Instr.json</CoverletInstrPath>
    </PropertyGroup>
  <UsingTask TaskName="Coverlet.MSbuild.Tasks.InstrumentationTask" AssemblyFile="%s/coverlet.msbuild.tasks.dll"/>

  <Target Name="InstrumentModulesAfterBuild" AfterTargets="AfterBuild">
    <Coverlet.MSbuild.Tasks.InstrumentationTask
      Condition="'$(VSTestNoBuild)' != 'true' and $(CollectCoverage) == 'true'"
      InstrPath="$(CoverletInstrPath)"
      Path="$(TargetPath)" />
  </Target>
</Project>""" path

let propsFileContent =
    """<?xml version="1.0" encoding="utf-8" standalone="no"?>
<Project ToolsVersion="14.0" xmlns="http://schemas.microsoft.com/developer/msbuild/2003">
    <PropertyGroup>
        <CollectCoverage>true</CollectCoverage>
        <CoverletOutputFormat>opencover</CoverletOutputFormat>
        <CoverletInstrPath>$(OutputPath)/Instr.json</CoverletInstrPath>
    </PropertyGroup>
</Project>
    """

let pluginPath =
    try
        (Ionide.VSCode.Helpers.VSCode.getPluginPath "LambdaFactory.neptune")
    with
    | _ ->  (Ionide.VSCode.Helpers.VSCode.getPluginPath "LambdaFactory.Neptune")
