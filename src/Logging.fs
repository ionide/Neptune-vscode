[<AutoOpen>]
module Logging

open Fable.Import.vscode
open System

[<RequireQualifiedAccess>]
type Level = DEBUG|INFO|WARN|ERROR
    with
        static member GetLevelNum = function DEBUG->10|INFO->20|WARN->30|ERROR->40
        override this.ToString() = match this with ERROR->"ERROR"|INFO->"INFO"|WARN->"WARN"|DEBUG->"DEBUG"
        member this.IsGreaterOrEqualTo level = Level.GetLevelNum(this) >= Level.GetLevelNum(level)
        member this.IsLessOrEqualTo level = Level.GetLevelNum(this) <= Level.GetLevelNum(level)

let private writeDevToolsConsole (_level: Level) (source: string option) (template: string) (args: obj[]) =
    // just replace %j (Util.format->JSON specifier --> console->OBJECT %O specifier)
    // the other % specifiers are basically the same
    let browserLogTemplate = String.Format("[{0}] {1}", source.ToString(), template.Replace("%j", "%O"))
    match args.Length with
    | 0 -> Fable.Import.Browser.console.log (browserLogTemplate)
    | 1 -> Fable.Import.Browser.console.log (browserLogTemplate, args.[0])
    | 2 -> Fable.Import.Browser.console.log (browserLogTemplate, args.[0], args.[1])
    | 3 -> Fable.Import.Browser.console.log (browserLogTemplate, args.[0], args.[1], args.[2])
    | 4 -> Fable.Import.Browser.console.log (browserLogTemplate, args.[0], args.[1], args.[2], args.[3])
    | _ -> Fable.Import.Browser.console.log (browserLogTemplate, args)

let private writeOutputChannel (out: OutputChannel) level _source template args =
    let formattedMessage = Utils.NodeUtil.Util.format(template, args)
    let formattedLogLine = String.Format("[{0:HH:mm:ss} {1,-5}] {2}", DateTime.Now, string level, formattedMessage)
    out.appendLine (formattedLogLine)

let private writeBothIfConfigured (out: OutputChannel option)
          (chanMinLevel: Level)
          (consoleMinLevel: Level option)
          (level: Level)
          (source: string option)
          (template: string)
          (args: obj[]) =
    if consoleMinLevel.IsSome && level.IsGreaterOrEqualTo(consoleMinLevel.Value) then
        writeDevToolsConsole level source template args

    if out.IsSome && level.IsGreaterOrEqualTo(chanMinLevel) then
        writeOutputChannel out.Value level source template args

/// The templates may use node util.format placeholders: %s, %d, %j, %%
/// https://nodejs.org/api/util.html#util_util_format_format
type ConsoleAndOutputChannelLogger(source: string option, chanDefaultMinLevel: Level, out:OutputChannel option, consoleMinLevel: Level option) =
    member val ChanMinLevel = chanDefaultMinLevel with get, set

    /// Logs a different message in either DEBUG (if enabled) or INFO (otherwise).
    /// The templates may use node util.format placeholders: %s, %d, %j, %%
    /// https://nodejs.org/api/util.html#util_util_format_format
    member this.DebugOrInfo
                    (debugTemplateAndArgs: string * obj[])
                    (infoTemplateAndArgs: string * obj[]) =
        // OutputChannel: when at DEBUG level, use the DEBUG template and args, otherwise INFO
        if out.IsSome then
            if this.ChanMinLevel.IsLessOrEqualTo(Level.DEBUG) then
                writeOutputChannel out.Value Level.DEBUG source (fst debugTemplateAndArgs) (snd debugTemplateAndArgs)
            elif this.ChanMinLevel.IsLessOrEqualTo(Level.INFO) then
                writeOutputChannel out.Value Level.INFO source (fst infoTemplateAndArgs) (snd infoTemplateAndArgs)

        // Console: when at DEBUG level, use the DEBUG template and args, otherwise INFO
        if consoleMinLevel.IsSome then
            if Level.DEBUG.IsGreaterOrEqualTo(consoleMinLevel.Value) then
                writeDevToolsConsole Level.DEBUG source (fst debugTemplateAndArgs) (snd debugTemplateAndArgs)
            elif Level.INFO.IsGreaterOrEqualTo(consoleMinLevel.Value) then
                writeDevToolsConsole Level.INFO source (fst infoTemplateAndArgs) (snd infoTemplateAndArgs)

    /// Logs a message that should/could be seen by developers when diagnosing problems.
    /// The templates may use node util.format placeholders: %s, %d, %j, %%
    /// https://nodejs.org/api/util.html#util_util_format_format
    member this.Debug (template, [<ParamArray>]args:obj[]) =
        writeBothIfConfigured out this.ChanMinLevel consoleMinLevel Level.DEBUG source template args
    /// Logs a message that should/could be seen by the user in the output channel.
    /// The templates may use node util.format placeholders: %s, %d, %j, %%
    /// https://nodejs.org/api/util.html#util_util_format_format
    member this.Info (template, [<ParamArray>]args:obj[]) =
        writeBothIfConfigured out this.ChanMinLevel consoleMinLevel Level.INFO source template args
    /// Logs a message that should/could be seen by the user in the output channel when a problem happens.
    /// The templates may use node util.format placeholders: %s, %d, %j, %%
    /// https://nodejs.org/api/util.html#util_util_format_format
    member this.Error (template, [<ParamArray>]args:obj[]) =
        writeBothIfConfigured out this.ChanMinLevel consoleMinLevel Level.ERROR source template args
    /// Logs a message that should/could be seen by the user in the output channel when a problem happens.
    /// The templates may use node util.format placeholders: %s, %d, %j, %%
    /// https://nodejs.org/api/util.html#util_util_format_format
    member this.Warn (template, [<ParamArray>]args:obj[]) =
        writeBothIfConfigured out this.ChanMinLevel consoleMinLevel Level.WARN source template args
    /// Logs a message that should/could be seen by the user in the output channel if the promise fail.
    /// The templates may use node util.format placeholders: %s, %d, %j, %%
    /// https://nodejs.org/api/util.html#util_util_format_format
    member this.ErrorOnFailed text (p: Fable.Import.JS.Promise<_>) =
        p.catch(Func<obj,unit>(fun err -> this.Error(text + ": %O", err)))
        |> ignore