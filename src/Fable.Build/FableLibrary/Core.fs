namespace Build.FableLibrary

open BlackFox.CommandLine
open Fake.IO
open System.IO
open Build.Utils
open Build.Utils
open System.Diagnostics
open SimpleExec

/// <summary>
/// Building fable-library is similar enough for all the targets
/// that we can use this class to standardise the process.
/// </summary>
type BuildFableLibrary
    (language: string, libraryDir: string, sourceDir: string, buildDir: string, outDir: string, ?fableLibArg: string)
    =

    // It seems like the different target have a different way of supporting
    // --fableLib argument.
    // For example,
    // If we provide `--fableLib < out dir path>`, then the Dart target
    // generates import './Option.dart' as option;
    // Bt if we provides `--fableLib .`, then the Dart target
    // generates import '../../src/fable-library-dart/Option.dart' as option;
    // Python seems to ignore completely the --fableLib argument.
    // Investigation are required to make all the targets behave the same way.
    // For now, I am providing a way to override the --fableLib argument in the
    // build system but it should be removed once the issue is fixed.
    let fableLibArg = defaultArg fableLibArg "."

    member val LibraryDir = libraryDir
    member val SourceDir = sourceDir
    member val BuildDir = buildDir
    member val OutDir = outDir

    // Allow language to be orverriden from "do constructor"
    // Useful for JavaScript target which is a specialisation of the TypeScript
    member val Language = language with get, set

    abstract member FableArgsBuilder: (CmdLine -> CmdLine)
    default _.FableArgsBuilder = id

    abstract member PostFableBuildStage: unit -> unit
    default _.PostFableBuildStage() = ()

    abstract member CopyStage: unit -> unit
    default _.CopyStage() = ()

    /// <summary>
    /// Robustly delete a directory, handling file system issues on MacOS and Windows.
    /// Ref: https://stackoverflow.com/questions/329355/cannot-delete-directory-with-directory-deletepath-true
    /// </summary>
    member private this.deleteDirectoryRobust(path: string) =
        if Directory.Exists path then
            let rec tryDelete retries =
                try
                    Directory.Delete(path, true)
                with :? IOException when retries > 0 ->
                    System.Threading.Thread.Sleep 50
                    tryDelete (retries - 1)

            tryDelete 3

    member this.Run(?skipIfExist: bool) =
        let toConsole (s: string) = System.Console.WriteLine(s)

        let skipIfExist = defaultArg skipIfExist false

        if skipIfExist && Directory.Exists outDir then
            "Skipping Fable build stage" |> toConsole

        else

            "Cleaning build directory" |> toConsole

            this.deleteDirectoryRobust buildDir

            "Building Fable.Library" |> toConsole

            let args =
                CmdLine.appendRaw sourceDir
                >> CmdLine.appendPrefix "--outDir" outDir
                >> CmdLine.appendPrefix "--fableLib" fableLibArg
                >> CmdLine.appendPrefix "--lang" language
                >> CmdLine.appendPrefix "--exclude" "Fable.Core"
                >> CmdLine.appendPrefix "--define" "FABLE_LIBRARY"
                >> CmdLine.appendRaw "--noCache"
                // Target implementation can require additional arguments
                >> this.FableArgsBuilder

            Command.Fable(args)

            "Copy stage" |> toConsole
            this.CopyStage()

            "Post Fable build stage" |> toConsole
            this.PostFableBuildStage()
