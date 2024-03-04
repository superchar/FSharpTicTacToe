open Avalonia
open Avalonia.Controls.ApplicationLifetimes
open Avalonia.Themes.Fluent
open Avalonia.FuncUI.Hosts
open TicTacToer.Components

module Main =

    type MainWindow() =
        inherit HostWindow()

        do
            base.Title <- "Tic tac toe"
            base.Content <- gameBoard ()

    type App() =
        inherit Application()

        override this.Initialize() = this.Styles.Add(FluentTheme())

        override this.OnFrameworkInitializationCompleted() =
            match this.ApplicationLifetime with
            | :? IClassicDesktopStyleApplicationLifetime as desktopLifetime -> desktopLifetime.MainWindow <- MainWindow()
            | _ -> ()

module Program =

    [<EntryPoint>]
    let main (args: string[]) =
        AppBuilder
            .Configure<Main.App>()
            .UsePlatformDetect()
            .UseSkia()
            .StartWithClassicDesktopLifetime(args)
