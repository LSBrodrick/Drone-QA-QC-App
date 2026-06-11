Set WshShell = CreateObject("WScript.Shell")
WshShell.CurrentDirectory = "C:\Users\15039\Landscan\shiny_app"
WshShell.Run """C:\Program Files\R\R-4.4.2\bin\Rscript.exe"" launch_app.R", 0, False
