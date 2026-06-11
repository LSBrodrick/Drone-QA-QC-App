@echo off
powershell -NoProfile -ExecutionPolicy Bypass -Command ^
  "$WshShell = New-Object -ComObject WScript.Shell;" ^
  "$Shortcut = $WshShell.CreateShortcut([System.Environment]::GetFolderPath('Desktop') + '\Drone QAQC.lnk');" ^
  "$Shortcut.TargetPath = 'C:\Users\15039\Landscan\shiny_app\launch_silent.vbs';" ^
  "$Shortcut.WorkingDirectory = 'C:\Users\15039\Landscan\shiny_app';" ^
  "$Shortcut.IconLocation = 'C:\Program Files\R\R-4.4.2\bin\R.exe, 0';" ^
  "$Shortcut.Description = 'Drone QA/QC Analysis App';" ^
  "$Shortcut.Save();" ^
  "Write-Host 'Shortcut created on Desktop.'"
pause
