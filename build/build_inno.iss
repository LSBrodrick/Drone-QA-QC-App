; ==============================================================================
; Drone QA/QC Analysis - Inno Setup Installer
; Run AFTER build_standalone.R has created the DroneQAQC folder
;
; PREREQUISITES:
;   1. Run build_standalone.R first (creates build/output/DroneQAQC/)
;   2. Install Inno Setup 6+ (free): https://jrsoftware.org/isinfo.php
;
; USAGE:
;   1. Open this file in Inno Setup Compiler
;   2. Click Build > Compile
;   3. Output: build/output/DroneQAQC_Setup.exe (~500MB standalone installer)
;
; The installer bundles R Portable + all packages + the app.
; No R installation required on the target machine.
; ==============================================================================

#define MyAppName "Drone QA/QC Analysis"
#define MyAppVersion "1.0.0"
#define MyAppPublisher "LandScan"
#define MyAppURL "https://landscan.com"
#define MyAppExeName "DroneQAQC.vbs"

[Setup]
AppId={{A1B2C3D4-E5F6-7890-ABCD-EF1234567890}
AppName={#MyAppName}
AppVersion={#MyAppVersion}
AppPublisher={#MyAppPublisher}
AppPublisherURL={#MyAppURL}
DefaultDirName={autopf}\{#MyAppName}
DefaultGroupName={#MyAppName}
AllowNoIcons=yes
; Point to the folder created by build_standalone.R
OutputDir=output
OutputBaseFilename=DroneQAQC_Setup
Compression=lzma2/ultra64
SolidCompression=yes
WizardStyle=modern
PrivilegesRequired=lowest
; Use R.exe icon from the bundled R Portable
SetupIconFile=output\DroneQAQC\R-Portable\bin\R.exe
UninstallDisplayIcon={app}\R-Portable\bin\R.exe

[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"

[Tasks]
Name: "desktopicon"; Description: "{cm:CreateDesktopIcon}"; GroupDescription: "{cm:AdditionalIcons}"
Name: "startmenu"; Description: "Create Start Menu shortcut"; GroupDescription: "{cm:AdditionalIcons}"; Flags: checked

[Files]
; Bundle the entire DroneQAQC folder (R Portable + app + exiftool)
Source: "output\DroneQAQC\*"; DestDir: "{app}"; Flags: ignoreversion recursesubdirs createallsubdirs

[Icons]
; Desktop shortcut (silent launcher - no console window)
Name: "{autodesktop}\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"; WorkingDir: "{app}"; Tasks: desktopicon
; Start Menu shortcut
Name: "{group}\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"; WorkingDir: "{app}"; Tasks: startmenu
; Uninstaller in Start Menu
Name: "{group}\Uninstall {#MyAppName}"; Filename: "{uninstallexe}"

[Run]
; Offer to launch after install
Filename: "{app}\{#MyAppExeName}"; Description: "Launch {#MyAppName}"; Flags: nowait postinstall skipifsilent shellexec
