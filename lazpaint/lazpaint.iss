#define MyAppName "LazPaint"
#define MyAppOutputName "lazpaint"
#define MyInstallerSuffix "_setup_win32_win64"
#define MyAppVersion "6.4.1"
#define MyAppPublisher "Circular, Fabien Wang, Lainz and others"
#define MyAppURL "http://sourceforge.net/projects/lazpaint/"
#define MyAppExeName "lazpaint.exe"
#define ReleaseDir "release\"

[Setup]
AppId={{A177F82E-B44A-4348-A265-3D1C089D6304}
AppName={#MyAppName}
AppVersion={#MyAppVersion}
AppPublisher={#MyAppPublisher}
AppPublisherURL={#MyAppURL}
AppSupportURL={#MyAppURL}
AppUpdatesURL={#MyAppURL}
ChangesAssociations=yes
DefaultDirName={pf}\{#MyAppName}
DefaultGroupName={#MyAppName}
AllowNoIcons=yes
OutputDir=installer
OutputBaseFilename={#MyAppOutputName}{#MyAppVersion}{#MyInstallerSuffix}
Compression=lzma2/ultra64
InternalCompressLevel=ultra
SolidCompression=yes
CompressionThreads=2
ArchitecturesInstallIn64BitMode=x64
WizardImageFile=lazpaint_setup.bmp
WizardSmallImageFile=lazpaint_setup_icon.bmp
WizardImageBackColor=clWhite
WizardImageStretch=False
;The next options are to do a quick setup with less dialogs
;If program already installed only show tasks dialog
;ShowLanguageDialog=no
;DisableWelcomePage=yes
;DisableDirPage=auto
;DisableProgramGroupPage=auto
;DisableReadyPage=yes
;DisableFinishedPage=yes

[Code]
// Start - Quick Setup //
{procedure CurPageChanged(CurPageID: Integer);
begin
  if CurPageID = wpSelectTasks then
    WizardForm.NextButton.Caption := SetupMessage(msgButtonInstall)
  else if CurPageID = wpFinished then
    WizardForm.NextButton.Caption := SetupMessage(msgButtonFinish)
  else
    WizardForm.NextButton.Caption := SetupMessage(msgButtonNext);
end;}
// End - Quick Setup //

[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"
Name: "brazilianportuguese"; MessagesFile: "compiler:Languages\BrazilianPortuguese.isl"
Name: "catalan"; MessagesFile: "compiler:Languages\Catalan.isl"
Name: "corsican"; MessagesFile: "compiler:Languages\Corsican.isl"
Name: "czech"; MessagesFile: "compiler:Languages\Czech.isl"
Name: "danish"; MessagesFile: "compiler:Languages\Danish.isl"
Name: "dutch"; MessagesFile: "compiler:Languages\Dutch.isl"
Name: "finnish"; MessagesFile: "compiler:Languages\Finnish.isl"
Name: "french"; MessagesFile: "compiler:Languages\French.isl"
Name: "german"; MessagesFile: "compiler:Languages\German.isl"
Name: "greek"; MessagesFile: "compiler:Languages\Greek.isl"
Name: "hebrew"; MessagesFile: "compiler:Languages\Hebrew.isl"
Name: "hungarian"; MessagesFile: "compiler:Languages\Hungarian.isl"
Name: "italian"; MessagesFile: "compiler:Languages\Italian.isl"
Name: "japanese"; MessagesFile: "compiler:Languages\Japanese.isl"
Name: "norwegian"; MessagesFile: "compiler:Languages\Norwegian.isl"
Name: "polish"; MessagesFile: "compiler:Languages\Polish.isl"
Name: "portuguese"; MessagesFile: "compiler:Languages\Portuguese.isl"
Name: "russian"; MessagesFile: "compiler:Languages\Russian.isl"
Name: "serbiancyrillic"; MessagesFile: "compiler:Languages\SerbianCyrillic.isl"
Name: "serbianlatin"; MessagesFile: "compiler:Languages\SerbianLatin.isl"
Name: "slovenian"; MessagesFile: "compiler:Languages\Slovenian.isl"
Name: "spanish"; MessagesFile: "compiler:Languages\Spanish.isl"
Name: "ukrainian"; MessagesFile: "compiler:Languages\Ukrainian.isl"

[Tasks]
Name: "desktopicon"; Description: "{cm:CreateDesktopIcon}"
Name: "assoc_lzp"; Description: "{cm:AssocFileExtension,{#MyAppName},.lzp}"
Name: "assoc_ora"; Description: "{cm:AssocFileExtension,{#MyAppName},.ora}"
Name: "assoc_pdn"; Description: "{cm:AssocFileExtension,{#MyAppName},.pdn}"; Flags: unchecked
Name: "assoc_bmp"; Description: "{cm:AssocFileExtension,{#MyAppName},.bmp}"; Flags: unchecked
Name: "assoc_pcx"; Description: "{cm:AssocFileExtension,{#MyAppName},.pcx}"; Flags: unchecked
Name: "assoc_png"; Description: "{cm:AssocFileExtension,{#MyAppName},.png}"; Flags: unchecked
Name: "assoc_jpg"; Description: "{cm:AssocFileExtension,{#MyAppName},.jpg}"; Flags: unchecked
Name: "assoc_gif"; Description: "{cm:AssocFileExtension,{#MyAppName},.gif}"; Flags: unchecked
Name: "assoc_ico"; Description: "{cm:AssocFileExtension,{#MyAppName},.ico}"; Flags: unchecked

[Files]
Source: "{#ReleaseDir}lazpaint32.exe"; DestDir: "{app}"; DestName: "{#MyAppExeName}"; Flags: ignoreversion; Check: not Is64BitInstallMode
Source: "{#ReleaseDir}lazpaint_x64.exe"; DestDir: "{app}"; DestName: "{#MyAppExeName}"; Flags: ignoreversion; Check: Is64BitInstallMode
Source: "{#ReleaseDir}i18n\*.po"; DestDir: "{app}\i18n"; Excludes: "i18n\lazpaint_x64.po"; Flags: ignoreversion
Source: "{#ReleaseDir}models\*.*"; DestDir: "{app}\models"; Flags: ignoreversion
Source: "{#ReleaseDir}readme.txt"; DestDir: "{app}"; Flags: ignoreversion

[Icons]
Name: "{group}\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"; Comment: "LazPaint"
Name: "{group}\{cm:ProgramOnTheWeb,{#MyAppName}}"; Filename: "{#MyAppURL}"
Name: "{group}\{cm:UninstallProgram,{#MyAppName}}"; Filename: "{uninstallexe}"
Name: "{commondesktop}\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"; Comment: "LazPaint"; Tasks: desktopicon

[Run]
Filename: "{app}\{#MyAppExeName}"; Description: "{cm:LaunchProgram,{#StringChange(MyAppName, "&", "&&")}}"; Flags: nowait postinstall skipifsilent

[Registry]
Root: HKLM; Subkey: "Software\Classes\LazPaint.AssocFile.lzp"; ValueType: String; ValueData: "LazPaint image"; Flags: uninsdeletekey
Root: HKLM; Subkey: "Software\Classes\LazPaint.AssocFile.lzp\DefaultIcon"; ValueType: String; ValueData: "{app}\{#MyAppExeName},0"; Flags: uninsdeletekey
Root: HKLM; Subkey: "Software\Classes\LazPaint.AssocFile.lzp\Shell\Open"; ValueName: Icon; ValueType: String; ValueData: "{app}\{#MyAppExeName}"; Flags: uninsdeletekey
Root: HKLM; Subkey: "Software\Classes\LazPaint.AssocFile.lzp\Shell\Open\Command"; ValueType: String; ValueData: """{app}\{#MyAppExeName}"" ""%1"""; Flags: uninsdeletekey

Root: HKLM; Subkey: "Software\Classes\LazPaint.AssocFile.ora"; ValueType: String; ValueData: "OpenRaster format"; Flags: uninsdeletekey
Root: HKLM; Subkey: "Software\Classes\LazPaint.AssocFile.ora\DefaultIcon"; ValueType: String; ValueData: "{app}\{#MyAppExeName},0"; Flags: uninsdeletekey
Root: HKLM; Subkey: "Software\Classes\LazPaint.AssocFile.ora\Shell\Open"; ValueName: Icon; ValueType: String; ValueData: "{app}\{#MyAppExeName}"; Flags: uninsdeletekey
Root: HKLM; Subkey: "Software\Classes\LazPaint.AssocFile.ora\Shell\Open\Command"; ValueType: String; ValueData: """{app}\{#MyAppExeName}"" ""%1"""; Flags: uninsdeletekey

Root: HKLM; Subkey: "Software\Classes\LazPaint.AssocFile.bmp"; ValueType: String; ValueData: "Bitmap"; Flags: uninsdeletekey
Root: HKLM; Subkey: "Software\Classes\LazPaint.AssocFile.bmp\DefaultIcon"; ValueType: String; ValueData: "{app}\{#MyAppExeName},0"; Flags: uninsdeletekey
Root: HKLM; Subkey: "Software\Classes\LazPaint.AssocFile.bmp\Shell\Open"; ValueName: Icon; ValueType: String; ValueData: "{app}\{#MyAppExeName}"; Flags: uninsdeletekey
Root: HKLM; Subkey: "Software\Classes\LazPaint.AssocFile.bmp\Shell\Open\Command"; ValueType: String; ValueData: """{app}\{#MyAppExeName}"" ""%1"""; Flags: uninsdeletekey

Root: HKLM; Subkey: "Software\Classes\LazPaint.AssocFile.png"; ValueType: String; ValueData: "Portable Network Graphic"; Flags: uninsdeletekey
Root: HKLM; Subkey: "Software\Classes\LazPaint.AssocFile.png\DefaultIcon"; ValueType: String; ValueData: "{app}\{#MyAppExeName},0"; Flags: uninsdeletekey
Root: HKLM; Subkey: "Software\Classes\LazPaint.AssocFile.png\Shell\Open"; ValueName: Icon; ValueType: String; ValueData: "{app}\{#MyAppExeName}"; Flags: uninsdeletekey
Root: HKLM; Subkey: "Software\Classes\LazPaint.AssocFile.png\Shell\Open\Command"; ValueType: String; ValueData: """{app}\{#MyAppExeName}"" ""%1"""; Flags: uninsdeletekey

Root: HKLM; Subkey: "Software\Classes\LazPaint.AssocFile.jpg"; ValueType: String; ValueData: "JPEG"; Flags: uninsdeletekey
Root: HKLM; Subkey: "Software\Classes\LazPaint.AssocFile.jpg\DefaultIcon"; ValueType: String; ValueData: "{app}\{#MyAppExeName},0"; Flags: uninsdeletekey
Root: HKLM; Subkey: "Software\Classes\LazPaint.AssocFile.jpg\Shell\Open"; ValueName: Icon; ValueType: String; ValueData: "{app}\{#MyAppExeName}"; Flags: uninsdeletekey
Root: HKLM; Subkey: "Software\Classes\LazPaint.AssocFile.jpg\Shell\Open\Command"; ValueType: String; ValueData: """{app}\{#MyAppExeName}"" ""%1"""; Flags: uninsdeletekey

Root: HKLM; Subkey: "Software\Classes\LazPaint.AssocFile.pdn"; ValueType: String; ValueData: "Paint.NET image"; Flags: uninsdeletekey
Root: HKLM; Subkey: "Software\Classes\LazPaint.AssocFile.pdn\DefaultIcon"; ValueType: String; ValueData: "{app}\{#MyAppExeName},0"; Flags: uninsdeletekey
Root: HKLM; Subkey: "Software\Classes\LazPaint.AssocFile.pdn\Shell\Open"; ValueName: Icon; ValueType: String; ValueData: "{app}\{#MyAppExeName}"; Flags: uninsdeletekey
Root: HKLM; Subkey: "Software\Classes\LazPaint.AssocFile.pdn\Shell\Open\Command"; ValueType: String; ValueData: """{app}\{#MyAppExeName}"" ""%1"""; Flags: uninsdeletekey

Root: HKLM; Subkey: "Software\Classes\LazPaint.AssocFile.gif"; ValueType: String; ValueData: "Animated GIF"; Flags: uninsdeletekey
Root: HKLM; Subkey: "Software\Classes\LazPaint.AssocFile.gif\DefaultIcon"; ValueType: String; ValueData: "{app}\{#MyAppExeName},0"; Flags: uninsdeletekey
Root: HKLM; Subkey: "Software\Classes\LazPaint.AssocFile.gif\Shell\Open"; ValueName: Icon; ValueType: String; ValueData: "{app}\{#MyAppExeName}"; Flags: uninsdeletekey
Root: HKLM; Subkey: "Software\Classes\LazPaint.AssocFile.gif\Shell\Open\Command"; ValueType: String; ValueData: """{app}\{#MyAppExeName}"" ""%1"""; Flags: uninsdeletekey

Root: HKLM; Subkey: "Software\Classes\LazPaint.AssocFile.ico"; ValueType: String; ValueData: "Icon"; Flags: uninsdeletekey
Root: HKLM; Subkey: "Software\Classes\LazPaint.AssocFile.ico\DefaultIcon"; ValueType: String; ValueData: "{app}\{#MyAppExeName},0"; Flags: uninsdeletekey
Root: HKLM; Subkey: "Software\Classes\LazPaint.AssocFile.ico\Shell\Open"; ValueName: Icon; ValueType: String; ValueData: "{app}\{#MyAppExeName}"; Flags: uninsdeletekey
Root: HKLM; Subkey: "Software\Classes\LazPaint.AssocFile.ico\Shell\Open\Command"; ValueType: String; ValueData: """{app}\{#MyAppExeName}"" ""%1"""; Flags: uninsdeletekey

Root: HKLM; Subkey: "Software\Classes\LazPaint.AssocFile.pcx"; ValueType: String; ValueData: "Personal Computer eXchange"; Flags: uninsdeletekey
Root: HKLM; Subkey: "Software\Classes\LazPaint.AssocFile.pcx\DefaultIcon"; ValueType: String; ValueData: "{app}\{#MyAppExeName},0"; Flags: uninsdeletekey
Root: HKLM; Subkey: "Software\Classes\LazPaint.AssocFile.pcx\Shell\Open"; ValueName: Icon; ValueType: String; ValueData: "{app}\{#MyAppExeName}"; Flags: uninsdeletekey
Root: HKLM; Subkey: "Software\Classes\LazPaint.AssocFile.pcx\Shell\Open\Command"; ValueType: String; ValueData: """{app}\{#MyAppExeName}"" ""%1"""; Flags: uninsdeletekey

Root: HKLM; Subkey: "Software\Classes\.bmp"; ValueType: String; ValueData: "LazPaint.AssocFile.bmp"; Flags: uninsdeletevalue uninsdeletekeyifempty; Tasks: assoc_bmp
Root: HKLM; Subkey: "Software\Classes\.lzp"; ValueType: String; ValueData: "LazPaint.AssocFile.lzp"; Flags: uninsdeletevalue uninsdeletekeyifempty; Tasks: assoc_lzp
Root: HKLM; Subkey: "Software\Classes\.ora"; ValueType: String; ValueData: "LazPaint.AssocFile.ora"; Flags: uninsdeletevalue uninsdeletekeyifempty; Tasks: assoc_ora
Root: HKLM; Subkey: "Software\Classes\.png"; ValueType: String; ValueData: "LazPaint.AssocFile.png"; Flags: uninsdeletevalue uninsdeletekeyifempty; Tasks: assoc_png
Root: HKLM; Subkey: "Software\Classes\.jpg"; ValueType: String; ValueData: "LazPaint.AssocFile.jpg"; Flags: uninsdeletevalue uninsdeletekeyifempty; Tasks: assoc_jpg
Root: HKLM; Subkey: "Software\Classes\.pdn"; ValueType: String; ValueData: "LazPaint.AssocFile.pdn"; Flags: uninsdeletevalue uninsdeletekeyifempty; Tasks: assoc_pdn
Root: HKLM; Subkey: "Software\Classes\.gif"; ValueType: String; ValueData: "LazPaint.AssocFile.gif"; Flags: uninsdeletevalue uninsdeletekeyifempty; Tasks: assoc_gif
Root: HKLM; Subkey: "Software\Classes\.ico"; ValueType: String; ValueData: "LazPaint.AssocFile.ico"; Flags: uninsdeletevalue uninsdeletekeyifempty; Tasks: assoc_ico
Root: HKLM; Subkey: "Software\Classes\.pcx"; ValueType: String; ValueData: "LazPaint.AssocFile.pcx"; Flags: uninsdeletevalue uninsdeletekeyifempty; Tasks: assoc_pcx

Root: HKLM; Subkey: "Software\LazPaint"; Flags: uninsdeletekey
Root: HKLM; Subkey: "Software\LazPaint\Capabilities"; ValueType: String; ValueName: "ApplicationName"; ValueData: "LazPaint"; Flags: uninsdeletekey
Root: HKLM; Subkey: "Software\LazPaint\Capabilities"; ValueType: String; ValueName: "ApplicationDescription"; ValueData: "This program is designed to draw like with Paint.Net and to experiment this kind of programming with Lazarus."; Flags: uninsdeletekey
Root: HKLM; Subkey: "Software\LazPaint\Capabilities\FileAssociations"; ValueName: ".lzp"; ValueType: String; ValueData: "LazPaint.AssocFile.lzp"; Flags: uninsdeletekey
Root: HKLM; Subkey: "Software\LazPaint\Capabilities\FileAssociations"; ValueName: ".ora"; ValueType: String; ValueData: "LazPaint.AssocFile.ora"; Flags: uninsdeletekey
Root: HKLM; Subkey: "Software\LazPaint\Capabilities\FileAssociations"; ValueName: ".bmp"; ValueType: String; ValueData: "LazPaint.AssocFile.bmp"; Flags: uninsdeletekey
Root: HKLM; Subkey: "Software\LazPaint\Capabilities\FileAssociations"; ValueName: ".png"; ValueType: String; ValueData: "LazPaint.AssocFile.png"; Flags: uninsdeletekey
Root: HKLM; Subkey: "Software\LazPaint\Capabilities\FileAssociations"; ValueName: ".jpg"; ValueType: String; ValueData: "LazPaint.AssocFile.jpg"; Flags: uninsdeletekey
Root: HKLM; Subkey: "Software\LazPaint\Capabilities\FileAssociations"; ValueName: ".pdn"; ValueType: String; ValueData: "LazPaint.AssocFile.pdn"; Flags: uninsdeletekey
Root: HKLM; Subkey: "Software\LazPaint\Capabilities\FileAssociations"; ValueName: ".gif"; ValueType: String; ValueData: "LazPaint.AssocFile.gif"; Flags: uninsdeletekey
Root: HKLM; Subkey: "Software\LazPaint\Capabilities\FileAssociations"; ValueName: ".ico"; ValueType: String; ValueData: "LazPaint.AssocFile.ico"; Flags: uninsdeletekey
Root: HKLM; Subkey: "Software\LazPaint\Capabilities\FileAssociations"; ValueName: ".pcx"; ValueType: String; ValueData: "LazPaint.AssocFile.pcx"; Flags: uninsdeletekey

Root: HKLM; Subkey: "Software\RegisteredApplications"; ValueType: String; ValueName: "LazPaint"; ValueData: "Software\LazPaint\Capabilities"; Flags: uninsdeletevalue uninsdeletekeyifempty
