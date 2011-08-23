;NSIS Modern User Interface version 1.70
;Barberpole Tuner Installer
;Written by Christian Budde

SetCompressor lzma

;--------------------------------
;Include Modern UI
;  !include "Sections.nsh"
  !include "MUI.nsh"


;--------------------------------
;General

  ;Name and file
  Name "Barberpole Tuner Installer"
  OutFile "Barberpole Tuner_Install.exe"

  ;Default installation folder
  InstallDir "$PROGRAMFILES\VSTPlugIns"
  
  ;Get installation folder from registry if available
  InstallDirRegKey HKLM "SOFTWARE\VST" "VSTPluginsPath"

  BrandingText "Delphi ASIO && VST Project"

  ; Turn on the xp style of drawing
  XPStyle ON


;--------------------------------
;Variables

  Var BugReportState


;--------------------------------
;Interface Settings

  !define PRODUCT_NAME "Barberpole Tuner"
  !define PRODUCT_VERSION "1.0.0"
  !define PRODUCT_PUBLISHER "Christian Budde"
  !define PRODUCT_WEB_SITE "http://delphiasiovst.sourceforge.net/"
  !define PRODUCT_DIR_REGKEY "Software\Delphi ASIO & VST Packages\${PRODUCT_NAME}"
  !define PRODUCT_DIR_ROOT_KEY "HKLM"
  !define PRODUCT_UNINST_KEY "Software\Delphi ASIO & VST Packages\Uninstall\${PRODUCT_NAME}"
  !define PRODUCT_UNINST_ROOT_KEY "HKLM"
  !define MUI_ABORTWARNING


;--------------------------------
;Language Selection Dialog Settings

  ;Remember the installer language
  !define MUI_LANGDLL_REGISTRY_ROOT "HKLM" 
  !define MUI_LANGDLL_REGISTRY_KEY "SOFTWARE\Delphi ASIO & VST Packages\${PRODUCT_NAME}"
  !define MUI_LANGDLL_REGISTRY_VALUENAME "Installer Language"


;--------------------------------
;Reserve Files
  
  ;These files should be inserted before other files in the data block
  ;Keep these lines before any File command
  ;Only for solid compression (by default, solid compression is enabled for BZIP2 and LZMA)
  
    ReserveFile "madExcept Patch.dll"
    ReserveFile "ioBugReport.ini"
  !insertmacro MUI_RESERVEFILE_INSTALLOPTIONS
;  !insertmacro MUI_RESERVEFILE_LANGDLL


;--------------------------------
;Installer Functions

Function .onInit

;  !insertmacro MUI_LANGDLL_DISPLAY  
  !insertmacro MUI_INSTALLOPTIONS_EXTRACT "ioBugReport.ini"

FunctionEnd


;--------------------------------
;Pages

  !insertmacro MUI_PAGE_WELCOME
  !insertmacro MUI_PAGE_LICENSE "..\Bin\License.txt"
  !insertmacro MUI_PAGE_COMPONENTS
  !insertmacro MUI_PAGE_DIRECTORY
  Page custom BugReportPatch
  !insertmacro MUI_PAGE_INSTFILES
  !insertmacro MUI_PAGE_FINISH
  !insertmacro MUI_UNPAGE_WELCOME
  !insertmacro MUI_UNPAGE_CONFIRM
  !insertmacro MUI_UNPAGE_INSTFILES


;--------------------------------
;Languages
 
  !insertmacro MUI_LANGUAGE "English"
;  !insertmacro MUI_LANGUAGE "German"


;--------------------------------
;Installer Sections

Section "VST-Plugin" SecVstPlugin
  SetOutPath "$INSTDIR"
  
  !system 'copy "..\Bin\BarberpoleTuner.dll" "..\Bin\Barberpole Tuner.dll"'  

  ;ADD YOUR OWN FILES HERE...
  File "..\Bin\Barberpole Tuner.dll"

  !insertmacro MUI_INSTALLOPTIONS_READ $BugReportState "ioBugReport.ini" "Field 1" "State"  
  IntCmp $BugReportState 0 SkipDLLCall
    
  SetOutPath $TEMP                      ; create temp directory
  File "madExcept Patch.dll"            ; copy dll there
  
  StrCpy $0 "$INSTDIR\Barberpole Tuner.dll" 
  System::Call 'madExcept Patch::PatchMadExceptDLL(t) i (r0).r1'
  System::Free 0
  Delete "madExcept Patch.dll"
  
  IntCmp $1 0 SkipDLLCall
  DetailPrint  "Bug Report DLL Patch applied"
SkipDLLCall:

  ;Store installation folder
  WriteRegStr HKLM "SOFTWARE\Delphi ASIO & VST Packages\${PRODUCT_NAME}" "" $INSTDIR
  
  ;Create uninstaller
  WriteUninstaller "$INSTDIR\Uninstall_Barberpole_Tuner.exe"

SectionEnd

Section "Barberpole Tuner Manual" SecManual
  SetOutPath "$INSTDIR"
  
  ;ADD YOUR OWN FILES HERE...
  File "..\Manuals\Barberpole Tuner.pdf"

  ;Store installation folder
  WriteRegStr HKLM "SOFTWARE\Delphi ASIO & VST Packages\${PRODUCT_NAME}" "" $INSTDIR
  
  ;Create uninstaller
  WriteUninstaller "$INSTDIR\Uninstall_Barberpole_Tuner.exe"

SectionEnd
  

;--------------------------------
;Installer Functions

Function BugReportPatch
  ${If} ${SectionIsSelected} ${SecVSTPlugin}
  Goto IsVST
  ${EndIf}
  Goto NoVST

  IsVST:
  !insertmacro MUI_HEADER_TEXT "$(TEXT_IO_TITLE)" "$(TEXT_IO_SUBTITLE)"
  !insertmacro MUI_INSTALLOPTIONS_DISPLAY "ioBugReport.ini"

  NoVST:
FunctionEnd


;--------------------------------
;Language strings

  LangString TEXT_IO_TITLE ${LANG_ENGLISH} "InstallOptions page"
  LangString TEXT_IO_SUBTITLE ${LANG_ENGLISH} "Barberpole Tuner VST Plugin"

  LangString DESC_SecVstPlugin ${LANG_ENGLISH} "Barberpole Tuner VST Plugin"
  LangString DESC_SecManual ${LANG_ENGLISH} "Barberpole Tuner Manual"


;--------------------------------
;Descriptions

  ;Assign language strings to sections
  !insertmacro MUI_FUNCTION_DESCRIPTION_BEGIN
    !insertmacro MUI_DESCRIPTION_TEXT ${SecVstPlugin} $(DESC_SecVstPlugin)
    !insertmacro MUI_DESCRIPTION_TEXT ${SecManual} $(DESC_SecManual)
  !insertmacro MUI_FUNCTION_DESCRIPTION_END


;--------------------------------
;Uninstaller Section

Section "Uninstall"

  Delete "$INSTDIR\Barberpole Tuner.dll"
  Delete "$INSTDIR\Barberpole Tuner.pdf"
  DeleteRegKey HKLM "SOFTWARE\Delphi ASIO & VST Packages\${PRODUCT_NAME}"

SectionEnd