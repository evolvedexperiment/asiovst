;NSIS Modern User Interface version 1.70
;Oversampler Installer
;Written by Christian Budde

SetCompressor lzma

;--------------------------------
;Include Modern UI
;  !include "Sections.nsh"
  !include "MUI.nsh"

;--------------------------------
;General

  ;Name and file
  Name "Oversampler"
  OutFile "OversamplerInstall.exe"
  BrandingText "Oversampler"

  ;Default installation folder
  InstallDir "$PROGRAMFILES\Oversampler"
  
  ; Turn on the xp style of drawing
  XPStyle ON

;--------------------------------
;Interface Settings

  !define PRODUCT_NAME "Oversampler"
  !define PRODUCT_VERSION "1.0.0"
  !define PRODUCT_PUBLISHER "Delphi ASIO & VST Package"
  !define PRODUCT_WEB_SITE "http://delphiasiovst.sourceforge.net"
  !define PRODUCT_DIR_REGKEY "Software\Delphi ASIO & VST Packages\${PRODUCT_NAME}"
  !define PRODUCT_UNINST_KEY "Software\Delphi ASIO & VST Packages\Uninstall\${PRODUCT_NAME}"
  !define PRODUCT_UNINST_ROOT_KEY "HKLM"
  
;--------------------------------
;Language Selection Dialog Settings

  ;Remember the installer language
  !define MUI_LANGDLL_REGISTRY_ROOT "HKLM" 
  !define MUI_LANGDLL_REGISTRY_KEY "Software\Delphi ASIO & VST Packages\${PRODUCT_NAME}"
  !define MUI_LANGDLL_REGISTRY_VALUENAME "Installer Language"

;--------------------------------
;Pages

  !insertmacro MUI_PAGE_WELCOME
;  !insertmacro MUI_PAGE_LICENSE "License.txt"
  !insertmacro MUI_PAGE_COMPONENTS
  !insertmacro MUI_PAGE_DIRECTORY

  !insertmacro MUI_PAGE_INSTFILES
  
  !insertmacro MUI_PAGE_FINISH
  !insertmacro MUI_UNPAGE_WELCOME
  !insertmacro MUI_UNPAGE_CONFIRM
  !insertmacro MUI_UNPAGE_INSTFILES
;--------------------------------
;Languages
 
  !insertmacro MUI_LANGUAGE "English"
;  !insertmacro MUI_LANGUAGE "German"
;  !insertmacro MUI_RESERVEFILE_LANGDLL

;--------------------------------

;Installer Sections

Section "Oversampler" SecExecutable
  SetOutPath "$INSTDIR"
  
  ;ADD YOUR OWN FILES HERE...
  File "..\Bin\OversampleVSTPlugin.dll"
  ExecWait '$SYSDIR\regsvr32.exe /s "$INSTDIR\OversampleVSTPlugin.dll"'
  
  ;Create uninstaller
  WriteUninstaller "$INSTDIR\UninstallOversampleVSTPlugin.exe"

SectionEnd

;--------------------------------
;Installer Functions

Function .onInit

;  !insertmacro MUI_LANGDLL_DISPLAY

FunctionEnd

  LangString TEXT_IO_TITLE ${LANG_ENGLISH} "InstallOptions page"
  LangString TEXT_IO_SUBTITLE ${LANG_ENGLISH} "Oversampler"
;  LangString TEXT_IO_TITLE ${LANG_GERMAN} "Auswahlseite"
;  LangString TEXT_IO_SUBTITLE ${LANG_GERMAN} "Oversampler"

;--------------------------------
;Descriptions

  ;Language strings
  LangString DESC_SecExecutable ${LANG_ENGLISH} "Oversampler"

  ;Assign language strings to sections
  !insertmacro MUI_FUNCTION_DESCRIPTION_BEGIN
    !insertmacro MUI_DESCRIPTION_TEXT ${SecExecutable} $(DESC_SecExecutable)
  !insertmacro MUI_FUNCTION_DESCRIPTION_END

;--------------------------------
;Uninstaller Section

Section "Uninstall"

  ;ADD YOUR OWN FILES HERE...
  ExecWait '$SYSDIR\regsvr32.exe /s /u "$INSTDIR\OversampleVSTPlugin.dll"'
  Delete "$INSTDIR\OversampleVSTPlugin.dll"
  Delete "$INSTDIR\UninstallOversampleVSTPlugin.exe"
  DeleteRegKey HKLM "Software\Delphi ASIO & VST Packages\${PRODUCT_NAME}"
SectionEnd

Function un.onInit

;  !insertmacro MUI_UNGETLANGUAGE
  
FunctionEnd
