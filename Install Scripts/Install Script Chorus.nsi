;NSIS Modern User Interface version 1.70
;Chorus Installer
;Written by Christian Budde

SetCompressor lzma

;--------------------------------
;Include Modern UI
;  !include "Sections.nsh"
  !include "MUI.nsh"

;--------------------------------
;General

  ;Name and file
  Name "Chorus Installer"
  OutFile "Chorus_Install.exe"

  ;Default installation folder
  InstallDir "$VSTPlugin\VSTPlugIns"
  
  ;Get installation folder from registry if available
  InstallDirRegKey HKLM "SOFTWARE\VST" "VSTPluginsPath"

  BrandingText "Delphi ASIO & VST Packages"

  ; Turn on the xp style of drawing
  XPStyle ON

;--------------------------------
;Interface Settings

  !define PRODUCT_NAME "Chorus"
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
;Pages

  !insertmacro MUI_PAGE_WELCOME
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

;--------------------------------
;Installer Sections

Section "Chorus VST-Plugin" SecVSTPlugin
  SetOutPath "$INSTDIR"
  
  ;ADD YOUR OWN FILES HERE...
  File "..\Bin\Chorus.dll"

  ;Store installation folder
  WriteRegStr HKLM "SOFTWARE\Delphi ASIO & VST Packages\${PRODUCT_NAME}" "" $INSTDIR
  
  ;Create uninstaller
  WriteUninstaller "$INSTDIR\UninstallChorus.exe"
SectionEnd

Section "Chorus Manual" SecManual
  SetOutPath "$INSTDIR"
  
  ;ADD YOUR OWN FILES HERE...
  File "..\Bin\Chorus Manual.pdf"

  ;Store installation folder
  WriteRegStr HKLM "SOFTWARE\Delphi ASIO & VST Packages\${PRODUCT_NAME}" "" $INSTDIR
  
  ;Create uninstaller
  WriteUninstaller "$INSTDIR\UninstallChorus.exe"
SectionEnd

;--------------------------------
;Installer Functions

  LangString TEXT_IO_TITLE ${LANG_ENGLISH} "InstallOptions page"
  LangString TEXT_IO_SUBTITLE ${LANG_ENGLISH} "Chorus VST Plugin"

;--------------------------------
;Descriptions

  ;Language strings
  LangString DESC_SecVSTPlugin ${LANG_ENGLISH} "Chorus VST Plugin"
  LangString DESC_SecManual ${LANG_ENGLISH} "Chorus Manual"

  ;Assign language strings to sections
  !insertmacro MUI_FUNCTION_DESCRIPTION_BEGIN
    !insertmacro MUI_DESCRIPTION_TEXT ${SecVSTPlugin} $(DESC_SecVSTPlugin)
    !insertmacro MUI_DESCRIPTION_TEXT ${SecManual} $(DESC_SecManual)
  !insertmacro MUI_FUNCTION_DESCRIPTION_END

;--------------------------------
;Uninstaller Section

Section "Uninstall"

  ;ADD YOUR OWN FILES HERE...
  Delete "$INSTDIR\Chorus.dll"
  Delete "$INSTDIR\Chorus Manual.pdf"
  DeleteRegKey HKLM "SOFTWARE\Delphi ASIO & VST Packages\${PRODUCT_NAME}"

SectionEnd
