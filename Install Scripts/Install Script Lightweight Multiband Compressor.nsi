;NSIS Modern User Interface version 1.70
;Lightweight Multiband Compressor Installer
;Written by Christian Budde

SetCompressor lzma

;--------------------------------
;Include Modern UI
;  !include "Sections.nsh"
  !include "MUI.nsh"

;--------------------------------
;General

  ;Name and file
  Name "Lightweight Multiband Compressor Installer"
  OutFile "Lightweight_Multiband_Compressor_Install.exe"

  ;Default installation folder
  InstallDir "$PROGRAMFILES\VSTPlugIns"
  
  ;Get installation folder from registry if available
  InstallDirRegKey HKLM "SOFTWARE\VST" "VSTPluginsPath"

  BrandingText "Delphi ASIO & VST Packages"

  ; Turn on the xp style of drawing
  XPStyle ON

;--------------------------------
;Interface Settings

  !define PRODUCT_NAME "Lightweight Multiband Compressor"
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

Section "Lightweight Multiband Compressor VST-Plugin" SecVSTPlugin
  SetOutPath "$INSTDIR"
  
  ;ADD YOUR OWN FILES HERE...
  File "..\Bin\LightweightMultibandCompressor.dll"

  ;Store installation folder
  WriteRegStr HKLM "SOFTWARE\Delphi ASIO & VST Packages\${PRODUCT_NAME}" "" $INSTDIR
  
  ;Create uninstaller
  WriteUninstaller "$INSTDIR\UninstallMultibandCompressor.exe"
SectionEnd

Section "Lightweight Multiband Compressor Manual" SecManual
  SetOutPath "$INSTDIR"
  
  ;ADD YOUR OWN FILES HERE...
  File "..\Bin\Lightweight Multiband Compressor Manual.pdf"

  ;Store installation folder
  WriteRegStr HKLM "SOFTWARE\Delphi ASIO & VST Packages\${PRODUCT_NAME}" "" $INSTDIR
  
  ;Create uninstaller
  WriteUninstaller "$INSTDIR\UninstallMultibandCompressor.exe"
SectionEnd

;--------------------------------
;Installer Functions

  LangString TEXT_IO_TITLE ${LANG_ENGLISH} "InstallOptions page"
  LangString TEXT_IO_SUBTITLE ${LANG_ENGLISH} "Lightweight Multiband Compressor VST Plugin"

;--------------------------------
;Descriptions

  ;Language strings
  LangString DESC_SecVSTPlugin ${LANG_ENGLISH} "Lightweight Multiband Compressor VST Plugin"
  LangString DESC_SecManual ${LANG_ENGLISH} "Lightweight Multiband Compressor Manual"

  ;Assign language strings to sections
  !insertmacro MUI_FUNCTION_DESCRIPTION_BEGIN
    !insertmacro MUI_DESCRIPTION_TEXT ${SecVSTPlugin} $(DESC_SecVSTPlugin)
    !insertmacro MUI_DESCRIPTION_TEXT ${SecManual} $(DESC_SecManual)
  !insertmacro MUI_FUNCTION_DESCRIPTION_END

;--------------------------------
;Uninstaller Section

Section "Uninstall"

  ;ADD YOUR OWN FILES HERE...
  Delete "$INSTDIR\LightweightMultibandCompressor.dll"
  Delete "$INSTDIR\Lightweight Multiband Compressor Manual.pdf"
  DeleteRegKey HKLM "SOFTWARE\Delphi ASIO & VST Packages\${PRODUCT_NAME}"

SectionEnd
