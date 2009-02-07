;NSIS Modern User Interface version 1.70
;Lightweight Compressor Installer
;Written by Christian Budde

SetCompressor lzma

;--------------------------------
;Include Modern UI
;  !include "Sections.nsh"
  !include "MUI.nsh"

;--------------------------------
;General

  ;Name and file
  Name "Lightweight Series Installer"
  OutFile "Lightweight_Series_Install.exe"

  ;Default installation folder
  InstallDir "$PROGRAMFILES\VSTPlugIns"
  
  ;Get installation folder from registry if available
  InstallDirRegKey HKLM "SOFTWARE\VST" "VSTPluginsPath"

  BrandingText "Delphi ASIO & VST Packages"

  ; Turn on the xp style of drawing
  XPStyle ON

;--------------------------------
;Interface Settings

  !define PRODUCT_NAME "Lightweight Series"
  !define PRODUCT_VERSION "1.0.1"
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

Section "Lightweight Series VST-Plugin" SecVSTPlugin
  SetOutPath "$INSTDIR"
  
  ;ADD YOUR OWN FILES HERE...
  File "..\Bin\LightweightLimiter.dll"
  File "..\Bin\LightweightGate.dll"
  File "..\Bin\LightweightCompressor.dll"
  File "..\Bin\LightweightFeedbackCompressor.dll"
  File "..\Bin\LightweightMultibandCompressor.dll"

  ;Store installation folder
  WriteRegStr HKLM "SOFTWARE\Delphi ASIO & VST Packages\${PRODUCT_NAME}" "" $INSTDIR
  
  ;Create uninstaller
  WriteUninstaller "$INSTDIR\UninstallLightweightSeries.exe"
SectionEnd

Section "Lightweight Series Manual" SecManual
  SetOutPath "$INSTDIR"
  
  ;ADD YOUR OWN FILES HERE...
  File "..\Bin\Lightweight Limiter Manual.pdf"
  File "..\Bin\Lightweight Gate Manual.pdf"
  File "..\Bin\Lightweight Compressor Manual.pdf"
  File "..\Bin\Lightweight Feedback Compressor Manual.pdf"
  File "..\Bin\Lightweight Multiband Compressor Manual.pdf"

  ;Store installation folder
  WriteRegStr HKLM "SOFTWARE\Delphi ASIO & VST Packages\${PRODUCT_NAME}" "" $INSTDIR
  
  ;Create uninstaller
  WriteUninstaller "$INSTDIR\UninstallLightweightSeries.exe"
SectionEnd

;--------------------------------
;Installer Functions

  LangString TEXT_IO_TITLE ${LANG_ENGLISH} "InstallOptions page"
  LangString TEXT_IO_SUBTITLE ${LANG_ENGLISH} "Lightweight Series VST Plugin"

;--------------------------------
;Descriptions

  ;Language strings
  LangString DESC_SecVSTPlugin ${LANG_ENGLISH} "Lightweight Series VST Plugin"
  LangString DESC_SecManual ${LANG_ENGLISH} "Lightweight Series Manual"

  ;Assign language strings to sections
  !insertmacro MUI_FUNCTION_DESCRIPTION_BEGIN
    !insertmacro MUI_DESCRIPTION_TEXT ${SecVSTPlugin} $(DESC_SecVSTPlugin)
    !insertmacro MUI_DESCRIPTION_TEXT ${SecManual} $(DESC_SecManual)
  !insertmacro MUI_FUNCTION_DESCRIPTION_END

;--------------------------------
;Uninstaller Section

Section "Uninstall"

  ;ADD YOUR OWN FILES HERE...
  Delete "$INSTDIR\LightweightLimiter.dll"
  Delete "$INSTDIR\LightweightGate.dll"
  Delete "$INSTDIR\LightweightCompressor.dll"
  Delete "$INSTDIR\LightweightFeedbackCompressor.dll"
  Delete "$INSTDIR\LightweightMultibandCompressor.dll"
  Delete "$INSTDIR\Lightweight Limiter Manual.pdf"
  Delete "$INSTDIR\Lightweight Gate Manual.pdf"
  Delete "$INSTDIR\Lightweight Compressor Manual.pdf"
  Delete "$INSTDIR\Lightweight Feedback Compressor Manual.pdf"
  Delete "$INSTDIR\Lightweight Multiband Compressor Manual.pdf"
  DeleteRegKey HKLM "SOFTWARE\Delphi ASIO & VST Packages\${PRODUCT_NAME}"

SectionEnd
