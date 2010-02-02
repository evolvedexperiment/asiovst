library AnalogueMojo;

uses
  Interfaces,
  {$IFNDEF DARWIN}
  DAV_WinAmp,
  {$ENDIF}
  DAV_VSTEffect,
  DAV_VSTBasicModule,
  AnalogueMojoDSP in 'AnalogueMojoDSP.pas' {AnalogueMojoDM: TVSTModule};

function VstPluginMain(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
  Result := VstModuleMain(AudioMasterCallback, TAnalogueMojoDM);
end;

{$IFNDEF DARWIN}
function WinampDSPGetHeader: PWinAmpDSPHeader; cdecl; export;
begin
  Result := WinampDSPModuleHeader(TAnalogueMojoDM);
end;
{$ENDIF}

exports
{$IFDEF DARWIN}  {OS X entry points}
  VSTPluginMain name '_main',
  VSTPluginMain name '_main_macho',
  VSTPluginMain name '_VSTPluginMain';
{$ELSE}
  VSTPluginMain name 'main',
  VSTPluginMain name 'main_plugin',
  VSTPluginMain name 'VSTPluginMain',
  WinampDSPGetHeader name 'winampDSPGetHeader2';
{$ENDIF}

begin
end.
