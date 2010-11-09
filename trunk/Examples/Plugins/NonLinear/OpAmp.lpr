{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library OpAmp;

{$I DAV_Compiler.inc}

uses
  Interfaces,
  Forms,
  {$IFDEF MSWINDOWS}
  DAV_WinAmp,
  {$ENDIF}
  DAV_VSTEffect,
  DAV_VSTBasicModule,
  DAV_Common,
  DAV_VSTPlugin_Lazarus,
  OpAmpModule in 'OpAmpModule.pas' {VSTOpAmp: TVSTModule},
  OpAmpGUI in 'OpAmpGUI.pas' {VSTGUI};

function main(audioMaster: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
var
  VSTOpAmp : TVSTOpAmp;
begin
 try
  Application.Initialize;
//  AppInitialized := True;

  VSTOpAmp := TVSTOpAmp.Create(Application);
  VSTOpAmp.Effect^.user := VSTOpAmp;
  VSTOpAmp.AudioMaster := audioMaster;
  Result := VSTOpAmp.Effect;
 except
  Result := nil;
 end;
end;

exports
{$IFDEF DARWIN}  {OS X entry points}
  VSTPluginMain name '_main',
  VSTPluginMain name '_main_macho',
  VSTPluginMain name '_VSTPluginMain';
{$ELSE}
  VSTPluginMain name 'main',
  VSTPluginMain name 'main_plugin',
  VSTPluginMain name 'VSTPluginMain',
{$ENDIF}

begin
end.