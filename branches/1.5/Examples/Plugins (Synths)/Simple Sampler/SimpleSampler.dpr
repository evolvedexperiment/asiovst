{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library SimpleSampler;

{$I DAV_Compiler.inc}

uses
  FastMM4,  // either download the library or comment if there is an error here
  {$IFDEF UseFastMove}
  FastMove, // either download the library or comment if there is an error here
  {$ENDIF}
  Forms,
  DAV_VSTEffect,
  DAV_VSTModule,
  SimpleSamplerModule in 'SimpleSamplerModule.pas' {VSTSSModule: TVSTModule},
  SimpleSamplerGUI in 'SimpleSamplerGUI.pas' {VSTGUI},
  SimpleSamplerVoice in 'SimpleSamplerVoice.pas',
  VoiceList in 'VoiceList.pas';

function main(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
 try
  with TVSTSSModule.Create(Application) do
   begin
    AudioMaster := AudioMasterCallback;
    Result := Effect;
   end;
 except
  Result := nil;
 end;
end;

exports Main name 'main';
exports Main name 'VSTPluginMain';

begin
end.
                                                             
