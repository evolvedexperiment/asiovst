{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library VariableDelay;

uses
  FastMM4,
  FastMove,
  Forms,
  DAV_VSTEffect,
  DAV_VSTModule,
  VariableDelayModule in 'VariableDelayModule.pas' {VariableDelayVST: TVST2Module},
  VariableDelayGUI in 'VariableDelayGUI.pas' {VSTGUI};

function main(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
 try
  with TVariableDelayVST.Create(Application) do
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
