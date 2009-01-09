{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library SimpleSampleDelay;

uses
  FastMM4,
  FastMove,
  Forms,
  DAV_VSTEffect,
  DAV_VSTModule,
  SimpleSampleDelayModule in 'SimpleSampleDelayModule.pas' {SimpleSampleDelayVST: TVST2Module},
  SimpleSampleDelayGUI in 'SimpleSampleDelayGUI.pas' {FmSimpleSampleDelay};

function main(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
 try
  with TSimpleSampleDelayVST.Create(Application) do
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
