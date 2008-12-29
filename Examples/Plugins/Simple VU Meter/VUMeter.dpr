{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library VUMeter;

uses
  Forms,
  DAV_VSTEffect,
  DAV_VSTModule,
  VUMeterModule in 'VUMeterModule.pas' {VSTVUMeterModule: TVSTModule},
  VUMeterGUI in 'VUMeterGUI.pas' {VSTVUMeterGUI};

function main(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
 try
  with TVSTVUMeterModule.Create(Application) do
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