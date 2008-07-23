{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library VUMeter;

uses
  Forms,
  DVSTEffect,
  DVSTModule,
  VUMeterModule in 'VUMeterModule.pas' {VSTVUMeterModule: TVSTModule},
  VUMeterGUI in 'VUMeterGUI.pas' {VSTVUMeterGUI};

function main(audioMaster: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
var VSTVUMeterModule : TVSTVUMeterModule;
begin
 try
  VSTVUMeterModule := TVSTVUMeterModule.Create(Application);
  VSTVUMeterModule.Effect^.user := VSTVUMeterModule;
  VSTVUMeterModule.AudioMaster := audioMaster;
  Result := VSTVUMeterModule.Effect;
 except
  Result := nil;
 end;
end;

exports Main name 'main';
exports Main name 'VSTPluginMain';

begin
end.