{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library Decimator;

uses
  Forms,
  DVSTEffect,
  DVSTModule,
  DecimatorModule in 'DecimatorModule.pas' {VSTDecimator: TVSTModule},
  DecimatorGUI in 'DecimatorGUI.pas' {VSTGUI};

function main(audioMaster: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
var VSTModule1 : TVSTDecimator;
begin
 try
  VSTModule1:=TVSTDecimator.Create(Application);
  VSTModule1.Effect^.user:=VSTModule1;
  VSTModule1.AudioMaster:=audioMaster;
  Result := VSTModule1.Effect;
 except
  Result := nil;
 end;
end;

exports Main name 'main';
exports Main name 'VSTPluginMain';

begin
end.

