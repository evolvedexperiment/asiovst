{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library Decimator;

uses
  Forms,
  DVSTEffect,
  DVSTModule,
  DecimatorModule in 'DecimatorModule.pas' {VSTDecimator: TVSTModule},
  DecimatorGUI in 'DecimatorGUI.pas' {VSTGUI};

function main(audioMaster: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
var
  VSTDecimator : TVSTDecimator;
begin
 try
  VSTDecimator := TVSTDecimator.Create(Application);
  VSTDecimator.Effect^.user := VSTDecimator;
  VSTDecimator.AudioMaster := audioMaster;
  Result := VSTDecimator.Effect;
 except
  Result := nil;
 end;
end;

exports Main name 'main';
exports Main name 'VSTPluginMain';

begin
end.

