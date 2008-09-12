{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library AmpSim;

{$R 'AmpKnob.res' 'AmpKnob.rc'}

uses
  Forms,
  DVSTEffect,
  DVSTModule,
  AmpSimDM in 'AmpSimDM.pas' {ComboDataModule: TVSTModule},
  AmpSimGUI in 'AmpSimGUI.pas' {FmCombo};

function main(audioMaster: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
var
  ComboDataModule: TComboDataModule;
begin
  try
    ComboDataModule := TComboDataModule.Create(Application);
    ComboDataModule.Effect^.user := ComboDataModule;
    ComboDataModule.AudioMaster := audioMaster;
    Result := ComboDataModule.Effect;
  except
    Result := nil;
  end;
end;

exports Main name 'main';
exports Main name 'VSTPluginMain';

begin
end.
