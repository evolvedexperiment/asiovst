{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library MultibandCompressor;

{$R 'SlimSlowKnob.res' 'SlimSlowKnob.rc'}

uses
  Forms,
  DVSTEffect,
  DVSTModule,
  MBCDM in 'MBCDM.pas' {MBCDataModule: TVSTModule},
  MBCGUI in 'MBCGUI.pas' {FmMBC};

function main(audioMaster: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
var
  MBCDataModule: TMBCDataModule;
begin
  try
    MBCDataModule := TMBCDataModule.Create(Application);
    MBCDataModule.Effect^.user := MBCDataModule;
    MBCDataModule.AudioMaster := audioMaster;
    Result := MBCDataModule.Effect;
  except
    Result := nil;
  end;
end;

exports Main name 'main';
exports Main name 'VSTPluginMain';

begin
end.