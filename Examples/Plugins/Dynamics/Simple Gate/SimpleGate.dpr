{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library SimpleGate;

uses
  FastMM4,
  Forms,
  DVSTEffect,
  DVSTModule,
  SimpleGateDM in 'SimpleGateDM.pas' {SimpleGateDataModule: TVSTModule},
  EditorFrm in 'EditorFrm.pas' {EditorForm};

function main(audioMaster: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
var
  SimpleGateDataModule: TSimpleGateDataModule;
begin
  try
    SimpleGateDataModule := TSimpleGateDataModule.Create(Application);
    SimpleGateDataModule.AudioMaster := audioMaster;
    with SimpleGateDataModule do
    begin
      Effect^.user := SimpleGateDataModule;
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