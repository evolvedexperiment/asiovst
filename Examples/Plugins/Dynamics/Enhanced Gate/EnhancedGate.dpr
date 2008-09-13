{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library EnhancedGate;

{$R 'Knob3D.res' 'Knob3D.rc'}

uses
  FastMM4,
  Forms,
  DAV_VSTEffect,
  DAV_VSTModule,
  EnhancedGateDM in 'EnhancedGateDM.pas' {EnhancedGateDataModule: TVSTModule},
  EditorFrm in 'EditorFrm.pas' {EditorForm};

function main(audioMaster: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
var
  EnhancedGateDataModule: TEnhancedGateDataModule;
begin
  try
    EnhancedGateDataModule := TEnhancedGateDataModule.Create(Application);
    EnhancedGateDataModule.AudioMaster := audioMaster;
    with EnhancedGateDataModule do
    begin
      Effect^.user := EnhancedGateDataModule;
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