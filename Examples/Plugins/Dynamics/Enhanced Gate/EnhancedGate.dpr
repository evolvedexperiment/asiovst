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

function main(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
 try
  with TEnhancedGateDataModule.Create(Application) do
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