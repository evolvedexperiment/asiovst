{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library FastMultibandCompressor;

{$R 'MultibandCompressor.res' 'MultibandCompressor.rc'}

uses
  FastMM4,
  madExcept,
  madLinkDisAsm,
  madListProcesses,
  madListModules,
  FastMove,
  Forms,
  DAV_VSTEffect,
  DAV_VSTModule,
  FastMultibandCompressorDM in 'FastMultibandCompressorDM.pas' {FastMultibandCompressorDataModule: TVSTModule},
  FastMultibandCompressorGUI in 'FastMultibandCompressorGUI.pas' {FmFastMultibandCompressor};

function main(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
  try
    with TFastMultibandCompressorDataModule.Create(Application) do
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
