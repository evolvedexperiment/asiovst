{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library FastCompressor;

{$R 'Compressor.res' 'Compressor.rc'}

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
  FastCompressorDM in 'FastCompressorDM.pas' {FastCompressorDataModule: TVSTModule},
  FastCompressorGUI in 'FastCompressorGUI.pas' {FmFastCompressor};

function main(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
  try
    with TFastCompressorDataModule.Create(Application) do
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