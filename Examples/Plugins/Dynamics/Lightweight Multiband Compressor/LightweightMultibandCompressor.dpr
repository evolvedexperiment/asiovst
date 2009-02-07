{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library LightweightMultibandCompressor;

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
  LightweightMultibandCompressorDM in 'LightweightMultibandCompressorDM.pas' {LightweightMultibandCompressorDataModule: TVSTModule},
  LightweightMultibandCompressorGUI in 'LightweightMultibandCompressorGUI.pas' {FmLightweightMultibandCompressor};

function main(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
  try
    with TLightweightMultibandCompressorDataModule.Create(Application) do
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
