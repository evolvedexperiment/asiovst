{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library mdaVocoder;

uses
  Forms,
  DVSTEffect,
  DVSTModule,
  VocoderDM in 'VocoderDM.pas' {VocoderDataModule: TVSTModule};

function main(audioMaster: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
var
  VocoderDataModule: TVocoderDataModule;
begin
  try
    VocoderDataModule := TVocoderDataModule.Create(Application);
    VocoderDataModule.Effect^.user := VocoderDataModule;
    VocoderDataModule.AudioMaster := audioMaster;
    Result := VocoderDataModule.Effect;
  except
    Result := nil;
  end;
end;

exports Main name 'main';
exports Main name 'VSTPluginMain';

begin
end.