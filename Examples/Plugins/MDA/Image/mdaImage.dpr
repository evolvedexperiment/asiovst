{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library mdaImage;

uses
  Forms,
  DAV_VSTEffect,
  DAV_VSTModule,
  ImageDM in 'ImageDM.pas' {ImageDataModule: TVSTModule};

function main(audioMaster: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
var
  ImageDataModule: TImageDataModule;
begin
  try
    ImageDataModule := TImageDataModule.Create(Application);
    ImageDataModule.Effect^.user := ImageDataModule;
    ImageDataModule.AudioMaster := audioMaster;
    Result := ImageDataModule.Effect;
  except
    Result := nil;
  end;
end;

exports Main name 'main';
exports Main name 'VSTPluginMain';

begin
end.