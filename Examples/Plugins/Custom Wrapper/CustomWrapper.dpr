{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library CustomWrapper;

uses
  Forms,
  DAV_VSTEffect,
  DAV_VSTModule,
  CustomWrapperDM in 'CustomWrapperDM.pas' {CustomWrapperDataModule: TVSTModule};

function main(audioMaster: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
var
  CustomWrapperDataModule: TCustomWrapperDataModule;
begin
  try
    CustomWrapperDataModule := TCustomWrapperDataModule.Create(Application);
    CustomWrapperDataModule.Effect^.user := CustomWrapperDataModule;
    CustomWrapperDataModule.AudioMaster := audioMaster;
    Result := CustomWrapperDataModule.Effect;
  except
    Result := nil;
  end;
end;

exports Main name 'main';
exports Main name 'VSTPluginMain';

begin
end.
