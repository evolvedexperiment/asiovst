{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library Wrapper;

{$R 'Wrapper.res' 'Wrapper.rc'}

uses
  Forms,
  DAV_VSTEffect,
  DAV_VSTModule,
  WrapperDM in 'WrapperDM.pas' {WrapperDataModule: TVSTModule};

function main(audioMaster: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
var
  WrapperDataModule: TWrapperDataModule;
begin
  try
    WrapperDataModule := TWrapperDataModule.Create(Application);
    WrapperDataModule.Effect^.user := WrapperDataModule;
    WrapperDataModule.AudioMaster := audioMaster;
    Result := WrapperDataModule.Effect;
  except
    Result := nil;
  end;
end;

exports Main name 'main';
exports Main name 'VSTPluginMain';

begin
end.