{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library GraphicEQ;

uses
  Forms,
  DAV_VSTEffect,
  DAV_VSTModule,
  PluginDM in 'PluginDM.pas' {PluginDataModule: TVSTModule},
  EditorFrm in 'EditorFrm.pas' {EditorForm};

function main(audioMaster: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
var
  PluginDataModule: TPluginDataModule;
begin
  try
    PluginDataModule := TPluginDataModule.Create(Application);
    PluginDataModule.Effect^.user := PluginDataModule;
    PluginDataModule.AudioMaster := audioMaster;
    Result := PluginDataModule.Effect;
  except
    Result := nil;
  end;
end;

exports Main name 'main';
exports Main name 'VSTPluginMain';

begin
end.