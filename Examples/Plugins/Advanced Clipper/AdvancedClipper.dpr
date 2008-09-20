{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library AdvancedClipper;

{$R 'AdvancedClipper.res' 'AdvancedClipper.rc'}

uses
  Forms,
  DAV_VSTEffect,
  DAV_VSTModule,
  AdvancedClipperDM in 'AdvancedClipperDM.pas' {AdvancedClipperDataModule: TVSTModule},
  AdvancedClipperGUI in 'AdvancedClipperGUI.pas' {FmAdvancedClipper};

function main(audioMaster: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
var
  AdvancedClipperDataModule: TAdvancedClipperDataModule;
begin
  try
    AdvancedClipperDataModule := TAdvancedClipperDataModule.Create(Application);
    AdvancedClipperDataModule.Effect^.user := AdvancedClipperDataModule;
    AdvancedClipperDataModule.AudioMaster := audioMaster;
    Result := AdvancedClipperDataModule.Effect;
  except
    Result := nil;
  end;
end;

exports Main name 'main';
exports Main name 'VSTPluginMain';

begin
end.