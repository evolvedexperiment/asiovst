{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library OversampleTemplate;

{$R 'Test.res' 'Test.rc'}

uses
  FastMM4,
  madExcept,
  madLinkDisAsm,
  madListHardware,
  madListProcesses,
  madListModules,
  FastMove,
  RTLVCLOptimize,
  Forms,
  DAV_VSTEffect,
  DAV_VSTModule,
  OversampleTemplateDM in 'OversampleTemplateDM.pas' {OversampleTemplateDataModule: TVSTModule},
  OversampleTemplateGUI in 'OversampleTemplateGUI.pas' {FmOversampleter};

function main(audioMaster: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
var
  OversampleTemplateDataModule: TOversampleTemplateDataModule;
begin
  try
    OversampleTemplateDataModule := TOversampleTemplateDataModule.Create(Application);
    OversampleTemplateDataModule.Effect^.user := OversampleTemplateDataModule;
    OversampleTemplateDataModule.AudioMaster := audioMaster;
    Result := OversampleTemplateDataModule.Effect;
  except
    Result := nil;
  end;
end;

exports Main name 'main';
exports Main name 'VSTPluginMain';

begin
end.
