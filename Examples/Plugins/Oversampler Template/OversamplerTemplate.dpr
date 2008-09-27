{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library OversamplerTemplate;

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
  OversamplerTemplateDM in 'OversamplerTemplateDM.pas' {OversamplerTemplateDataModule: TVSTModule},
  OversamplerTemplateGUI in 'OversamplerTemplateGUI.pas' {FmOversamplerter};

function main(audioMaster: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
var
  OversamplerTemplateDataModule: TOversamplerTemplateDataModule;
begin
  try
    OversamplerTemplateDataModule := TOversamplerTemplateDataModule.Create(Application);
    OversamplerTemplateDataModule.Effect^.user := OversamplerTemplateDataModule;
    OversamplerTemplateDataModule.AudioMaster := audioMaster;
    Result := OversamplerTemplateDataModule.Effect;
  except
    Result := nil;
  end;
end;

exports Main name 'main';
exports Main name 'VSTPluginMain';

begin
end.
