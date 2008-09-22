{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library FrequencySplitTemplate;

{$R 'Limiter.res' 'Limiter.rc'}

uses
  madExcept,
  madLinkDisAsm,
  madListHardware,
  madListProcesses,
  madListModules,
  Forms,
  DAV_VSTEffect,
  DAV_VSTModule,
  FrequencySplitTemplateDM in 'FrequencySplitTemplateDM.pas' {FrequencySplitTemplateDataModule: TVSTModule},
  FrequencySplitTemplateGUI in 'FrequencySplitTemplateGUI.pas' {FmFrequencySplitter};

function main(audioMaster: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
var
  FrequencySplitTemplateDataModule: TFrequencySplitTemplateDataModule;
begin
  try
    FrequencySplitTemplateDataModule := TFrequencySplitTemplateDataModule.Create(Application);
    FrequencySplitTemplateDataModule.Effect^.user := FrequencySplitTemplateDataModule;
    FrequencySplitTemplateDataModule.AudioMaster := audioMaster;
    Result := FrequencySplitTemplateDataModule.Effect;
  except
    Result := nil;
  end;
end;

exports Main name 'main';
exports Main name 'VSTPluginMain';

begin
end.
