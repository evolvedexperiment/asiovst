{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library SplitTemplate;

uses
  FastMM4,
  madExcept,
  madLinkDisAsm,
  FastMove,
  RTLVCLOptimize,
  Forms,
  DAV_VSTEffect,
  DAV_VSTModule,
  SplitTemplateDM in 'SplitTemplateDM.pas' {SplitTemplateDataModule: TVSTModule},
  SplitTemplateGUI in 'SplitTemplateGUI.pas' {FmSplitter};

function main(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
 try
  with TSplitTemplateDataModule.Create(Application) do
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
