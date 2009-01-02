{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library OversampleTemplate;

{$R 'Test.res' 'Test.rc'}

uses
  FastMM4,  // either download the library or comment if there is an error here
  FastMove, // either download the library or comment if there is an error here
  RTLVCLOptimize, // "
  Forms,
  DAV_VSTEffect,
  DAV_VSTModule,
  OversampleTemplateDM in 'OversampleTemplateDM.pas' {OversampleTemplateDataModule: TVSTModule},
  OversampleTemplateGUI in 'OversampleTemplateGUI.pas' {FmOversampleter};

function main(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
 try
  with TOversampleTemplateDataModule.Create(Application) do
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
