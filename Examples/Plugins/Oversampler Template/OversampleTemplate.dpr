{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library OversampleTemplate;

{-$R 'Test.res' 'Test.rc'}
{-$R 'Chainer.res' 'Chainer.rc'}

uses
  FastMM4,  // either download the library or comment if there is an error here
  FastMove, // either download the library or comment if there is an error here
  RTLVCLOptimize, // "
  DAV_WinAmp,
  DAV_VSTEffect,
  DAV_VSTBasicModule,
  OversampleTemplateDM in 'OversampleTemplateDM.pas' {OversampleTemplateDataModule: TVSTModule},
  OversampleTemplateGUI in 'OversampleTemplateGUI.pas' {FmOversampleter};

function VstPluginMain(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
 Result := VstModuleMain(AudioMasterCallback, TOversampleTemplateDataModule);
end;

function WinampDSPGetHeader: PWinAmpDSPHeader; cdecl; export;
begin
 Result := WinampDSPModuleHeader(TOversampleTemplateDataModule);
end;

exports VstPluginMain name 'main';
exports VstPluginMain name 'VSTPluginMain';
exports WinampDSPGetHeader name 'winampDSPGetHeader2';

begin
end.
