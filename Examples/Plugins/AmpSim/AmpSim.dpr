{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library AmpSim;

{$R 'AmpKnob.res' 'AmpKnob.rc'}

uses
  FastMM4,  // either download the library or comment if there is an error here
  FastMove, // either download the library or comment if there is an error here
  madExcept,
  madLinkDisAsm,
  madListProcesses,
  madListModules,
  RTLVCLOptimize,
  Forms,
  DAV_VSTEffect,
  DAV_VSTModule,
  AmpSimDM in 'AmpSimDM.pas' {ComboDataModule: TVSTModule},
  AmpSimGUI in 'AmpSimGUI.pas' {FmCombo};

function main(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
 try
  with TComboDataModule.Create(Application) do
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
