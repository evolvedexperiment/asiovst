{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library LA1701;

{$R 'LA1701.res' 'LA1701.rc'}

uses
  FastMM4,
  madExcept,
  madLinkDisAsm,
  madListHardware,
  madListProcesses,
  madListModules,
  Forms,
  DAV_VSTEffect,
  DAV_VSTModule,
  LA1701DM in 'LA1701DM.pas' {LA1701DataModule: TVSTModule},
  EditorFrm in 'EditorFrm.pas' {FmLA1701},
  DAV_DSPLevelingAmplifier in '..\..\..\..\Source\DSP\DAV_DSPLevelingAmplifier.pas',
  DAV_GuiVUMeter in '..\..\..\..\Source\DAV_GuiVUMeter.pas';

function main(audioMaster: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
var
  LA1701DataModule: TLA1701DataModule;
begin
 try
  LA1701DataModule := TLA1701DataModule.Create(Application);
  LA1701DataModule.AudioMaster := audioMaster;
  with LA1701DataModule do
   begin
    Effect^.user := LA1701DataModule;
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