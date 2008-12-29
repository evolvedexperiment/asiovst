{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library LA4029;

{$R 'LA4029.res' 'LA4029.rc'}

uses
  FastMM4,
  madExcept,
  madLinkDisAsm,
  Forms,
  DAV_VSTEffect,
  DAV_VSTModule,
  LA4029DM in 'LA4029DM.pas' {LA4029DataModule: TVSTModule},
  EditorFrm in 'EditorFrm.pas' {FmLA4029},
  DAV_DSPLevelingAmplifier in '..\..\..\..\Source\DSP\DAV_DSPLevelingAmplifier.pas',
  DAV_GuiVUMeter in '..\..\..\..\Source\DAV_GuiVUMeter.pas';

function main(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
 try
  with TLA4029DataModule.Create(Application) do
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
