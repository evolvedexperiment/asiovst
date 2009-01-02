{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library fReeverb;

{$R 'fReeverbKnob.res' 'fReeverbKnob.rc'}

uses
  FastMM4, // either download the library or comment if there is an error here
  madExcept,
  madLinkDisAsm,
  Forms,
  DAV_VSTEffect,
  DAV_VSTModule,
  fReeverbModule in 'fReeverbModule.pas' {fReeverbVST: TVSTModule},
  fReeverbGUI in 'fReeverbGUI.pas' {FmReverb};

function main(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
 try
  with TfReeverbVST.Create(Application) do
   begin
    AudioMaster := AudioMasterCallback;
    Result := Effect;
   end;
 except
  Result := nil;
 end;
end;

exports Main name 'main';
exports Main name 'VstPluginMain';

begin
end.