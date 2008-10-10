{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library ChebyshevWaveshaper;

{$R 'ChebyshevWaveshaper.res' 'ChebyshevWaveshaper.rc'}

uses
  Forms,
  DAV_VSTEffect,
  DAV_VSTModule,
  ChebyshevWaveshaperDM in 'ChebyshevWaveshaperDM.pas' {ChebyshevWaveshaperDataModule: TVSTModule},
  ChebyshevWaveshaperGUI in 'ChebyshevWaveshaperGUI.pas' {FmChebyshevWaveshaper};

function main(audioMaster: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
var
  ChebyshevWaveshaperDataModule: TChebyshevWaveshaperDataModule;
begin
  try
    ChebyshevWaveshaperDataModule := TChebyshevWaveshaperDataModule.Create(Application);
    ChebyshevWaveshaperDataModule.Effect^.user := ChebyshevWaveshaperDataModule;
    ChebyshevWaveshaperDataModule.AudioMaster := audioMaster;
    Result := ChebyshevWaveshaperDataModule.Effect;
  except
    Result := nil;
  end;
end;

exports Main name 'main';
exports Main name 'VSTPluginMain';

begin
end.