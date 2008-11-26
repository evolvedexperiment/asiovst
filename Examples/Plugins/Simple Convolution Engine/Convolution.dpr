{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library Convolution;

uses
  Forms,
  DAV_VSTEffect,
  DAV_VSTModule,
  ConvolutionDM in 'ConvolutionDM.pas' {ConvolutionDataModule: TVSTModule},
  ConvolutionGUI in 'ConvolutionGUI.pas' {FmConvolution};

function main(audioMaster: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
var
  ConvolutionDataModule: TConvolutionDataModule;
begin
  try
    ConvolutionDataModule := TConvolutionDataModule.Create(Application);
    ConvolutionDataModule.Effect^.user := ConvolutionDataModule;
    ConvolutionDataModule.AudioMaster := audioMaster;
    Result := ConvolutionDataModule.Effect;
  except
    Result := nil;
  end;
end;

exports Main name 'main';
exports Main name 'VSTPluginMain';

begin
end.
