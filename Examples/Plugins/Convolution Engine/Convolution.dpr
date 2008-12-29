{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library Convolution;

uses
  Forms,
  DAV_VSTEffect,
  DAV_VSTModule,
  ConvolutionDM in 'ConvolutionDM.pas' {ConvolutionDataModule: TVSTModule},
  ConvolutionGUI in 'ConvolutionGUI.pas' {FmConvolution};

function main(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
 try
  with TConvolutionDataModule.Create(Application) do
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
