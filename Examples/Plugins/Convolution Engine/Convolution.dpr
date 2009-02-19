{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library Convolution;

uses
  FastMM4, // either download the library or comment if there is an error here
  madExcept,
  madLinkDisAsm,
  madListHardware,
  madListProcesses,
  madListModules,
  FastMove, // either download the library or comment if there is an error here
  DAV_VSTBasicModule,
  ConvolutionDM in 'ConvolutionDM.pas' {ConvolutionDataModule: TVSTModule},
  ConvolutionGUI in 'ConvolutionGUI.pas' {FmConvolution};

begin
 BasicVSTModuleClass := TConvolutionDataModule;
end.
