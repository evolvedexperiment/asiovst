{ Diese Datei wurde automatisch von Lazarus erzeugt. Sie darf nicht bearbeitet 
  werden!
  Dieser Quelltext dient nur dem Übersetzen und Installieren des Packages.
 }

unit DAV_DSP_Lazarus; 

interface

uses
    DAV_DspBesselFilter, DAV_DspButterworthFilter, DAV_DspChebyshevFilter, 
  DAV_DspDFT, DAV_DspDitherNoiseShaper, DAV_DspDynamics, DAV_DspFFT, 
  DAV_DspFilter, DAV_DspInterpolation, DAV_DspMetronome, DAV_DspMinBlep, 
  DAV_DspPhaser, DAV_DspRegister, DAV_DspRemez, DAV_DspStateVariableFilter, 
  DAV_DspWaveshaper, DAV_DspWindowing, DAV_DSPLevelingAmplifier, 
  DAV_DSPUpDownsampling, DAV_DSPLFO, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('DAV_DspRegister', @DAV_DspRegister.Register); 
end; 

initialization
  RegisterPackage('DAV_DSP_Lazarus', @Register); 
end.
