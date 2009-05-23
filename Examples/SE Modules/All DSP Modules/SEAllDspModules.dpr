library SEAllDspModules;

{$I DAV_Compiler.INC}



uses
  SysUtils,
  Classes,
  DAV_SECommon,
  DAV_SEModule,
  SEApproximationsModule in '..\Approximations\SEApproximationsModule.pas',
  SEAudioRGBModule in '..\AudioRGB\SEAudioRGBModule.pas',
  SEBarberpoleModule in '..\Barberpole\SEBarberpoleModule.pas',
  SEButterworthModule in '..\Butterworth\SEButterworthModule.pas',
  SEChebyshevFilterModule in '..\Chebyshev\SEChebyshevFilterModule.pas',
  SEChebyshevWaveshaperModule in '..\ChebyshevWaveshaper\SEChebyshevWaveshaperModule.pas',
  SEChorusModule in '..\Chorus\SEChorusModule.pas',
  SEConvolutionModule in '..\Convolution\SEConvolutionModule.pas',
  SELowLatencyConvolutionModule in '..\Convolution\SELowLatencyConvolutionModule.pas',
  SEDynamicsModule in '..\Dynamics\SEDynamicsModule.pas',
  SEEnvelopeModule in '..\Envelope\SEEnvelopeModule.pas',
  SEFiltersModule in '..\Filters\SEFiltersModule.pas',
  SELightweightDynamicsModule in '..\Lightweight Dynamics\SELightweightDynamicsModule.pas',
  SELinkwitzRileyModule in '..\Linkwitz-Riley\SELinkwitzRileyModule.pas',
  SEPhaserModule in '..\Phaser\SEPhaserModule.pas',
  SEPitchShifterModule in '..\Pitch Shifter\SEPitchShifterModule.pas',
  SERealverbModule in '..\Realverb\SERealverbModule.pas',
  SERealverbStereoModule in '..\Realverb\SERealverbStereoModule.pas',
  SEPlateVerbModule in '..\Reverb\SEPlateVerbModule.pas',
  SEReverbModule in '..\Reverb\SEReverbModule.pas',
  SESineModule in '..\Sine\SESineModule.pas',
  SETanhAproximationsModule in '..\Tanh Aproximations\SETanhAproximationsModule.pas',
  SETunerModule in '..\Tuner\SETunerModule.pas',
  SEVibratoModule in '..\Vibrato\SEVibratoModule.pas',
  SESimpleVocoderModule in '..\Vocoder\SESimpleVocoderModule.pas',
  SEVocoderModule in '..\Vocoder\SEVocoderModule.pas',
  SEVoiceSynthModule in '..\VoiceSynth\SEVoiceSynthModule.pas';

{$E sem}
{$R *.res}

type
  TSEModuleBaseClass = class of TSEModuleBase;

const
  CModuleClasses : array [0..135] of TSEModuleBaseClass = (
    TSEFastSineApproximationsSingleModule,
    TSEFastSineApproximationsDoubleModule,
    TSEFastCosineApproximationsSingleModule,
    TSEFastCosineApproximationsDoubleModule,
    TSEFastTangensApproximationsSingleModule,
    TSEFastTangensApproximationsDoubleModule,
    TSEFastCotangensApproximationsSingleModule,
    TSEFastCotangensApproximationsDoubleModule,
    TSEFastArcTanApproximationsSingleModule,
    TSEFastArcTanApproximationsDoubleModule,
    TSEFastArcCotanApproximationsSingleModule,
    TSEFastArcCotanApproximationsDoubleModule,
    TSEFastLog2ApproximationsSingleModule,
    TSEFastLog2ApproximationsDoubleModule,
    TSEFastPower2ApproximationsSingleModule,
    TSEFastPower2ApproximationsDoubleModule,
    TSEAudioRGBModule,
    TSERGBToHSLModule,
    TSEHSLToRGBModule,
    TSEBarberpoleModule,
    TSEStaticButterworthLPModule,
    TSEStaticButterworthHPModule,
    TSEStaticControlableButterworthLPModule,
    TSEStaticControlableButterworthHPModule,
    TSEAutomatableButterworthLPModule,
    TSEAutomatableButterworthHPModule,
    TSEStaticChebyshevFilterLPModule,
    TSEStaticChebyshevFilterHPModule,
    TSEAutomatebleChebyshevFilterLPModule,
    TSEAutomatebleChebyshevFilterHPModule,
    TSEStaticChebyshevWaveshaperModule,
    TSEControlableChebyshevWaveshaperModule,
    TSEAutomatableChebyshevWaveshaperModule,
    TSEChorusModule,
    TSEStkChorusModule,
    TSEConvolutionModule,
    TSELowLatencyConvolutionModule,
    TSimpleDirectGateStaticSEModule,
    TSimpleDirectGateParamStaticSEModule,
    TSimpleDirectGateAutomatableSEModule,
    TSoftDirectGateStaticSEModule,
    TSoftDirectGateParamStaticSEModule,
    TSoftDirectGateAutomatableSEModule,
    TBrickwallLimiterStaticSEModule,
    TBrickwallLimiterParamStaticSEModule,
    TBrickwallLimiterAutomatableSEModule,
    TBrickwallSoftLimiterStaticSEModule,
    TBrickwallSoftLimiterParamStaticSEModule,
    TBrickwallSoftLimiterAutomatableSEModule,
    TBrickwallSimpleSoftLimiterStaticSEModule,
    TBrickwallSimpleSoftLimiterParamStaticSEModule,
    TBrickwallSoftLimiterAutomatableSEModule,
    TClassicGateStaticSEModule,
    TClassicGateParamStaticSEModule,
    TClassicGateAutomatableSEModule,
    TSoftClassicGateStaticSEModule,
    TSoftClassicGateParamStaticSEModule,
    TSoftClassicGateAutomatableSEModule,
    TLimiterStaticSEModule,
    TLimiterParamStaticSEModule,
    TLimiterAutomatableSEModule,
    TSoftLimiterStaticSEModule,
    TSoftLimiterParamStaticSEModule,
    TSoftLimiterAutomatableSEModule,
    TSimpleSoftLimiterStaticSEModule,
    TSimpleSoftLimiterParamStaticSEModule,
    TSimpleSoftLimiterAutomatableSEModule,
    TRangeGateStaticSEModule,
    TRangeGateParamStaticSEModule,
    TRangeGateAutomatableSEModule,
    TSimpleCompressorStaticSEModule,
    TSimpleCompressorParamStaticSEModule,
    TSimpleCompressorAutomatableSEModule,
    TSoftKneeCompressorStaticSEModule,
    TSoftKneeCompressorParamStaticSEModule,
    TSoftKneeCompressorAutomatableSEModule,
    TRMSCompressorStaticSEModule,
    TRMSCompressorParamStaticSEModule,
    TRMSCompressorAutomatableSEModule,
    TSEEnvelopeModule,
    TSEHilbertModule,
    TSEBasicLowpassModule,
    TSEBasicHighpassModule,
    TSEBasicBandpassModule,
    TSEBasicNotchModule,
    TSEBasicLowshelfModule,
    TSEBasicHighshelfModule,
    TSEBasicPeakModule,
    TSEBasicAllpassModule,
    TSEBasicShapeModule,
    TLightweightCompressorStaticSEModule,
    TLightweightCompressorParamStaticSEModule,
    TLightweightCompressorAutomatableSEModule,
    TLightweightFeedbackCompressorStaticSEModule,
    TLightweightFeedbackCompressorParamStaticSEModule,
    TLightweightFeedbackCompressorAutomatableSEModule,
    TLightweightLimiterStaticSEModule,
    TLightweightLimiterParamStaticSEModule,
    TLightweightLimiterAutomatableSEModule,
    TLightweightGateStaticSEModule,
    TLightweightGateParamStaticSEModule,
    TLightweightGateAutomatableSEModule,
    TSELinkwitzRileyStaticModule,
    TSELinkwitzRileyControlableModule,
    TSELinkwitzRileyAutomatableModule,
    TSEPhaserStaticModule,
    TSEPhaserControllableModule,
    TSEStkPitchshifterModule,
    TSERealverbModule,
    TSERealverbStereoModule,
    TSEPlateReverbStaticModule,
    TSEPlateReverbControllableModule,
    TSEFreeverbStaticModule,
    TSEFreeverbControllableModule,
    TSEStkNReverbStaticModule,
    TSEStkNReverbControllableModule,
    TSEStkJCReverbStaticModule,
    TSEStkJCReverbControllableModule,
    TSEStkNReverb2StaticModule,
    TSEStkNReverb2ControllableModule,
    TSEStkJCReverb2StaticModule,
    TSEStkJCReverb2ControllableModule,
    TSESineModule,
    TSESineCosineModule,
    TSESine2Module,
    TSETanhAproximationsModule,
    TSETanhAproxModule,
    TSETunerStaticModule,
    TSETunerControllableModule,
    TSEVibratoStaticModule,
    TSEVibratoControllableModule,
    TSESimpleVocoderStaticModule,
    TSESimpleVocoderControllableModule,
    TSESimpleVocoderAutomatableModule,
    TSEVoiceSynthStaticModule,
    TSEVoiceSynthControllableModule
  );

function getModuleProperties(Index: Integer; Properties: PSEModuleProperties): Boolean; cdecl; export;
begin
 if (Index >= 0) and (Index < Length(CModuleClasses)) then
  begin
   CModuleClasses[Index].GetModuleProperties(Properties);
   result := True;
  end
 else result := False;
end;

function makeModule(Index: Integer; ProcessType: Integer; SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer): Pointer; cdecl; export;
begin
 if (Index >= 0) and (Index < Length(CModuleClasses)) and (ProcessType = 1)
  then result := CModuleClasses[Index].Create(SEAudioMaster, Reserved).Effect
  else result := nil;
end;

exports makeModule name 'makeModule';
exports getModuleProperties name 'getModuleProperties';

end.
