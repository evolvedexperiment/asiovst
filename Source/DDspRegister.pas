unit DDspRegister;

interface

{$I ASIOVST.INC}

procedure Register;

implementation

{$IFNDEF FPC}{$R DDspRegister.res}{$ENDIF}
uses Classes, DDspEnvelopeFollower, DDSPRemez, DDspFilter, DDspVoiceController,
  DDspOscSine, DDspOscSaw, DDspOscRamp, DDspOscSquare, DDspOscNoise, DDspOscAbsSine,
  ToolsAPI,
  {$IFDEF DELPHI6_UP}
  DesignIntf,    // DsgnIntf renamed to DesignIntf from Delphi 6
//DesignEditors, // TCustomModule moved to DesignEditors from Delphi 6
  DMForm,
  {$ELSE}
  DsgnIntf,
  DMDesigner,
  {$ENDIF}
  DDspVoice;

procedure Register;
begin
  RegisterComponents('ASIO/VST DSP', [TRemezLowpassFilterDesigner,
                                      TRemezHighpassFilterDesigner,
                                      TRemezBandpassFilterDesigner,
                                      TRemezBandstopFilterDesigner,
                                      TDspEnvelopeFollower,
                                      TDspLowpassFilter,
                                      TDspVoiceController,
                                      TDspOscSine,
                                      TDspOscSaw,
                                      TDspOscRamp,
                                      TDspOscSquare,
                                      TDspOscNoise,
                                      TDspOscAbsSine]);
  {$IFDEF DELPHI5}
  RegisterCustomModule(TDspVoice, TDataModuleDesignerCustomModule);
  {$ELSE}
  RegisterCustomModule(TDspVoice, TDataModuleCustomModule);
  {$ENDIF}
end;

end.
