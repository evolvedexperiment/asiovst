unit DAV_DspRegister;

interface

{$I ASIOVST.INC}

procedure Register;

implementation

uses
  Classes, DAV_DspEnvelopeFollower, DAVDspRemez, DAVDspFilter,
  DAV_DspVoiceController, DAV_DspOscSine, DAV_DspOscSaw, DAV_DspOscRamp,
  DAV_DspOscSquare, DAV_DspOscNoise, DAV_DspOscAbsSine, DAV_DspEnvelope,
  {$IFNDEF FPC}
   ToolsAPI,
   {$IFDEF DELPHI6_UP}
   DesignIntf,    // DsgnIntf renamed to DesignIntf from Delphi 6
// DesignEditors, // TCustomModule moved to DesignEditors from Delphi 6
   DMForm,
   {$ELSE}
   DsgnIntf,
   DMDesigner,
   {$ENDIF}
  {$ENDIF}
  DAV_DspVoice;

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
                                      TDspOscAbsSine,
                                      TDspEnvelope]);
  {$IFNDEF FPC}
   {$IFDEF DELPHI5}
   RegisterCustomModule(TDspVoice, TDataModuleDesignerCustomModule);
   {$ELSE}
   RegisterCustomModule(TDspVoice, TDataModuleCustomModule);
   {$ENDIF}
  {$ENDIF}
end;

end.