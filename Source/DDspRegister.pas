unit DDspRegister;

interface

{$I ASIOVST.INC}

procedure Register;

implementation

uses
  Classes, DDspEnvelopeFollower, DDSPRemez, DDspFilter, DDspVoiceController,
  DDspOscSine, DDspOscSaw, DDspOscRamp, DDspOscSquare, DDspOscNoise,
  DDspOscAbsSine, DDspEnvelope,
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
