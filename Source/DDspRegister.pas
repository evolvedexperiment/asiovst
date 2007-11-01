unit DDspRegister;

interface

procedure Register;

implementation

{$IFNDEF FPC}{$R DDspRegister.res}{$ENDIF}
uses Classes, DDspEnvelopeFollower, DDSPRemez;

procedure Register;
begin
  RegisterComponents('ASIO/VST DSP', [TRemezLowpassFilterDesigner,
                                      TRemezHighpassFilterDesigner,
                                      TRemezBandpassFilterDesigner,
                                      TRemezBandstopFilterDesigner,
                                      TDspEnvelopeFollower]);
end;


end.
