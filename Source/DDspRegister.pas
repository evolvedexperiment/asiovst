unit DDspRegister;

interface

procedure Register;

implementation

{$IFNDEF FPC}{$R DDspRegister.res}{$ENDIF}
uses Classes, DDspEnvelopeFollower;

procedure Register;
begin
  RegisterComponents('ASIO/VST DSP', [TDspEnvelopeFollower]);
end;

end.
