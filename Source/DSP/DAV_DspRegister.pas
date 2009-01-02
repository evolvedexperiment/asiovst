unit DAV_DspRegister;

interface

{$I ..\DAV_Compiler.inc}

procedure Register;

implementation

{$IFNDEF FPC}{$R '..\..\Resources\DAV_DspRegister.res'}{$ENDIF}

uses
  {$IFDEF FPC} LResources, {$ENDIF} Classes, DAV_DspRemez;

procedure Register;
begin
  RegisterComponents('ASIO/VST DSP', [TRemezLowpassFilterDesigner,
                                      TRemezHighpassFilterDesigner,
                                      TRemezBandpassFilterDesigner,
                                      TRemezBandstopFilterDesigner]);
end;

{$IFDEF FPC}
initialization
  {$i ..\..\Resources\DAV_DspRegister.lrs}
{$ENDIF}

end.
