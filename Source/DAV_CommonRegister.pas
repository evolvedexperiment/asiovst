unit DAV_CommonRegister;

interface

{$I ASIOVST.INC}

procedure Register;

implementation

{$IFNDEF FPC}{$R ..\Resources\DAV_CommonRegister.res}{$ENDIF}

uses
  {$IFDEF FPC} LResources, {$ENDIF} Classes, DAV_Common, DAV_MidiFile,
  DAV_AudioData, DAV_ComplexData;

procedure Register;
begin
  RegisterComponents('ASIO/VST Basics', [TMidiFile, TSampleRateSource,
    TAudioData32, TAudioData64, TComplexData32, TComplexData64,
    TAudioDataCollection32, TAudioDataCollection64]);
end;

{$IFDEF FPC}
initialization
  {$i ..\Resources\DAV_CommonRegister.lrs}
{$ENDIF}

end.
