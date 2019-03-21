unit DAV_StringConvert;

interface

{$I DAV_Compiler.inc}

function OnOff(Value: Boolean): string; overload; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function OnOff(Value: Double): string; overload; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

function FloatToTimeWithUnit(const Value: Double): string;
function FloatToString(Value: Extended): string; overload;
function FloatToString(Value: Extended; Digits: Integer): string; overload;
function FloatToString(Value: Extended; Digits, Precision: Integer): string; overload;
function FloatToAnsiString(Value: Extended): AnsiString; overload;
function FloatToAnsiString(Value: Extended; Digits: Integer): AnsiString; overload;
function FloatToAnsiString(Value: Extended; Digits, Precision: Integer): AnsiString; overload;
function FloatToHertz(Value: Extended): String;
function FloatToHertzShiftNoUnit(Value: Extended): String;

implementation

uses
  Math, SysUtils, DAV_Strings;

function OnOff(Value: Boolean): string;
begin
  if Value then
    Result := 'On'
  else
    Result := 'Off';
end;

function OnOff(Value: Double): string; overload;
begin
  Result := OnOff(Boolean(Round(Value)));
end;

function FloatToTimeWithUnit(const Value: Double): string;
begin
  if Value > 1 then
    Result := FloatToStrF(Value, ffFixed, 6, 3)+ 's'
  else
  if Value > 1E-3 then
    Result := FloatToStrF(1E3 * Value, ffFixed, 6, 3)+ 'ms'
  else
  if Value > 1E-6 then
    Result := FloatToStrF(1E6 * Value, ffFixed, 6, 3)+ 'µs'
  else
    Result := FloatToStrF(1E9 * Value, ffFixed, 6, 3)+ 'ns'
end;

function FloatToString(Value: Extended): string;
begin
{$IFDEF UseNativeFloatToStringConversion}
  Result := FloatToStr(Value);
{$ELSE}
  if IsNan(Value) then
    Result := 'Error'
  else
  if IsInfinite(Value) then
    Result := 'oo'
  else
    Result := IntToStr(Round(Value));
{$ENDIF}
end;

function FloatToString(Value: Extended; Digits: Integer): string;
begin
{$IFDEF UseNativeFloatToStringConversion}
  Result := FloatToStr(RoundTo(Value, -Digits));
{$ELSE}
  if IsNan(Value) then
    Result := 'Error'
  else
  if IsInfinite(Value) then
    Result := 'oo'
  else
    Result := IntToStr(Round(Value));
{$ENDIF}
end;

function FloatToString(Value: Extended; Digits, Precision: Integer): string;
begin
{$IFDEF UseNativeFloatToStringConversion}
  Value := RoundTo(Value, -Digits);
  Result := FloatToStrF(Value, ffGeneral, Digits, Precision);
{$ELSE}
  if IsNan(Value) then
    Result := 'Error'
  else
  if IsInfinite(Value) then
    Result := 'oo'
  else
    Result := IntToStr(Round(Value));
{$ENDIF}
end;

function FloatToAnsiString(Value: Extended): AnsiString;
begin
{$IFDEF UseNativeFloatToStringConversion}
  Result := AnsiString(FloatToStr(Value));
  {$ELSE}
  if IsNan(Value) then
    Result := 'Error'
  else
  if IsInfinite(Value) then
    Result := 'oo'
  else
    Result := IntToStr(Round(Value));
{$ENDIF}
end;

function FloatToAnsiString(Value: Extended; Digits: Integer): AnsiString;
begin
{$IFDEF UseNativeFloatToStringConversion}
  Result := AnsiString(FloatToStr(RoundTo(Value, -Digits)));
  {$ELSE}
  if IsNan(Value) then
    Result := 'Error'
  else
  if IsInfinite(Value) then
    Result := 'oo'
  else
    Result := IntToStr(Round(Value));
{$ENDIF}
end;

function FloatToAnsiString(Value: Extended; Digits, Precision: Integer): AnsiString;
begin
{$IFDEF UseNativeFloatToStringConversion}
  Result := AnsiString(FloatToStrF(Value, ffGeneral, Digits, Precision));
  {$ELSE}
  if IsNan(Value) then
    Result := 'Error'
  else
  if IsInfinite(Value) then
    Result := 'oo'
  else
    Result := IntToStr(Round(Value));
{$ENDIF}
end;

function FloatToHertz(Value: Extended): String;
begin
  // convert frequency into string
  if Value < 1000 then
    Result := FloatToString(Value, 3, 4) + ' Hz'
  else
    Result := FloatToString(1E-3 * Value, 3, 3) + ' kHz';
end;

function FloatToHertzShiftNoUnit(Value: Extended): String;
begin
  // convert frequency value into string and shift the range if above 1kHz
  if Value < 1000 then
    Result := FloatToString(Value, 3, 4)
  else
    Result := FloatToString(1E-3 * Value, 3, 3);
end;

end.
