unit DAV_XPlatform;

interface

{$IFNDEF FPC}
{$IFDEF MSWINDOWS}
function GetApplicationFilename: string; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function GetApplicationDirectory: string; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

procedure Msg(b: Boolean); overload;
procedure Msg(m: string; m2: string = ''); overload;
procedure Msg(i: Integer); overload;
procedure Msg(s: Single); overload;
procedure Msg(m: string; i: Integer); overload;
{$ENDIF}
{$ENDIF}


implementation

uses
  Windows, SysUtils;

{$IFNDEF FPC}
{$IFDEF MSWINDOWS}
function GetApplicationFilename: string;
var
  s : PAnsiChar;
begin
  GetMem(s, $7FF);
  GetModuleFileNameA(hInstance, s, SizeOf(s));
  Result := ExtractFilename(string(StrPas(s)));
end;

function GetApplicationDirectory: string;
var
  s : PAnsiChar;
begin
  GetMem(s, $7FF);
  GetModuleFilenameA(hInstance, s, SizeOf(s));
  Result := ExtractFileDir(string(StrPas(s)));
end;

procedure Msg(b: Boolean);
begin
  if b then
    Msg('TRUE')
  else
    Msg('FALSE');
end;

procedure Msg(m: string; m2: string = '');
begin
  MessageBox(0, PChar(m), PChar(m2), MB_OK);
end;

procedure Msg(i: Integer);
begin
  Msg(IntToStr(i));
end;

procedure Msg(s: Single);
begin
  Msg(FloatToStrF(s, ffFixed, 3, 3));
end;

procedure Msg(m: string; i:Integer);
begin
  MessageBox(0, PChar(m + ' ' + IntToStr(i)), '', MB_OK);
end;
{$ENDIF}
{$ENDIF}

end.

