unit DAV_StereoBuffer;

interface

{$I DAV_Compiler.inc}

uses
  DAV_Common;

type
  TOBuffer = class
  private
    FBufferPos  : array [0..1] of Integer;
    FBufferSize : Integer;
    procedure SetBufferSize(const Value: Integer);
    procedure BufferSizeChanged;
  public
    Output : array [0..1] of PDAVSingleFixedArray;
    constructor Create;
    destructor Destroy; override;
    procedure Append(Channel: Cardinal; Value: Single);
    procedure Reset;
    procedure Clear;

    property BufferSize: Integer read FBufferSize write SetBufferSize;
  end;

implementation

const
  COutputBufferSize  = 1152; // max. 2 * 1152 samples per frame

constructor TOBuffer.Create;
begin
 inherited;
 FBufferSize := COutputBufferSize;
 GetMem(Output[0], FBufferSize * SizeOf(Single));
 GetMem(Output[1], FBufferSize * SizeOf(Single));
 Reset;
end;

destructor TOBuffer.Destroy;
begin
 Dispose(Output[0]);
 Dispose(Output[1]);
 inherited;
end;

procedure TOBuffer.Append(Channel: Cardinal; Value: Single);
begin
 Output[Channel, FBufferPos[Channel]] := Value;
 FBufferPos[Channel] := FBufferPos[Channel] + 1;
end;

procedure TOBuffer.Clear;
begin
 FillChar(Output[0]^, FBufferSize * SizeOf(Single), 0);
 FillChar(Output[1]^, FBufferSize * SizeOf(Single), 0);
end;

procedure TOBuffer.Reset;
begin
 FBufferPos[0] := 0;
 FBufferPos[1] := 0;
end;

procedure TOBuffer.SetBufferSize(const Value: Integer);
begin
 if FBufferSize <> Value then
  begin
   FBufferSize := Value;
   BufferSizeChanged;
  end;
end;

procedure TOBuffer.BufferSizeChanged;
begin
 ReallocMem(Output[0], FBufferSize * SizeOf(Single));
 ReallocMem(Output[1], FBufferSize * SizeOf(Single));
end;

end.
