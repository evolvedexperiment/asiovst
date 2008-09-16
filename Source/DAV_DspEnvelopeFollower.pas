unit DAV_DspEnvelopeFollower;

interface

{$I ASIOVST.inc}

uses
  DAV_DspBaseComponent, DAV_Common;

type
  TDspEnvelopeFollower = class(TDspBaseComponent)
  protected
    fLastOutputSingle : TDAVSingleDynArray;
    fLastOutputDouble : TDAVDoubleDynArray;
    fInternalAttack   : Single;
    fInternalRelease  : Single;
    fAttack           : Single;
    fRelease          : Single;
    procedure SetAttack(const Value: single);
    procedure SetRelease(const Value: single);
    procedure BeforeDestroy; override;
  protected
    procedure SampleRateChanged; override;
    procedure ChannelsChanged; override;
    procedure Process(var Data: Single; const channel: integer); overload;
    procedure Process(var Data: Double; const channel: integer); overload;
  public
    procedure Init; override;
    procedure Reset; override;
  published
    property Attack:  single read fAttack write SetAttack;   // 0..1
    property Release: single read fRelease write SetRelease; // 0..1
  end;

implementation

uses Math;

procedure TDspEnvelopeFollower.Init;
begin
  fStdProcessS  := Process;
  fStdProcessD  := Process;

  fAttack:=0;
  fRelease:=0;
  Reset;
end;

procedure TDspEnvelopeFollower.BeforeDestroy;
begin
  SetLength(fLastOutputSingle, 0);
  SetLength(fLastOutputDouble, 0);
end;

procedure TDspEnvelopeFollower.Reset;
begin
  ChannelsChanged;
  SampleRateChanged;
end;

procedure TDspEnvelopeFollower.SampleRateChanged;
begin
  fInternalAttack  := power(0.01, 1/((0.001 + fAttack  * 1.999) * fSampleRate));
  fInternalRelease := power(0.01, 1/((0.001 + fRelease * 1.999) * fSampleRate));
end;

procedure TDspEnvelopeFollower.ChannelsChanged;
begin
  SetLength(fLastOutputSingle, fChannels);
  SetLength(fLastOutputDouble, fChannels);

  FillChar(fLastOutputSingle[0], fChannels * SizeOf(Single), 0);
  FillChar(fLastOutputDouble[0], fChannels * SizeOf(Double), 0);
end;


procedure TDspEnvelopeFollower.SetAttack(const Value: single);
begin
  if fAttack <> Value then
  begin
    fAttack := max(0,min(1,Value));
    SampleRateChanged;
  end;
end;

procedure TDspEnvelopeFollower.SetRelease(const Value: single);
begin
  if fRelease <> Value then
  begin
    fRelease := max(0,min(1,Value));
    SampleRateChanged;
  end;
end;

procedure TDspEnvelopeFollower.Process(var Data: Double; const channel: integer);
var tmp: Double;
begin
 {$IFDEF FPC}
  Data := abs(Data);
 {$ELSE}
  f_abs(Data);
 {$ENDIF}

  if Data>=fLastOutputDouble[channel] then
    tmp:=fInternalAttack
  else
    tmp:=fInternalRelease;

  fLastOutputDouble[channel] := tmp * (fLastOutputDouble[channel] - Data) + Data;
  Data:=fLastOutputDouble[channel];
end;

procedure TDspEnvelopeFollower.Process(var Data: Single; const channel: integer);
var tmp: Single;
begin
 {$IFDEF FPC}
  Data := abs(Data);
 {$ELSE}
  f_abs(Data);
 {$ENDIF}

  if Data>=fLastOutputSingle[channel] then
    tmp:=fInternalAttack
  else
    tmp:=fInternalRelease;

  fLastOutputSingle[channel] := tmp * (fLastOutputSingle[channel] - Data) + Data;
  Data:=fLastOutputSingle[channel];  
end;

end.
