unit DAV_DspFeedbackDelayNetwork;

interface

{$I DAV_Compiler.inc}

uses
  DAV_Common, DAV_Complex, DAV_DspCommon, DAV_VectorMath;

type
  TCustomFeedbackZDelayNetwork = class(TDspObject)
  protected
    function GetDelaySamples(Index: Integer): Integer; virtual; abstract;
    function GetDelayTimes(Index: Integer): Single; virtual; abstract;
    function GetFeedbackMatrix(InputIndex, OutputIndex: Integer): Double; virtual; abstract;
    function GetInputVector(Index: Integer): Double; virtual; abstract;
    function GetOutputVector(Index: Integer): Double; virtual; abstract;
    procedure SetDelaySamples(Index: Integer; const Value: Integer); virtual; abstract;
    procedure SetDelayTimes(Index: Integer; const Value: Single); virtual; abstract;
    procedure SetFeedbackMatrix(InputIndex, OutputIndex: Integer; const Value: Double); virtual; abstract;
    procedure SetInputVector(Index: Integer; const Value: Double); virtual; abstract;
    procedure SetOutputVector(Index: Integer; const Value: Double); virtual; abstract;
  public
    constructor Create; virtual; abstract;
    property InputVector[Index: Integer]: Double read GetInputVector write SetInputVector;
    property OutputVector[Index: Integer]: Double read GetOutputVector write SetOutputVector;
    property DelaySamples[Index: Integer]: Integer read GetDelaySamples write SetDelaySamples;
    property FeedbackMatrix[InputIndex, OutputIndex: Integer]: Double read GetFeedbackMatrix write SetFeedbackMatrix;
  end;

  TCustomFeedbackDelayNetwork = class(TCustomFeedbackZDelayNetwork)
  private
    function GetDelayTimes(Index: Integer): Single; reintroduce; virtual; abstract;
    procedure SetDelayTimes(Index: Integer; const Value: Single); reintroduce; virtual; abstract;
  public
    property DelayTimes[Index: Integer]: Single read GetDelayTimes write SetDelayTimes;
  end;

  TFeedbackPathProcessEvent32 = procedure(var FeedbackVector: TDAVVector32) of object;

  TFeedbackZDelayNetwork32 = class(TCustomFeedbackZDelayNetwork)
  private
    FInputVector           : TDAVVector32;
    FOutputVector          : TDAVVector32;
    FDelaySamples          : Array [0..3] of Integer;
    FDelayPos              : Array [0..3] of Integer;
    FDelayBuffers          : Array [0..3] of PDAVSingleFixedArray;
    FFeedbackMatrix        : TDAVMatrix32;
    FRotationMatrix        : TDAVMatrix32;
    FOnProcessFeedbackPath : TFeedbackPathProcessEvent32;
  protected
    function GetDelaySamples(Index: Integer): Integer; override;
    function GetFeedbackMatrix(InputIndex, OutputIndex: Integer): Double; override;
    function GetInputVector(Index: Integer): Double; override;
    function GetOutputVector(Index: Integer): Double; override;
    procedure SetDelaySamples(Index: Integer; const Value: Integer); override;
    procedure SetFeedbackMatrix(InputIndex, OutputIndex: Integer; const Value: Double); override;
    procedure SetInputVector(Index: Integer; const Value: Double); override;
    procedure SetOutputVector(Index: Integer; const Value: Double); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function ProcessSample(const Input: Single): Single;
    procedure ProcessStereo(const InLeft, InRight: Single; out OutLeft, OutRight: Single);
    property OnProcessFeedbackPath: TFeedbackPathProcessEvent32 read FOnProcessFeedbackPath write FOnProcessFeedbackPath;
  end;

(*
  TFeedbackDelayNetwork32 = class(TCustomFeedbackDelayNetwork)
  private
    FInputVector    : TDAVVector32;
    FOutputVector   : TDAVVector32;
    FHalflifeVector : TDAVVector32;
    FDelaySamples   : Array [0..3] of Integer;
    FDelayPos       : Array [0..3] of Integer;
    FDelayBuffers   : Array [0..3] of PDAVSingleFixedArray;
    FFeedbackMatrix : TDAVMatrix32;
    FRotationMatrix : TDAVMatrix32;
    FDampingFilter  : Array [0..3] of TDampingFilter;
  protected
    procedure HalfLifeChanged; override;
    procedure DampingChanged; override;
    procedure NonLinearActiveChanged; override;
    procedure NonLinearGainChanged; override;
    procedure SampleRateChanged; override;
    function GetDelaySamples(Index: Integer): Integer; override;
    function GetFeedbackMatrix(InputIndex, OutputIndex: Integer): Double; override;
    function GetInputVector(Index: Integer): Double; override;
    function GetOutputVector(Index: Integer): Double; override;
    procedure SetDelaySamples(Index: Integer; const Value: Integer); override;
    procedure SetFeedbackMatrix(InputIndex, OutputIndex: Integer; const Value: Double); override;
    procedure SetInputVector(Index: Integer; const Value: Double); override;
    procedure SetOutputVector(Index: Integer; const Value: Double); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function ProcessSample(const Input: Single): Single;
    procedure ProcessStereo(const InLeft, InRight: Single; out OutLeft, OutRight: Single);
  end;
*)

implementation

uses
  Math, SysUtils, DAV_DspInterpolation;

resourcestring
  RCIndexOutOfBounds = 'Index out of bounds (%d)';

{ TFeedbackZDelayNetwork32 }

constructor TFeedbackZDelayNetwork32.Create;
var
  n : Integer;
begin
 inherited;

 FInputVector  := CHomogeneousXVector32;
 FOutputVector := CHomogeneousXVector32;

 FRotationMatrix := CIdentityHomogeneousMatrix32;
 FFeedbackMatrix := CIdentityHomogeneousMatrix32;

 for n := 0 to 3 do
  begin
   FDelayBuffers[n]  := nil;
   FDelayPos[n]      := 0;
   DelaySamples[n]   := 1;
  end;
end;

destructor TFeedbackZDelayNetwork32.Destroy;
var
  n : Integer;
begin
 for n := 0 to 3
  do Dispose(FDelayBuffers[n]);
 inherited;
end;

function TFeedbackZDelayNetwork32.GetDelaySamples(Index: Integer): Integer;
begin
 if Index in [0..3]
  then result := FDelaySamples[Index]
  else result := 0;
end;

function TFeedbackZDelayNetwork32.GetFeedbackMatrix(InputIndex,
  OutputIndex: Integer): Double;
begin
 if (InputIndex in [0..3]) and (OutputIndex in [0..3])
  then result := FFeedbackMatrix[InputIndex, OutputIndex]
  else result := 0;
end;

function TFeedbackZDelayNetwork32.GetInputVector(Index: Integer): Double;
begin
 if Index in [0..3]
  then result := FInputVector[Index]
  else result := 0;
end;

function TFeedbackZDelayNetwork32.GetOutputVector(Index: Integer): Double;
begin
 case Index of
  0..3 : result := FOutputVector[Index];
  else result := 0;
 end;
end;

procedure TFeedbackZDelayNetwork32.SetDelaySamples(Index: Integer;
  const Value: Integer);
begin
 if Index in [0..3] then
  if FDelaySamples[Index] < Value then
   begin
    ReallocMem(FDelayBuffers[Index], Value * SizeOf(Single));
    FillChar(FDelayBuffers[Index]^[FDelaySamples[Index]], (Value - FDelaySamples[Index]) * SizeOf(Single), 0);
    FDelaySamples[Index] := Value;
   end else
  if FDelaySamples[Index] > Value then
   begin
    FDelaySamples[Index] := Value;
    ReallocMem(FDelayBuffers[Index], Value * SizeOf(Single));
    if FDelayPos[Index] >= FDelaySamples[Index]
     then FDelayPos[Index] := 0;
   end else
  else raise Exception.CreateFmt(RCIndexOutOfBounds, [Index]);
end;

procedure TFeedbackZDelayNetwork32.SetFeedbackMatrix(InputIndex,
  OutputIndex: Integer; const Value: Double);
begin
 if (InputIndex in [0..3]) then
  if (OutputIndex in [0..3])
   then FFeedbackMatrix[InputIndex, OutputIndex] := Value
   else raise Exception.CreateFmt('Output Index out of bounds (%d)', [OutputIndex])
  else raise Exception.CreateFmt('Input Index out of bounds (%d)', [InputIndex]);
end;

procedure TFeedbackZDelayNetwork32.SetInputVector(Index: Integer;
  const Value: Double);
begin
 if Index in [0..3]
  then FInputVector[Index] := Value
  else raise Exception.CreateFmt(RCIndexOutOfBounds, [Index]);
end;

procedure TFeedbackZDelayNetwork32.SetOutputVector(Index: Integer;
  const Value: Double);
begin
 if Index in [0..3]
  then FOutputVector[Index] := Value
  else raise Exception.CreateFmt(RCIndexOutOfBounds, [Index]);
end;

function TFeedbackZDelayNetwork32.ProcessSample(const Input: Single): Single;
var
  DelayedSignal : TDAV4SingleArray;
  FeedbackInput : TDAV4SingleArray;
begin
 // Build Delay Vector
 DelayedSignal[0] := FDelayBuffers[0]^[FDelayPos[0]];
 DelayedSignal[1] := FDelayBuffers[1]^[FDelayPos[1]];
 DelayedSignal[2] := FDelayBuffers[2]^[FDelayPos[2]];
 DelayedSignal[3] := FDelayBuffers[3]^[FDelayPos[3]];

 // Output
 result := VectorDotProduct(FOutputVector, DelayedSignal);

 // Feedback Matrix
 FeedbackInput := VectorTransform(DelayedSignal, FFeedbackMatrix);

 // Process Feedback Path
 if assigned(FOnProcessFeedbackPath)
  then FOnProcessFeedbackPath(FeedbackInput);

 FDelayBuffers[0]^[FDelayPos[0]] := FInputVector[0] * Input + FeedbackInput[0];
 FDelayBuffers[1]^[FDelayPos[1]] := FInputVector[1] * Input + FeedbackInput[1];
 FDelayBuffers[2]^[FDelayPos[2]] := FInputVector[2] * Input + FeedbackInput[2];
 FDelayBuffers[3]^[FDelayPos[3]] := FInputVector[3] * Input + FeedbackInput[3];

 inc(FDelayPos[0]);
 inc(FDelayPos[1]);
 inc(FDelayPos[2]);
 inc(FDelayPos[3]);

 if FDelayPos[0] >= FDelaySamples[0] then FDelayPos[0] := 0;
 if FDelayPos[1] >= FDelaySamples[1] then FDelayPos[1] := 0;
 if FDelayPos[2] >= FDelaySamples[2] then FDelayPos[2] := 0;
 if FDelayPos[3] >= FDelaySamples[3] then FDelayPos[3] := 0;
end;

procedure TFeedbackZDelayNetwork32.ProcessStereo(const InLeft, InRight: Single;
  out OutLeft, OutRight: Single);
var
  DelayedSignal : TDAV4SingleArray;
  FeedbackInput : TDAV4SingleArray;
begin
(*
 FBuffer^[FBufferPos] := Input;

 inc(FBufferPos);
 if FBufferPos >= BufferSize - 1
  then FBufferPos := 0;

 Move(FIntBuffer[1], FIntBuffer[0], 2 * SizeOf(Single));
 FIntBuffer[2] := FBuffer^[FBufferPos];
 result := Hermite32_asm(FFractional, @FIntBuffer);
*)

 // Build Delay Vector
 DelayedSignal[0] := FDelayBuffers[0]^[FDelayPos[0]];
 DelayedSignal[1] := FDelayBuffers[1]^[FDelayPos[1]];
 DelayedSignal[2] := FDelayBuffers[2]^[FDelayPos[2]];
 DelayedSignal[3] := FDelayBuffers[3]^[FDelayPos[3]];

 // Output Left
 OutLeft := VectorDotProduct(FOutputVector, DelayedSignal);

 OutRight := FOutputVector[1] * FDelayBuffers[0]^[FDelayPos[0]] +
             FOutputVector[2] * FDelayBuffers[1]^[FDelayPos[1]] +
             FOutputVector[3] * FDelayBuffers[2]^[FDelayPos[2]] +
             FOutputVector[0] * FDelayBuffers[3]^[FDelayPos[3]];

 // Feedback Matrix
 FeedbackInput := VectorTransform(DelayedSignal, FFeedbackMatrix);

 // Process Feedback Path
 if assigned(FOnProcessFeedbackPath)
  then FOnProcessFeedbackPath(FeedbackInput);

 FDelayBuffers[0]^[FDelayPos[0]] := FInputVector[0] * InLeft + FInputVector[1] * InRight + FeedbackInput[0];
 FDelayBuffers[1]^[FDelayPos[1]] := FInputVector[1] * InLeft + FInputVector[2] * InRight + FeedbackInput[1];
 FDelayBuffers[2]^[FDelayPos[2]] := FInputVector[2] * InLeft + FInputVector[3] * InRight + FeedbackInput[2];
 FDelayBuffers[3]^[FDelayPos[3]] := FInputVector[3] * InLeft + FInputVector[0] * InRight + FeedbackInput[3];

 inc(FDelayPos[0]);
 inc(FDelayPos[1]);
 inc(FDelayPos[2]);
 inc(FDelayPos[3]);

 if FDelayPos[0] >= FDelaySamples[0] then FDelayPos[0] := 0;
 if FDelayPos[1] >= FDelaySamples[1] then FDelayPos[1] := 0;
 if FDelayPos[2] >= FDelaySamples[2] then FDelayPos[2] := 0;
 if FDelayPos[3] >= FDelaySamples[3] then FDelayPos[3] := 0;
end;

(*
{ TFeedbackNetwork32 }

function TFeedbackNetwork32.GetDelayTimes(Index: Integer): Single;
begin
 case Index of
  0..3 : result := (FDelaySamples[Index] + FDelayFracs[Index]) * FSampleRate;
  else result := 0;
 end;
end;

procedure TFeedbackNetwork32.SetDelayTimes(Index: Integer;
  const Value: Single);
begin
 case Index of
  0..3 : begin
          DelaySamples[Index] := round(Value * SampleRate + 0.5);
          FDelayFracs[Index] := DelaySamples[Index] - Value * SampleRate;
          assert(FDelayFracs[Index] >= 0);
          assert(FDelayFracs[Index] <= 1);
         end;
  else raise Exception.CreateFmt(RCIndexOutOfBounds, [Index]);
 end;
end;
*)

end.
