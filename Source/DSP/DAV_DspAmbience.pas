unit DAV_DspAmbience;

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Common, DAV_DspCommon, DAV_DspFilter, DAV_DSPFilterButterworth,
  DAV_DspFilterBasics;

type
  TCustomAmbience = class(TDspObject)
  private
    FDamping    : Single;
    FDry, FWet  : Single;
    FRoomsize   : Single;
    FOutputGain : Single;
    FSampleRate : Single;
    function GetMix: Single;
    procedure SetDamping(const Value: Single);
    procedure SetDry(const Value: Single);
    procedure SetMix(const Value: Single);
    procedure SetOutputGain(const Value: Single);
    procedure SetRoomSize(const Value: Single);
    procedure SetWet(const Value: Single);
    procedure SetSampleRate(const Value: Single);
    procedure CalculateDryFactor;
    procedure CalculateWetFactor;
    procedure CalculateDampingFactor;
    procedure CalculateRoomsizeFactor;
    procedure CalculateOutputFactor;
  protected
    FBuffers        : Array [0..3] of PDAVSingleFixedArray;
    FPos            : Integer;
    FHfDampState    : Single;
    FDampFactor     : Single;
    FOutputFactor   : Single;
    FRoomsizeFactor : Double;
    FDryFactor      : Single;
    FWetFactor      : Single;
    FFlushedBuffers : Boolean;
    FHighShelf      : TBasicHighShelfFilter;
    procedure AllocateBuffers; virtual;
    procedure DampingChanged; virtual;
    procedure DryChanged; virtual;
    procedure FlushBuffers; virtual;
    procedure OutputGainChanged; virtual;
    procedure RoomsizeChanged; virtual;
    procedure SampleRateChanged; virtual;
    procedure WetChanged; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function Process(Input: Single): Single; overload;
    procedure Process(var Left, Right: Single); overload;

    property Damping: Single read FDamping write SetDamping;
    property Dry: Single read FDry write SetDry;
    property Mix: Single read GetMix write SetMix;
    property OutputGain: Single read FOutputGain write SetOutputGain;
    property RoomSize: Single read FRoomsize write SetRoomSize;
    property SampleRate: Single read FSampleRate write SetSampleRate;
    property Wet: Single read FWet write SetWet;
  end;

  TAmbience = class(TCustomAmbience)
  published
    property Damping;
    property Dry;
    property Mix;
    property OutputGain;
    property RoomSize;
    property Wet;
  end;

implementation

uses
  SysUtils;

const
  CBufferSize = 1024;
  CFeedback = 0.8;

{ TCustomAmbience }

constructor TCustomAmbience.Create;
begin
 inherited;

 AllocateBuffers;

 FHfDampState := 0.0;
 FSampleRate  := 44100;
 FFlushedBuffers := FPos = 0;

 // initialize
 FRoomsize   := 0.7;
 FDamping    := 0.7;
 FWet        := 0.9;
 FDry        := 0.1;
 FOutputGain := 0.5;
 FHighShelf  := TBasicHighShelfFilter.Create;
 with FHighShelf do
  begin
   Frequency := 1900;
   SampleRate := Self.SampleRate;
   Bandwidth := 2.8;
   Gain := 12;
  end;

 CalculateOutputFactor;
 CalculateDryFactor;
 CalculateWetFactor;
 CalculateDampingFactor;
 CalculateRoomsizeFactor;
end;

destructor TCustomAmbience.Destroy;
begin
 FreeAndNil(FHighShelf);
 Dispose(FBuffers[0]);
 Dispose(FBuffers[1]);
 Dispose(FBuffers[2]);
 Dispose(FBuffers[3]);
 inherited;
end;

procedure TCustomAmbience.AllocateBuffers;
begin
 GetMem(FBuffers[0], CBufferSize * SizeOf(Single));
 GetMem(FBuffers[1], CBufferSize * SizeOf(Single));
 GetMem(FBuffers[2], CBufferSize * SizeOf(Single));
 GetMem(FBuffers[3], CBufferSize * SizeOf(Single));
end;

function TCustomAmbience.GetMix: Single;
begin
 if FWet + FDry <= 0
  then result := 0.5
  else result := FWet / (FWet + FDry);
end;

procedure TCustomAmbience.SetDamping(const Value: Single);
begin
 if FDamping <> Value then
  begin
   FDamping := Value;
   DampingChanged;
  end;
end;

procedure TCustomAmbience.SetDry(const Value: Single);
begin
 if FDry <> Value then
  begin
   FDry := Value;
   DryChanged;
  end;
end;

procedure TCustomAmbience.SetMix(const Value: Single);
var
  Sum : Single;
begin
 if Mix <> Value then
  begin
   Sum := (FWet + FDry);

   FWet := Value;
   FDry := 1 - Value;

   // todo: verify this!!!
   FWet := FWet * Sum;
   FDry := FDry * Sum;
  end;
end;

procedure TCustomAmbience.SetOutputGain(const Value: Single);
begin
 if FOutputGain <> Value then
  begin
   FOutputGain := Value;
   OutputGainChanged;
  end;
end;

procedure TCustomAmbience.OutputGainChanged;
begin
 CalculateOutputFactor;
 CalculateDryFactor;
 CalculateWetFactor;
end;

procedure TCustomAmbience.CalculateOutputFactor;
begin
 FOutputFactor := dB_to_Amp(FOutputGain);
end;

procedure TCustomAmbience.SetRoomSize(const Value: Single);
begin
 if FRoomsize <> Value then
  begin
   FRoomSize := Value;
   RoomSizeChanged;
  end;
end;

procedure TCustomAmbience.SetSampleRate(const Value: Single);
begin
 if FSampleRate <> Value then
  begin
   FSampleRate := Value;
   SampleRateChanged;
  end;
end;

procedure TCustomAmbience.SampleRateChanged;
begin
 FHighShelf.SampleRate := SampleRate;
end;

procedure TCustomAmbience.RoomsizeChanged;
begin
 CalculateRoomsizeFactor;
 FlushBuffers;
end;

procedure TCustomAmbience.CalculateRoomsizeFactor;
begin
 FRoomsizeFactor := 0.025 + 0.2665 * FRoomSize;
end;

procedure TCustomAmbience.FlushBuffers;
begin
 FillChar(FBuffers[0]^[0], CBufferSize * SizeOf(Single), 0);
 FillChar(FBuffers[1]^[0], CBufferSize * SizeOf(Single), 0);
 FillChar(FBuffers[2]^[0], CBufferSize * SizeOf(Single), 0);
 FillChar(FBuffers[3]^[0], CBufferSize * SizeOf(Single), 0);
end;

procedure TCustomAmbience.SetWet(const Value: Single);
begin
 if FWet <> Value then
  begin
   FWet := Value;
   WetChanged;
  end;
end;

procedure TCustomAmbience.DryChanged;
begin
 CalculateDryFactor;
end;

procedure TCustomAmbience.CalculateDryFactor;
begin
 FDryFactor := FOutputFactor - sqr(FDry) * FOutputFactor;
end;

procedure TCustomAmbience.WetChanged;
begin
 CalculateWetFactor;
end;

procedure TCustomAmbience.CalculateWetFactor;
begin
 FWetFactor := 0.8 * FWet * FOutputFactor;
end;

procedure TCustomAmbience.DampingChanged;
begin
 CalculateDampingFactor; 
end;

procedure TCustomAmbience.CalculateDampingFactor;
begin
 FDampFactor := 0.05 + 0.01 * FDamping;
end;

function TCustomAmbience.Process(Input: Single): Single;
var
  r : Double;
  t : Double;
begin
 Input := FHighShelf.ProcessSample(Input);

 // apply HF damping
 FHfDampState := FHfDampState + FDampFactor * (FWet * Input - FHfDampState);  // HF damping
 r := FHfDampState;

 if (abs(FHfDampState) > 1E-10) then
  begin
   // Catch Denormals
   FFlushedBuffers := False;
  end
 else
  begin
   FHfDampState := 0;
   if FFlushedBuffers = False then
    begin
     FFlushedBuffers := True;
     FlushBuffers;
    end;
  end;

 // decorrelation allpass delay filters
 t := FBuffers[0]^[FPos];
 r := r - CFeedback * t;
 FBuffers[0]^[(FPos + round(107 * FRoomsize)) and 1023] := r; // Allpass
 r := r + t;

 t := FBuffers[1]^[FPos];
 r := r - CFeedback * t;
 FBuffers[1]^[(FPos + round(142 * FRoomsize)) and 1023] := r; // Allpass
 r := r + t;

 t := FBuffers[2]^[FPos];
 r := r - CFeedback * t;
 FBuffers[2]^[(FPos + round(277 * FRoomsize)) and 1023] := r; // Allpass
 r := r + t;
 Result := FDry * Input + r - FHfDampState; // Left Output

 t := FBuffers[3]^[FPos];
 r := r - CFeedback * t;
 FBuffers[3]^[(FPos + round(379 * FRoomsize)) and 1023] := r; // Allpass
 r := r + t;
 result := CHalf32 * (result + (FDry * Input + r - FHfDampState)); // Right Output

 // advance position
 FPos := (FPos + 1) and 1023;
end;

procedure TCustomAmbience.Process(var Left, Right: Single);
var
  r : Double;
  t : Double;
begin
 // apply HF damping
 FHfDampState := FHfDampState + FDampFactor * (FWet * FHighShelf.ProcessSample(Left + Right) - FHfDampState);  // HF damping
 r := FHfDampState;

 if (abs(FHfDampState) > 1E-10) then
  begin
   // Catch Denormals
   FFlushedBuffers := False;
  end
 else
  begin
   FHfDampState := 0;
   if FFlushedBuffers = False then
    begin
     FFlushedBuffers := True;
     FlushBuffers;
    end;
  end;

 // decorrelation allpass delay filters
 t := FBuffers[0]^[FPos];
 r := r - CFeedback * t;
 FBuffers[0]^[(FPos + round(107 * FRoomsize)) and 1023] := r; // Allpass
 r := r + t;

 t := FBuffers[1]^[FPos];
 r := r - CFeedback * t;
 FBuffers[1]^[(FPos + round(142 * FRoomsize)) and 1023] := r; // Allpass
 r := r + t;

 t := FBuffers[2]^[FPos];
 r := r - CFeedback * t;
 FBuffers[2]^[(FPos + round(277 * FRoomsize)) and 1023] := r; // Allpass
 r := r + t;
 Left := FDry * Left + r - FHfDampState; // Left Output

 t := FBuffers[3]^[FPos];
 r := r - CFeedback * t;
 FBuffers[3]^[(FPos + round(379 * FRoomsize)) and 1023] := r; // Allpass
 r := r + t;
 Right := FDry * Right + r - FHfDampState; // Right Output

 // advance position
 FPos := (FPos + 1) and 1023;
end;

end.
