unit DAV_DspFreeverb;

interface

{$I ..\DAV_Compiler.INC}

uses
  Classes, DAV_Common, DAV_DspCommon, DAV_DspFreeverbFilter;

const
  CCombFilterCount = 8;
  CAllpassCount    = 4;

  // These values assume 44.1KHz sample rate
  // they will probably be OK for 48KHz sample rate
  // but would need scaling for 96KHz (or other) sample rates.
  // The values were obtained by listening tests.
  CCombTuningL1    = 1116;
  CCombTuningL2    = 1188;
  CCombTuningL3    = 1277;
  CCombTuningL4    = 1356;
  CCombTuningL5    = 1422;
  CCombTuningL6    = 1491;
  CCombTuningL7    = 1557;
  CCombTuningL8    = 1617;
  CAllpassTuningL1 = 556;
  CAllpassTuningL2 = 441;
  CAllpassTuningL3 = 341;
  CAllpassTuningL4 = 225;

  // Allpass filter class declaration

type
  TFreeverbCombArray = array [0..CCombFilterCount - 1] of TFreeverbCombFilter;
  TFreeverbAllpassArray = array [0..CAllpassCount - 1] of TFreeverbAllpass;

  // Reverb model class declaration
  TFreeverb = class(TDspObject)
  private
    FSampleRate : Double;
    FGain       : Double;
    FRoomSize   : Double;
    FDamp       : Double;
    FWet        : Double;
    FDry        : Double;
    FComb       : TFreeverbCombArray;    // Comb filters
    FAllpass    : TFreeverbAllpassArray;
    procedure SetSampleRate(const Value: Double); // Allpass filters
    procedure SetDamp(const Value: Double);
    procedure SetDry(const Value: Double);
    procedure SetRoomSize(const Value: Double);
    procedure SetWet(const Value: Double);
  protected
    procedure DampChanged; virtual;
    procedure DryChanged; virtual;
    procedure RoomsizeChanged; virtual;
    procedure WetChanged; virtual;
    procedure SampleRateChanged; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Reset;
    function ProcessSample(const Input: Double): Double;
  published
    property Dry: Double read FDry write SetDry;
    property Wet: Double read FWet write SetWet;
    property Damp: Double read FDamp write SetDamp;
    property RoomSize: Double read FRoomSize write SetRoomSize;
    property SampleRate: Double read FSampleRate write SetSampleRate;
  end;

implementation

uses
  SysUtils;

constructor TFreeverb.Create;
const
  CReci441 : Single = 1 / 44100;
begin
  inherited Create;
  FComb[0]    := TFreeverbCombFilter.Create(round(CCombTuningL1 * SampleRate * CReci441));
  FComb[1]    := TFreeverbCombFilter.Create(round(CCombTuningL2 * SampleRate * CReci441));
  FComb[2]    := TFreeverbCombFilter.Create(round(CCombTuningL3 * SampleRate * CReci441));
  FComb[3]    := TFreeverbCombFilter.Create(round(CCombTuningL4 * SampleRate * CReci441));
  FComb[4]    := TFreeverbCombFilter.Create(round(CCombTuningL5 * SampleRate * CReci441));
  FComb[5]    := TFreeverbCombFilter.Create(round(CCombTuningL6 * SampleRate * CReci441));
  FComb[6]    := TFreeverbCombFilter.Create(round(CCombTuningL7 * SampleRate * CReci441));
  FComb[7]    := TFreeverbCombFilter.Create(round(CCombTuningL8 * SampleRate * CReci441));
  FAllpass[0] := TFreeverbAllpass.Create(round(CAllpassTuningL1 * SampleRate * CReci441));
  FAllpass[1] := TFreeverbAllpass.Create(round(CAllpassTuningL2 * SampleRate * CReci441));
  FAllpass[2] := TFreeverbAllpass.Create(round(CAllpassTuningL3 * SampleRate * CReci441));
  FAllpass[3] := TFreeverbAllpass.Create(round(CAllpassTuningL4 * SampleRate * CReci441));

  Wet      := 1;
  Dry      := 1;
  RoomSize := 0.5;
  Damp     := 0.5;
end;

destructor TFreeverb.Destroy;
var
  i : Integer;
begin
 for i := 0 to CCombFilterCount - 1 do FreeAndNil(FAllpass[i]);
 for i := 0 to CAllpassCount - 1 do FreeAndNil(FComb[i]);
 inherited Destroy;
end;

function TFreeverb.ProcessSample(const Input: Double): Double;
var
  i: Integer;
begin
 result := FGain * Input;

 // Accumulate comb filters in parallel
 for i := 0 to CCombFilterCount - 1
  do result := result + FComb[i].Process(Input);

 // Feed through allpasses in series
 for i := 0 to CAllpassCount - 1
  do result := FAllpass[i].Process(result);

 result := result * FWet + Input * FDry;
end;

procedure TFreeverb.Reset;
var
  i : Integer;
begin
 for i := 0 to CCombFilterCount - 1 do FComb[i].Mute;
 for i := 0 to CAllpassCount - 1    do FAllpass[i].Mute;
end;

procedure TFreeverb.SetDamp(const Value: Double);
begin
 if FDamp <> Value then
  begin
   FDamp := Value;
   DampChanged;
  end;
end;

procedure TFreeverb.DampChanged;
var
  i : Integer;
begin
 for i := 0 to CCombFilterCount - 1 do
  begin
   FComb[i].Feedback := FRoomSize;
   FComb[i].Damp     := FDamp;
  end;
end;

procedure TFreeverb.SetRoomSize(const Value: Double);
begin
 if FRoomSize <> Value then
  begin
   FRoomSize := Value;
   RoomsizeChanged;
  end;
end;

procedure TFreeverb.RoomsizeChanged;
var
  i : Integer;
begin
 for i := 0 to CCombFilterCount - 1
  do FComb[i].Feedback := FRoomSize;
end;

procedure TFreeverb.SetSampleRate(const Value: Double);
begin
 if FSampleRate <> Value then
  begin
   FSampleRate := Value;
   SamplerateChanged;
  end;
end;

procedure TFreeverb.SampleRateChanged;
begin
 // nothing in here yet
end;

procedure TFreeverb.SetWet(const Value: Double);
begin
 if FWet <> Value then
  begin
   FWet := Value;
   WetChanged;
  end;
end;

procedure TFreeverb.WetChanged;
begin
 // nothing in here yet
end;

procedure TFreeverb.SetDry(const Value: Double);
begin
 if FDry <> Value then
  begin
   FDry := Value;
  end;
end;

procedure TFreeverb.DryChanged;
begin
 // nothing in here yet
end;

end.
