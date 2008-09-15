unit XSynthVoice;

interface

{$I ASIOVST.INC}

uses
  DAV_VSTModule, DAV_Complex;

{$i Consts.inc}

type
  TOscilatorType = (otNone, otSine, otHalfSine, otSquare, otNoise);
  TADSRStage = (adsrAttack, adsrDecay, adsrSustain, adsrRelease);
  TOsc = record
           OType   : TOscilatorType;
           Attack  : Single;
           Decay   : Single;
           Release : Single;
           Sustain : Single;
           Level   : Single;
  end;

  TOscilator = class(TObject)
  private
    fSampleRate : Single;
    fFrequency  : Single;
    fAmplitude  : Single;
    fAttack,
    fDecay,
    fSustain,
    fRelease    : Single;
    fADSRStage  : TADSRStage;
    fADSRGain   : Single;
    fLevel      : Single;
    fReleased   : Boolean;
    procedure SetAmplitude(const Value: Single); virtual;
    procedure SetFrequency(const Value: Single); virtual;
    procedure SetSampleRate(const Value: Single); virtual;
    procedure SetOsc(Osc : TOsc); virtual;
    procedure SetLevel(const Value: Single);
  protected
    function ProcessADSR: Single; virtual;
  public
    constructor Create; overload; virtual;
    constructor Create(SampleRate: Single); overload; virtual;
    destructor Destroy; override;
    function Process: Single; virtual;
    procedure ReleaseOsc; virtual;
  published
    property Frequency: Single read fFrequency write SetFrequency;
    property SampleRate: Single read fSampleRate write SetSampleRate;
    property Level: Single read fLevel write SetLevel;
    property Amplitude: Single read fAmplitude write SetAmplitude;
    property Attack: Single read fAttack write fAttack;
    property Decay: Single read fDecay write fDecay;
    property Sustain: Single read fSustain write fSustain;
    property Release: Single read fRelease write fRelease;
  end;

  TSineOscilator = class(TOscilator)
  private
    fAngle,
    fPosition   : TComplexDouble;
    procedure SetFrequency(const Value: Single); override;
  public
    constructor Create; override;
    function Process:Single; override;
  end;

  TSquareOscilator = class(TOscilator)
  private
    fAngle,
    fPosition   : TComplexDouble;
    procedure SetFrequency(const Value: Single); override;
  public
    constructor Create; override;
    function Process:Single; override;
  end;

  THalfSineOscilator = class(TSineOscilator)
  private
  public
    function Process:Single; override;
  end;

  TNoiseOscilator = class(TOscilator)
  private
  public
    constructor Create; override;
    function Process:Single; override;
  end;

  TXSynthVoice = class(TObject)
  private
    fMidiKeyNr  : Integer;
    fVelocity   : Integer;
    fSampleRate : Single;
    fFrequency  : Single;
    fAmplitude  : Single;
    fReleased   : Boolean;
    fOscilators : Array[0..1] of TOscilator;
    fVSTModule  : TVSTModule;
    function GetSampleRate: Single; virtual;
    procedure SetSampleRate(v: Single); virtual;
  public
    constructor Create(theModule: TVSTModule);
    destructor Destroy; override;
    procedure SetFrequency(Frequency: Single); virtual;
    procedure NoteOn(Frequency, Amplitude: Single);
    procedure NoteOff;
    function Process: Single; virtual;
  published
    property Frequency: Single read fFrequency write SetFrequency;
    property SampleRate: Single read GetSampleRate write SetSampleRate;
    property Released: Boolean read fReleased;
    property MidiKeyNr: Integer read fMidiKeyNr write fMidiKeyNr;
    property Velocity: Integer read fVelocity write fVelocity;
  end;

implementation

uses
  Math, SysUtils, DAV_Common, XSynthModule;

{ TOscilator }

constructor TOscilator.Create;
begin
 fSampleRate := 44100;
 fFrequency  := 1000;
 fAmplitude  := 1;
 fADSRStage  := adsrAttack;
 fReleased   := False;
end;

constructor TOscilator.Create(SampleRate: Single);
begin
 inherited Create;
 Self.SampleRate := SampleRate;
end;

destructor TOscilator.Destroy;
begin
 inherited;
end;

function TOscilator.ProcessADSR: Single;
begin
 case fADSRStage of
  adsrAttack  : begin
                 fADSRGain := fADSRGain + fAttack*(1 - fADSRGain);
                 if fADSRGain > 0.999
                  then fADSRStage := adsrDecay;
                end;
  adsrDecay   : begin
                 fADSRGain := fADSRGain - fDecay * (fADSRGain - fSustain);
                 if fADSRGain < 1.001 * fSustain
                  then fADSRStage := adsrSustain;
                end;
  adsrSustain : if fReleased then fADSRStage := adsrRelease;
  adsrRelease : begin
                 fADSRGain := fADSRGain - fRelease * fADSRGain;
                 if fADSRGain < 0.001
                  then fADSRGain := 0;
                end;
 end;
 Result := fADSRGain;
end;

procedure TOscilator.ReleaseOsc;
begin
 fReleased := True;
 fADSRStage := adsrRelease;
end;

function TOscilator.Process: Single;
begin
 result := 0;
end;

procedure TOscilator.SetAmplitude(const Value: Single);
begin
 if fAmplitude <> Value then
  begin
   fAmplitude := Value * fLevel;
  end;
end;

procedure TOscilator.SetFrequency(const Value: Single);
begin
 if fFrequency <> Value then
  begin
   fFrequency := Value;
  end;
end;

procedure TOscilator.SetLevel(const Value: Single);
begin
 if fLevel <> Value then
  begin
   fLevel := Value;
  end;
end;

procedure TOscilator.SetOsc(Osc: TOsc);
begin
 Attack  := Power(10, -8 * Osc.Attack);
 Decay   := Power(10, -8 * Osc.Decay);
 Release := Power(10, -8 * Osc.Release);
 Sustain := Osc.Sustain;
 Level   := Osc.Level;
end;

procedure TOscilator.SetSampleRate(const Value: Single);
begin
 if fSampleRate <> Value then
  begin
   fSampleRate := Value;
  end;
end;

{ TSineOscilator }

constructor TSineOscilator.Create;
begin
 inherited;
 fPosition.Re := 0;
 fPosition.Im := -1;
 fADSRGain := 0;
end;

function TSineOscilator.Process: Single;
begin
 result := fPosition.Re*fAngle.Re-fPosition.Im*fAngle.Im;
 fPosition.Im := fPosition.Im*fAngle.Re+fPosition.Re*fAngle.Im;
 fPosition.Re := result; result := result * fAmplitude * ProcessADSR;
end;

procedure TSineOscilator.SetFrequency(const Value: Single);
begin
 fFrequency := Value;
 GetSinCos(2 * Pi * fFrequency / fSampleRate, fAngle.Im, fAngle.Re);
end;

{ THalfSineOscilator }

function THalfSineOscilator.Process: Single;
begin
 Result := abs(inherited Process);
end;

{ TSquareOscilator }

constructor TSquareOscilator.Create;
begin
 inherited;
 fPosition.Re :=  0;
 fPosition.Im := -1;
end;

function TSquareOscilator.Process: Single;
begin
 result := fPosition.Re * fAngle.Re - fPosition.Im * fAngle.Im;
 fPosition.Im := fPosition.Im * fAngle.Re + fPosition.Re * fAngle.Im;
 fPosition.Re := result;
 if Result > 0
  then Result :=  fAmplitude * ProcessADSR
  else Result := -fAmplitude * ProcessADSR;
end;

procedure TSquareOscilator.SetFrequency(const Value: Single);
begin
 fFrequency := Value;
 GetSinCos(2 * Pi * fFrequency / fSampleRate, fAngle.Im, fAngle.Re);
end;

{ TNoiseOscilator }

constructor TNoiseOscilator.Create;
begin
 inherited;
 Randomize;
end;

function TNoiseOscilator.Process: Single;
begin
 result := (2 * random - 1) * fAmplitude * ProcessADSR;
end;

{ TXSynthVoice }

constructor TXSynthVoice.Create(theModule: TVSTModule);
var i : Integer;
begin
 fVSTModule := theModule;
 fReleased := False;
 if theModule.SampleRate=0
  then SampleRate := 44100
  else SampleRate := theModule.SampleRate;
 for i := 0 to 1 do
  with (fVSTModule as TVSTSSModule) do
   begin
    case Oscilators[i].OType of
     otNone     : fOscilators[i] := TOscilator.Create;
     otSine     : fOscilators[i] := TSineOscilator.Create;
     otHalfSine : fOscilators[i] := THalfSineOscilator.Create;
     otSquare   : fOscilators[i] := TSquareOscilator.Create;
     otNoise    : fOscilators[i] := TNoiseOscilator.Create;
    end;
    fOscilators[i].SampleRate := SampleRate;
    fOscilators[i].SetOsc(Oscilators[i]);
   end;
end;

destructor TXSynthVoice.Destroy;
begin
 fOscilators[0].Free;
 fOscilators[1].Free;
 inherited;
end;

function TXSynthVoice.GetSampleRate: Single;
begin
 result := fSampleRate;
end;

procedure TXSynthVoice.SetSampleRate(v: Single);
begin
 if (v > 0) then fSampleRate := v;
end;

function TXSynthVoice.Process: Single;
var
  i : Integer;
begin
 result := fOscilators[0].Process + fOscilators[1].Process;
 if (fOscilators[0].fADSRGain = 0) and
    (fOscilators[1].fADSRGain = 0) then
  with (fVSTModule as TVSTSSModule) do
   for i := 0 to Voices.Count - 1 do
    if Voices.Items[i] = Self then
     begin
      Voices.Delete(i);
      Exit;
     end;
end;

procedure TXSynthVoice.SetFrequency(Frequency: Single);
begin
 fFrequency := Frequency;
 fOscilators[0].Frequency := fFrequency;
 fOscilators[1].Frequency := fFrequency;
end;

procedure TXSynthVoice.NoteOn(Frequency, Amplitude: Single);
begin
 fFrequency := Frequency;
 SetFrequency(Frequency);
 fAmplitude := Amplitude;

 fOscilators[0].Frequency := Frequency;
 fOscilators[1].Frequency := Frequency;
 fOscilators[0].Amplitude := fAmplitude;
 fOscilators[1].Amplitude := fAmplitude;
end;

procedure TXSynthVoice.NoteOff;
begin
 fOscilators[0].ReleaseOsc;
 fOscilators[1].ReleaseOsc;
 fReleased := True;
end;

end.
