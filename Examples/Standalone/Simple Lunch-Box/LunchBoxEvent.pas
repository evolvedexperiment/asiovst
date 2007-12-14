unit LunchBoxEvent;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses DAVDComplex;

type
  TLunchBoxSample = class(TObject)
  private
    fMidiKeyNr   : Integer;
    fVelocity    : Integer;
    fSampleRate  : Single;
    fFrequency   : Single;
    fAmplitude   : Single;
    fSamplePos   : Integer;
    fSampleFrac  : Single;
    fSampleInc   : Single;
    fIsPlaying   : Boolean;
    fSampleIndex : Integer;
    fMem         : Array [0..3] of Single;

    fAngle,
    fPosition    : TComplexDouble;
    fPatPos      : Integer;
    function GetSampleRate:Single; virtual;
    procedure SetSampleRate(v:Single); virtual;
  public
    constructor Create(Sample : Integer);
    destructor Destroy; override;
    procedure SetFrequency(Frequency:Single); virtual;
    function Process:Single; virtual;
    procedure NoteOn(Amplitude : Single);
    procedure NoteOff;
  published
    property IsPlaying : Boolean read fIsPlaying write fIsPlaying;
    property PatternPosition : Integer read fPatPos write fPatPos;
    property Frequency : Single read fFrequency write SetFrequency;
    property SampleRate : Single read GetSampleRate write SetSampleRate;
    property MidiKeyNr : Integer read fMidiKeyNr write fMidiKeyNr;
    property Velocity : Integer read fVelocity write fVelocity;
    property SampleIndex : Integer read fSampleIndex;
  end;

implementation

uses DAVDCommon, LunchBoxMain, DDspInterpolation;

{ TLunchBoxSample }

constructor TLunchBoxSample.Create(Sample : Integer);
begin
 SampleRate   := 44100;
 fPosition.Re := 0;
 fPosition.Im := -1;
 fSamplePos   := 0;
 fSampleFrac  := 0;
 fSampleInc   := 0;
 fPatPos      := 0;
 fSampleIndex := Sample;
 fIsPlaying   := False;
end;

destructor TLunchBoxSample.Destroy;
begin
 inherited;
end;

function TLunchBoxSample.GetSampleRate: Single;
begin
 result:=fSampleRate;
end;

procedure TLunchBoxSample.NoteOn(Amplitude: Single);
begin
 fIsPlaying:=True;
 fAmplitude:=Amplitude;
end;

procedure TLunchBoxSample.NoteOff;
begin
 fSamplePos:=0;
 fIsPlaying:=False;
end;

procedure TLunchBoxSample.SetSampleRate(v: Single);
begin
 if (v > 0) then fSampleRate:=v;
end;

function TLunchBoxSample.Process: Single;
begin
 Result := fAmplitude * Hermite32_asm(fSampleFrac, @fMem[0]);
 fSampleFrac := fSampleFrac + fSampleInc;
 while fSampleFrac >= 1 do
  begin
   inc(fSamplePos);
   if fSamplePos >= Length(Samples[fSampleIndex].Data)
    then NoteOff;
   fSampleFrac := fSampleFrac-1;
   Move(fMem[1],fMem[0],12);
   fMem[3] := Samples[fSampleIndex].Data[fSamplePos];
  end;
end;

procedure TLunchBoxSample.SetFrequency(Frequency: Single);
  procedure GetSinCos(Frequency: Double; var SinValue, CosValue : Double);
  asm
   fld Frequency.Double;
   fsincos
   fstp [CosValue].Double;
   fstp [SinValue].Double;
  end;
begin
 fFrequency:=Frequency;
 fSampleInc:=Frequency;
 GetSinCos(2*Pi*fFrequency/fSampleRate,fAngle.Im,fAngle.Re);
end;

end.
