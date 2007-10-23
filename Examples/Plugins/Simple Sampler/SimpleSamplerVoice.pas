unit SimpleSamplerVoice;

interface

uses DVSTModule, DAVDComplex;

{$i Consts.inc}

type
  TSimpleSamplerVoice = class(TObject)
  private
    fMidiKeyNr  : Integer;
    fVelocity   : Integer;
    fSampleRate : Single;
    fFrequency  : Single;
    fAmplitude  : Single;
    fVSTModule  : TVSTModule;
    fSamplePos  : Integer;
    fSampleFrac : Single;
    fSampleInc  : Single;
    fMem        : Array [0..3] of Single;

    fAngle,
    fPosition   : TComplexDouble;
    function GetSampleRate:Single; virtual;
    procedure SetSampleRate(v:Single); virtual;
  public
    constructor Create(theModule:TVSTModule);
    destructor Destroy; override;
    procedure SetFrequency(Frequency:Single); virtual;
    procedure NoteOn(Frequency, Amplitude:Single);
    procedure NoteOff;
    function Process:Single; virtual;
  published
    property Frequency : Single read fFrequency write SetFrequency;
    property SampleRate : Single read GetSampleRate write SetSampleRate;
    property MidiKeyNr : Integer read fMidiKeyNr write fMidiKeyNr;
    property Velocity : Integer read fVelocity write fVelocity;
  end;

implementation

uses DAVDCommon, SimpleSamplerModule;

{ TSimpleSamplerVoice }

constructor TSimpleSamplerVoice.Create(theModule: TVSTModule);
begin
 fVSTModule:=theModule;
 if theModule.SampleRate=0
  then SampleRate:=44100
  else SampleRate:=theModule.SampleRate;
 fPosition.Re:=0;
 fPosition.Im:=-1;
 fSamplePos:=0;
 fSampleFrac:=0;
 fSampleInc:=0;
end;

destructor TSimpleSamplerVoice.Destroy;
begin
 inherited;
end;

function TSimpleSamplerVoice.GetSampleRate: Single;
begin
 result:=fSampleRate;
end;

procedure TSimpleSamplerVoice.SetSampleRate(v: Single);
begin
 if (v > 0) then fSampleRate:=v;
end;

function TSimpleSamplerVoice.Process: Single;
begin
 if Length(TVSTSSModule(fVSTModule).Sample)<=0 then
  begin
   result:=fPosition.Re*fAngle.Re-fPosition.Im*fAngle.Im;
   fPosition.Im:=fPosition.Im*fAngle.Re+fPosition.Re*fAngle.Im;
   fPosition.Re:=result; result:=result*fAmplitude;
  end
 else
  begin
   Result:=fAmplitude*Hermite_asm(fSampleFrac, @fMem[0]);
   fSampleFrac:=fSampleFrac+fSampleInc;
   while fSampleFrac>=1 do
     begin
      inc(fSamplePos);
      if fSamplePos>=Length(TVSTSSModule(fVSTModule).Sample)
       then fSamplePos:=0;
      fSampleFrac:=fSampleFrac-1;
      Move(fMem[1],fMem[0],12);
      fMem[3]:=TVSTSSModule(fVSTModule).Sample[fSamplePos];
     end;
  end;
end;

procedure TSimpleSamplerVoice.SetFrequency(Frequency: Single);
  procedure GetSinCos(Frequency: Double; var SinValue, CosValue : Double);
  asm
   fld Frequency.Double;
   fsincos
   fstp [CosValue].Double;
   fstp [SinValue].Double;
  end;
begin
 fFrequency:=Frequency;
 fSampleInc:=Frequency/440;
 GetSinCos(2*Pi*fFrequency/fSampleRate,fAngle.Im,fAngle.Re);
end;

procedure TSimpleSamplerVoice.NoteOn(Frequency, Amplitude: Single);
begin
 SetFrequency(Frequency);
 fAmplitude:=Amplitude;
end;

procedure TSimpleSamplerVoice.NoteOff;
begin
 fAmplitude:=0;
end;

end.
