unit SineSynthVoice;

interface

uses DVSTModule;

{$i Consts.inc}

type
  TComplexDouble = record
                    Re, Im : Double;
                   end;
  TSineSynthVoice = class(TObject)
  private
    fMidiKeyNr  : Integer;
    fVelocity   : Integer;
    fSampleRate : Single;
    fFrequency  : Single;
    fAmplitude  : Single;
    fVSTModule  : TVSTModule;
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

uses DDSPBase, SineSynthModule;

{ TSineSynthVoice }

constructor TSineSynthVoice.Create(theModule: TVSTModule);
begin
 fVSTModule:=theModule;
 if theModule.SampleRate=0
  then SampleRate:=44100
  else SampleRate:=theModule.SampleRate;
 fPosition.Re:=0;
 fPosition.Im:=-1;
end;

destructor TSineSynthVoice.Destroy;
begin
 inherited;
end;

function TSineSynthVoice.GetSampleRate: Single;
begin
 result:=fSampleRate;
end;

procedure TSineSynthVoice.SetSampleRate(v: Single);
begin
 if (v > 0) then fSampleRate:=v;
end;

function TSineSynthVoice.Process: Single;
begin
 result:=fPosition.Re*fAngle.Re-fPosition.Im*fAngle.Im;
 fPosition.Im:=fPosition.Im*fAngle.Re+fPosition.Re*fAngle.Im;
 fPosition.Re:=result; result:=result * fAmplitude;
end;

procedure TSineSynthVoice.SetFrequency(Frequency: Single);
  procedure GetSinCos(Frequency: Double; var SinValue, CosValue : Double);
  asm
   fld Frequency.Double;
   fsincos
   fstp [CosValue].Double;
   fstp [SinValue].Double;
  end;
begin
 fFrequency:=Frequency;
 GetSinCos(2*Pi*fFrequency/fSampleRate,fAngle.Im,fAngle.Re);
end;

procedure TSineSynthVoice.NoteOn(Frequency, Amplitude: Single);
begin
 fFrequency:=Frequency;
 SetFrequency(Frequency);
 fAmplitude:=Amplitude;
end;

procedure TSineSynthVoice.NoteOff;
begin
 fAmplitude:=0;
end;

end.
