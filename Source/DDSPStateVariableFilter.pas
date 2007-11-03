unit DDSPStateVariableFilter;

interface

uses
  DAVDCommon;

type
  TFrequencyTuningMethod= (ftmSimple, ftmIdeal);
  TSVF = class
  private
    fQ1,fQ  : Single;
    fF1,fF  : Single;
    fFS,fFr : Single;
    fD1,fD2 : Double;
    //ftL,ftH : Double;
    //ftB,ftN : Double;
    fFTM    : TFrequencyTuningMethod;
    procedure SetFrequency(v:Single);
    procedure SetQ(v:Single);
    procedure CalcQ;
    procedure SetFS(const Value: Single);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Process(const I : Single; var L,B,N,H: Single);
    procedure ProcessBlock(I,L,B,N,H: PSingle; SampleFrames : Integer);
    property Frequency: Single read fF write SetFrequency;
    property SampleRate: Single read fFS write SetFS;
    property Q: Single read fQ write SetQ;
    property FrequencyTuningMethod: TFrequencyTuningMethod read fFTM write fFTM;
  end;

implementation

uses sysutils;

const
  kDenorm = 1.0e-32;

constructor TSVF.Create;
begin
 inherited;
 fQ1  := 1;
 fF1  := 1000;
 fFS  := 44100;
 fFr  := 1/fFS;
 fFTM := ftmIdeal;
end;

destructor TSVF.Destroy;
begin
 inherited;
end;

procedure TSVF.SetFrequency(v:Single);
begin
 if fFS <= 0
  then raise exception.create('Sample Rate Error!');
 if v <> fF then
  begin
   fF := v;
   case fFTM of
    ftmSimple:
     begin
      // simple frequency tuning with error towards nyquist
      // F is the filter's center frequency, and Fs is the sampling rate
      if fF>17000
       then fF1 := 2*pi*17000*fFr
       else fF1 := 2*pi*fF*fFr;
      CalcQ;
     end;
    ftmIdeal:
     begin
      // ideal tuning:
      if fF>17000
       then fF1 := 2*sin(pi*17000*fFr)
       else fF1 := 2*sin(pi*fF*fFr);
      CalcQ;
     end;
   end;
  end;
end;

procedure TSVF.CalcQ;
const fspd:Double=1/1200;
begin
 if fF>5000
  then fQ1 := 1 / (fQ + ((fF - 5000) * fspd))
  else fQ1 := 1 /  fQ;
end;

procedure TSVF.SetQ(v:Single);
begin
 if v <> fQ then
  begin
   if v >= 0.5
    then fQ := v
    else fQ := 0.5;
   CalcQ;
  end;
end;

procedure TSVF.Process(const I : Single; var L,B,N,H: Single);
begin
 L := fD2+fF1*fD1;
 H := (I+kDenorm)-L-fQ1*fD1;
 B := fF1*H+fD1;
 N := H+L;
 // store delays
 fD1 := B;
 fD2 := L-kDenorm;
end;

procedure TSVF.ProcessBlock(I,L,B,N,H: PSingle; SampleFrames : Integer);
var c: Integer;
begin
 for c := 0 to SampleFrames-1 do
  begin
   L^  := fD2+fF1*fD1;
   H^  := (I^+kDenorm)-L^-fQ1*fD1-kDenorm;
   B^  := fF1*H^+fD1;
   N^  := H^+L^;
   // store delays
   fD1 := B^;
   fD2 := L^-kDenorm;
   Inc(L,4);
   inc(H);
   inc(B);
   inc(N);
  end;
end;

procedure TSVF.SetFS(const Value: Single);
begin
 if fFS <> Value then
  begin
   fFS := Value;
   fFr := 1/fFS;
  end;
end;

end.
