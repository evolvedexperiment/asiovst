unit DAV_StkWavePlayer;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

{ TWavePlayer }

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Stk, DAV_StkLfo, Windows, waveiox;

type
  TWavePlayer = class(TLFO)
  private
    os         : Boolean;
    sdata      : psingle;
    pfofs      : Single;
    FLoopstart : Integer;
    FLoopend   : Integer;
    ls, le     : Single;
  public
    Size: Longint;
    Length: Single;
    IsFinished: Boolean;

    // Set the loop points
    procedure SetLoop(st, en: Longint);

    // Seek to position in file
    procedure SetPos(p: Longint);

    // Class constructor, taking the desired number of modes to create.
    constructor Create(const SampleRate: Single); overload; override;

    // Overloaded constructor, load file on create
    constructor Create(const SampleRate: Single; FileName: String); overload; override;

    // Class destructor.
    destructor Destroy;

    // Reset and clear all internal state.
    procedure Reset;

    // Loads a WAV audio file
    procedure LoadFile(FileName: String);

    // Sets the playback rate
    procedure SetRate(r: Single);

    procedure SetOneShot(i: Boolean);

    // Compute one output sample.
    function Tick: Single;
  end;

implementation

constructor TWavePlayer.Create(const SampleRate: Single);
begin
  inherited Create(SampleRate);
  Size := 0;
  sdata := nil;
  FLoopstart := 0;
  FLoopend := 0;
  ls := 0;
  le := 0;
end;

constructor TWavePlayer.Create(const SampleRate: Single; const FileName: String);
begin
  if SampleRate <= 0
   then raise Exception.Create('Samplerate must be larger than zero');
  inherited Create(SampleRate);
  Size := 0;
  sdata := nil;
  FLoopstart := 0;
  FLoopend := 0;
  ls := 0;
  le := 0;
  LoadFile(FileName);
end;

destructor TWavePlayer.Destroy;
begin
  inherited Destroy;
  if assigned(sdata) then Dispose(sdata);
end;

procedure TWavePlayer.Reset;
begin
  setpos(0);
end;

procedure TWavePlayer.LoadFile(FileName: String);
var
  p: pointer;
  SampleRate, ch, fsize: Longint;
begin
  p := loadwavfile(FileName, SampleRate, ch, fsize);
  if (p <> nil) and (fsize > 0) then
   begin
    sdata := p;
    Size := fsize;
    Length := Size / SampleRate;
    FLoopstart := 0;
    FLoopend := Size - 1;
    ls := 0;
    le := (Size - 1) / Size;
    os := True;
    IsFinished := False;
   end;
end;

function TWavePlayer.Tick: Single;
var
  x, y: Longint;
  q: Single;
  s1, s2: psingle;
begin
  phase := pfofs + phase + (freq / srate);
  if os then
   begin
    IsFinished := (phase >= 1);
    if IsFinished then
     begin
      Result := 0;
      exit;
     end;
   end
  else
    while (phase >= 1) do
      phase := phase - 1;
  q := (phase * (le - ls) + ls);
  if q > 1 then
    q := 1
  else if q < 0 then
    q := 0;
  q := q * Size;
  x := round(q);
  q := q - x;
  s1 := psingle(Longint(sdata) + 4 * x);
  y := x + 1;
  if y > FLoopend then
    y := FLoopstart;
  s2 := psingle(Longint(sdata) + 4 * y);
  Result := s1^ * (1 - q) + s2^ * q;
end;

procedure TWavePlayer.SetLoop(st, en: Integer);
begin
  if st < 0 then
    st := 0
  else if st > Size - 1 then
    st := Size - 1;
  if en < 1 then
    en := 1
  else if en > Size - 1 then
    en := Size - 1;
  if st > en then
    st := 0;
  FLoopstart := st;
  FLoopend := en;
  ls := FLoopstart / Size;
  le := FLoopend / Size;
end;

procedure TWavePlayer.SetPos(p: Longint);
begin
  SetPhase(p / Size);
end;

procedure TWavePlayer.SetRate(r: Single);
begin
  setfrequency(r);
end;

procedure TWavePlayer.SetOneShot(i: Boolean);
begin
  os := i;
end;

end.

