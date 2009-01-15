unit DAV_StkWavePlayer;

{
/***************************************************/
/*! \class TWavePlayer
*/
/***************************************************/
}
interface

uses stk, lfo, Windows, waveiox;

type
  TWavePlayer = class(TLFO)
  public
    size: longint;
    length: my_float;
    isfinished: boolean;

  //! Set the loop points
    procedure SetLoop(st, en: longint);

  //! Seek to position in file
    procedure SetPos(p: longint);

  //! Class constructor, taking the desired number of modes to create.
    constructor Create(sr: my_float); overload;

  //! Overloaded constructor, load file on create
    constructor Create(sr: my_float; fn: string); overload;

  //! Class destructor.
    destructor Destroy;

  //! Reset and clear all internal state.
    procedure reset;

  //! Loads a WAV audio file
    procedure loadfile(fn: string);

  //! Sets the playback rate
    procedure SetRate(r: single);

    procedure SetOneShot(i: boolean);

  //! Compute one output sample.
    function tick: MY_FLOAT;

  private
    os: boolean;
    sdata: psingle;
    pfofs: single;
    loopstart, loopend: longint;
    ls, le: single;
  end;

implementation

constructor TWavePlayer.Create(sr: my_float);
begin
  inherited Create(sr);
  size := 0;
  sdata := nil;
  loopstart := 0;
  loopend := 0;
  ls := 0;
  le := 0;
end;

constructor TWavePlayer.Create(sr: my_float; fn: string);
begin
  if sr < 10 then
    sr := 44100;
  inherited Create(sr);
  size := 0;
  sdata := nil;
  loopstart := 0;
  loopend := 0;
  ls := 0;
  le := 0;
  loadfile(fn);
end;

destructor TWavePlayer.Destroy;
begin
  inherited Destroy;
  if assigned(sdata) then
    freemem(sdata);
end;

procedure TWavePlayer.reset;
begin
  setpos(0);
end;

procedure TWavePlayer.loadfile(fn: string);
var
  p: pointer;
  sr, ch, fsize: longint;
begin
  p := loadwavfile(fn, sr, ch, fsize);
  if (p <> nil) and (fsize > 0) then
   begin
    sdata := p;
    size := fsize;
    length := size / sr;
    loopstart := 0;
    loopend := size - 1;
    ls := 0;
    le := (size - 1) / size;
    os := True;
    isfinished := False;
   end;
end;

function TWavePlayer.tick: my_float;
var
  x, y: longint;
  q: single;
  s1, s2: psingle;
begin
  phase := pfofs + phase + (freq / srate);
  if os then
   begin
    isfinished := (phase >= 1);
    if isfinished then
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
  q := q * size;
  x := round(q);
  q := q - x;
  s1 := psingle(longint(sdata) + 4 * x);
  y := x + 1;
  if y > loopend then
    y := loopstart;
  s2 := psingle(longint(sdata) + 4 * y);
  Result := s1^ * (1 - q) + s2^ * q;
end;

procedure TWavePlayer.SetLoop(st, en: Integer);
begin
  if st < 0 then
    st := 0
  else if st > size - 1 then
    st := size - 1;
  if en < 1 then
    en := 1
  else if en > size - 1 then
    en := size - 1;
  if st > en then
    st := 0;
  loopstart := st;
  loopend := en;
  ls := loopstart / size;
  le := loopend / size;
end;

procedure TWavePlayer.SetPos(p: longint);
begin
  SetPhase(p / size);
end;

procedure TWavePlayer.SetRate(r: single);
begin
  setfrequency(r);
end;

procedure TWavePlayer.SetOneShot(i: boolean);
begin
  os := i;
end;

end.

