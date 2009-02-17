unit DAV_DspLorenzOscilator;

interface

// C++ version (c) 2004 Russell Borogove / www.tinygod.com
// Delphi Pascal Version ©2005, Thaddy de Koning / www.thaddy.com
//
//  Lorenz/Rossler iterative function systems as LFOs
//

//
// This module defines the classes TLorenzOsc and TRosslerOsc - low frequency
// oscillators suitable for modeling 'analog drift' or other random-but-smooth
// processes. Both classes have identical APIs - you could unify the interface
// with virtual functions easily.
//
// SetSampleRate:
// Sets the sample SampleRate at which the Iterate function will be called. Only
// meaningful in conjunction with the SetFreq function.
//
// SetFreq:
// Sets the fundamental frequency of the oscillator. The Rossler oscillator
// should exhibit harmonic peaks at multiples of that frequency; the Lorenz
// oscillator has a linear frequency-amplitude relation, so SetFreq will
// only control the scale of waveform features in a general way.
//
// Iterate:
// Advances the clock by one sample period and returns the value of the
// function at the current clock; it should be called once per sample-tick.
//
// GetCurrent:
// Returns the same value returned by the latest call to Iterate. Useful
// in cases where one generator modulates multiple destinations, for example.
//
// GetAlternate:
// Returns a value separate from the current value but correlated with it;
// these are the X and Y values used for the well-known "butterfly" plots
// of the Lorenz and Rossler functions. You can use GetAlternate if you
// want two separate LFOs which are related in mysterious ways at a low
// cost - for example, you can fine-tune one audio oscillator with the return
// from Iterate and another oscillator with the return from GetAlternate.
//
// Both the primary and alternate returns are calibrated to a -1.0 to +1.0
// range in normal usage. The implementation is discrete, though, so if the
// sample SampleRate is low or the frequency high, it may occasionally jump outside
// that range -- the user is responsible for clamping if the range is
// critical.
//


uses
  SysUtils;

const
  CLorenzScale: Single = 0.05107;
  CLorenzAltScale: Single = 0.03679;
  CRosslerScale: Single = 0.05757;
  CRosslerAltScale: Single = 0.06028;

type
  TCustomLorenzRosslerOsc = class
  protected
    FDX   : Single;
    FDY   : Single;
    FDZ   : Single;
    FDT   : Single;
    FFreq : Single;
    FX    : Single;
    FY    : Single;
    FZ    : Single;
    FA    : Single;
    FB    : Single;
    FC    : Single;
    FRate : Single;
  public
    constructor Create; virtual;
    function Iterate: Single; virtual; abstract;
  end;


  // Lorenz function - very broad spectrum noise function with amplitude
  // decreasing with increasing frequency, but tight short-term correlation.
  //
  // The scale of waveform features will change somewhat with the set frequency
  // and sample SampleRate, but not drastically - it's fairly fractal. In particular,
  // there will not be substantial spectral peaks at multiples of the frequency
  // selected by SetFreq.

  TLorenzOsc = class(TCustomLorenzRosslerOsc)
  public
    constructor Create; virtual;
    procedure SetSampleRate(const SampleRate: Single);
    procedure SetFreq(const Frequency: Single);
    function GetCurrent: Single;
    function GetAlternate: Single;
    function Iterate: Single; override;
  end;


  // Rossler function - broad spectrum noise function with amplitude
  // decreasing with increasing frequency, and distinct harmonic peaks. The
  // peaks should occur at harmonics of the frequency set by SetFreq.

  TRosslerOsc = class(TCustomLorenzRosslerOsc)
  public
    constructor Create; virtual;
    procedure SetSampleRate(const SampleRate: Single);
    procedure SetFreq(const Frequency: Single);
    function GetCurrent: Single;
    function GetAlternate: Single;
    function Iterate: Single; override;
  end;

implementation

{ TCustomLorenzRosslerOsc }

constructor TCustomLorenzRosslerOsc.Create;
begin
  inherited;
  FDX := 0;
  FDY := 0;
  FDZ := 0;
  FX := 1;
  FY := 1;
  FZ := 1;
end;

{ TLorenzOsc }

constructor TLorenzOsc.Create;
begin  
  inherited;
  FA := 10.0;
  FB := 28.0;
  FC := 2.666;
  FFreq := 440;
  SetSampleRate(44100);
  SetFreq(440);
end;

procedure TLorenzOsc.SetSampleRate(const SampleRate: Single);
begin
 if FRate <> SampleRate then
  begin
   FRate := SampleRate;
   FDT := FFreq / SampleRate;
  end;
end;

procedure TLorenzOsc.SetFreq(const Frequency: Single);
begin
 if FFreq <> Frequency then
  begin
   FFreq := Frequency;
   FDT := Frequency / FRate;
  end;
end;


function TLorenzOsc.GetCurrent: Single;
begin
  Result:= FX * CLorenzScale;
end;

function TLorenzOsc.GetAlternate: Single;
begin
  Result:= FY * CLorenzAltScale;
end;

function TLorenzOsc.Iterate: Single;
begin
  FDX := FA * (FY-FX);
  FDY := FX * (FB-FZ) - FY;
  FDZ := FX * FY - FC * FZ;

  FX := FX + FDX * FDT;
  FY := FY + FDY * FDT;
  FZ := FZ + FDZ * FDT;

  Result:= FX * CLorenzScale;
end;


{ TRosslerOsc }

constructor TRosslerOsc.Create;
begin
  inherited;
  FA := 0.15;
  FB := 0.20;
  FC := 10;
  FFreq := 440;
  SetSampleRate(44100);
  SetFreq(440);
end;

procedure TRosslerOsc.SetSampleRate(const SampleRate: Single);
begin
  FRate := SampleRate;
  FDT := 2.91 * FFreq / SampleRate;
end;

procedure TRosslerOsc.SetFreq(const Frequency: Single);
begin
  FFreq := Frequency;
  FDT := 2.91 * Frequency / FRate;
end;


function TRosslerOsc.GetCurrent: Single;
begin
  Result:= FX * CRosslerScale;
end;

function TRosslerOsc.GetAlternate: Single;
begin
  Result:= FY * CRosslerAltScale;
end;

function TRosslerOsc.Iterate: Single;
begin
  FDX := -FY - FZ;
  FDY := FX + FA * FY;
  FDZ := FB + FZ * (FX - FC);

  FX := FX + FDX * FDT;
  FY := FY + FDY * FDT;
  FZ := FZ + FDZ * FDT;

  Result := FX * CRosslerScale;
end;

end.
