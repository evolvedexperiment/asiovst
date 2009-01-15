unit DAV_StkBrass;
{
/***************************************************/
/*! \class TBrass
    \brief STK simple TBrass instrument class.

    This class implements a simple Brass instrument
    waveguide model, a la Cook (TBone, HosePlayer).

    This is a digital waveguide model, making its
    use possibly subject to patents held by
    Stanford University, Yamaha, and others.

    Control Change Numbers: 
       - Lip Tension := 2
       - Slide Length := 4
       - Vibrato Frequency := 11
       - Vibrato Gain := 1
       - Volume := 128

    by Perry R. Cook and Gary P. Scavone, 1995 - 2002.
*/
/***************************************************/
}

interface

uses
  DAV_StkCommon, DAV_StkInstrmnt, DAV_StkDelaya, DAV_StkBiquad,
  DAV_StkPolezero, DAV_StkAdsr, DAV_StkLfo, Math;

type TBrass=class(TInstrmnt)
public
  //! Class constructor, taking the lowest desired playing frequency.
  constructor create(sr,lowestFrequency:my_float);

  //! Class destructor.
  destructor destroy;

  //! Reset and clear all internal state.
  procedure clear;

  //! Set instrument parameters for a particular frequency.
  procedure setFrequency(frequency:my_float);

  //! Set the lips frequency.
  procedure setLip(frequency:my_float);

  //! Apply breath pressure to instrument with given amplitude and rate of increase.
  procedure startBlowing(amplitude,rate:my_float);

  //! Decrease breath pressure with given rate of decrease.
  procedure stopBlowing(rate:my_float);

  //! Start a note with the given frequency and amplitude.
  procedure noteOn(frequency,amplitude:my_float);

  //! Stop a note with the given amplitude (speed of decay).
  procedure noteOff(amplitude:my_float);

  //! Compute one output sample.
  function tick:my_float;

  //! Perform the control change specified by \e number and \e value (0.0 - 128.0).
  procedure controlChange(number:integer;value:my_float);

 protected
  delayLine:TDelayA;
  lipFilter:TBiQuad;
  dcBlock:TPoleZero;
  adsr:TADSR;
  vibrato:TLFO;
  length:longint;
  lipTarget,slideTarget,vibratoGain,maxPressure:my_float;
end;

implementation

constructor TBrass.create;
begin
 inherited create(sr);
 length := round(srate/lowestFrequency+1);
 delayLine :=TDelayA.create(srate, 0.5 * length, length );

  lipFilter := TBiQuad.create(srate);
  lipFilter.setGain( 0.03 );
  dcBlock := TPoleZero.create(srate);
  dcBlock.setBlockZero(0.99);

  adsr := TADSR.create(srate);
  adsr.setAllTimes( 0.005, 0.001, 1.0, 0.010);

  vibrato := TLFO.create(srate);
  vibrato.setFrequency( 6.137 );
  vibratoGain := 0.0;

  clear;
  maxPressure := 0.0;
  lipTarget := 0.0;

  // Necessary to initialize variables.
  setFrequency( 220.0 );
end;

destructor TBrass.destroy;
begin
  inherited destroy;
  delayLine.free;
  lipFilter.free;
  dcBlock.free;
  adsr.free;
  vibrato.free;
end;

procedure TBrass.clear;
begin
 delayLine.clear;
 lipFilter.clear;
 dcBlock.clear;
end;

procedure TBrass.setFrequency;
var freakency:my_float;
begin
  freakency:= frequency;
  if ( frequency <= 0.0 ) then freakency := 220.0;

  // Fudge correction for filter delays.
  slideTarget := (srate / freakency * 2.0) + 3.0;
  delayLine.setDelay(slideTarget); // play a harmonic

  lipTarget := freakency;
  lipFilter.setResonance( freakency, 0.997, false );
end;

procedure TBrass.setLip;
var freakency:my_float;
begin
  freakency := frequency;
  if ( frequency <= 0.0 ) then freakency := 220.0;

  lipFilter.setResonance( freakency, 0.997, false );
end;

procedure TBrass.startBlowing;
begin
  adsr.setAttackRate(rate);
  maxPressure := amplitude;
  adsr.keyOn;
end;

procedure TBrass.stopBlowing;
begin
  adsr.setReleaseRate(rate);
  adsr.keyOff;
end;

procedure TBrass.noteOn;
begin
 setFrequency(frequency);
 startBlowing(amplitude, amplitude * 0.001);
end;

procedure TBrass.noteOff;
begin
 stopBlowing(amplitude * 0.005);
end;

function TBrass.tick:my_float;
var deltaPressure,borePressure,mouthPressure,breathPressure:my_float;
begin
  breathPressure := maxPressure * adsr.tick;
  breathPressure := breathPressure + vibratoGain * vibrato.tick;

  mouthPressure := 0.3 * breathPressure;
  borePressure := 0.85 * delayLine.lastOut;
  deltaPressure := mouthPressure - borePressure; // Differential pressure.
  deltaPressure := lipFilter.tick( deltaPressure );      // Force - > position.
  deltaPressure := deltaPressure*deltaPressure;          // Basic position to area mapping.
  if ( deltaPressure > 1.0 ) then deltaPressure := 1.0;         // Non-linear saturation.
  // The following input scattering assumes the mouthPressure := area.
  lastOutput := deltaPressure * mouthPressure + ( 1.0 - deltaPressure) * borePressure;
  lastOutput := delayLine.tick( dcBlock.tick( lastOutput ) );

  result:=lastOutput;
end;

procedure TBrass.controlChange;
var temp, norm:my_float;
begin
  norm := value; // * ONE_OVER_128;
  if ( norm < 0 ) then norm := 0.0
  else if ( norm > 1.0 ) then norm := 1.0;

  if (number = __SK_LipTension_) then
  begin // 2
    temp := lipTarget * power( 4.0, (2.0 * norm) - 1.0 );
    setLip(temp);
  end
  else if (number = __SK_SlideLength_) then // 4
    delayLine.setDelay( slideTarget * (0.5 + norm) )
  else if (number = __SK_ModFrequency_) then // 11
    vibrato.setFrequency( norm * 12.0 )
  else if (number = __SK_ModWheel_ ) then // 1
    vibratoGain := norm * 0.4
  else if (number = __SK_AfterTouch_Cont_) then // 128
    adsr.setTarget( norm );
end;

end.
