unit DAV_StkShakers;

{
/***************************************************/
/*! \class TShakers
    \brief PhISEM and PhOLIES class.

    PhISEM (Physically Informed Stochastic Event
    Modeling) is an algorithmic approach for
    simulating collisions of multiple independent
    sound producing objects.  This class is a
    meta-model that can simulate a Maraca, Sekere,
    Cabasa, Bamboo Wind Chimes, Water Drops,
    Tambourine, Sleighbells, and a Guiro.

    PhOLIES (Physically-Oriented Library of
    Imitated Environmental Sounds) is a similar
    approach for the synthesis of environmental
    sounds.  This class implements simulations of
    breaking sticks, crunchy snow (or not), a
    wrench, sandpaper, and more.

    Control Change Numbers: 
      - Shake Energy:=2
      - System Decay:=4
      - Number Of Objects:=11
      - Resonance Frequency:=1
      - Shake Energy:=128
      - Instrument Selection:=1071
        - Maraca:=0
        - Cabasa:=1
        - Sekere:=2
        - Guiro:=3
        - Water Drops:=4
        - Bamboo Chimes:=5
        - Tambourine:=6
        - Sleigh Bells:=7
        - Sticks:=8
        - Crunch:=9
        - Wrench:=10
        - Sand Paper:=11
        - Coke Can:=12
        - Next Mug:=13
        - Penny + Mug:=14
        - Nickle + Mug:=15
        - Dime + Mug:=16
        - Quarter + Mug:=17
        - Franc + Mug:=18
        - Peso + Mug:=19
        - Big Rocks:=20
        - Little Rocks:=21
        - Tuned Bamboo Chimes:=22

    by Perry R. Cook, 1996 - 1999.
*/
/***************************************************/
}
interface

uses stk, instrmnt, Math;

const
  MAX_FREQS = 8;
  NUM_INSTR = 24;

type
  TShakers = class(TInstrmnt)
  public
  //! Class constructor.
    constructor Create(sr: my_float);

  //! Class destructor.
    destructor Destroy;

  //! Start a note with the given instrument and amplitude.
  {
    Use the instrument numbers above, converted to frequency values
    as if MIDI note numbers, to select a particular instrument.
  }
    procedure noteOn(frequency, amplitude: MY_FLOAT); overload;

  //! Stop a note with the given amplitude (speed of decay).
    procedure noteOff(amplitude: MY_FLOAT);

  //! Compute one output sample.
    function tick: MY_FLOAT;

  //! Perform the control change specified by \e number and \e value (0.0 - 128.0).
    procedure controlChange(number: integer; Value: MY_FLOAT);

  protected
    shakeEnergy, sndLevel, baseGain, soundDecay, systemDecay,
    nObjects, collLikely, totalEnergy, ratchet, ratchetDelta: my_float;
    center_freqs, t_center_freqs, gains,
    freq_rand, resons, inputs: array[0..MAX_FREQS - 1] of my_float;
    coeffs, outputs: array[0..MAX_FREQS - 1, 0..1] of my_float;
    finalZCoeffs, finalZ: array[0..2] of my_float;
    decayScale, defDecays, defObjs: array[0..NUM_INSTR - 1] of my_float;
    freqalloc: array[0..MAX_FREQS - 1] of integer;
    nFreqs, ratchetPos, lastRatchetPos, instType: integer;
  //  int setupName(char* instr);
    function setFreqAndReson(which: integer; freq, reson: my_float): integer;
    procedure setDecays(sndDecay, sysDecay: my_float);
    procedure setFinalZs(z0, z1, z2: my_float);
    function setupNum(inst: integer): integer;
    function wuter_tick: my_float;
    function tbamb_tick: my_float;
    function ratchet_tick: my_float;
  end;

implementation

function my_random(max: integer): integer;
 //  Return Random Int Between 0 and max
begin
  Result := round(max * random);
end;

function float_random(max: my_float): my_float;
 // Return random float between 0.0 and max
begin
  Result := max * random;
end;

function noise_tick: my_float;
 //  Return random MY_FLOAT float between -1.0 and 1.0
begin
  Result := 2 * random - 1;
end;

// Maraca
const
  MARA_SOUND_DECAY = 0.95;
  MARA_SYSTEM_DECAY = 0.999;
  MARA_GAIN = 20.0;
  MARA_NUM_BEANS = 25;
  MARA_CENTER_FREQ = 3200.0;
  MARA_RESON = 0.96;

// Sekere
  SEKE_SOUND_DECAY = 0.96;
  SEKE_SYSTEM_DECAY = 0.999;
  SEKE_GAIN = 20.0;
  SEKE_NUM_BEANS = 64;
  SEKE_CENTER_FREQ = 5500.0;
  SEKE_RESON = 0.6;

// Sandpaper
  SANDPAPR_SOUND_DECAY = 0.999;
  SANDPAPR_SYSTEM_DECAY = 0.999;
  SANDPAPR_GAIN = 0.5;
  SANDPAPR_NUM_GRAINS = 128;
  SANDPAPR_CENTER_FREQ = 4500.0;
  SANDPAPR_RESON = 0.6;

// Cabasa
  CABA_SOUND_DECAY = 0.96;
  CABA_SYSTEM_DECAY = 0.997;
  CABA_GAIN = 40.0;
  CABA_NUM_BEADS = 512;
  CABA_CENTER_FREQ = 3000.0;
  CABA_RESON = 0.7;

// Bamboo Wind Chimes
  BAMB_SOUND_DECAY = 0.95;
  BAMB_SYSTEM_DECAY = 0.9999;
  BAMB_GAIN = 2.0;
  BAMB_NUM_TUBES = 1.25;
  BAMB_CENTER_FREQ0 = 2800.0;
  BAMB_CENTER_FREQ1 = 0.8 * 2800.0;
  BAMB_CENTER_FREQ2 = 1.2 * 2800.0;
  BAMB_RESON = 0.995;

// Tuned Bamboo Wind Chimes (Anklung)
  TBAMB_SOUND_DECAY = 0.95;
  TBAMB_SYSTEM_DECAY = 0.9999;
  TBAMB_GAIN = 1.0;
  TBAMB_NUM_TUBES = 1.25;
  TBAMB_CENTER_FREQ0 = 1046.6;
  TBAMB_CENTER_FREQ1 = 1174.8;
  TBAMB_CENTER_FREQ2 = 1397.0;
  TBAMB_CENTER_FREQ3 = 1568.0;
  TBAMB_CENTER_FREQ4 = 1760.0;
  TBAMB_CENTER_FREQ5 = 2093.3;
  TBAMB_CENTER_FREQ6 = 2350.0;
  TBAMB_RESON = 0.996;

// Water Drops
  WUTR_SOUND_DECAY = 0.95;
  WUTR_SYSTEM_DECAY = 0.996;
  WUTR_GAIN = 1.0;
  WUTR_NUM_SOURCES = 10;
  WUTR_CENTER_FREQ0 = 450.0;
  WUTR_CENTER_FREQ1 = 600.0;
  WUTR_CENTER_FREQ2 = 750.0;
  WUTR_RESON = 0.9985;
  WUTR_FREQ_SWEEP = 1.0001;

// Tambourine
  TAMB_SOUND_DECAY = 0.95;
  TAMB_SYSTEM_DECAY = 0.9985;
  TAMB_GAIN = 5.0;
  TAMB_NUM_TIMBRELS = 32;
  TAMB_SHELL_FREQ = 2300;
  TAMB_SHELL_GAIN = 0.1;
  TAMB_SHELL_RESON = 0.96;
  TAMB_CYMB_FREQ1 = 5600;
  TAMB_CYMB_FREQ2 = 8100;
  TAMB_CYMB_RESON = 0.99;

// Sleighbells
  SLEI_SOUND_DECAY = 0.97;
  SLEI_SYSTEM_DECAY = 0.9994;
  SLEI_GAIN = 1.0;
  SLEI_NUM_BELLS = 32;
  SLEI_CYMB_FREQ0 = 2500;
  SLEI_CYMB_FREQ1 = 5300;
  SLEI_CYMB_FREQ2 = 6500;
  SLEI_CYMB_FREQ3 = 8300;
  SLEI_CYMB_FREQ4 = 9800;
  SLEI_CYMB_RESON = 0.99;

// Guiro
  GUIR_SOUND_DECAY = 0.95;
  GUIR_GAIN = 10.0;
  GUIR_NUM_PARTS = 128;
  GUIR_GOURD_FREQ = 2500.0;
  GUIR_GOURD_RESON = 0.97;
  GUIR_GOURD_FREQ2 = 4000.0;
  GUIR_GOURD_RESON2 = 0.97;

// Wrench
  WRENCH_SOUND_DECAY = 0.95;
  WRENCH_GAIN = 5;
  WRENCH_NUM_PARTS = 128;
  WRENCH_FREQ = 3200.0;
  WRENCH_RESON = 0.99;
  WRENCH_FREQ2 = 8000.0;
  WRENCH_RESON2 = 0.992;

// Cokecan
  COKECAN_SOUND_DECAY = 0.97;
  COKECAN_SYSTEM_DECAY = 0.999;
  COKECAN_GAIN = 0.8;
  COKECAN_NUM_PARTS = 48;
  COKECAN_HELMFREQ = 370;
  COKECAN_HELM_RES = 0.99;
  COKECAN_METLFREQ0 = 1025;
  COKECAN_METLFREQ1 = 1424;
  COKECAN_METLFREQ2 = 2149;
  COKECAN_METLFREQ3 = 3596;
  COKECAN_METL_RES = 0.992;

// PhOLIES (Physically-Oriented Library of Imitated Environmental
// Sounds), Perry Cook,=1997-8

// Stix1
  STIX1_SOUND_DECAY = 0.96;
  STIX1_SYSTEM_DECAY = 0.998;
  STIX1_GAIN = 30.0;
  STIX1_NUM_BEANS = 2;
  STIX1_CENTER_FREQ = 5500.0;
  STIX1_RESON = 0.6;

// Crunch1
  CRUNCH1_SOUND_DECAY = 0.95;
  CRUNCH1_SYSTEM_DECAY = 0.99806;
  CRUNCH1_GAIN = 20.0;
  CRUNCH1_NUM_BEADS = 7;
  CRUNCH1_CENTER_FREQ = 800.0;
  CRUNCH1_RESON = 0.95;

// Nextmug
  NEXTMUG_SOUND_DECAY = 0.97;
  NEXTMUG_SYSTEM_DECAY = 0.9995;
  NEXTMUG_GAIN = 0.8;
  NEXTMUG_NUM_PARTS = 3;
  NEXTMUG_FREQ0 = 2123;
  NEXTMUG_FREQ1 = 4518;
  NEXTMUG_FREQ2 = 8856;
  NEXTMUG_FREQ3 = 10753;
  NEXTMUG_RES = 0.997;

  PENNY_FREQ0 = 11000;
  PENNY_FREQ1 = 5200;
  PENNY_FREQ2 = 3835;
  PENNY_RES = 0.999;

  NICKEL_FREQ0 = 5583;
  NICKEL_FREQ1 = 9255;
  NICKEL_FREQ2 = 9805;
  NICKEL_RES = 0.9992;

  DIME_FREQ0 = 4450;
  DIME_FREQ1 = 4974;
  DIME_FREQ2 = 9945;
  DIME_RES = 0.9993;

  QUARTER_FREQ0 = 1708;
  QUARTER_FREQ1 = 8863;
  QUARTER_FREQ2 = 9045;
  QUARTER_RES = 0.9995;

  FRANC_FREQ0 = 5583;
  FRANC_FREQ1 = 11010;
  FRANC_FREQ2 = 1917;
  FRANC_RES = 0.9995;

  PESO_FREQ0 = 7250;
  PESO_FREQ1 = 8150;
  PESO_FREQ2 = 10060;
  PESO_RES = 0.9996;

// Big Gravel
  BIGROCKS_SOUND_DECAY = 0.98;
  BIGROCKS_SYSTEM_DECAY = 0.9965;
  BIGROCKS_GAIN = 20.0;
  BIGROCKS_NUM_PARTS = 23;
  BIGROCKS_FREQ = 6460;
  BIGROCKS_RES = 0.932;

// Little Gravel
  LITLROCKS_SOUND_DECAY = 0.98;
  LITLROCKS_SYSTEM_DECAY = 0.99586;
  LITLROCKS_GAIN = 20.0;
  LITLROCKS_NUM_PARTS = 1600;
  LITLROCKS_FREQ = 9000;
  LITLROCKS_RES = 0.843;

// Finally ... the class code!

constructor TShakers.Create;
var
  i: integer;
begin
  inherited Create(sr);
  instType := 0;
  shakeEnergy := 0.0;
  nFreqs := 0;
  sndLevel := 0.0;

  for i := 0 to MAX_FREQS - 1 do
   begin
    inputs[i] := 0.0;
    outputs[i][0] := 0.0;
    outputs[i][1] := 0.0;
    coeffs[i][0] := 0.0;
    coeffs[i][1] := 0.0;
    gains[i] := 0.0;
    center_freqs[i] := 0.0;
    resons[i] := 0.0;
    freq_rand[i] := 0.0;
    freqalloc[i] := 0;
   end;

  soundDecay := 0.0;
  systemDecay := 0.0;
  nObjects := 0.0;
  collLikely := 0.0;
  totalEnergy := 0.0;
  ratchet := 0.0;
  ratchetDelta := 0.0005;
  lastRatchetPos := 0;
  finalZ[0] := 0.0;
  finalZ[1] := 0.0;
  finalZ[2] := 0.0;
  finalZCoeffs[0] := 1.0;
  finalZCoeffs[1] := 0.0;
  finalZCoeffs[2] := 0.0;

  setupNum(instType);
end;

destructor TShakers.Destroy;
begin
  inherited Destroy;
end;

const
  MAX_SHAKE = 2000.0;

{
char instrs[NUM_INSTR][10]:=begin
  "Maraca", "Cabasa", "Sekere", "Guiro",
  "Waterdrp", "Bamboo", "Tambourn", "Sleighbl",
  "Stix1", "Crunch1", "Wrench", "SandPapr",
  "CokeCan", "NextMug", "PennyMug", "NicklMug",
  "DimeMug", "QuartMug", "FrancMug", "PesoMug",
  "BigRocks", "LitlRoks", "TBamboo"
end;;

int TShakers.setupName(char* instr)
begin
  int which:=0;

  for (int i=0;i<NUM_INSTR;i++)  begin
    if ( !strcmp(instr,instrs[i]) )
      which:=i;
  end;

#if defined(_STK_DEBUG_)
  cerr << "TShakers: Setting instrument to " << instrs[which] << endl;
#endif

  result:= setupNum(which);
end;

}

procedure TShakers.setFinalZs;
begin
  finalZCoeffs[0] := z0;
  finalZCoeffs[1] := z1;
  finalZCoeffs[2] := z2;
end;

procedure TShakers.setDecays;
begin
  soundDecay := sndDecay;
  systemDecay := sysDecay;
end;

function TShakers.setFreqAndReson(which: integer; freq, reson: my_float): integer;
begin
  if (which < MAX_FREQS) then
   begin
    resons[which] := reson;
    center_freqs[which] := freq;
    t_center_freqs[which] := freq;
    coeffs[which][1] := reson * reson;
    coeffs[which][0] := -reson * 2.0 * cos(freq * TWO_PI / srate);
    Result := 1;
   end
  else
    Result := 0;
end;

function TShakers.setupNum(inst: integer): integer;
var
  i, rv: integer;
  temp: my_float;
begin
  if (inst = 1) then
   begin // Cabasa
    rv := inst;
    nObjects := CABA_NUM_BEADS;
    defObjs[inst] := CABA_NUM_BEADS;
    setDecays(CABA_SOUND_DECAY, CABA_SYSTEM_DECAY);
    defDecays[inst] := CABA_SYSTEM_DECAY;
    decayScale[inst] := 0.97;
    nFreqs := 1;
    baseGain := CABA_GAIN;
    temp := log10(nObjects) * baseGain / nObjects;
    gains[0] := temp;
    freqalloc[0] := 0;
    setFreqAndReson(0, CABA_CENTER_FREQ, CABA_RESON);
    setFinalZs(1.0, -1.0, 0.0);
   end
  else if (inst = 2) then
   begin // Sekere
    rv := inst;
    nObjects := SEKE_NUM_BEANS;
    defObjs[inst] := SEKE_NUM_BEANS;
    setDecays(SEKE_SOUND_DECAY, SEKE_SYSTEM_DECAY);
    defDecays[inst] := SEKE_SYSTEM_DECAY;
    decayScale[inst] := 0.94;
    nFreqs := 1;
    baseGain := SEKE_GAIN;
    temp := log10(nObjects) * baseGain / nObjects;
    gains[0] := temp;
    freqalloc[0] := 0;
    setFreqAndReson(0, SEKE_CENTER_FREQ, SEKE_RESON);
    setFinalZs(1.0, 0.0, -1.0);
   end
  else if (inst = 3) then
   begin //  Guiro
    rv := inst;
    nObjects := GUIR_NUM_PARTS;
    defObjs[inst] := GUIR_NUM_PARTS;
    setDecays(GUIR_SOUND_DECAY, 1.0);
    defDecays[inst] := 0.9999;
    decayScale[inst] := 1.0;
    nFreqs := 2;
    baseGain := GUIR_GAIN;
    temp := log10(nObjects) * baseGain / nObjects;
    gains[0] := temp;
    gains[1] := temp;
    freqalloc[0] := 0;
    freqalloc[1] := 0;
    freq_rand[0] := 0.0;
    freq_rand[1] := 0.0;
    setFreqAndReson(0, GUIR_GOURD_FREQ, GUIR_GOURD_RESON);
    setFreqAndReson(1, GUIR_GOURD_FREQ2, GUIR_GOURD_RESON2);
    ratchet := 0;
    ratchetPos := 10;
   end
  else if (inst = 4) then
   begin //  Water Drops
    rv := inst;
    nObjects := WUTR_NUM_SOURCES;
    defObjs[inst] := WUTR_NUM_SOURCES;
    setDecays(WUTR_SOUND_DECAY, WUTR_SYSTEM_DECAY);
    defDecays[inst] := WUTR_SYSTEM_DECAY;
    decayScale[inst] := 0.8;
    nFreqs := 3;
    baseGain := WUTR_GAIN;
    temp := log10(nObjects) * baseGain / nObjects;
    gains[0] := temp;
    gains[1] := temp;
    gains[2] := temp;
    freqalloc[0] := 1;
    freqalloc[1] := 1;
    freqalloc[2] := 1;
    freq_rand[0] := 0.2;
    freq_rand[1] := 0.2;
    freq_rand[2] := 0.2;
    setFreqAndReson(0, WUTR_CENTER_FREQ0, WUTR_RESON);
    setFreqAndReson(1, WUTR_CENTER_FREQ0, WUTR_RESON);
    setFreqAndReson(2, WUTR_CENTER_FREQ0, WUTR_RESON);
    setFinalZs(1.0, 0.0, 0.0);
   end
  else if (inst = 5) then
   begin // Bamboo
    rv := inst;
    nObjects := BAMB_NUM_TUBES;
    defObjs[inst] := BAMB_NUM_TUBES;
    setDecays(BAMB_SOUND_DECAY, BAMB_SYSTEM_DECAY);
    defDecays[inst] := BAMB_SYSTEM_DECAY;
    decayScale[inst] := 0.7;
    nFreqs := 3;
    baseGain := BAMB_GAIN;
    temp := log10(nObjects) * baseGain / nObjects;
    gains[0] := temp;
    gains[1] := temp;
    gains[2] := temp;
    freqalloc[0] := 1;
    freqalloc[1] := 1;
    freqalloc[2] := 1;
    freq_rand[0] := 0.2;
    freq_rand[1] := 0.2;
    freq_rand[2] := 0.2;
    setFreqAndReson(0, BAMB_CENTER_FREQ0, BAMB_RESON);
    setFreqAndReson(1, BAMB_CENTER_FREQ1, BAMB_RESON);
    setFreqAndReson(2, BAMB_CENTER_FREQ2, BAMB_RESON);
    setFinalZs(1.0, 0.0, 0.0);
   end
  else if (inst = 6) then
   begin // Tambourine
    rv := inst;
    nObjects := TAMB_NUM_TIMBRELS;
    defObjs[inst] := TAMB_NUM_TIMBRELS;
    setDecays(TAMB_SOUND_DECAY, TAMB_SYSTEM_DECAY);
    defDecays[inst] := TAMB_SYSTEM_DECAY;
    decayScale[inst] := 0.95;
    nFreqs := 3;
    baseGain := TAMB_GAIN;
    temp := log10(nObjects) * baseGain / nObjects;
    gains[0] := temp * TAMB_SHELL_GAIN;
    gains[1] := temp * 0.8;
    gains[2] := temp;
    freqalloc[0] := 0;
    freqalloc[1] := 1;
    freqalloc[2] := 1;
    freq_rand[0] := 0.0;
    freq_rand[1] := 0.05;
    freq_rand[2] := 0.05;
    setFreqAndReson(0, TAMB_SHELL_FREQ, TAMB_SHELL_RESON);
    setFreqAndReson(1, TAMB_CYMB_FREQ1, TAMB_CYMB_RESON);
    setFreqAndReson(2, TAMB_CYMB_FREQ2, TAMB_CYMB_RESON);
    setFinalZs(1.0, 0.0, -1.0);
   end
  else if (inst = 7) then
   begin // Sleighbell
    rv := inst;
    nObjects := SLEI_NUM_BELLS;
    defObjs[inst] := SLEI_NUM_BELLS;
    setDecays(SLEI_SOUND_DECAY, SLEI_SYSTEM_DECAY);
    defDecays[inst] := SLEI_SYSTEM_DECAY;
    decayScale[inst] := 0.9;
    nFreqs := 5;
    baseGain := SLEI_GAIN;
    temp := log10(nObjects) * baseGain / nObjects;
    gains[0] := temp;
    gains[1] := temp;
    gains[2] := temp;
    gains[3] := temp * 0.5;
    gains[4] := temp * 0.3;
    for i := 0 to nFreqs - 1 do
     begin
      freqalloc[i] := 1;
      freq_rand[i] := 0.03;
     end;
    setFreqAndReson(0, SLEI_CYMB_FREQ0, SLEI_CYMB_RESON);
    setFreqAndReson(1, SLEI_CYMB_FREQ1, SLEI_CYMB_RESON);
    setFreqAndReson(2, SLEI_CYMB_FREQ2, SLEI_CYMB_RESON);
    setFreqAndReson(3, SLEI_CYMB_FREQ3, SLEI_CYMB_RESON);
    setFreqAndReson(4, SLEI_CYMB_FREQ4, SLEI_CYMB_RESON);
    setFinalZs(1.0, 0.0, -1.0);
   end
  else if (inst = 8) then
   begin // Stix1
    rv := inst;
    nObjects := STIX1_NUM_BEANS;
    defObjs[inst] := STIX1_NUM_BEANS;
    setDecays(STIX1_SOUND_DECAY, STIX1_SYSTEM_DECAY);
    defDecays[inst] := STIX1_SYSTEM_DECAY;

    decayScale[inst] := 0.96;
    nFreqs := 1;
    baseGain := STIX1_GAIN;
    temp := log10(nObjects) * baseGain / nObjects;
    gains[0] := temp;
    freqalloc[0] := 0;
    setFreqAndReson(0, STIX1_CENTER_FREQ, STIX1_RESON);
    setFinalZs(1.0, 0.0, -1.0);
   end
  else if (inst = 9) then
   begin // Crunch1
    rv := inst;
    nObjects := CRUNCH1_NUM_BEADS;
    defObjs[inst] := CRUNCH1_NUM_BEADS;
    setDecays(CRUNCH1_SOUND_DECAY, CRUNCH1_SYSTEM_DECAY);
    defDecays[inst] := CRUNCH1_SYSTEM_DECAY;
    decayScale[inst] := 0.96;
    nFreqs := 1;
    baseGain := CRUNCH1_GAIN;
    temp := log10(nObjects) * baseGain / nObjects;
    gains[0] := temp;
    freqalloc[0] := 0;
    setFreqAndReson(0, CRUNCH1_CENTER_FREQ, CRUNCH1_RESON);
    setFinalZs(1.0, -1.0, 0.0);
   end
  else if (inst = 10) then
   begin // Wrench
    rv := inst;
    nObjects := WRENCH_NUM_PARTS;
    defObjs[inst] := WRENCH_NUM_PARTS;
    setDecays(WRENCH_SOUND_DECAY, 1.0);
    defDecays[inst] := 0.9999;
    decayScale[inst] := 0.98;
    nFreqs := 2;
    baseGain := WRENCH_GAIN;
    temp := log10(nObjects) * baseGain / nObjects;
    gains[0] := temp;
    gains[1] := temp;
    freqalloc[0] := 0;
    freqalloc[1] := 0;
    freq_rand[0] := 0.0;
    freq_rand[1] := 0.0;
    setFreqAndReson(0, WRENCH_FREQ, WRENCH_RESON);
    setFreqAndReson(1, WRENCH_FREQ2, WRENCH_RESON2);
    ratchet := 0;
    ratchetPos := 10;
   end
  else if (inst = 11) then
   begin // Sandpapr
    rv := inst;
    nObjects := SANDPAPR_NUM_GRAINS;
    defObjs[inst] := SANDPAPR_NUM_GRAINS;
    setDecays(SANDPAPR_SOUND_DECAY, SANDPAPR_SYSTEM_DECAY);
    defDecays[inst] := SANDPAPR_SYSTEM_DECAY;
    decayScale[inst] := 0.97;
    nFreqs := 1;
    baseGain := SANDPAPR_GAIN;
    temp := log10(nObjects) * baseGain / nObjects;
    gains[0] := temp;
    freqalloc[0] := 0;
    setFreqAndReson(0, SANDPAPR_CENTER_FREQ, SANDPAPR_RESON);
    setFinalZs(1.0, 0.0, -1.0);
   end
  else if (inst = 12) then
   begin // Cokecan
    rv := inst;
    nObjects := COKECAN_NUM_PARTS;
    defObjs[inst] := COKECAN_NUM_PARTS;
    setDecays(COKECAN_SOUND_DECAY, COKECAN_SYSTEM_DECAY);
    defDecays[inst] := COKECAN_SYSTEM_DECAY;
    decayScale[inst] := 0.95;
    nFreqs := 5;
    baseGain := COKECAN_GAIN;
    temp := log10(nObjects) * baseGain / nObjects;
    gains[0] := temp;
    gains[1] := temp * 1.8;
    gains[2] := temp * 1.8;
    gains[3] := temp * 1.8;
    gains[4] := temp * 1.8;
    freqalloc[0] := 0;
    freqalloc[1] := 0;
    freqalloc[2] := 0;
    freqalloc[3] := 0;
    freqalloc[4] := 0;
    setFreqAndReson(0, COKECAN_HELMFREQ, COKECAN_HELM_RES);
    setFreqAndReson(1, COKECAN_METLFREQ0, COKECAN_METL_RES);
    setFreqAndReson(2, COKECAN_METLFREQ1, COKECAN_METL_RES);
    setFreqAndReson(3, COKECAN_METLFREQ2, COKECAN_METL_RES);
    setFreqAndReson(4, COKECAN_METLFREQ3, COKECAN_METL_RES);
    setFinalZs(1.0, 0.0, -1.0);
   end
  else if ((inst > 12) and (inst < 20)) then
   begin // Nextmug
    rv := inst;
    nObjects := NEXTMUG_NUM_PARTS;
    defObjs[inst] := NEXTMUG_NUM_PARTS;
    setDecays(NEXTMUG_SOUND_DECAY, NEXTMUG_SYSTEM_DECAY);
    defDecays[inst] := NEXTMUG_SYSTEM_DECAY;
    decayScale[inst] := 0.95;
    nFreqs := 4;
    baseGain := NEXTMUG_GAIN;
    temp := log10(nObjects) * baseGain / nObjects;
    gains[0] := temp;
    gains[1] := temp * 0.8;
    gains[2] := temp * 0.6;
    gains[3] := temp * 0.4;
    freqalloc[0] := 0;
    freqalloc[1] := 0;
    freqalloc[2] := 0;
    freqalloc[3] := 0;
    freqalloc[4] := 0;
    freqalloc[5] := 0;
    setFreqAndReson(0, NEXTMUG_FREQ0, NEXTMUG_RES);
    setFreqAndReson(1, NEXTMUG_FREQ1, NEXTMUG_RES);
    setFreqAndReson(2, NEXTMUG_FREQ2, NEXTMUG_RES);
    setFreqAndReson(3, NEXTMUG_FREQ3, NEXTMUG_RES);
    setFinalZs(1.0, 0.0, -1.0);

    if (inst = 14) then
     begin // Mug + Penny
      nFreqs := 7;
      gains[4] := temp;
      gains[5] := temp * 0.8;
      gains[6] := temp * 0.5;
      setFreqAndReson(4, PENNY_FREQ0, PENNY_RES);
      setFreqAndReson(5, PENNY_FREQ1, PENNY_RES);
      setFreqAndReson(6, PENNY_FREQ2, PENNY_RES);
     end
    else if (inst = 15) then
     begin // Mug + Nickel
      nFreqs := 6;
      gains[4] := temp;
      gains[5] := temp * 0.8;
      gains[6] := temp * 0.5;
      setFreqAndReson(4, NICKEL_FREQ0, NICKEL_RES);
      setFreqAndReson(5, NICKEL_FREQ1, NICKEL_RES);
      setFreqAndReson(6, NICKEL_FREQ2, NICKEL_RES);
     end
    else if (inst = 16) then
     begin // Mug + Dime
      nFreqs := 6;
      gains[4] := temp;
      gains[5] := temp * 0.8;
      gains[6] := temp * 0.5;
      setFreqAndReson(4, DIME_FREQ0, DIME_RES);
      setFreqAndReson(5, DIME_FREQ1, DIME_RES);
      setFreqAndReson(6, DIME_FREQ2, DIME_RES);
     end
    else if (inst = 17) then
     begin // Mug + Quarter
      nFreqs := 6;
      gains[4] := temp * 1.3;
      gains[5] := temp * 1.0;
      gains[6] := temp * 0.8;
      setFreqAndReson(4, QUARTER_FREQ0, QUARTER_RES);
      setFreqAndReson(5, QUARTER_FREQ1, QUARTER_RES);
      setFreqAndReson(6, QUARTER_FREQ2, QUARTER_RES);
     end
    else if (inst = 18) then
     begin // Mug + Franc
      nFreqs := 6;
      gains[4] := temp * 0.7;
      gains[5] := temp * 0.4;
      gains[6] := temp * 0.3;
      setFreqAndReson(4, FRANC_FREQ0, FRANC_RES);
      setFreqAndReson(5, FRANC_FREQ1, FRANC_RES);
      setFreqAndReson(6, FRANC_FREQ2, FRANC_RES);
     end
    else if (inst = 19) then
     begin // Mug + Peso
      nFreqs := 6;
      gains[4] := temp;
      gains[5] := temp * 1.2;
      gains[6] := temp * 0.7;
      setFreqAndReson(4, PESO_FREQ0, PESO_RES);
      setFreqAndReson(5, PESO_FREQ1, PESO_RES);
      setFreqAndReson(6, PESO_FREQ2, PESO_RES);
     end
   end
  else if (inst = 20) then
   begin // Big Rocks
    nFreqs := 1;
    rv := inst;
    nObjects := BIGROCKS_NUM_PARTS;
    defObjs[inst] := BIGROCKS_NUM_PARTS;
    setDecays(BIGROCKS_SOUND_DECAY, BIGROCKS_SYSTEM_DECAY);
    defDecays[inst] := BIGROCKS_SYSTEM_DECAY;
    decayScale[inst] := 0.95;
    baseGain := BIGROCKS_GAIN;
    temp := log10(nObjects) * baseGain / nObjects;
    gains[0] := temp;
    freqalloc[0] := 1;
    freq_rand[0] := 0.11;
    setFreqAndReson(0, BIGROCKS_FREQ, BIGROCKS_RES);
    setFinalZs(1.0, 0.0, -1.0);
   end
  else if (inst = 21) then
   begin // Little Rocks
    nFreqs := 1;
    rv := inst;
    nObjects := LITLROCKS_NUM_PARTS;
    defObjs[inst] := LITLROCKS_NUM_PARTS;
    setDecays(LITLROCKS_SOUND_DECAY, LITLROCKS_SYSTEM_DECAY);
    defDecays[inst] := LITLROCKS_SYSTEM_DECAY;
    decayScale[inst] := 0.95;
    baseGain := LITLROCKS_GAIN;
    temp := log10(nObjects) * baseGain / nObjects;
    gains[0] := temp;
    freqalloc[0] := 1;
    freq_rand[0] := 0.18;
    setFreqAndReson(0, LITLROCKS_FREQ, LITLROCKS_RES);
    setFinalZs(1.0, 0.0, -1.0);
   end
  else if (inst = 22) then
   begin // Tuned Bamboo
    rv := inst;
    nObjects := TBAMB_NUM_TUBES;
    defObjs[inst] := TBAMB_NUM_TUBES;
    setDecays(TBAMB_SOUND_DECAY, TBAMB_SYSTEM_DECAY);
    defDecays[inst] := TBAMB_SYSTEM_DECAY;
    decayScale[inst] := 0.7;
    nFreqs := 7;
    baseGain := TBAMB_GAIN;
    temp := log10(nObjects) * baseGain / nObjects;
    gains[0] := temp;
    gains[1] := temp;
    gains[2] := temp;
    gains[3] := temp;
    gains[4] := temp;
    gains[5] := temp;
    gains[6] := temp;
    freqalloc[0] := 0;
    freqalloc[1] := 0;
    freqalloc[2] := 0;
    freqalloc[3] := 0;
    freqalloc[4] := 0;
    freqalloc[5] := 0;
    freqalloc[6] := 0;
    freq_rand[0] := 0.0;
    freq_rand[1] := 0.0;
    freq_rand[2] := 0.0;
    freq_rand[3] := 0.0;
    freq_rand[4] := 0.0;
    freq_rand[5] := 0.0;
    freq_rand[6] := 0.0;
    setFreqAndReson(0, TBAMB_CENTER_FREQ0, TBAMB_RESON);
    setFreqAndReson(1, TBAMB_CENTER_FREQ1, TBAMB_RESON);
    setFreqAndReson(2, TBAMB_CENTER_FREQ2, TBAMB_RESON);
    setFreqAndReson(3, TBAMB_CENTER_FREQ3, TBAMB_RESON);
    setFreqAndReson(4, TBAMB_CENTER_FREQ4, TBAMB_RESON);
    setFreqAndReson(5, TBAMB_CENTER_FREQ5, TBAMB_RESON);
    setFreqAndReson(6, TBAMB_CENTER_FREQ6, TBAMB_RESON);
    setFinalZs(1.0, 0.0, -1.0);
   end
  else
   begin // Maraca (inst = 0) or default
    rv := 0;
    nObjects := MARA_NUM_BEANS;
    defObjs[0] := MARA_NUM_BEANS;
    setDecays(MARA_SOUND_DECAY, MARA_SYSTEM_DECAY);
    defDecays[0] := MARA_SYSTEM_DECAY;
    decayScale[inst] := 0.9;
    nFreqs := 1;
    baseGain := MARA_GAIN;
    temp := log10(nObjects) * baseGain / nObjects;
    gains[0] := temp;
    freqalloc[0] := 0;
    setFreqAndReson(0, MARA_CENTER_FREQ, MARA_RESON);
    setFinalZs(1.0, -1.0, 0.0);
   end;
  Result := rv;
end;

procedure TShakers.noteOn(frequency, amplitude: MY_FLOAT);
var
  notenum: integer;
begin
  // Yep ... pretty kludgey, but it works!
//  noteNum:=round((12*log10(frequency/220)/log10(2)) + 57.01) mod 32;
  notenum := round(frequency) mod 23;
  //if (instType <>  noteNum) then
  instType := setupNum(noteNum);
  shakeEnergy := shakeenergy + amplitude * MAX_SHAKE * 0.1;
  if (shakeEnergy > MAX_SHAKE) then
    shakeEnergy := MAX_SHAKE;
  if ((instType = 10) or (instType = 3)) then
    ratchetPos := ratchetpos + 1;
end;

procedure TShakers.noteOff;
begin
  shakeEnergy := 0.0;
  if ((instType = 10) or (instType = 3)) then
    ratchetPos := 0;
end;

const
  MIN_ENERGY = 0.3;

function TShakers.tick: MY_FLOAT;
var
  Data, temp_rand: my_float;
  i: integer;
begin
  if (instType = 4) then
   begin
    if (shakeEnergy > MIN_ENERGY) then
     begin
      lastOutput := wuter_tick;
      lastOutput := lastoutput * 0.0001;
     end
    else
     begin
      lastOutput := 0.0;
     end;
   end
  else if (instType = 22) then
    lastOutput := tbamb_tick
  else if ((instType = 10) or (instType = 3)) then
   begin
    if (ratchetPos > 0) then
     begin
      ratchet := ratchet - (ratchetDelta + (0.002 * totalEnergy));
      if (ratchet < 0.0) then
       begin
        ratchet := 1.0;
        ratchetPos := ratchetpos - 1;
       end;
      totalEnergy := ratchet;
      lastOutput := ratchet_tick;
      lastOutput := lastoutput * 0.0001;
     end
    else
      lastOutput := 0.0;
   end
  else
  if (shakeEnergy > MIN_ENERGY) then
   begin
    shakeEnergy := shakeenergy * systemDecay;
               // Exponential system decay
    if (float_random(1024.0) < nObjects) then
     begin
      sndLevel := sndlevel + shakeEnergy;
      for i := 0 to nFreqs - 1 do
        if (freqalloc[i] > 0) then
         begin
          temp_rand := t_center_freqs[i] * (1.0 + (freq_rand[i] * noise_tick));
          coeffs[i][0] := -resons[i] * 2.0 * cos(temp_rand * TWO_PI / srate);
         end;
     end;
    inputs[0] := sndLevel * noise_tick;      // Actual Sound is Random
    for i := 1 to nFreqs - 1 do
      inputs[i] := inputs[0];
    sndLevel := sndlevel * soundDecay;
                   // Exponential Sound decay 
    finalZ[2] := finalZ[1];
    finalZ[1] := finalZ[0];
    finalZ[0] := 0;
    for i := 0 to nFreqs - 1 do
     begin
      inputs[i] := inputs[i] - outputs[i][0] * coeffs[i][0];  // Do
      inputs[i] := inputs[i] - outputs[i][1] * coeffs[i][1];  // resonant
      outputs[i][1] := outputs[i][0];            // filter
      outputs[i][0] := inputs[i];                // calculations
      finalZ[0] := finalz[0] + gains[i] * outputs[i][1];
     end;
    Data := finalZCoeffs[0] * finalZ[0];     // Extra zero(s) for shape
    Data := Data + finalZCoeffs[1] * finalZ[1];    // Extra zero(s) for shape
    Data := Data + finalZCoeffs[2] * finalZ[2];    // Extra zero(s) for shape
    if (Data > 10000.0) then
      Data := 10000.0;
    if (Data < -10000.0) then
      Data := -10000.0;
    lastOutput := Data * 0.0001;
   end
  else
    lastOutput := 0.0//  MY_FLOAT generic_tick  begin
  ;

  Result := lastOutput;
end;

procedure TShakers.controlChange;
var
  temp, norm: my_float;
  i: integer;
begin
  norm := Value;// * ONE_OVER_128;
  if (norm < 0) then
    norm := 0.0
  else if (norm > 1.0) then
    norm := 1.0;

  if (number = __SK_Breath_) then
   begin // 2 ... energy
    shakeEnergy := shakeenergy + norm * MAX_SHAKE * 0.1;
    if (shakeEnergy > MAX_SHAKE) then
      shakeEnergy := MAX_SHAKE;
    if ((instType = 10) or (instType = 3)) then
     begin
      ratchetPos := round(abs(Value - lastRatchetPos));
      ratchetDelta := 0.0002 * ratchetPos;
      lastRatchetPos := round(Value);
     end;
   end
  else if (number = __SK_ModFrequency_) then
   begin // 4 ... decay
    if ((instType <> 3) and (instType <> 10)) then
     begin
      systemDecay := defDecays[instType] + ((Value - 64.0) *
        decayScale[instType] *
        (1.0 -
        defDecays[instType]) / 64.0);
      gains[0] := log10(nObjects) * baseGain / nObjects;
      for i := 1 to nFreqs - 1 do
        gains[i] := gains[0];
      if (instType = 6) then
       begin // tambourine
        gains[0] := gains[0] * TAMB_SHELL_GAIN;
        gains[1] := gains[1] * 0.8;
       end
      else if (instType = 7) then
       begin // sleighbell
        gains[3] := gains[3] * 0.5;
        gains[4] := gains[4] * 0.3;
       end
      else if (instType = 12) then
        for i := 1 to nFreqs - 1 do
          gains[i] := gains[i] * 1.8// cokecan
      ;
      for i := 0 to nFreqs - 1 do
        gains[i] := gains[i] * ((128 - Value) / 100.0 + 0.36);
     end;
   end
  else if (number = __SK_FootControl_) then
   begin // 11 ... number of objects
    if (instType = 5) then // bamboo
      nObjects := (Value * defObjs[instType] / 64.0) + 0.3
    else
      nObjects := (Value * defObjs[instType] / 64.0) + 1.1;
    gains[0] := log10(nObjects) * baseGain / nObjects;
    for i := 1 to nFreqs - 1 do
      gains[i] := gains[0];
    if (instType = 6) then
     begin // tambourine
      gains[0] := gains[0] * TAMB_SHELL_GAIN;
      gains[1] := gains[1] * 0.8;
     end
    else if (instType = 7) then
     begin // sleighbell
      gains[3] := gains[3] * 0.5;
      gains[4] := gains[4] * 0.3;
     end
    else if (instType = 12) then
      for i := 1 to nFreqs - 1 do
        gains[i] := gains[i] * 1.8// cokecan
    ;
    if ((instType <> 3) and (instType <> 10)) then
     begin
    // reverse calculate decay setting
      temp := (64.0 * (systemDecay - defDecays[instType]) /
        (decayScale[instType] * (1 - defDecays[instType])) + 64.0);
    // scale gains by decay setting
      for i := 0 to nFreqs - 1 do
        gains[i] := gains[i] * ((128 - temp) / 100.0 + 0.36);
     end;
   end
  else if (number = __SK_ModWheel_) then
   begin // 1 ... resonance frequency
    for i := 0 to nFreqs - 1 do
     begin
      if ((instType = 6) or (instType = 2) or (instType = 7)) then
 // limit range a bit for tambourine
        temp := center_freqs[i] * power(1.008, Value - 64)
      else
        temp := center_freqs[i] * power(1.015, Value - 64);
      t_center_freqs[i] := temp;

      coeffs[i][0] := -resons[i] * 2.0 * cos(temp * TWO_PI / srate);
      coeffs[i][1] := resons[i] * resons[i];
     end;
   end
  else if (number = __SK_AfterTouch_Cont_) then
   begin // 128
    shakeEnergy := shakeenergy + norm * MAX_SHAKE * 0.1;
    if (shakeEnergy > MAX_SHAKE) then
      shakeEnergy := MAX_SHAKE;
    if ((instType = 10) or (instType = 3)) then
     begin
      ratchetPos := round(abs(Value - lastRatchetPos));
      ratchetDelta := 0.0002 * ratchetPos;
      lastRatchetPos := round(Value);
     end;
   end
  else if (number = __SK_ShakerInst_) then
   begin // 1071
    instType := round(norm * 22);  //  Just to be safe
    setupNum(instType);
   end;
end;

// KLUDGE-O-MATIC-O-RAMA

function TShakers.wuter_tick: my_float;
var
  Data: my_float;
  j: integer;
begin
  shakeEnergy := shakeenergy * systemDecay;
               // Exponential system decay
  if (my_random(32767) < nObjects) then
   begin
    sndLevel := shakeEnergy;
    j := my_random(3);
    if (j = 0) then
     begin
      center_freqs[0] := WUTR_CENTER_FREQ1 * (0.75 + (0.25 * noise_tick));
      gains[0] := abs(noise_tick);
     end
    else if (j = 1) then
     begin
      center_freqs[1] := WUTR_CENTER_FREQ1 * (1.0 + (0.25 * noise_tick));
      gains[1] := abs(noise_tick);
     end
    else
     begin
      center_freqs[2] := WUTR_CENTER_FREQ1 * (1.25 + (0.25 * noise_tick));
      gains[2] := abs(noise_tick);
     end;
   end;

  gains[0] := gains[0] * resons[0];
  if (gains[0] > 0.001) then
   begin
    center_freqs[0] := center_freqs[0] * WUTR_FREQ_SWEEP;
    coeffs[0][0] := -resons[0] * 2.0 * cos(center_freqs[0] *
      TWO_PI / srate);
   end;
  gains[1] := gains[1] * resons[1];
  if (gains[1] > 0.001) then
   begin
    center_freqs[1] := center_freqs[1] * WUTR_FREQ_SWEEP;
    coeffs[1][0] := -resons[1] * 2.0 * cos(center_freqs[1] *
      TWO_PI / srate);
   end;
  gains[2] := gains[2] * resons[2];
  if (gains[2] > 0.001) then
   begin
    center_freqs[2] := center_freqs[2] * WUTR_FREQ_SWEEP;
    coeffs[2][0] := -resons[2] * 2.0 * cos(center_freqs[2] *
      TWO_PI / srate);
   end;

  sndLevel := sndlevel * soundDecay;        // Each (all) event(s) 
                                 // decay(s) exponentially 
  inputs[0] := sndLevel;
  inputs[0] := inputs[0] * noise_tick;     // Actual Sound is Random
  inputs[1] := inputs[0] * gains[1];
  inputs[2] := inputs[0] * gains[2];
  inputs[0] := inputs[0] * gains[0];
  inputs[0] := inputs[0] - outputs[0][0] * coeffs[0][0];
  inputs[0] := inputs[0] - outputs[0][1] * coeffs[0][1];
  outputs[0][1] := outputs[0][0];
  outputs[0][0] := inputs[0];
  Data := gains[0] * outputs[0][0];
  inputs[1] := inputs[1] - outputs[1][0] * coeffs[1][0];
  inputs[1] := inputs[1] - outputs[1][1] * coeffs[1][1];
  outputs[1][1] := outputs[1][0];
  outputs[1][0] := inputs[1];
  Data := Data + gains[1] * outputs[1][0];
  inputs[2] := inputs[2] - outputs[2][0] * coeffs[2][0];
  inputs[2] := inputs[2] - outputs[2][1] * coeffs[2][1];
  outputs[2][1] := outputs[2][0];
  outputs[2][0] := inputs[2];
  Data := Data + gains[2] * outputs[2][0];

  finalZ[2] := finalZ[1];
  finalZ[1] := finalZ[0];
  finalZ[0] := Data * 4;

  Data := finalZ[2] - finalZ[0];
  Result := Data;
end;

function TShakers.ratchet_tick: my_float;
var
  Data: my_float;
begin
  if (my_random(1024) < nObjects) then
    sndLevel := sndlevel + 512 * ratchet * totalEnergy;
  inputs[0] := sndLevel;
  inputs[0] := inputs[0] * noise_tick * ratchet;
  sndLevel := sndlevel * soundDecay;

  inputs[1] := inputs[0];
  inputs[0] := inputs[0] - outputs[0][0] * coeffs[0][0];
  inputs[0] := inputs[0] - outputs[0][1] * coeffs[0][1];
  outputs[0][1] := outputs[0][0];
  outputs[0][0] := inputs[0];
  inputs[1] := inputs[1] - outputs[1][0] * coeffs[1][0];
  inputs[1] := inputs[1] - outputs[1][1] * coeffs[1][1];
  outputs[1][1] := outputs[1][0];
  outputs[1][0] := inputs[1];

  finalZ[2] := finalZ[1];
  finalZ[1] := finalZ[0];
  finalZ[0] := gains[0] * outputs[0][1] + gains[1] * outputs[1][1];
  Data := finalZ[0] - finalZ[2];
  Result := Data;
end;

function TShakers.tbamb_tick: my_float;
var
  Data, temp: my_float;
  which, i: integer;
begin
  if (shakeEnergy > MIN_ENERGY) then
   begin
    shakeEnergy := shakeenergy * systemDecay;    // Exponential system decay
    if (float_random(1024.0) < nObjects) then
     begin
      sndLevel := sndlevel + shakeEnergy;
      which := my_random(7);
     end;
    temp := sndLevel * noise_tick;      // Actual Sound is Random
    for i := 0 to nFreqs - 1 do
      inputs[i] := 0;
    inputs[which mod 7] := temp;
    sndLevel := sndlevel * soundDecay;
                   // Exponential Sound decay
    finalZ[2] := finalZ[1];
    finalZ[1] := finalZ[0];
    finalZ[0] := 0;
    for i := 0 to nFreqs - 1 do
     begin
      inputs[i] := inputs[i] - outputs[i][0] * coeffs[i][0];  // Do
      inputs[i] := inputs[i] - outputs[i][1] * coeffs[i][1];  // resonant
      outputs[i][1] := outputs[i][0];            // filter
      outputs[i][0] := inputs[i];                // calculations
      finalZ[0] := finalz[0] + gains[i] * outputs[i][1];
     end;
    Data := finalZCoeffs[0] * finalZ[0];     // Extra zero(s) for shape
    Data := Data + finalZCoeffs[1] * finalZ[1];    // Extra zero(s) for shape
    Data := Data + finalZCoeffs[2] * finalZ[2];    // Extra zero(s) for shape
    if (Data > 10000.0) then
      Data := 10000.0
    else
    if (Data < -10000.0) then
      Data := -10000.0;
    Data := Data * 0.0001;
   end
  else
    Data := 0.0;
  Result := Data;
end;

end.

