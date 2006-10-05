// define *one* of the following to compile that plugin:
{$DEFINE EQ}
{DEFINE WAVESREVERB}
{DEFINE Gargle}
{DEFINE Echo}

const
 kNumParams    = 7;            // number of parameters
 kNumPrograms  = 16;           // number of programs per fxb bank

{$IFDEF Gargle}
 PlugID           = 5;
 kEffectName   = 'DX9DMOGargle';
{$ENDIF}

{$IFDEF Echo}
 PlugID           = 6;
 kEffectName   = 'DX9DMOEcho';
{$ENDIF}

{$IFDEF WavesReverb}
 PlugID           = 7;
 kEffectName   = 'DX9DMOWavesReverb';
{$ENDIF}

{$IFDEF EQ}
 PlugID           = 8;
 kEffectName   = 'DX9DMOParamEQ';
{$ENDIF}

 kVendor       = 'Tobybear'; // vendor name
 kIsSynth      = false;        // false = audio effect, true = synth

 kHasMidiIn    = true;         // plugin can receive MIDI
 kHasMidiOut   = false;         // plugin can output MIDI
 kSyncToHost   = not true;         // plugin can sync to host tempo


 // you should normally not need to change the options below:
 kCanMono      = true;         // true if can be fed with mono signals
 kCanReplacing = true;         // true if implements replacing algorithm
 kDenorm       = 1.0e-25;      // denormalisation fix, especially for P4
