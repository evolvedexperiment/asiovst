// define *one* of the following to compile that plugin:
{DEFINE Compressor} // not working in VST!
{DEFINE Chorus}
{DEFINE Flanger}
{$DEFINE Distortion}

const
 kNumParams    = 7;            // number of parameters
 kNumPrograms  = 16;           // number of programs per fxb bank

{$IFDEF Compressor}
 PlugID           = 4;
 kEffectName   = 'DX9DMOCompressor';
{$ENDIF}

{$IFDEF Chorus}
 PlugID           = 3;
 kEffectName   = 'DX9DMOChorus';
{$ENDIF}

{$IFDEF Flanger}
 PlugID           = 2;
 kEffectName   = 'DX9DMOFlanger';
{$ENDIF}

{$IFDEF Distortion}
 PlugID           = 1;
 kEffectName   = 'DX9DMODistortion';
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
