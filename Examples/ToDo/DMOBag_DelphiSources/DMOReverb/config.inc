const
 kNumParams    = 16;            // number of parameters
 kNumPrograms  = 16;           // number of programs per fxb bank

 kPlugID       = 0;
 kEffectName   = 'DX9DMOI3DL2Reverb';  // effect name

 kVendor       = 'Tobybear'; // vendor name
 kIsSynth      = false;        // false = audio effect, true = synth

 kHasMidiIn    = true;         // plugin can receive MIDI
 kHasMidiOut   = false;         // plugin can output MIDI
 kSyncToHost   = not true;         // plugin can sync to host tempo

 kpreset=999;

 // you should normally not need to change the options below:
 kCanMono      = true;         // true if can be fed with mono signals
 kCanReplacing = true;         // true if implements replacing algorithm
 kDenorm       = 1.0e-25;      // denormalisation fix, especially for P4
