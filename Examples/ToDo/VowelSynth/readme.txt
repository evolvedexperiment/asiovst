Hello and welcome to Tobybear VowelSynth/VowelFilter!

This is a simple VST plugin (PC only) by Tobybear (www.tobybear.de) 
using the formant filter algorithm from Alex (alex@smartelectronix.com),
hence it is called another "Tobybear VS SmartElectronix" open source
plugin thing. The full Delphi source code is included as this is mainly
just a simple example to show how you could implement some DSP related
things in Delphi. This plugin is available on Bram's truly excellent DSP source 
code page www.musicdsp.org

1. What does it do?
VowelSynth is a VSTi (VST instrument) that reacts to MIDI input by
generating a (non-bandlimited) waveform (saw, square or noise) of
the appropriate frequency and filters this waveform then with Alex'
formant filter. Simple envelope and glide control is also included.
VowelFilter is the effect version of this plugin which processes the
audio input with the formant filter. If you have a host that can send
MIDI notes to effect plugins, you can also use the effect plugin 
just like the regular synth: as soon as a MIDI note is received,
the appropriate waveform will be generated.

2. What can I do with the source code?
Look through it, learn from it, enhance it, add new features, but always
give proper credits to both of us! I say it again: this plugin and its
source code are mainly meant as a learning resource!

3. What do I need to compile this plugin?
A Delphi version, naturally :-), 3.0 or above should work. I coded it 
with the Delphi 6 Personal Edition that is available for free at 
www.borland.com (150MB though!). This is the whole compiler/editor/IDE 
with no restrictions, but just for personal use, you are not allowed to 
sell any programs created with it.
[UPDATE: After the release of the brandnew Delphi 7, the Delphi 6 PE
version went offline. There is also a new free version of Delphi 7, but it is
distributed on magazine cover disks only it seems]

4. Final words
I am not responsible if anything goes wrong with this plugin, you 
use it at your own risk. Of course I tried to prevent any possible harm, 
but you never know what might happen...

Cheers

Tobybear (tobybear@web.de)

www.tobybear.de
www.smartelectronix.com
www.musicdsp.org
 