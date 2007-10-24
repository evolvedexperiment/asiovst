Tobybear & Christian Budde present:
DVSTHost - a Delphi unit for hosting VST plugins

Info:
-----
This is a component to load a VST plugin and make its
properties and methods accessible, ie. show the interface,
fill the buffers and call the processing routines.

It is still at a very basic stage, so feel free to add
anything, but please tell me about it!

All host and plugin loading code was written by
Christan W. Budde (http://www.savioursofsoul.de/Christian)

Based on the VST plugin template by:
Tobias Fleischer (http://www.tobybear.de)

Delphi VST SDK translation by:
Frederic Vanmol (http://www.axiworld.be)

VST is a trademark of:
Steinberg Media GmbH (http://www.steinberg.net)


Installation:
-------------

1. If you already have my latest Delphi VST template 
installed (available on my site http://www.tobybear.de),
then you should already have the common VST SDK units
in your path. If not, either download the template or
unzip the included CommonVST.zip file to a folder that
is accessible from the Delphi library path.

2. Double-click on VSTHost.dpk (or open it manually
in Delphi), then click on "compile", then on "install".
After installation, a message should pop up notifying
you that a new component has been added to the palette.

3. You should now open the included demo project and
see if it works. Note that the demo currently only
loads the plugin, displays the interface and sends
randomly filled buffers to the plugin, it does
*NOT* output any sound!


Comments, suggestions and extensions are *very* welcome!
Feel free to extend this component in any way that suits
you, and if you think you have made something cool, please
share it with other people and tell me about it!

Regards,

Tobias Fleischer
alias
Tobybear

web: www.tobybear.de
mail: tobybear@web.de



