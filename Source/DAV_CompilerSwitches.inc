////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  Delphi ASIO & VST Package - Compiler Switches                             //
//  ---------------------------------------------                             //
//                                                                            //
//  With this include file you can finetune some settings. Not all of them    //
//  are documented in full length, but at least a hint is given here as well. //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

{ Enable this option, in order to use the native types for arrays as defined
  in the types.pas unit. Advantages are native assigns to dynamic arrays.
  However, if you experience random IDE crashes this might be the reason. To
  solve those conflicts you can either disable this option or uninstall the
  packages, delete all *.dcu and *.dcp files and reinstall the packages.
  Since the types.pas is not available for older Delphi versions (Delphi 5),
  this option is disabled by default for those IDEs.
  The default is enabled. }
{$DEFINE UseNativeTypes}

////////////////////////////////////
//  VST Plugin Compiler Switches  //
////////////////////////////////////

{ By default, a VST plugin is wrapped around a descendant of a TDataModule. It
  is responsible to handle the properties easily. If you prefer the old style
  of assigning property values by code, you may want to disable this option.
  In the examples you should find some examples that needs to have this option
  disabled.
  The default is enabled.}
{$DEFINE UseDelphi}

{ This switch was introduces by "the-real-myco". As far as I remember it
  handles automatic conversion of VST process calls to dynamic arrays. It slows
  down processing, but the handling is easier.
  The default is enabled.}
{$DEFINE CONVERT_TO_DYNARRAY}

{ Enable this option to create a special "debug.log". While it is not useful
  to be enabled for productional use, it offers some debug possibilities for
  hosts that do not allow any debuggers. The "debug.log" contains information
  about the dispatcher and some internal states.
  The default is disabled.}
{.$DEFINE Debug}


//////////////////////////////////
//  VST Host Compiler Switches  //
//////////////////////////////////

{ Enable this option if the VST host unit should contain code to display any
  GUI for the plugin. If only audio processing is favoured, the application
  size can be shrinked by disabling this switch
  The default is enabled.}
{$DEFINE VstHostGUI}

{ Enable this option if the VST host should use FlatScrollBars instead of the
  3-D like scrollbars. It is disabled by default, because it hasn't been tested
  in detail.
  The default is disabled.}
{.$DEFINE FlatSrcollBar}

{ To distinguish VST plugins and hosts, in the VST Host dispatcher, pointers
  are stored in the effect record. Unfortunately these information are not
  available for some dispatcher calls (especially those before instantiation).
  If this switch is disabled (default) only generic informations are returned
  by the host (e.g. the VendorString (audioMasterGetVendorString) is hard coded
  to "Delphi ASIO & VST Project". This needs to be completely adjustable enable
  this compiler switch. A known issues with this switch is the fact that in a
  multithread environment with several TVSTHost components containing several
  TVstPlugin items, the search might fail and point to another object instead.
  It probably won't crash, but the returned information is wrong.
  The default is disabled.}
{.$DEFINE SearchPluginAndHost}


///////////////////////////////////
//  ASIO Host Compiler Switches  //
///////////////////////////////////

{ Since the interface to ASIO can't be handled natively by pascal/delphi a
  special wrapper for the calling convention is mandatory. This can either be
  the build-in wrapper by BeRo or via the OpenASIO.DLL by Martin Fay. If you
  prefer the second option you need to enable this switch (needs OpenASIO.dll).
  The default is disabled.}
{.$DEFINE OpenASIO}

{ In the very early days a dedicated ASIO mixer was included with this
  component. It has not been used since ages, but if you long for testing it,
  you can enable this switch and experiment with the code. Not recommended!
  The default is disabled.}
{.$DEFINE ASIOMixer}

{ In the very early days it was possible to call the control panel from the
  application (right click -> "control panel"). This was handy in case some
  settings needed to be tweaked from within the IDE. The current state is
  unknown, but it might work (only Delphi 5 though).}
{.$DEFINE D5CP}
