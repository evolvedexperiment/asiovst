library dsp_decimator;

{$I DAV_Compiler.inc}

uses
  FastMM4,  // either download the library or comment if there is an error here
  FastMove, // either download the library or comment if there is an error here
  WADSPVST in 'WADSPVST.pas' {FmWinAmpVST},
  DecimatorModule in 'DecimatorModule.pas' {VSTDecimator: TVSTModule},
  DecimatorGUI in 'DecimatorGUI.pas' {VSTGUI};

exports winampDSPGetHeader2;

end.

