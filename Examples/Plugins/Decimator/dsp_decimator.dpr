library dsp_decimator;

uses
  FastMM4,
  FastMove,
  FastCode,
  WADSPVST in 'WADSPVST.pas' {FmWinAmpVST},
  DecimatorModule in 'DecimatorModule.pas' {VSTDecimator: TVSTModule},
  DecimatorGUI in 'DecimatorGUI.pas' {VSTGUI};

exports winampDSPGetHeader2;

end.

