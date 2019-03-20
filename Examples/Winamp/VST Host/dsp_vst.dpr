library dsp_vst;

{.$R 'EmbeddedVSTPlugin.res' 'EmbeddedVSTPlugin.rc'}

{$I DAV_Compiler.inc}

uses
  FastMM4,
  {$IFDEF UseFastMove}
  FastMove, // either download the library or disable the feature
  {$ENDIF}
  WinAmpDspVst in 'WinAmpDspVst.pas',
  WinAmpDspVstGui in 'WinAmpDspVstGui.pas' {FmWinAmpVST};

exports
  winampDSPGetHeader2 name 'winampDSPGetHeader2',
  winampDSPGetHeader2;

end.
