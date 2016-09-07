library ASIOVI;

uses
  FastMM4,
  Registry in 'Registry.pas',
  DAV_AsioVi in 'DAV_AsioVi.pas',
  DAV_LabView in 'DAV_LabView.pas';

exports
  ASIOInitialize, ASIOTerminate, ASIOControlPanel, ASIOGetNumDevices,
  ASIOInitDriver, ASIOInitDriverIndex, ASIOSetDriverIndex, ASIOGetDriverName,
  ASIOGetDriverNames, ASIOCanSampleRate, ASIOSetSampleRate, ASIODriverStart,
  ASIODriverStop, ASIOGetBufferSize, ASIOGetChannels, ASIOOutputType,
  ASIOGetOutputLevel, ASIOGetInputLevel, ASIOSetOutputVolume,
  ASIOSetOutputVolumedB, ASIOSetSineFrequency, ASIOReadWriteSize,
  ASIOReadWriteSizeFixed, ASIOReadWrite, ASIOReadWriteX, ASIOAutopilot,
  ASIOSetExtraBufferSize, ASIOBufferUnderrun, ASIOResetBufferUnderruns,
  ASIOGetLoopCounts, ASIOSetLoopCounts, ASIOSetClipFunction, ASIOCalcMeters,
  ASIORegisterCallback, ASIOUnegisterCallback, ASIOAbortCallback,
  ASIOSetUserEventRef;

begin
end.
