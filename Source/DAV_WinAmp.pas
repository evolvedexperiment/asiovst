unit DAV_WinAmp;

interface

uses
  Windows, Classes, SysUtils, DAV_Common;

type
  TSmallIntArray = array [0..40000] of Smallint;
  PSmallIntArray = ^TSmallIntArray;
  TShortIntArray = array [0..40000] of ShortInt;
  PShortIntArray = ^TShortIntArray;
  T3Bytes = array [0..2] of Byte;
  P3Bytes = ^T3Bytes;
  T3ByteArray = array [0..40000] of T3Bytes;
  P3ByteArray = ^T3ByteArray;
  PWinampDSPModule = ^TWinampDSPModule;
  PWinAmpDSPHeader = ^TWinAmpDSPheader;

  TWAGetHeader = function: PWinAmpDSPHeader; cdecl;
  TWAGetModule = function(const Which : Integer): PWinAmpDSPModule; cdecl;
  TWAConfig = procedure(const Handle : PWinAmpDSPModule); cdecl;
  TWAInit = function(const Handle: PWinAmpDSPModule): Integer; cdecl;
  TWAQuit = procedure(const Handle: PWinAmpDSPModule); cdecl;
  TWAModifySamples = function(const This_Mod: PWinAmpDSPModule;
    const Samples: Pointer; const SamplesFrame, BitPerSample, ChannelCount,
    SampleRate: Integer): Integer; cdecl;

  TWinampDSPModule = record
                      Description   : PChar;
                      HwndParent    : Hwnd;
                      HDLLinstance  : Hinst;
                      Config        : TWAConfig;
                      Init          : TWAInit;
                      ModifySamples : TWAModifySamples;
                      Quit          : TWAQuit;
                      UserData      : Pointer;
                     end;

  TWinAmpDSPHeader = record
                      Version      : Integer;
                      Description  : PChar;
                      GetModule    : TWAGetModule;
                      Key          : Integer;
                     end;

  TWinAmpConvert = procedure (const Data: Pointer; const ChannelCount, SampleFrames: Integer) of object;


implementation

end.
