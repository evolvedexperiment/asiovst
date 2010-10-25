unit DAV_VSTBasicModule;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  Version: MPL 1.1 or LGPL 2.1 with linking exception                       //
//                                                                            //
//  The contents of this file are subject to the Mozilla Public License       //
//  Version 1.1 (the "License"); you may not use this file except in          //
//  compliance with the License. You may obtain a copy of the License at      //
//  http://www.mozilla.org/MPL/                                               //
//                                                                            //
//  Software distributed under the License is distributed on an "AS IS"       //
//  basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the   //
//  License for the specific language governing rights and limitations under  //
//  the License.                                                              //
//                                                                            //
//  Alternatively, the contents of this file may be used under the terms of   //
//  the Free Pascal modified version of the GNU Lesser General Public         //
//  License Version 2.1 (the "FPC modified LGPL License"), in which case the  //
//  provisions of this license are applicable instead of those above.         //
//  Please see the file LICENSE.txt for additional information concerning     //
//  this license.                                                             //
//                                                                            //
//  The code is part of the Delphi ASIO & VST Project                         //
//                                                                            //
//  The initial developer of this code is Christian-W. Budde, additional      //
//  coding and refactoring done by Maik Menz                                  //
//                                                                            //
//  Portions created by Christian-W. Budde are Copyright (C) 2003-2010        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////
//                                                                            //
// This unit implements the basic VST-Plugin <--> Host communications
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I ..\DAV_Compiler.inc}

uses
  Classes, Forms, Sysutils, DAV_Types, DAV_VSTEffect, DAV_WinAmp;

type
  TBasicVSTModuleClass = class of TBasicVSTModule;

  { TBasicVSTModule }

  TBasicVSTModule = class({$IFDEF UseDelphi}TDataModule{$ELSE}TComponent{$ENDIF})
  private
    procedure ConvertDummy(const Data: Pointer; const ChannelCount, SampleFrames: Integer);
    procedure ConvertFloatToInterleaved(const Data: Pointer; const ChannelCount, SampleFrames: Integer);
    procedure ConvertFloatToInterleaved16bit(const Data: Pointer; const ChannelCount, SampleFrames: Integer);
    procedure ConvertFloatToInterleaved24bit(const Data: Pointer; const ChannelCount, SampleFrames: Integer);
    procedure ConvertFloatToInterleaved8bit(const Data: Pointer; const ChannelCount, SampleFrames: Integer);
    procedure ConvertInterleaved16bitToFloat(const Data: Pointer; const ChannelCount, SampleFrames: Integer);
    procedure ConvertInterleaved24bitToFloat(const Data: Pointer; const ChannelCount, SampleFrames: Integer);
    procedure ConvertInterleaved8bitToFloat(const Data: Pointer; const ChannelCount, SampleFrames: Integer);
    procedure ConvertInterleavedToFloat(const Data: Pointer; const ChannelCount, SampleFrames: Integer);
  protected
    FEffect             : TVSTEffect;
    FAudioMaster        : TAudioMasterCallbackFunc;

    {$IFNDEF UseAudioEffectPtr}
    FUseAudioEffectPtr  : Boolean;
    {$ENDIF}

    {$IFNDEF UseDelphi}
    FOnCreate           : TNotifyEvent;
    FOnDestroy          : TNotifyEvent;
    {$ENDIF}

    // WinAmp
    FWinAmpDSPModule    : PWinampDSPModule;
    FWinAmpEditorForm   : TForm;
    FWinAmpBypass       : Boolean;
    FWinAmpInputBuffer  : array of PDAVSingleFixedArray;
    FWinAmpOutputBuffer : array of PDAVSingleFixedArray;
    FWinAmpNrChannels   : Integer;
    FWinAmpSampleRate   : Integer;
    FWinAmpSampleFrames : Integer;

    FWinAmpConvertIn    : TWinAmpConvert;
    FWinAmpConvertOut   : TWinAmpConvert;

    {$IFDEF DebugLog}
    FLog                    : TStringList;
    FTmStmp                 : TDateTime;
    procedure AddLogMessage(const Text: string);
    {$ENDIF}

    function GetEffect: PVSTEffect; virtual;

    procedure SetAudioMaster(const AM: TAudioMasterCallbackFunc); virtual;
    function CallAudioMaster(const Opcode: TAudioMasterOpcode;
      const Index: Integer = 0; const Value: TVstIntPtr = 0;
      const Ptr: Pointer = nil; const Opt: Single = 0): TVstIntPtr; virtual;

    procedure WinAmpConfig; virtual;
    procedure WinAmpQuit; virtual;
    function WinAmpModifySamples(const Samples: Pointer; const SampleFrames,
      BitPerSample, ChannelCount, SampleRate: Integer): Integer; virtual;

    function  GetMasterVersion: Integer; virtual;
    function  GetCurrentUniqueID: TChunkName; virtual;
    procedure MasterIdle; virtual;
    function  IsInputConnected(const Input: Integer): Boolean; virtual;
    function  IsOutputConnected(const Output: Integer): Boolean; virtual;

    procedure WantEvents(const Filter: Integer);  // filter is currently ignored, midi channel data only (default) virtual void wantEvents (long filter = 1); default is 1 for this!
    function  GetTimeInfo(const Filter: Integer): PVstTimeInfo; virtual;  // returns const VstTimeInfo* (or 0 if not supported) filter should contain a mask indicating which fields are requested (see valid masks in aeffectx.h), as some items may require extensive conversions
    procedure SetTimeInfo(const Filter: Integer; var VstTimeInfo: PVstTimeInfo); virtual;
    function  TempoAt(const pos: Integer): Integer; virtual; // returns tempo (in bpm * 10000) at sample frame location <pos>
    function  SendVstEventsToHost(var Events: TVstEvents): Boolean;  // True: success

    function  GetNumAutomatableParameters: Integer; virtual;
    procedure SetParameterAutomated(const Index: Integer; const Value: Single); virtual;
    function  GetParameterQuantization: Integer; virtual; // returns the Integer Value for +1.0 representation, or 1 if full single float precision is maintained in automation. parameter Index in <Value> (-1: all, any)

    function  GetInputLatency: Integer; virtual;
    function  GetOutputLatency: Integer; virtual;
    function  GetPreviousPlug(const Input: Integer): PVSTEffect; virtual;  // input can be -1 in which case the first found is returned
    function  GetNextPlug(const Output: Integer): PVSTEffect; virtual;     // output can be -1 in which case the first found is returned

    function  WillProcessReplacing: Integer; virtual; // returns 0: not implemented, 1: replacing, 2: accumulating
    function  GetCurrentProcessLevel: Integer; virtual;  // returns: 0: not supported, 1: currently in user thread (gui) 2: currently in audio thread or irq (where Process is called) 3: currently in 'sequencer' thread or irq (midi, timer etc) 4: currently offline Processing and thus in user thread other: not defined, but probably pre-empting user thread.
    function  GetAutomationState: Integer; virtual;  // returns 0: not supported, 1: off, 2:read, 3:write, 4:read/write

    function  OfflineRead(var Offline: TVstOfflineTaskRecord; const Option: TVstOfflineOption; const ReadSource: Boolean): Boolean; virtual;
    function  OfflineWrite(var Offline: TVstOfflineTaskRecord; const Option: TVstOfflineOption): Boolean; virtual;
    function  OfflineStart(var AudioFile: TVstAudioFile; const numAudioFiles, numNewAudioFiles: Integer): Boolean; virtual;
    function  OfflineGetCurrentPass: Integer; virtual;
    function  OfflineGetCurrentMetaPass: Integer; virtual;

    procedure SetOutputSampleRate(const Samplerate: Single); virtual;

    function  GetHostVendorString(const Text: PAnsiChar): Boolean; virtual;  // fills <Text> with a string identifying the vendor (max 64 char)
    function  GetHostProductString(const Text: PAnsiChar): Boolean; virtual; // fills <Text> with a string with product name (max 64 char)
    function  GetHostVendorVersion: Integer; virtual;  // returns vendor-specific version
    function  HostVendorSpecific(const Arg1, Arg2: Integer; const ptrArg: Pointer; const floatArg: Single): Integer; virtual;  // no definition
    function  GetCanHostDo(Text: string): Integer; virtual;  // see 'hostCanDos' in audioeffectx.cpp returns 0: don't know (default), 1: yes, -1: no
    function  GetHostLanguage: Integer; virtual;   // returns VstHostLanguage
    function  OpenWindow(const aWindow: PVstWindow): Pointer; virtual;  // create new window
    function  CloseWindow(const aWindow: PVstWindow): Boolean; virtual; // close a newly created window
    function  GetDirectory: Pointer; virtual;  // get the plug's directory, FSSpec on mac, else char*

    function  UpdateDisplay: Boolean; virtual; // something has changed, update 'multi-fx' display returns True if supported
    function  IOChanged: Boolean; virtual;   // tell host numInputs and/or numOutputs and/or numParameters has changed
    function  NeedIdle: Boolean; virtual;    // plug needs idle calls (outside its editor window)
    function  SizeWindow(const Width, Height: Integer): Boolean; virtual;

    function  BeginEdit(const Index: Integer): Boolean; virtual; // to be called before a setParameterAutomated with mouse move (one per Mouse Down)
    function  EndEdit(const Index: Integer): Boolean; virtual;   // to be called after a setParameterAutomated (on Mouse Up)

    function  OpenFileSelector(var VstFileSelect: TVstFileSelect): Boolean; virtual;
    function  CloseFileSelector(var VstFileSelect: TVstFileSelect): Boolean;
    function  GetChunkFile(const NativePath: Pointer): Boolean;

    // HostCalls, protected methods that can be overwritten, but shall remain
    // hidden, since the user should not be able to call them directly!
    function HostCallOpen                      (const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallClose                     (const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallSetProgram                (const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallGetProgram                (const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallSetProgramName            (const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallGetProgramName            (const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallGetParamLabel             (const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallGetParamDisplay           (const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallGetParamName              (const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallGetVu                     (const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallSetSampleRate             (const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallSetBlockSize              (const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallMainsChanged              (const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallEditGetRect               (const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallEditOpen                  (const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallEditClose                 (const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallEditDraw                  (const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallEditMouse                 (const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallEditKey                   (const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallEditIdle                  (const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallEditTop                   (const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallEditSleep                 (const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallIdentify                  (const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallGetChunk                  (const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallSetChunk                  (const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallProcessEvents             (const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallCanBeAutomated            (const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallString2Parameter          (const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallGetNumProgramCategories   (const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallGetProgramNameIndexed     (const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallCopyProgram               (const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallConnectInput              (const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallConnectOutput             (const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallGetInputProperties        (const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallGetOutputProperties       (const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallGetPlugCategory           (const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallGetCurrentPosition        (const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallGetDestinationBuffer      (const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallOfflineNotify             (const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallOfflinePrepare            (const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallOfflineRun                (const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallProcessVarIo              (const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallSetSpeakerArrangement     (const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallSetBlockSizeAndSampleRate (const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallSetBypass                 (const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallGetEffectName             (const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallGetErrorText              (const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallGetVendorString           (const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallGetProductString          (const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallGetVendorVersion          (const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallVendorSpecific            (const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallCanDo                     (const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallGetTailSize               (const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallIdle                      (const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallGetIcon                   (const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallSetViewPosition           (const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallGetParameterProperties    (const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallKeysRequired              (const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallGetVstVersion             (const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallEditKeyDown               (const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallEditKeyUp                 (const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallSetEditKnobMode           (const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallGetMidiProgramName        (const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallGetCurrentMidiProgram     (const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallGetMidiProgramCategory    (const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallHasMidiProgramsChanged    (const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallGetMidiKeyName            (const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallBeginSetProgram           (const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallEndSetProgram             (const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallGetSpeakerArrangement     (const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallShellGetNextPlugin        (const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallStartProcess              (const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallStopProcess               (const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallSetTotalSampleToProcess   (const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallSetPanLaw                 (const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallBeginLoadBank             (const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallBeginLoadProgram          (const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallSetProcessPrecision       (const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallGetNumMidiInputChannels   (const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallGetNumMidiOutputChannels  (const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr; virtual;

    procedure HostCallProcess(const Inputs, Outputs: PPSingle; const SampleFrames: Integer); virtual; abstract;
    procedure HostCallProcess32Replacing(const Inputs, Outputs: PPSingle; const SampleFrames: Integer); virtual; abstract;
    procedure HostCallProcess64Replacing(const Inputs, Outputs: PPDouble; const SampleFrames: Integer); virtual; abstract;

    function  HostCallDispatchEffect(const Opcode: TDispatcherOpcode; const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr; virtual;
    function  HostCallGetParameter(const Index: Integer): Single; virtual; abstract;
    procedure HostCallSetParameter(const Index: Integer; const Value: Single); virtual; abstract;

    function  UpdateSampleRate: Double; virtual;  // gets and returns sample rate from host (may issue setSampleRate() )
    function  UpdateBlockSize: Integer; virtual;  // same for block size

    function  GetInputSpeakerArrangement: PVstSpeakerArrangement; virtual;
    function  GetOutputSpeakerArrangement: PVstSpeakerArrangement; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    class function GetStaticDescription: string; virtual;

    property Effect: PVSTEffect read GetEffect;
    property AudioMaster: TAudioMasterCallbackFunc read FAudioMaster write SetAudioMaster;
    {$IFNDEF UseDelphi}
    property OnDestroy: TNotifyEvent read FOnDestroy write FOnDestroy;
    property OnCreate: TNotifyEvent read FOnCreate write FOnCreate;
    {$ENDIF}

    {$IFDEF DebugLog} property DebugLog: TStringList read fLog; {$ENDIF}
  end;

  EVstError = class(Exception);

function DispatchEffectFuncAudioEffectPtr(Effect: PVSTEffect; OpCode : TDispatcherOpCode;
  const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer;
  const opt: Single): TVstIntPtr; cdecl;
function GetParameterFuncAudioEffectPtr(const Effect: PVSTEffect;
  const Index: Integer): Single; cdecl;
procedure SetParameterFuncAudioEffectPtr(const Effect: PVSTEffect; const Index: Integer;
  const Value: Single); cdecl;

{$IFNDEF UseAudioEffectPtr}
function DispatchEffectFuncUserPtr(Effect: PVSTEffect; OpCode : TDispatcherOpCode;
  const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer;
  const opt: Single): TVstIntPtr; cdecl;
function GetParameterFuncUserPtr(const Effect: PVSTEffect;
  const Index: Integer): Single; cdecl;
procedure SetParameterFuncUserPtr(const Effect: PVSTEffect; const Index: Integer;
  const Value: Single); cdecl;
{$ENDIF}

// checks (just in case)
procedure ProcessFuncCheck(const Effect: PVSTEffect;
  const Inputs, Outputs: PPSingle; const SampleFrames: Integer); cdecl;
procedure Process32ReplacingFuncCheck(const Effect: PVSTEffect;
  const Inputs, Outputs: PPSingle; const SampleFrames: Integer); cdecl;
procedure Process64ReplacingFuncCheck(const Effect: PVSTEffect;
  const Inputs, Outputs: PPDouble; const SampleFrames: Integer); cdecl;

procedure ProcessFuncAudioEffectPtr(const Effect: PVSTEffect;
  const Inputs, Outputs: PPSingle; const SampleFrames: Integer); cdecl;
procedure Process32ReplacingFuncAudioEffectPtr(const Effect: PVSTEffect;
  const Inputs, Outputs: PPSingle; const SampleFrames: Integer); cdecl;
procedure Process64ReplacingFuncAudioEffectPtr(const Effect: PVSTEffect;
  const Inputs, Outputs: PPDouble; const SampleFrames: Integer); cdecl;

{$IFNDEF UseAudioEffectPtr}
procedure ProcessFuncUserPtr(const Effect: PVSTEffect;
  const Inputs, Outputs: PPSingle; const SampleFrames: Integer); cdecl;
procedure Process32ReplacingFuncUserPtr(const Effect: PVSTEffect;
  const Inputs, Outputs: PPSingle; const SampleFrames: Integer); cdecl;
procedure Process64ReplacingFuncUserPtr(const Effect: PVSTEffect;
  const Inputs, Outputs: PPDouble; const SampleFrames: Integer); cdecl;
{$ENDIF}

// Dummy calls
function GetParameterFuncDummy(const Effect: PVSTEffect;
  const Index: Integer): Single; cdecl;
procedure SetParameterFuncDummy(const Effect: PVSTEffect;
  const Index: Integer; const Value: Single); cdecl;
procedure ProcessFuncDummy(const Effect: PVSTEffect;
  const Inputs, Outputs: Pointer; const SampleFrames: Integer); cdecl;

// WinAmp
function Init(const WinAmpDSPModule: PWinAmpDSPModule): Integer; cdecl;
procedure Config(const WinAmpDSPModule: PWinAmpDSPModule); cdecl;
function ModifySamples(const WinAmpDSPModule: PWinAmpDSPModule;
  const Samples: Pointer; const SampleFrames, BitPerSample, ChannelCount,
  SampleRate: Integer): Integer; cdecl;
function ModifySamplesDummy(const WinAmpDSPModule: PWinAmpDSPModule;
  const Samples: Pointer; const SampleFrames, BitPerSample, ChannelCount,
  SampleRate: Integer): Integer; cdecl;
procedure Quit(const WinAmpDSPModule: PWinAmpDSPModule); cdecl;

function VstModuleMain(AudioMasterCallback: TAudioMasterCallbackFunc;
  const BasicVSTModuleClass: TBasicVSTModuleClass): PVSTEffect;
function WinampDSPModuleHeader(const BasicVSTModuleClass: TBasicVSTModuleClass): PWinAmpDSPHeader;
function GetWinampModule(const Which : Integer): PWinAmpDSPModule; cdecl;

implementation

uses
  DAV_Common, Math, Contnrs;

var
  GVstInstanceList : TObjectList;

{$IFNDEF PUREPASCAL}
const
  {$IFDEF UseDelphi}
  CHostCallProcessOffset = $00000238;
  CHostCallProcess32ReplacingOffset = $0000023C;
  CHostCallProcess64ReplacingOffset = $00000240;
(*
  CHostCallProcessOffset = $00000234;
  CHostCallProcess32ReplacingOffset = $00000238;
  CHostCallProcess64ReplacingOffset = $0000023C;
*)
  {$ELSE}
  CHostCallProcessOffset = $00000228;
  CHostCallProcess32ReplacingOffset = $0000022C;
  CHostCallProcess64ReplacingOffset = $00000230;
  {$ENDIF}
{$ENDIF}

function VstModuleMain(AudioMasterCallback: TAudioMasterCallbackFunc;
  const BasicVSTModuleClass: TBasicVSTModuleClass): PVSTEffect;
var
  BasicVstModule : TBasicVSTModule;
begin
 try
  BasicVstModule := BasicVSTModuleClass.Create(Application);
  with BasicVstModule do
   begin
    AudioMaster := AudioMasterCallback;
    Result := Effect;
   end;
  GVstInstanceList.Add(BasicVstModule);
 except
  Result := nil;
 end;
end;

var
  WADSPHeader  : TWinAmpDSPheader;
  WAVstModule  : TBasicVSTModuleClass;

function GetWinampModule(const Which: Integer): PWinAmpDSPModule; cdecl;
begin
 case Which of
   0 : begin
        GetMem(Result, SizeOf(TWinAmpDSPModule));
        Result^.Description := PChar(WAVstModule.GetStaticDescription);
        Result^.Init := Init;
        Result^.Config := Config;
        Result^.ModifySamples := ModifySamplesDummy;
        Result^.Quit := Quit;
       end
 else
  Result := nil;
 end;
end;

function WinampDSPModuleHeader(const BasicVSTModuleClass: TBasicVSTModuleClass): PWinAmpDSPHeader;
begin
 WAVstModule := BasicVSTModuleClass;
 try
  with WADSPHeader do
   begin
    Version     := $20;
    Key         := $21;
    Description := PChar(WAVstModule.GetStaticDescription);
    GetModule   := GetWinampModule;
   end;
  Result := @WADSPHeader;
 except
  Result := nil;
 end;
end;


{ TBasicVSTModule }

constructor TBasicVSTModule.Create(AOwner: TComponent);
begin
 {$IFDEF DebugLog}
 FLog := TStringList.Create;
 FTmStmp := Now;
 FLog.Add('Create: ' + TimeToStr(FTmStmp));
 if not (csDesigning in ComponentState) then
  try
   FLog.SaveToFile('Debug.log');
  except
  end;
 {$ENDIF}

 {$IFDEF UseDelphi} inherited CreateNew(AOwner); {$ENDIF}
 with FEffect do
  begin
   Magic           := 'PtsV';
   EffectFlags     := [effFlagsCanReplacing];
   ReservedForHost := nil;
   Resvd2          := nil;
   {$IFDEF UseAudioEffectPtr}
   AudioEffectPtr  := Self;
   User            := nil;
   {$ELSE}
   AudioEffectPtr  := nil;
   User            := Self;
   {$ENDIF}
   uniqueID        := 'fEoN';
   ioRatio         := 1;
   numParams       := 0;
   numPrograms     := 0;
   numInputs       := 2;
   numOutputs      := 2;

   {$IFNDEF UseAudioEffectPtr}
   Dispatcher         := @DispatchEffectFuncUserPtr;
   SetParameter       := @SetParameterFuncUserPtr;
   GetParameter       := @GetParameterFuncUserPtr;
   {$ELSE}
   Dispatcher         := @DispatchEffectFuncAudioEffectPtr;
   SetParameter       := @SetParameterFuncAudioEffectPtr;
   GetParameter       := @GetParameterFuncAudioEffectPtr;
   {$ENDIF}
   Process            := @ProcessFuncCheck;
   Process32Replacing := @Process32ReplacingFuncCheck;
   Process64Replacing := @Process64ReplacingFuncCheck;
  end;

 FWinAmpDspModule    := nil;
 FWinAmpSampleRate   := 44100;
 FWinAmpSampleFrames := 0;
 FWinAmpNrChannels   := 0;
 FWinAmpBypass       := False;

 {$IFNDEF UseAudioEffectPtr}
 FUseAudioEffectPtr  := False;
 {$ENDIF}
end;

destructor TBasicVSTModule.Destroy;
var
  i : Integer;
begin
 try
  {$IFDEF DebugLog} if Assigned(FLog) then FLog.SaveToFile('Debug.log'); {$ENDIF}
  for i := 0 to Length(FWinAmpInputBuffer)  - 1 do Dispose(FWinAmpInputBuffer[i]);
  for i := 0 to Length(FWinAmpOutputBuffer) - 1 do Dispose(FWinAmpOutputBuffer[i]);
 finally
  {$IFDEF DebugLog} if Assigned(FLog) then FLog.Free; {$ENDIF}
  inherited;
 end;
end;

{$IFDEF DebugLog}
procedure TBasicVSTModule.AddLogMessage(const Text: string);
begin
 if Assigned(FLog) then
  try
   FLog.Add(TimeToStr(Now - FTmStmp) + ' | ' + Text);
   if not (csDesigning in ComponentState)
    then FLog.SaveToFile('Debug.log');
  except
  end;
end;
{$ENDIF}

function TBasicVSTModule.GetEffect: PVSTEffect;
begin
 Result := @FEffect;
end;

procedure TBasicVSTModule.SetAudioMaster(const AM :TAudioMasterCallbackFunc);
{$IFNDEF UseAudioEffectPtr}
var
  HostProduct : PAnsiChar;
{$ENDIF}  
begin
 {$IFDEF DebugLog} AddLogMessage('TBasicVSTModule.SetAudioMaster'); {$ENDIF}

 FAudioMaster := AM;
 if CallAudioMaster(amVersion) = 0
  then raise EVstError.Create('AudioMaster Error');

 {$IFDEF DebugLog} AddLogMessage('After check audiomaster version'); {$ENDIF}

 {$IFNDEF UseAudioEffectPtr}
 GetMem(HostProduct, 256);
 FillChar(HostProduct^, 256, 0);
 try
  GetHostProductString(HostProduct);
  FUseAudioEffectPtr := (StrPos('energyXT', HostProduct) <> nil) or
    (StrPos('Sound Forge Pro 10.0', HostProduct) <> nil);

  {$IFDEF DebugLog} AddLogMessage('Get Host Product String'); {$ENDIF}

  if FUseAudioEffectPtr then
   with FEffect do
    begin
     AudioEffectPtr := Self;
     User           := nil;
     Dispatcher     := @DispatchEffectFuncAudioEffectPtr;
     SetParameter   := @SetParameterFuncAudioEffectPtr;
     GetParameter   := @GetParameterFuncAudioEffectPtr;
    end;
 finally
  Dispose(HostProduct);
 end;
 {$ENDIF}
end;

function TBasicVSTModule.CallAudioMaster(const Opcode: TAudioMasterOpcode;
  const Index: Integer = 0; const Value: TVstIntPtr = 0;
  const Ptr: Pointer = nil; const Opt: Single = 0): TVstIntPtr;
begin
 {$IFDEF DebugLog} AddLogMessage('TBasicVSTModule.CallAudioMaster, Opcode:' + IntToStr(Integer(Opcode))); {$ENDIF}
 {$IFDEF DebugLog} AddLogMessage('TBasicVSTModule.CallAudioMaster: SizeOf(PVstEffect)' + IntToStr(SizeOf(PVstEffect))); {$ENDIF}
 {$IFDEF DebugLog} AddLogMessage('TBasicVSTModule.CallAudioMaster: SizeOf(TAudioMasterOpcode)' + IntToStr(SizeOf(TAudioMasterOpcode))); {$ENDIF}
 {$IFDEF DebugLog} AddLogMessage('TBasicVSTModule.CallAudioMaster: SizeOf(Index)' + IntToStr(SizeOf(Index))); {$ENDIF}
 {$IFDEF DebugLog} AddLogMessage('TBasicVSTModule.CallAudioMaster: SizeOf(Value)' + IntToStr(SizeOf(Value))); {$ENDIF}
 {$IFDEF DebugLog} AddLogMessage('TBasicVSTModule.CallAudioMaster: SizeOf(Ptr)' + IntToStr(SizeOf(Ptr))); {$ENDIF}
 {$IFDEF DebugLog} AddLogMessage('TBasicVSTModule.CallAudioMaster: SizeOf(Result)' + IntToStr(SizeOf(Result))); {$ENDIF}

 if Assigned(FAudioMaster)
  then Result := FAudioMaster(@FEffect, Opcode, Index, Value, Ptr, Opt)
  else Result := 0;
end;


class function TBasicVSTModule.GetStaticDescription: string;
begin
 Result := 'Delphi ASIO & VST Package Plugin';
end;


// ------------------------------------------------------------------
// Calls to the Host
// ------------------------------------------------------------------

function TBasicVSTModule.GetMasterVersion: Integer;
begin
 Result := CallAudioMaster(amVersion);
 if Result = 0 then Result := 1;
end;

function TBasicVSTModule.GetCurrentUniqueId: TChunkName;
begin
 if Assigned(FAudioMaster)
  then Result := TChunkName(Integer(CallAudioMaster(amCurrentId)))
  else Result := #0#0#0#0;
end;

procedure TBasicVSTModule.MasterIdle;
begin
 CallAudioMaster(amIdle);
end;

function TBasicVSTModule.IsInputConnected(const Input: Integer): Boolean;
begin
 Result := (CallAudioMaster(amPinConnected, Input) = 0);
end;

function TBasicVSTModule.IsOutputConnected(const Output: Integer): Boolean;
begin
 Result := (CallAudioMaster(amPinConnected, Output, 1) = 0);
end;

procedure TBasicVSTModule.WantEvents(const Filter: Integer);
begin
 CallAudioMaster(amWantMidi, 0, Filter);
end;

function TBasicVSTModule.GetTimeInfo(const Filter: Integer): PVstTimeInfo;
begin
 Result := PVstTimeInfo(CallAudioMaster(amGetTime, 0, Filter));
end;

procedure TBasicVSTModule.SetTimeInfo(const Filter: Integer; var VstTimeInfo: PVstTimeInfo);
begin
 CallAudioMaster(amSetTime, 0, Filter, @VstTimeInfo);
end;

function TBasicVSTModule.TempoAt(const Pos: Integer): Integer;
begin
 Result := CallAudioMaster(amTempoAt, 0, Pos);
end;

function TBasicVSTModule.SendVstEventsToHost(var Events: TVstEvents): Boolean;
begin
 Result := CallAudioMaster(amProcessEvents, 0, 0, @Events) = 1;
end;

function TBasicVSTModule.GetNumAutomatableParameters: Integer;
begin
 Result := CallAudioMaster(amGetNumAutomatableParameters);
end;

procedure TBasicVSTModule.SetParameterAutomated(const Index: Integer; const Value: Single);
begin
 CallAudioMaster(amGetNumAutomatableParameters, Index, 0, nil, Value);
end;

function TBasicVSTModule.GetParameterQuantization: Integer;
begin
 Result := CallAudioMaster(amGetParameterQuantization);
end;

function TBasicVSTModule.GetInputLatency: Integer;
begin
 Result := CallAudioMaster(amGetInputLatency);
end;

function TBasicVSTModule.GetOutputLatency: Integer;
begin
 Result := CallAudioMaster(amGetOutputLatency);
end;

function TBasicVSTModule.GetPreviousPlug(const Input: Integer): PVSTEffect;
begin
 Result := PVSTEffect(CallAudioMaster(amGetPreviousPlug, 0, Input));
end;

function TBasicVSTModule.GetNextPlug(const Output: Integer): PVSTEffect;
begin
 Result := PVSTEffect(CallAudioMaster(amGetNextPlug, 0, Output));
end;

function TBasicVSTModule.WillProcessReplacing: Integer;
begin
 Result := CallAudioMaster(amWillReplaceOrAccumulate);
end;

function TBasicVSTModule.GetCurrentProcessLevel: Integer;
begin
 Result := CallAudioMaster(amGetCurrentProcessLevel);
end;

function TBasicVSTModule.GetAutomationState: Integer;
begin
 Result := CallAudioMaster(amGetAutomationState);
end;

function TBasicVSTModule.OfflineRead(var Offline: TVstOfflineTaskRecord; const Option: TVstOfflineOption; const ReadSource: Boolean): Boolean;
begin
 Result := CallAudioMaster(amOfflineRead, Integer(ReadSource), Integer(Option), @Offline) <> 0;
end;

function TBasicVSTModule.OfflineWrite(var Offline: TVstOfflineTaskRecord; const Option: TVstOfflineOption): Boolean;
begin
 Result := CallAudioMaster(amOfflineWrite, 0, Integer(Option), @Offline) <> 0;
end;

function TBasicVSTModule.OfflineStart(var AudioFile: TVstAudioFile; const numAudioFiles, numNewAudioFiles: Integer): Boolean;
begin
 Result := CallAudioMaster(amOfflineStart, numNewAudioFiles, numAudioFiles, @AudioFile) <> 0;
end;

function TBasicVSTModule.OfflineGetCurrentPass: Integer;
begin
 Result := CallAudioMaster(amOfflineGetCurrentPass);
end;

function TBasicVSTModule.OfflineGetCurrentMetaPass: Integer;
begin
 Result := CallAudioMaster(amOfflineGetCurrentMetaPass);
end;

procedure TBasicVSTModule.SetOutputSampleRate(const SampleRate: Single);
begin
 CallAudioMaster(amSetOutputSampleRate, 0, 0, nil, SampleRate);
end;

function TBasicVSTModule.GetHostVendorString(const Text: PAnsiChar): Boolean;
begin
 Result := CallAudioMaster(amGetVendorString, 0, 0, Text) <> 0;
end;

function TBasicVSTModule.GetHostProductString(const Text: PAnsiChar): Boolean;
begin
 {$IFDEF DebugLog} AddLogMessage('TBasicVSTModule.GetHostProductString: Before AudioMaster Call'); {$ENDIF}

 Result := CallAudioMaster(amGetProductString, 0, 0, Text) <> 0;

 {$IFDEF DebugLog} AddLogMessage('TBasicVSTModule.GetHostProductString: After AudioMaster Call'); {$ENDIF}
end;

function TBasicVSTModule.GetHostVendorVersion: Integer;
begin
 Result := CallAudioMaster(amGetVendorVersion);
end;

function TBasicVSTModule.HostVendorSpecific(const Arg1, Arg2: Integer; const ptrArg: Pointer; const floatArg: Single): Integer;
begin
 Result := CallAudioMaster(amVendorSpecific, Arg1, Arg2, ptrArg, floatArg);
end;

function TBasicVSTModule.GetCanHostDo(Text: string): Integer;
var
  str : AnsiString;
begin
 str := AnsiString(Text);
 Result := CallAudioMaster(amCanDo, 0, 0, PAnsiChar(str));
end;

function TBasicVSTModule.GetHostLanguage: Integer;
begin
 Result := CallAudioMaster(amGetLanguage);
end;

function TBasicVSTModule.OpenWindow(const aWindow: PVstWindow): Pointer;
begin
 Result := Pointer(CallAudioMaster(amOpenWindow, 0, 0, aWindow));
end;

function TBasicVSTModule.CloseWindow(const aWindow: PVstWindow): Boolean;
begin
 Result := CallAudioMaster(amCloseWindow, 0, 0, aWindow) <> 0;
end;

function TBasicVSTModule.GetDirectory: Pointer;
begin
 Result := Pointer(CallAudioMaster(amGetDirectory));
end;

function TBasicVSTModule.UpdateDisplay: Boolean;
begin
 Result := CallAudioMaster(amUpdateDisplay) <> 0;
end;

function TBasicVSTModule.IOChanged: Boolean;
begin
 Result := CallAudioMaster(amIOChanged) <> 0;
end;

function TBasicVSTModule.NeedIdle: Boolean;
begin
 Result := CallAudioMaster(amNeedIdle) <> 0;
end;

function TBasicVSTModule.SizeWindow(const Width, Height: Integer): Boolean;
begin
 Result := CallAudioMaster(amSizeWindow, Width, Height) <> 0;
end;

function TBasicVSTModule.BeginEdit(const Index: Integer): Boolean;
begin
 Result := CallAudioMaster(amBeginEdit, Index) <> 0;
end;

function TBasicVSTModule.EndEdit(const Index: Integer): Boolean;
begin
 Result := CallAudioMaster(amEndEdit, Index) <> 0;
end;

function TBasicVSTModule.OpenFileSelector(var VstFileSelect: TVstFileSelect): Boolean;
begin
 Result := CallAudioMaster(amOpenFileSelector, 0, 0, @VstFileSelect) <> 0;
end;

function TBasicVSTModule.CloseFileSelector(var VstFileSelect: TVstFileSelect): Boolean;
begin
 Result := CallAudioMaster(amCloseFileSelector, 0, 0, @VstFileSelect) <> 0;
end;

function TBasicVSTModule.GetChunkFile(const NativePath: Pointer): Boolean;
begin
 Result := CallAudioMaster(amGetChunkFile, 0, 0, NativePath) <> 0;
end;

function TBasicVSTModule.UpdateSampleRate: Double;
begin
 Result := CallAudioMaster(amGetSampleRate);
end;

function TBasicVSTModule.UpdateBlockSize: Integer;
begin
 Result := CallAudioMaster(amGetBlockSize);
end;

function TBasicVSTModule.GetInputSpeakerArrangement: PVstSpeakerArrangement;
begin
 Result := PVstSpeakerArrangement(CallAudioMaster(amGetInputSpeakerArrangement));
end;

function TBasicVSTModule.GetOutputSpeakerArrangement: PVstSpeakerArrangement;
begin
 Result := PVstSpeakerArrangement(CallAudioMaster(amGetOutputSpeakerArrangement));
end;

function TBasicVSTModule.WinAmpModifySamples(const Samples: Pointer;
  const SampleFrames, BitPerSample, ChannelCount, SampleRate: Integer): Integer;
var
 i : Integer;
begin
 Result := SampleFrames;

 // test if maximum blocksize changed
 if SampleFrames > FWinAmpSampleRate then
  begin
   FWinAmpSampleRate := SampleFrames;
//   SetBlockSize(SampleFrames);

   // reallocate input VST buffers
   for i := 0 to Length(FWinAmpInputBuffer) - 1
    do ReallocMem(FWinAmpInputBuffer[i], SampleFrames * SizeOf(Single));

   // reallocate output VST buffers
   for i := 0 to Length(FWinAmpOutputBuffer) - 1
    do ReallocMem(FWinAmpOutputBuffer[i], SampleFrames * SizeOf(Single));
  end;

 // test if samplerate changed
 if SampleRate <> FWinAmpSampleRate then
  begin
   FWinAmpSampleRate := SampleRate;
//   SetSampleRate(SampleRate);
  end;

 case BitPerSample of
   8: begin
       FWinAmpConvertIn  := ConvertInterleaved8bitToFloat;
       FWinAmpConvertOut := ConvertFloatToInterleaved8bit;
      end;
  16: begin
       FWinAmpConvertIn  := ConvertInterleaved16bitToFloat;
       FWinAmpConvertOut := ConvertFloatToInterleaved16bit;
      end;
  24: begin
       FWinAmpConvertIn  := ConvertInterleaved24bitToFloat;
       FWinAmpConvertOut := ConvertFloatToInterleaved24bit;
      end;
  32: begin
       FWinAmpConvertIn  := ConvertInterleavedToFloat;
       FWinAmpConvertOut := ConvertFloatToInterleaved;
      end;
  else
   begin
    FWinAmpConvertIn  := ConvertDummy;
    FWinAmpConvertOut := ConvertDummy;
   end;
 end;

 // convert interleaved to float data
 FWinAmpConvertIn(Samples, ChannelCount, SampleFrames);

 // process VST plugin
 FEffect.Process32Replacing(@FEffect, @FWinAmpInputBuffer[0],
   @FWinAmpOutputBuffer[0], SampleFrames);

 // convert float to interleaved data
 FWinAmpConvertOut(Samples, ChannelCount, SampleFrames);
end;

procedure TBasicVSTModule.WinAmpConfig;
begin
 if not Assigned(FWinAmpEditorForm) then
  begin
   FWinAmpEditorForm := TForm.Create(Self);
  end;
 FWinAmpEditorForm.Show;
end;

procedure TBasicVSTModule.WinAmpQuit;
begin
 FWinAmpBypass := True;
end;

procedure TBasicVSTModule.ConvertDummy(const Data: Pointer; const ChannelCount,
  SampleFrames: Integer);
begin
 // do nothing (dummy)
end;

procedure TBasicVSTModule.ConvertFloatToInterleaved8bit(const Data: Pointer;
  const ChannelCount, SampleFrames: Integer);
var
  I8 : PShortIntArray absolute Data;
  Channel, Sample : Integer;
const
  MulFakDith8 : Single = $7E;
begin
 for Channel := 0 to min(Length(FWinAmpOutputBuffer), ChannelCount) - 1 do
  for Sample := 0 to SampleFrames - 1
   do I8^[Sample * ChannelCount + Channel] := round(FWinAmpOutputBuffer[Channel]^[Sample] * MulFakDith8 + random - random);
end;

procedure TBasicVSTModule.ConvertFloatToInterleaved16bit(const Data: Pointer;
  const ChannelCount, SampleFrames: Integer);
var
  I16             : PSmallIntArray absolute Data;
  Channel, Sample : Integer;
const
  MulFakDith16 : Single = $7FFE;
begin
 for Channel := 0 to min(Length(FWinAmpOutputBuffer), ChannelCount) - 1 do
  for Sample := 0 to SampleFrames - 1
   do I16^[Sample * ChannelCount + Channel] := round(Limit(FWinAmpOutputBuffer[Channel]^[Sample]) * MulFakDith16 + random - random);
end;

procedure TBasicVSTModule.ConvertFloatToInterleaved24bit(const Data: Pointer;
  const ChannelCount, SampleFrames: Integer);
var
  I24             : P3ByteArray absolute Data;
  Channel, Sample : Integer;
  TempData        : Integer;
  TempDataBytes   : array [0..3] of Byte absolute TempData;
const
  MulFakDith24 : Single = 1 / $7FFFFE;
begin
 for Channel := 0 to min(Length(FWinAmpOutputBuffer), ChannelCount) - 1 do
  for Sample := 0 to SampleFrames - 1 do
   begin
    TempData := round(Limit(FWinAmpOutputBuffer[Channel]^[Sample]) * MulFakDith24 + random - random);
    Move(TempDataBytes[1], I24^[Sample * ChannelCount + Channel], 3);
   end;
end;

procedure TBasicVSTModule.ConvertFloatToInterleaved(const Data: Pointer;
  const ChannelCount, SampleFrames: Integer);
var
  Interleaved     : PDAVSingleFixedArray absolute Data;
  Channel, Sample : Integer;
begin
 for Channel := 0 to min(Length(FWinAmpInputBuffer), ChannelCount) - 1 do
  for Sample := 0 to SampleFrames - 1
   do FWinAmpInputBuffer[Channel]^[Sample] := Interleaved^[Sample * ChannelCount + Channel];
end;

procedure TBasicVSTModule.ConvertInterleaved8bitToFloat(const Data: Pointer;
  const ChannelCount, SampleFrames: Integer);
var
  I8              : PShortIntArray absolute Data;
  Channel, Sample : Integer;
const
  DivFak8 : Single = 1 / $80;
begin
 for Channel := 0 to min(Length(FWinAmpInputBuffer), ChannelCount) - 1 do
  for Sample := 0 to SampleFrames - 1
   do FWinAmpInputBuffer[Channel]^[Sample] := I8^[Sample * ChannelCount + Channel] * DivFak8;
end;

procedure TBasicVSTModule.ConvertInterleaved16bitToFloat(const Data: Pointer;
  const ChannelCount, SampleFrames: Integer);
var
  I16             : PSmallIntArray absolute Data;
  Channel, Sample : Integer;
const
  DivFak16 : Single = 1 / $8000;
begin
 for Channel := 0 to min(Length(FWinAmpInputBuffer), ChannelCount) - 1 do
  for Sample := 0 to SampleFrames - 1
   do FWinAmpInputBuffer[Channel]^[Sample] := I16^[Sample * ChannelCount + Channel] * DivFak16;
end;

procedure TBasicVSTModule.ConvertInterleaved24bitToFloat(const Data: Pointer;
  const ChannelCount, SampleFrames: Integer);
var
  I24             : P3ByteArray absolute Data;
  Channel, Sample : Integer;
  TempData        : Integer;
const
  DivFak24 : Single = 1 / $800000;
begin
 for Channel := 0 to min(Length(FWinAmpInputBuffer), ChannelCount) - 1 do
  for Sample := 0 to SampleFrames - 1 do
   begin
    TempData := (ShortInt(I24^[Sample * ChannelCount + Channel][2]) shl 16) +
                         (I24^[Sample * ChannelCount + Channel][1]  shl 8)  +
                         (I24^[Sample * ChannelCount + Channel][0]);
    FWinAmpInputBuffer[Channel]^[Sample] := TempData * DivFak24;
   end;
end;

procedure TBasicVSTModule.ConvertInterleavedToFloat(const Data: Pointer;
  const ChannelCount, SampleFrames: Integer);
var
  Interleaved     : PDAVSingleFixedArray absolute Data;
  Channel, Sample : Integer;
begin
 for Channel := 0 to min(Length(FWinAmpInputBuffer), ChannelCount) - 1 do
  for Sample := 0 to SampleFrames - 1
   do FWinAmpInputBuffer[Channel]^[Sample] := Interleaved^[Sample * ChannelCount + Channel];
end;



// ------------------------------------------------------------------
// Calls from the host
// ------------------------------------------------------------------

function TBasicVSTModule.HostCallOpen(const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr;
begin
 Result := 0;
 {$IFNDEF UseAudioEffectPtr}
 if not FUseAudioEffectPtr then
  begin
   Effect^.GetParameter       := @GetParameterFuncUserPtr;
   Effect^.SetParameter       := @SetParameterFuncUserPtr;
   Effect^.Process            := @ProcessFuncUserPtr;
   Effect^.Process32Replacing := @Process32ReplacingFuncUserPtr;
   Effect^.Process64Replacing := @Process64ReplacingFuncUserPtr;
  end
 else
 {$ELSE}
  begin
   Effect^.GetParameter       := @GetParameterFuncAudioEffectPtr;
   Effect^.SetParameter       := @SetParameterFuncAudioEffectPtr;
   Effect^.Process            := @ProcessFuncAudioEffectPtr;
   Effect^.Process32Replacing := @Process32ReplacingFuncAudioEffectPtr;
   Effect^.Process64Replacing := @Process64ReplacingFuncAudioEffectPtr;
  end;
 {$ENDIF}
end;

function TBasicVSTModule.HostCallClose(const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr;
begin
 try
  Effect^.GetParameter := @GetParameterFuncDummy;
  Effect^.SetParameter := @SetParameterFuncDummy;
  Effect^.Process := @ProcessFuncDummy;
  Effect^.Process32Replacing := @ProcessFuncDummy;
  Effect^.Process64Replacing := @ProcessFuncDummy;
  {$IFDEF UseAudioEffectPtr}
  Effect^.AudioEffectPtr := nil;
  {$ELSE}
  if FUseAudioEffectPtr
   then Effect^.AudioEffectPtr := nil
   else Effect^.User := nil;
  {$ENDIF}

  {$IFNDEF FPC}
  Free;
  {$ENDIF}

  Result := 1;
 except
  Result := 0;
 end;
end;

function TBasicVSTModule.HostCallSetProgram(const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallGetProgram(const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallSetProgramName(const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallGetProgramName(const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallGetParamLabel(const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallGetParamDisplay(const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallGetParamName(const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallGetVu(const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallSetSampleRate(const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallSetBlockSize(const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallMainsChanged(const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallEditGetRect(const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallEditOpen(const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallEditClose(const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallEditDraw(const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallEditMouse(const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallEditKey(const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallEditIdle(const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallEditTop(const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallEditSleep(const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallIdentify(const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr;
var
  ChunkName : TChunkName;
begin
 ChunkName := 'fEvN';
 Result := Integer(ChunkName);
end;

function TBasicVSTModule.HostCallGetChunk(const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallSetChunk(const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallProcessEvents(const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallCanBeAutomated(const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallString2Parameter(const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallGetNumProgramCategories(const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallGetProgramNameIndexed(const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallCopyProgram(const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallDispatchEffect(const Opcode: TDispatcherOpcode; const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr;
begin
 case OpCode of
  effOpen:                      Result := HostCallOpen(Index, Value, ptr, opt);
  effClose:                     Result := HostCallClose(Index, Value, ptr, opt);
  effSetProgram:                Result := HostCallSetProgram(Index, Value, ptr, opt);
  effGetProgram:                Result := HostCallGetProgram(Index, Value, ptr, opt);
  effSetProgramName:            Result := HostCallSetProgramName(Index, Value, ptr, opt);
  effGetProgramName:            Result := HostCallGetProgramName(Index, Value, ptr, opt);
  effGetParamLabel:             Result := HostCallGetParamLabel(Index, Value, ptr, opt);
  effGetParamDisplay:           Result := HostCallGetParamDisplay(Index, Value, ptr, opt);
  effGetParamName:              Result := HostCallGetParamName(Index, Value, ptr, opt);
  effGetVu:                     Result := HostCallGetVu(Index, Value, ptr, opt);
  effSetSampleRate:             Result := HostCallSetSampleRate(Index, Value, ptr, opt);
  effSetBlockSize:              Result := HostCallSetBlockSize(Index, Value, ptr, opt);
  effMainsChanged:              Result := HostCallMainsChanged(Index, Value, ptr, opt);
  effEditGetRect:               Result := HostCallEditGetRect(Index, Value, ptr, opt);
  effEditOpen:                  Result := HostCallEditOpen(Index, Value, ptr, opt);
  effEditClose:                 Result := HostCallEditClose(Index, Value, ptr, opt);
  effEditDraw:                  Result := HostCallEditDraw(Index, Value, ptr, opt);
  effEditMouse:                 Result := HostCallEditMouse(Index, Value, ptr, opt);
  effEditKey:                   Result := HostCallEditKey(Index, Value, ptr, opt);
  effEditIdle:                  Result := HostCallEditIdle(Index, Value, ptr, opt);
  effEditTop:                   Result := HostCallEditTop(Index, Value, ptr, opt);
  effEditSleep:                 Result := HostCallEditSleep(Index, Value, ptr, opt);
  effIdentify:                  Result := HostCallIdentify(Index, Value, ptr, opt);
  effGetChunk:                  Result := HostCallGetChunk(Index, Value, ptr, opt);
  effSetChunk:                  Result := HostCallSetChunk(Index, Value, ptr, opt);
  effProcessEvents:             Result := HostCallProcessEvents(Index, Value, ptr, opt);
  effCanBeAutomated:            Result := HostCallCanBeAutomated(Index, Value, ptr, opt);
  effString2Parameter:          Result := HostCallString2Parameter(Index, Value, ptr, opt);
  effGetNumProgramCategories:   Result := HostCallGetNumProgramCategories(Index, Value, ptr, opt);
  effGetProgramNameIndexed:     Result := HostCallGetProgramNameIndexed(Index, Value, ptr, opt);
  effCopyProgram:               Result := HostCallCopyProgram(Index, Value, ptr, opt);
  effConnectInput:              Result := HostCallConnectInput(Index, Value, ptr, opt);
  effConnectOutput:             Result := HostCallConnectOutput(Index, Value, ptr, opt);
  effGetInputProperties:        Result := HostCallGetInputProperties(Index, Value, ptr, opt);
  effGetOutputProperties:       Result := HostCallGetOutputProperties(Index, Value, ptr, opt);
  effGetPlugCategory:           Result := HostCallGetPlugCategory(Index, Value, ptr, opt);
  effGetCurrentPosition:        Result := HostCallGetCurrentPosition(Index, Value, ptr, opt);
  effGetDestinationBuffer:      Result := HostCallGetDestinationBuffer(Index, Value, ptr, opt);
  effOfflineNotify:             Result := HostCallOfflineNotify(Index, Value, ptr, opt);
  effOfflinePrepare:            Result := HostCallOfflinePrepare(Index, Value, ptr, opt);
  effOfflineRun:                Result := HostCallOfflineRun(Index, Value, ptr, opt);
  effProcessVarIo:              Result := HostCallProcessVarIo(Index, Value, ptr, opt);
  effSetSpeakerArrangement:     Result := HostCallSetSpeakerArrangement(Index, Value, ptr, opt);
  effSetBlockSizeAndSampleRate: Result := HostCallSetBlockSizeAndSampleRate(Index, Value, ptr, opt);
  effSetBypass:                 Result := HostCallSetBypass(Index, Value, ptr, opt);
  effGetEffectName:             Result := HostCallGetEffectName(Index, Value, ptr, opt);
  effGetErrorText:              Result := HostCallGetErrorText(Index, Value, ptr, opt);
  effGetVendorString:           Result := HostCallGetVendorString(Index, Value, ptr, opt);
  effGetProductString:          Result := HostCallGetProductString(Index, Value, ptr, opt);
  effGetVendorVersion:          Result := HostCallGetVendorVersion(Index, Value, ptr, opt);
  effVendorSpecific:            Result := HostCallVendorSpecific(Index, Value, ptr, opt);
  effCanDo:                     Result := HostCallCanDo(Index, Value, ptr, opt);
  effGetTailSize:               Result := HostCallGetTailSize(Index, Value, ptr, opt);
  effIdle:                      Result := HostCallIdle(Index, Value, ptr, opt);
  effGetIcon:                   Result := HostCallGetIcon(Index, Value, ptr, opt);
  effSetViewPosition:           Result := HostCallSetViewPosition(Index, Value, ptr, opt);
  effGetParameterProperties:    Result := HostCallGetParameterProperties(Index, Value, ptr, opt);
  effKeysRequired:              Result := HostCallKeysRequired(Index, Value, ptr, opt);
  effGetVstVersion:             Result := HostCallGetVstVersion(Index, Value, ptr, opt);
  effEditKeyDown:               Result := HostCallEditKeyDown(Index, Value, ptr, opt);
  effEditKeyUp:                 Result := HostCallEditKeyUp(Index, Value, ptr, opt);
  effSetEditKnobMode:           Result := HostCallSetEditKnobMode(Index, Value, ptr, opt);
  effGetMidiProgramName:        Result := HostCallGetMidiProgramName(Index, Value, ptr, opt);
  effGetCurrentMidiProgram:     Result := HostCallGetCurrentMidiProgram(Index, Value, ptr, opt);
  effGetMidiProgramCategory:    Result := HostCallGetMidiProgramCategory(Index, Value, ptr, opt);
  effHasMidiProgramsChanged:    Result := HostCallHasMidiProgramsChanged(Index, Value, ptr, opt);
  effGetMidiKeyName:            Result := HostCallGetMidiKeyName(Index, Value, ptr, opt);
  effBeginSetProgram:           Result := HostCallBeginSetProgram(Index, Value, ptr, opt);
  effEndSetProgram:             Result := HostCallEndSetProgram(Index, Value, ptr, opt);
  effGetSpeakerArrangement:     Result := HostCallGetSpeakerArrangement(Index, Value, ptr, opt);
  effShellGetNextPlugin:        Result := HostCallShellGetNextPlugin(Index, Value, ptr, opt);
  effStartProcess:              Result := HostCallStartProcess(Index, Value, ptr, opt);
  effStopProcess:               Result := HostCallStopProcess(Index, Value, ptr, opt);
  effSetTotalSampleToProcess:   Result := HostCallSetTotalSampleToProcess(Index, Value, ptr, opt);
  effSetPanLaw:                 Result := HostCallSetPanLaw(Index, Value, ptr, opt);
  effBeginLoadBank:             Result := HostCallBeginLoadBank(Index, Value, ptr, opt);
  effBeginLoadProgram:          Result := HostCallBeginLoadProgram(Index, Value, ptr, opt);
  effSetProcessPrecision:       Result := HostCallSetProcessPrecision(Index, Value, ptr, opt);
  effGetNumMidiInputChannels:   Result := HostCallGetNumMidiInputChannels(Index, Value, ptr, opt);
  effGetNumMidiOutputChannels:  Result := HostCallGetNumMidiOutputChannels(Index, Value, ptr, opt);
  else
   try
     raise EVstError.Create('Unknown OpCode');
   except
     Result := 0;
   end;
  end;
end;

function TBasicVSTModule.HostCallConnectInput(const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallConnectOutput(const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallGetInputProperties(const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallGetOutputProperties(const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallGetPlugCategory(const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallGetCurrentPosition(const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallGetDestinationBuffer(const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallOfflineNotify(const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallOfflinePrepare(const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallOfflineRun(const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallProcessVarIo(const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallSetSpeakerArrangement(const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallSetBlockSizeAndSampleRate(const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallSetBypass(const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallGetEffectName(const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallGetErrorText(const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallGetVendorString(const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallGetProductString(const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallGetVendorVersion(const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallVendorSpecific(const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallCanDo(const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallGetTailSize(const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallIdle(const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallGetIcon(const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallSetViewPosition(const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallGetParameterProperties(const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallKeysRequired(const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallGetVstVersion(const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallEditKeyDown(const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallEditKeyUp(const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallSetEditKnobMode(const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallGetMidiProgramName(const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallGetCurrentMidiProgram(const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallGetMidiProgramCategory(const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallHasMidiProgramsChanged(const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallGetMidiKeyName(const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallBeginSetProgram(const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallEndSetProgram(const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallGetSpeakerArrangement(const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallShellGetNextPlugin(const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallStartProcess(const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallStopProcess(const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallSetTotalSampleToProcess(const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallSetPanLaw(const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallBeginLoadBank(const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallBeginLoadProgram(const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallSetProcessPrecision(const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallGetNumMidiInputChannels(const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallGetNumMidiOutputChannels(const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;



// effect functions

function DispatchEffectFuncAudioEffectPtr(Effect: PVSTEffect; OpCode: TDispatcherOpCode; const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr; cdecl;
begin
 if Assigned(Effect) and (TObject(Effect^.AudioEffectPtr) is TBasicVSTModule)
   then Result := TBasicVSTModule(Effect^.AudioEffectPtr).HostCallDispatchEffect(OpCode, Index, Value, ptr, opt)
 else Result := 0;
end;

function GetParameterFuncAudioEffectPtr(const Effect: PVSTEffect; const Index: Integer): Single; cdecl;
begin
 Assert(Assigned(Effect));
 if TObject(Effect^.AudioEffectPtr) is TBasicVSTModule
  then Result := TBasicVSTModule(Effect^.AudioEffectPtr).HostCallGetParameter(Index)
  else Result := 0;
end;

procedure SetParameterFuncAudioEffectPtr(const Effect: PVSTEffect; const Index: Integer; const Value: Single); cdecl;
begin
 Assert(Assigned(Effect));
 if TObject(Effect^.AudioEffectPtr) is TBasicVSTModule
  then TBasicVSTModule(Effect^.AudioEffectPtr).HostCallSetParameter(Index, Value);
end;

{$IFNDEF UseAudioEffectPtr}
function DispatchEffectFuncUserPtr(Effect: PVSTEffect; OpCode: TDispatcherOpCode; const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr; cdecl;
begin
 if Assigned(Effect) and
  (TObject(Effect^.User) is TBasicVSTModule)
   then Result := TBasicVSTModule(Effect^.User).HostCallDispatchEffect(OpCode, Index, Value, ptr, opt)
 else Result := 0;
end;

function GetParameterFuncUserPtr(const Effect: PVSTEffect; const Index: Integer): Single; cdecl;
begin
 Assert(Assigned(Effect));
 if TObject(Effect^.User) is TBasicVSTModule
  then Result := TBasicVSTModule(Effect^.User).HostCallGetParameter(Index)
  else Result := 0;
end;

procedure SetParameterFuncUserPtr(const Effect: PVSTEffect; const Index: Integer; const Value: Single); cdecl;
begin
 Assert(Assigned(Effect));
 if TObject(Effect^.User) is TBasicVSTModule
  then TBasicVSTModule(Effect^.User).HostCallSetParameter(Index, Value);
end;
{$ENDIF}

{$IFDEF FPC}
{$DEFINE PUREPASCAL}
{$ENDIF}

procedure ProcessFuncCheck(const Effect: PVSTEffect; const Inputs, Outputs: PPSingle; const SampleFrames: Integer); cdecl;
begin
 if Assigned(Effect) then
  {$IFDEF UseAudioEffectPtr}
  if Addr(Effect^.Process) = Addr(ProcessFuncAudioEffectPtr)
   then ProcessFuncAudioEffectPtr(Effect, Inputs, Outputs, SampleFrames);
  {$ELSE}
  if Addr(Effect^.Process) = Addr(ProcessFuncUserPtr)
   then ProcessFuncUserPtr(Effect, Inputs, Outputs, SampleFrames);
  {$ENDIF}
end;

procedure Process32ReplacingFuncCheck(const Effect: PVSTEffect; const Inputs, Outputs: PPSingle; const SampleFrames: Integer); cdecl;
begin
 if Assigned(Effect) then
  {$IFDEF UseAudioEffectPtr}
  if Addr(Effect^.Process32Replacing) = Addr(Process32ReplacingFuncAudioEffectPtr)
   then Process32ReplacingFuncAudioEffectPtr(Effect, Inputs, Outputs, SampleFrames);
  {$ELSE}
  if Addr(Effect^.Process32Replacing) = Addr(Process32ReplacingFuncUserPtr)
   then Process32ReplacingFuncUserPtr(Effect, Inputs, Outputs, SampleFrames);
  {$ENDIF}
end;

procedure Process64ReplacingFuncCheck(const Effect: PVSTEffect; const Inputs, Outputs: PPDouble; const SampleFrames: Integer); cdecl;
begin
 if Assigned(Effect) then
  {$IFDEF UseAudioEffectPtr}
  if Addr(Effect^.Process64Replacing) = Addr(Process64ReplacingFuncAudioEffectPtr)
   then Process64ReplacingFuncAudioEffectPtr(Effect, Inputs, Outputs, SampleFrames);
  {$ELSE}
  if Addr(Effect^.Process64Replacing) = Addr(Process64ReplacingFuncUserPtr)
   then Process64ReplacingFuncUserPtr(Effect, Inputs, Outputs, SampleFrames);
  {$ENDIF}
end;

procedure ProcessFuncAudioEffectPtr(const Effect: PVSTEffect; const Inputs, Outputs: PPSingle; const SampleFrames: Integer); cdecl;
{$IFDEF PUREPASCAL}
begin
 // check consistency
 if not Assigned(Effect) or (SampleFrames = 0) or (Inputs = nil) or (Outputs = nil)
  then Exit;

 if TObject(Effect^.AudioEffectPtr) is TBasicVSTModule
  then TBasicVSTModule(Effect^.AudioEffectPtr).HostCallProcess(Inputs, Outputs, SampleFrames);
end;
{$ELSE}
asm
  push ebx

  // test SampleFrames <> 0
  mov eax, SampleFrames
  test eax, eax
  jz @end

  // test Effect <> 0
  mov ebx, Effect
  test ebx, ebx
  jz @end

  // test Effect^.AudioEffectPtr <> 0
  mov ebx, [ebx + $40]
  test ebx, ebx
  jz @end

  // test Outputs <> 0
  mov ecx, [Outputs]
  test ecx, ecx
  jz @end

  // test Inputs <> 0
  mov edx, [Inputs]
  test edx, edx
  jz @end

  // push SampleFrames on stack
  push eax
  mov eax, ebx
  mov ebx, [eax]

  // call HostCallProcess
  call dword ptr [ebx + CHostCallProcessOffset] // [TBasicVSTModule(ebx).HostCallProcess]

 @end:
  pop ebx
end;
{$ENDIF}

procedure Process32ReplacingFuncAudioEffectPtr(const Effect: PVSTEffect; const Inputs, Outputs: PPSingle; const SampleFrames: Integer); cdecl;
{$IFDEF PUREPASCAL}
begin
 // check consistency
 if not Assigned(Effect) or (SampleFrames = 0) or (Inputs = nil) or (Outputs = nil)
  then Exit;

 if TObject(Effect^.AudioEffectPtr) is TBasicVSTModule
  then TBasicVSTModule(Effect^.AudioEffectPtr).HostCallProcess32Replacing(Inputs, Outputs, SampleFrames);
end;
{$ELSE}
asm
  push ebx

  // test SampleFrames <> 0
  mov eax, SampleFrames
  test eax, eax
  jz @end

  // test Effect <> 0
  mov ebx, Effect
  test ebx, ebx
  jz @end

  // test Effect^.AudioEffectPtr <> 0
  mov ebx, [ebx + $40]
  test ebx, ebx
  jz @end

  // test Outputs <> 0
  mov ecx, [Outputs]
  test ecx, ecx
  jz @end

  // test Inputs <> 0
  mov edx, [Inputs]
  test edx, edx
  jz @end

  // push SampleFrames on stack
  push eax
  mov eax, ebx
  mov ebx, [eax]

  // call HostCallProcess32Replacing (damn hack!!!)
  call dword ptr [ebx + CHostCallProcess32ReplacingOffset] // [TBasicVSTModule(ebx).HostCallProcess32Replacing]

 @end:
  pop ebx
end;
{$ENDIF}

procedure Process64ReplacingFuncAudioEffectPtr(const Effect: PVSTEffect;
  const Inputs, Outputs: PPDouble; const SampleFrames: Integer); cdecl;
{$IFDEF PUREPASCAL}
begin
 // check consistency
 if not Assigned(Effect) or (SampleFrames = 0) or (Inputs = nil) or (Outputs = nil)
  then Exit;

 if TObject(Effect^.AudioEffectPtr) is TBasicVSTModule
  then TBasicVSTModule(Effect^.AudioEffectPtr).HostCallProcess64Replacing(Inputs, Outputs, SampleFrames);
end;
{$ELSE}
asm
  push ebx

  // test SampleFrames <> 0
  mov eax, SampleFrames
  test eax, eax
  jz @end

  // test Effect <> 0
  mov ebx, Effect
  test ebx, ebx
  jz @end

  // test Effect^.AudioEffectPtr <> 0
  mov ebx, [ebx + $40]
  test ebx, ebx
  jz @end

  // test Outputs <> 0
  mov ecx, [Outputs]
  test ecx, ecx
  jz @end

  // test Inputs <> 0
  mov edx, [Inputs]
  test edx, edx
  jz @end

  // push SampleFrames on stack
  push eax
  mov eax, ebx
  mov ebx, [eax]

  // call HostCallProcess64Replacing (damn hack!!!)
  call dword ptr [ebx + CHostCallProcess64ReplacingOffset] // [TBasicVSTModule(ebx).HostCallProcess64Replacing]

 @end:
  pop ebx
end;
{$ENDIF}

{$IFNDEF UseAudioEffectPtr}
procedure ProcessFuncUserPtr(const Effect: PVSTEffect; const Inputs, Outputs: PPSingle; const SampleFrames: Integer); cdecl;
{$IFDEF PUREPASCAL}
begin
 // check consistency
 if not Assigned(Effect) or (SampleFrames = 0) or (Inputs = nil) or (Outputs = nil)
  then Exit;

 if TObject(Effect^.User) is TBasicVSTModule
  then TBasicVSTModule(Effect^.User).HostCallProcess(Inputs, Outputs, SampleFrames);
end;
{$ELSE}
asm
  push ebx

  // test SampleFrames <> 0
  mov eax, SampleFrames
  test eax, eax
  jz @end

  // test Effect <> 0
  mov ebx, Effect
  test ebx, ebx
  jz @end

  // test Effect^.User <> 0
  mov ebx, [ebx + $44]
  test ebx, ebx
  jz @end

  // test Outputs <> 0
  mov ecx, [Outputs]
  test ecx, ecx
  jz @end

  // test Inputs <> 0
  mov edx, [Inputs]
  test edx, edx
  jz @end

  // push SampleFrames on stack
  push eax
  mov eax, ebx
  mov ebx, [eax]

  // call HostCallProcess
  call dword ptr [ebx + CHostCallProcessOffset] // [TBasicVSTModule(ebx).HostCallProcess]

 @end:
  pop ebx
end;
{$ENDIF}

procedure Process32ReplacingFuncUserPtr(const Effect: PVSTEffect; const Inputs, Outputs: PPSingle; const SampleFrames: Integer); cdecl;
{$IFDEF PUREPASCAL}
begin
 // check consistency
 if not Assigned(Effect) or (SampleFrames = 0) or (Inputs = nil) or (Outputs = nil)
  then Exit;

 if TObject(Effect^.User) is TBasicVSTModule
  then TBasicVSTModule(Effect^.User).HostCallProcess32Replacing(Inputs, Outputs, SampleFrames);
end;
{$ELSE}
asm
  push ebx

  // test SampleFrames <> 0
  mov eax, SampleFrames
  test eax, eax
  jz @end

  // test Effect <> 0
  mov ebx, Effect
  test ebx, ebx
  jz @end

  // test Effect^.User <> 0
  mov ebx, [ebx + $44]
  test ebx, ebx
  jz @end

  // test Outputs <> 0
  mov ecx, [Outputs]
  test ecx, ecx
  jz @end

  // test Inputs <> 0
  mov edx, [Inputs]
  test edx, edx
  jz @end

  // push SampleFrames on stack
  push eax
  mov eax, ebx
  mov ebx, [eax]

  call dword ptr [ebx + CHostCallProcess32ReplacingOffset] // [TBasicVSTModule(ebx).HostCallProcess32Replacing]

 @end:
  pop ebx
end;
{$ENDIF}

procedure Process64ReplacingFuncUserPtr(const Effect: PVSTEffect; const Inputs, Outputs: PPDouble; const SampleFrames: Integer); cdecl;
{$IFDEF PUREPASCAL}
begin
 // check consistency
 if not Assigned(Effect) or (SampleFrames = 0) or (Inputs = nil) or (Outputs = nil)
  then Exit;

 if TObject(Effect^.User) is TBasicVSTModule
  then TBasicVSTModule(Effect^.User).HostCallProcess64Replacing(Inputs, Outputs, SampleFrames);
end;
{$ELSE}
asm
  push ebx

  // test SampleFrames <> 0
  mov eax, SampleFrames
  test eax, eax
  jz @end

  // test Effect <> 0
  mov ebx, Effect
  test ebx, ebx
  jz @end

  // test Effect^.User <> 0
  mov ebx, [ebx + $44]
  test ebx, ebx
  jz @end

  // test Outputs <> 0
  mov ecx, [Outputs]
  test ecx, ecx
  jz @end

  // test Inputs <> 0
  mov edx, [Inputs]
  test edx, edx
  jz @end

  // push SampleFrames on stack
  push eax
  mov eax, ebx
  mov ebx, [eax]

  // call HostCallProcess64Replacing (damn hack!!!)
  call dword ptr [ebx + CHostCallProcess64ReplacingOffset] // [TBasicVSTModule(ebx).HostCallProcess64Replacing]

 @end:
  pop ebx
end;
{$ENDIF}
{$ENDIF}

// Dummy Functions

function GetParameterFuncDummy(const Effect: PVSTEffect; const Index: Integer): Single; cdecl;
begin
 Result := 0;
end;

procedure SetParameterFuncDummy(const Effect: PVSTEffect; const Index: Integer; const Value: Single); cdecl;
begin
end;

procedure ProcessFuncDummy(const Effect: PVSTEffect; const Inputs, Outputs: Pointer; const SampleFrames: Integer); cdecl;
begin
end;


// WinAmp Stuff

function Init(const WinAmpDSPModule: PWinAmpDSPModule): Integer; cdecl;
begin
 // make sure a pointer to the TWinAmpDSPModule exists
 if not Assigned(WinAmpDSPModule) then
  begin
   Result := 1;
   Exit;
  end;

 // assert that no other instance exists already
 Assert(WinAmpDSPModule^.UserData = nil);

 try
  // instanciate TWinAmpObject
  WinAmpDSPModule^.UserData := WAVstModule.Create(Application);
  TBasicVSTModule(WinAmpDSPModule^.UserData).FWinAmpDSPModule := WinAmpDSPModule;
  WinAmpDSPModule^.ModifySamples := ModifySamples;
  Result := 0;
 except
  Result := 1;
 end;
end;

procedure Config(const WinAmpDSPModule: PWinAmpDSPModule); cdecl;
begin
 // assert that a pointer to the TWinAmpDSPModule exists
 Assert(Assigned(WinAmpDSPModule));

 // open config dialog
 if Assigned(WinAmpDSPModule^.UserData) then
  try
   TBasicVSTModule(WinAmpDSPModule^.UserData).WinAmpConfig;
  except
  end;
end;

function ModifySamples(const WinAmpDSPModule: PWinAmpDSPModule;
  const Samples: Pointer; const SampleFrames, BitPerSample, ChannelCount,
  SampleRate: Integer): Integer; cdecl;
begin
 Result := SampleFrames;

 // make sure a pointer to the TWinAmpDSPModule exists
 if not Assigned(WinAmpDSPModule) then Exit;

 // make sure a TWinAmpObject instance exists
 if not Assigned(WinAmpDSPModule^.UserData) then Exit;

 // call the objects 'ModifySamples'
 Result := TBasicVSTModule(WinAmpDSPModule^.UserData).WinAmpModifySamples(Samples,
   SampleFrames, BitPerSample, ChannelCount, SampleRate);
end;

function ModifySamplesDummy(const WinAmpDSPModule: PWinAmpDSPModule;
  const Samples: Pointer; const SampleFrames, BitPerSample, ChannelCount,
  SampleRate: Integer): Integer; cdecl;
begin
 Result := SampleFrames;
end;

procedure Quit(const WinAmpDSPModule: PWinAmpDSPModule);
begin
 // assert that a pointer to the TWinAmpDSPModule exists
 Assert(Assigned(WinAmpDSPModule));
 try
  WinAmpDSPModule^.ModifySamples := ModifySamplesDummy;
  sleep(5);
  TBasicVSTModule(WinAmpDSPModule^.UserData).WinAmpQuit;
 finally
  FreeAndNil(WinAmpDSPModule^.UserData);
 end;
end;

initialization
  GVstInstanceList := TObjectList.Create(False);

finalization
  FreeAndNil(GVstInstanceList);

end.
