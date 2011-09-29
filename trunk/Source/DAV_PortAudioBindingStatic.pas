unit DAV_PortAudioBindingStatic;

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
//  The initial developer of this code is Christian-W. Budde                  //
//                                                                            //
//  Portions created by Christian-W. Budde are Copyright (C) 2011             //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I DAV_Compiler.inc}

uses
  Types, DAV_PortAudioTypes;

const
{$IF Defined(MSWINDOWS)}
  LibName = 'portaudio.dll';
{$ELSEIF Defined(MACOS)}
  // this is for portaudio version 19
  LibName = 'libportaudio.2.dylib';
  {$IFDEF FPC}
  {$LINKLIB libportaudio.2}
  {$ENDIF}
{$ELSEIF Defined(UNIX)}
  LibName = 'libportaudio.so';
{$IFEND}

function Pa_GetVersion: LongInt; cdecl; external LibName;
function Pa_GetVersionText: PAnsiChar; cdecl; external LibName;
function Pa_GetErrorText(ErrorCode: TPaError): PAnsiChar; cdecl; external LibName;
function Pa_Initialize: TPaError; cdecl; external LibName;
function Pa_Terminate: TPaError; cdecl; external LibName;
function Pa_GetHostApiCount: TPaHostApiIndex; cdecl; external LibName;
function Pa_GetDefaultHostApi: TPaHostApiIndex; cdecl; external LibName;
function Pa_GetHostApiInfo(HostApi: TPaHostApiIndex): PPaHostApiInfo; cdecl; external LibName;
function Pa_HostApiTypeIdToHostApiIndex(HostApiTypeId: TPaHostApiTypeId): TPaHostApiIndex; cdecl; external LibName;
function Pa_HostApiDeviceIndexToDeviceIndex(HostApi: TPaHostApiIndex; HostApiDeviceIndex: LongInt): TPaDeviceIndex; cdecl; external LibName;
function Pa_GetLastHostErrorInfo: PPaHostErrorInfo; cdecl; external LibName;
function Pa_GetDeviceCount: TPaDeviceIndex; cdecl; external LibName;
function Pa_GetDefaultInputDevice: TPaDeviceIndex; cdecl; external LibName;
function Pa_GetDefaultOutputDevice: TPaDeviceIndex; cdecl; external LibName;
function Pa_GetDeviceInfo(Device: TPaDeviceIndex): PPaDeviceInfo; cdecl; external LibName;
function Pa_IsFormatSupported(InputParameters: PPaStreamParameters; OutputParameters: PPaStreamParameters; SampleRate: Double): TPaError; cdecl; external LibName;
function Pa_OpenStream(var Stream: PPaStream; InputParameters: PPaStreamParameters; OutputParameters: PPaStreamParameters; SampleRate: Double; FramesPerBuffer: NativeUInt; StreamFlags: TPaStreamFlags; StreamCallback: PPaStreamCallback; UserData: Pointer): TPaError; cdecl; external LibName;
function Pa_OpenDefaultStream(var Stream: PPaStream; NumInputChannels: LongInt; NumOutputChannels: LongInt; SampleFormat: TPaSampleFormat; SampleRate: Double; FramesPerBuffer: NativeUInt; StreamCallback: PPaStreamCallback; UserData: Pointer): TPaError; cdecl; external LibName;
function Pa_CloseStream (Stream: PPaStream): TPaError; cdecl; external LibName;
function Pa_SetStreamFinishedCallback (Stream: PPaStream; StreamFinishedCallback: PPaStreamFinishedCallback): TPaError; cdecl; external LibName;
function Pa_StartStream(Stream: PPaStream): TPaError; cdecl; external LibName;
function Pa_StopStream(Stream: PPaStream): TPaError; cdecl; external LibName;
function Pa_AbortStream(Stream: PPaStream): TPaError; cdecl; external LibName;
function Pa_IsStreamStopped(Stream: PPaStream): TPaError; cdecl; external LibName;
function Pa_IsStreamActive(Stream: PPaStream): TPaError; cdecl; external LibName;
function Pa_GetStreamInfo(Stream: PPaStream): PPaStreamInfo; cdecl; external LibName;
function Pa_GetStreamTime(Stream: PPaStream): TPaTime; cdecl; external LibName;
function Pa_GetStreamCpuLoad(Stream: PPaStream): Double; cdecl; external LibName;
function Pa_ReadStream(Stream: PPaStream; Buffer: Pointer; Frames: NativeUInt): TPaError; cdecl; external LibName;
function Pa_WriteStream(Stream: PPaStream; Buffer: Pointer; Frames: NativeUInt): TPaError; cdecl; external LibName;
function Pa_GetStreamReadAvailable(Stream: PPaStream): NativeInt; cdecl; external LibName;
function Pa_GetStreamWriteAvailable(Stream: PPaStream): NativeInt; cdecl; external LibName;
function Pa_GetStreamHostApiType(Stream: PPaStream): TPaHostApiTypeId; cdecl; external LibName;
function Pa_GetSampleSize(Format: TPaSampleFormat): TPaError; cdecl; external LibName;
procedure Pa_Sleepe(MSec: Int64); cdecl; external LibName;

implementation

end.
