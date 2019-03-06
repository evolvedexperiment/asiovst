{******************************************************************************}
{                                                                              }
{  Version: MPL 1.1 or LGPL 2.1 with linking exception                         }
{                                                                              }
{  The contents of this file are subject to the Mozilla Public License         }
{  Version 1.1 (the "License"); you may not use this file except in            }
{  compliance with the License. You may obtain a copy of the License at        }
{  http://www.mozilla.org/MPL/                                                 }
{                                                                              }
{  Software distributed under the License is distributed on an "AS IS"         }
{  basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the     }
{  License for the specific language governing rights and limitations under    }
{  the License.                                                                }
{                                                                              }
{  Alternatively, the contents of this file may be used under the terms of     }
{  the Free Pascal modified version of the GNU Lesser General Public           }
{  License Version 2.1 (the "FPC modified LGPL License"), in which case the    }
{  provisions of this license are applicable instead of those above.           }
{  Please see the file LICENSE.txt for additional information concerning       }
{  this license.                                                               }
{                                                                              }
{  The code is part of the Delphi ASIO & VST Project                           }
{                                                                              }
{  The initial developer of this code is Christian-W. Budde                    }
{                                                                              }
{  Portions created by Christian-W. Budde are Copyright (C) 2003-2019          }
{  by Christian-W. Budde. All Rights Reserved.                                 }
{                                                                              }
{******************************************************************************}

unit DAV_OpenAsio;

// OpenASIO: import unit for OpenAsio.dll
//------------------------------------------------------------------------------
// Martin Fay (martin@martinfay.com), 2001
//
// Derived from iasiodrv.h (c) 1997 - 1999, Steinberg Soft- und Hardware GmbH
//
// Requires Asio.pas from the Delphi ASIO SDK translation, but you'll need that
//  anyway ;)
//
// Purpose:
// - The IASIO COM interface used by ASIO drivers has no explicitly declared
//    calling convention for the functions. This has resulted in the interface
//    being unusable from some compilers (i.e. Delphi).
// - OpenAsio provides a simple wrapper dll which exposes an interface where
//    this problem has been corrected.
//
// Usage:
// - Place OpenAsio.dll in the same directory as your application
// - Call OpenAsioLoaded to determine if the dll has loaded correctly
// - COM object creation:
//    "if OpenAsioCreate(MyAsioCLSID, MyAsioDriver) then"
// - Refer to ASIO2 documentation for usage of the interface functions
//-----------------------------------------------------------------------------------------------------------

interface

{$I DAV_Compiler.inc}

uses
  Windows, ActiveX, DAV_ASIO;

type
  IOpenASIO = interface(IUnknown)
    function Init(sysHandle: HWnd): TASIOError; stdcall;
    procedure GetDriverName(Name: PAnsiChar); stdcall;
    function GetDriverVersion: LongInt; stdcall;
    procedure GetErrorMessage(ErrorString: PAnsiChar); stdcall;
    function Start: TASIOError; stdcall;
    function Stop: TASIOError; stdcall;
    function GetChannels(out numInputChannels, numOutputChannels: LongInt): TASIOError; stdcall;
    function GetLatencies(out InputLatency, OutputLatency: LongInt): TASIOError; stdcall;
    function GetBufferSize(out MinSize, MaxSize, PreferredSize, Granularity: LongInt): TASIOError; stdcall;
    function CanSampleRate(SampleRate: TASIOSampleRate): TASIOError; stdcall;
    function GetSampleRate(out SampleRate: TASIOSampleRate): TASIOError; stdcall;
    function SetSampleRate(SampleRate: TASIOSampleRate): TASIOError; stdcall;
    function GetClockSources(Clocks: PASIOClockSource; out numSources: LongInt): TASIOError; stdcall;
    function SetClockSource(Reference: LongInt): HResult; stdcall;
    function GetSamplePosition(out sPos: TASIOSamples; out tStamp: TASIOTimeStamp): TASIOError; stdcall;
    function GetChannelInfo(out Info: TASIOChannelInfo): TASIOError; stdcall;
    function CreateBuffers(BufferInfos: PAsioBufferInfos; NumChannels, BufferSize: LongInt; const Callbacks: TASIOCallbacks): TASIOError; stdcall;
    function DisposeBuffers: TASIOError; stdcall;
    function ControlPanel: TASIOError; stdcall;
    function Future(Selector: LongInt; Optional: Pointer): TASIOError; stdcall;
    function OutputReady: TASIOError; stdcall;
  end;

function OpenAsioLoaded: Boolean;
function OpenAsioCreate(const AsioCLSID: TClsId; var OpenASIODriver: IOpenASIO): boolean;

var
  OpenAsioDll: HModule;

implementation

var
  CreateOpenAsio: function(const AsioCLSID: TClsId; var OpenASIODriver: IOpenASIO): HResult; stdcall;

function OpenAsioLoaded: Boolean;
begin
  Result := (OpenAsioDll <> 0);
end;

function OpenAsioCreate(const AsioCLSID: TClsId; var OpenASIODriver: IOpenASIO): boolean;
begin
  if OpenAsioLoaded then
    Result := Succeeded(CreateOpenAsio(AsioCLSID, OpenASIODriver))
  else
    Result := False;
end;

initialization
  OpenAsioDll := LoadLibrary('OpenAsio.dll');
{$IFNDEF FPC}
  if OpenAsioLoaded then
    CreateOpenAsio := GetProcAddress(OpenAsioDll, 'CreateOpenAsio');
{$ENDIF}

finalization
 FreeLibrary(OpenAsioDll);

end.
