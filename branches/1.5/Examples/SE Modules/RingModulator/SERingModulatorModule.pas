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
{   SynthEdit is witten by Jef McClintock (see http://www.synthedit.com/       }
{                                                                              }
{******************************************************************************}

unit SERingModulatorModule;

interface

uses
  DAV_Types, DAV_SECommon, DAV_SEModule, DAV_DspRingModulator;

type
  // define some constants to make referencing in/outs clearer
  TSERingModulatorPins = (pinInput, pinOutput, pinFrequency);

  TCustomSERingModulatorModule = class(TSEModuleBase)
  private
    FInputBuffer: PDAVSingleFixedArray; // pointer to circular buffer of samples
    FOutputBuffer: PDAVSingleFixedArray;
    FStaticCount: Integer;
    procedure ChooseProcess;
    procedure SubProcessStatic(const BufferOffset, SampleFrames: Integer);
  protected
    FRingModulator: TAutoRingModulator32;
    procedure Open; override;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
    procedure SampleRateChanged; override;
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback;
      Reserved: Pointer); override;
    destructor Destroy; override;

    class procedure GetModuleProperties(Properties
      : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer;
      Properties: PSEPinProperties): Boolean; override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer);
      virtual; abstract;
  end;

  TSERingModulatorStaticModule = class(TCustomSERingModulatorModule)
  private
    FFrequency: Single;
  protected
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    class procedure GetModuleProperties(Properties
      : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer;
      Properties: PSEPinProperties): Boolean; override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); override;
  end;

  TSERingModulatorControllableModule = class(TSERingModulatorStaticModule)
  public
    class procedure GetModuleProperties(Properties
      : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer;
      Properties: PSEPinProperties): Boolean; override;
  end;

implementation

uses
  SysUtils;

{ TCustomSERingModulatorModule }

constructor TCustomSERingModulatorModule.Create(SEAudioMaster
  : TSE2audioMasterCallback; Reserved: Pointer);
begin
  inherited Create(SEAudioMaster, Reserved);
  FRingModulator := TAutoRingModulator32.Create
end;

destructor TCustomSERingModulatorModule.Destroy;
begin
  FreeAndNil(FRingModulator);
  inherited;
end;

procedure TCustomSERingModulatorModule.Open;
begin
  inherited Open;

  // choose which function is used to process audio
  OnProcess := SubProcess;
end;

// The most important part, processing the audio
procedure TCustomSERingModulatorModule.SampleRateChanged;
begin
  inherited;
  FRingModulator.SampleRate := SampleRate;
end;

procedure TCustomSERingModulatorModule.SubProcessStatic(const BufferOffset,
  SampleFrames: Integer);
begin
  SubProcess(BufferOffset, SampleFrames);
  FStaticCount := FStaticCount - SampleFrames;
  if FStaticCount <= 0 then
    CallHost(SEAudioMasterSleepMode);
end;

procedure TCustomSERingModulatorModule.ChooseProcess;
begin
  if Pin[Integer(pinInput)].Status = stRun then
    OnProcess := SubProcess
  else
  begin
    FStaticCount := BlockSize;
    OnProcess := SubProcessStatic;
  end;
end;

// describe your module
class procedure TCustomSERingModulatorModule.GetModuleProperties
  (Properties: PSEModuleProperties);
begin
  with Properties^ do
  begin
    // Info, may include Author, Web page whatever
    About := 'by Christian-W. Budde';
    SDKVersion := CSeSdkVersion;
  end;
end;

// describe the pins (plugs)
function TCustomSERingModulatorModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
  Result := True;
  case TSERingModulatorPins(index) of
    pinInput:
      with Properties^ do
      begin
        Name := 'Input';
        VariableAddress := @FInputBuffer;
        Direction := drIn;
        Flags := [iofLinearInput];
        Datatype := dtFSample;
        DefaultValue := '0';
      end;
    pinOutput:
      with Properties^ do
      begin
        Name := 'Output';
        VariableAddress := @FOutputBuffer;
        Direction := drOut;
        Datatype := dtFSample;
      end;
  else
    Result := False;
    // host will ask for plugs 0,1,2,3 etc. return false to signal when done
  end;
end;

// An input plug has changed value
procedure TCustomSERingModulatorModule.PlugStateChange(const CurrentPin
  : TSEPin);
begin
  inherited;
  case TSERingModulatorPins(CurrentPin.PinID) of
    pinInput:
      begin
        ChooseProcess;
        Pin[1].TransmitStatusChange(SampleClock, Pin[0].Status);
      end;
  end;
end;

{ TSERingModulatorStaticModule }

// describe your module
class procedure TSERingModulatorStaticModule.GetModuleProperties
  (Properties: PSEModuleProperties);
begin
  inherited GetModuleProperties(Properties);
  with Properties^ do
  begin
    // describe the plugin, this is the name the end-user will see.
    Name := 'Ring Modulator (static)';

    // return a unique string 32 characters max
    // if posible include manufacturer and plugin identity
    // this is used internally by SE to identify the plug.
    // No two plugs may have the same id.
    ID := 'DAV Ring Modulator (static)';
  end;
end;

procedure TSERingModulatorStaticModule.SubProcess(const BufferOffset,
  SampleFrames: Integer);
var
  Inp: PDAVSingleFixedArray;
  Outp: PDAVSingleFixedArray;
  Sample: Integer;
begin
  // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
  Inp := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
  Outp := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);

  for Sample := 0 to SampleFrames - 1 do
    Outp^[Sample] := FRingModulator.ProcessSample32(Inp^[Sample]);
end;

// describe the pins (plugs)
function TSERingModulatorStaticModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
  Result := inherited GetPinProperties(Index, Properties);
  case TSERingModulatorPins(index) of
    pinFrequency:
      with Properties^ do
      begin
        Name := 'Frequency [Hz]';
        VariableAddress := @FFrequency;
        Direction := drParameter;
        Datatype := dtSingle;
        DefaultValue := '100';
        Result := True;
      end;
  end;
end;

// An input plug has changed value
procedure TSERingModulatorStaticModule.PlugStateChange(const CurrentPin
  : TSEPin);
begin
  inherited;
  case TSERingModulatorPins(CurrentPin.PinID) of
    pinFrequency:
      FRingModulator.Frequency := FFrequency;
  end;
end;

{ TSERingModulatorControllableModule }

class procedure TSERingModulatorControllableModule.GetModuleProperties
  (Properties: PSEModuleProperties);
begin
  inherited GetModuleProperties(Properties);
  with Properties^ do
  begin
    // describe the plugin, this is the name the end-user will see.
    Name := 'Ring Modulator';

    // return a unique string 32 characters max
    // if posible include manufacturer and plugin identity
    // this is used internally by SE to identify the plug.
    // No two plugs may have the same id.
    ID := 'DAV Ring Modulator';
  end;
end;

function TSERingModulatorControllableModule.GetPinProperties
  (const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
  Result := inherited GetPinProperties(Index, Properties);
  if TSERingModulatorPins(index) in [pinFrequency] then
    with Properties^ do
      Direction := drIn;
end;

end.
