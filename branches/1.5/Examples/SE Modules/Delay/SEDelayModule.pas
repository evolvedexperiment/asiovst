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
{  Portions created by Christian-W. Budde are Copyright (C) 2003-2012          }
{  by Christian-W. Budde. All Rights Reserved.                                 }
{                                                                              }
{   SynthEdit is witten by Jef McClintock (see http://www.synthedit.com/       }
{                                                                              }
{******************************************************************************}

unit SEDelayModule;

interface

uses
  DAV_Types, DAV_SECommon, DAV_SEModule;

type
  // define some constants to make referencing in/outs clearer
  TSEDelayPins = (pinInput1, pinInput2, pinOutput, pinDelayTime);

  TSEDelayModule = class(TSEModuleBase)
  private
    FInput1Buffer: PDAVSingleFixedArray;
    // pointer to circular buffer of samples
    FInput2Buffer: PDAVSingleFixedArray;
    FOutputBuffer: PDAVSingleFixedArray;
    FBuffer: PSEFloatSampleFixedArray;
    FCount: Integer;
    FBufferSize: Integer;
    FDelay_ms: Single;
  protected
    procedure Open; override;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback;
      Reserved: Pointer); override;
    destructor Destroy; override;

    class procedure GetModuleProperties(Properties
      : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer;
      Properties: PSEPinProperties): Boolean; override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer);
    procedure CreateBuffer;
  end;

implementation

constructor TSEDelayModule.Create(SEAudioMaster: TSE2audioMasterCallback;
  Reserved: Pointer);
begin
  inherited Create(SEAudioMaster, Reserved);
  FBuffer := nil;
end;

destructor TSEDelayModule.Destroy;
begin
  // This is where you free any memory/resources your module has created
  Dispose(FBuffer);
  inherited;
end;

procedure TSEDelayModule.Open;
begin
  inherited Open;

  // choose which function is used to process audio
  OnProcess := SubProcess;

  CreateBuffer;

  // let 'downstream' modules know audio data is coming
  Pin[Integer(pinOutput)].TransmitStatusChange(SampleClock, stRun);
end;

procedure TSEDelayModule.CreateBuffer;
begin
  FBufferSize := Round(SampleRate * FDelay_ms * 0.001);
  if (FBufferSize < 1) then
    FBufferSize := 1;

  if (FBufferSize > SampleRate * 10) // limit to 10 s sample
  then
    FBufferSize := Round(SampleRate * 10);

  ReallocMem(FBuffer, FBufferSize * SizeOf(TSEFLoatSample));

  // clear buffer
  FillChar(FBuffer^, FBufferSize * SizeOf(TSEFLoatSample), 0); // clear buffer

  // ensure we arn't accessing data outside buffer
  FCount := 0;
end;

// The most important part, processing the audio
procedure TSEDelayModule.SubProcess(const BufferOffset, SampleFrames: Integer);
var
  Inp: PDAVSingleFixedArray;
  Fbk: PDAVSingleFixedArray;
  Out1: PDAVSingleFixedArray;
  Temp: Single;
  Sample: Integer;
begin
  // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
  Inp := PDAVSingleFixedArray(@FInput1Buffer[BufferOffset]);
  Fbk := PDAVSingleFixedArray(@FInput2Buffer[BufferOffset]);
  Out1 := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);

  for Sample := 0 to SampleFrames - 1 do
  // sampleFrames = how many samples to process (can vary). repeat (loop) that many times
  begin
    // do the actual processing (multiplying the two input samples together)
    Out1^[Sample] := FBuffer[FCount];
    Temp := Inp[Sample] + Out1^[Sample] * Fbk[Sample];
    KillDenormal(Temp);
    FBuffer[FCount] := Temp;
    FCount := FCount mod FBufferSize;
  end;
end;

// describe your module
class procedure TSEDelayModule.GetModuleProperties
  (Properties: PSEModuleProperties);
begin
  with Properties^ do
  begin
    // describe the plugin, this is the name the end-user will see.
    Name := 'Delay Example';

    // return a unique string 32 characters max
    // if posible include manufacturer and plugin identity
    // this is used internally by SE to identify the plug.
    // No two plugs may have the same id.
    ID := 'Synthedit Delay Example';

    // Info, may include Author, Web page whatever
    About := 'by Christian-W. Budde';
    SDKVersion := CSeSdkVersion;
  end;
end;

// describe the pins (plugs)
function TSEDelayModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
  Result := True;
  case TSEDelayPins(index) of
    // typical input plug (inputs are listed first)
    pinInput1:
      with Properties^ do
      begin
        Name := 'Input';
        VariableAddress := @FInput1Buffer;
        Direction := drIn;
        Datatype := dtFSAMPLE;
        DefaultValue := '0';
      end;
    pinInput2:
      with Properties^ do
      begin
        Name := 'Feedback';
        VariableAddress := @FInput2Buffer;
        Direction := drIn;
        Datatype := dtFSAMPLE;
        DefaultValue := '5';
      end;

    // typical output plug
    pinOutput:
      with Properties^ do
      begin
        Name := 'Output';
        VariableAddress := @FOutputBuffer;
        Direction := drOut;
        Datatype := dtFSAMPLE;
      end;
    pinDelayTime:
      with Properties^ do
      begin
        Name := 'Max Delay ms';
        VariableAddress := @FDelay_ms;
        Direction := drParameter;
        Datatype := dtSingle;
        DefaultValue := '1000';
      end;
  else
    Result := False;
    // host will ask for plugs 0,1,2,3 etc. return false to signal when done
  end;
end;

// this routine is called whenever an input changes status.
// e.g when the user changes a module's parameters,
// or when audio stops/starts streaming into a pin
procedure TSEDelayModule.PlugStateChange(const CurrentPin: TSEPin);
begin
  // has user altered delay time parameter?
  if (CurrentPin.PinID = Integer(pinDelayTime)) then
    CreateBuffer; // re-create the audio buffer
  inherited;
end;

end.
