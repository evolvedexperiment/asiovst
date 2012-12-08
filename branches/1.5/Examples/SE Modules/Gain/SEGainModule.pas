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

unit SEGainModule;

interface

uses
  DAV_Types, DAV_SECommon, DAV_SEModule;

type
  // define some constants to make referencing in/outs clearer
  TSEGainPins = (pinInput1, pinInput2, pinOutput);

  TSEGainModule = class(TSEModuleBase)
  private
    FInput1Buffer: PDAVSingleFixedArray;
    // pointer to circular buffer of samples
    FInput2Buffer: PDAVSingleFixedArray;
    FOutputBuffer: PDAVSingleFixedArray;
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
  end;

implementation

constructor TSEGainModule.Create(SEAudioMaster: TSE2audioMasterCallback;
  Reserved: Pointer);
begin
  inherited Create(SEAudioMaster, Reserved);
end;

destructor TSEGainModule.Destroy;
begin
  // This is where you free any memory/resources your module has created
  inherited;
end;

procedure TSEGainModule.Open;
begin
  inherited Open;

  // choose which function is used to process audio
  OnProcess := SubProcess;
end;

// The most important part, processing the audio
procedure TSEGainModule.SubProcess(const BufferOffset, SampleFrames: Integer);
var
  In1: PDAVSingleFixedArray;
  In2: PDAVSingleFixedArray;
  Out1: PDAVSingleFixedArray;
  Sample: Integer;
begin
  // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
  In1 := PDAVSingleFixedArray(@FInput1Buffer[BufferOffset]);
  In2 := PDAVSingleFixedArray(@FInput2Buffer[BufferOffset]);
  Out1 := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);

  for Sample := 0 to SampleFrames - 1 do
  // sampleFrames = how many samples to process (can vary). repeat (loop) that many times
  begin
    // do the actual processing (multiplying the two input samples together)
    Out1^[Sample] := In1^[Sample] * In2^[Sample];
  end;
end;

// describe your module
class procedure TSEGainModule.GetModuleProperties
  (Properties: PSEModuleProperties);
begin
  with Properties^ do
  begin
    // describe the plugin, this is the name the end-user will see.
    Name := 'Gain Example';

    // return a unique string 32 characters max
    // if posible include manufacturer and plugin identity
    // this is used internally by SE to identify the plug.
    // No two plugs may have the same id.
    ID := 'Synthedit Gain Example';

    // Info, may include Author, Web page whatever
    About := 'by Christian-W. Budde';
    SDKVersion := CSeSdkVersion;
  end;
end;

// describe the pins (plugs)
function TSEGainModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
  Result := True;
  case TSEGainPins(index) of
    // typical input plug (inputs are listed first)
    pinInput1:
      with Properties^ do
      begin
        Name := 'Input';
        VariableAddress := @FInput1Buffer;
        Direction := drIn;
        Datatype := dtFSample;
        DefaultValue := '0';
      end;
    pinInput2:
      with Properties^ do
      begin
        Name := 'Input';
        VariableAddress := @FInput2Buffer;
        Direction := drIn;
        Datatype := dtFSample;
        DefaultValue := '5';
      end;

    // typical output plug
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
  end;;
end;

// An input plug has changed value
procedure TSEGainModule.PlugStateChange(const CurrentPin: TSEPin);
var
  InState: array [0 .. 1] of TSEStateType;
  OutState: TSEStateType;
begin
  // query the 'state of the input plugs...
  // stRun    = Normal Streaming Audio        (e.g. from an oscillator)
  // stStatic = Fixed, unchanging input value (e.g. a slider at rest)
  InState[0] := Pin[Integer(pinInput1)].Status;
  InState[1] := Pin[Integer(pinInput2)].Status;

  // we need to pass on the state of this module's output signal
  // it depends on the inputs...
  OutState := InState[0];
  if InState[1] > OutState then
    OutState := InState[1];

  // if either input zero, tell downstream modules audio has stopped
  if (InState[0] < stRun) and (Pin[Integer(pinInput1)].Value = 0) then
    OutState := stStatic;

  if (InState[1] < stRun) and (Pin[Integer(pinInput2)].Value = 0) then
    OutState := stStatic;

  // 'transmit' new output status to next module 'downstream'
  Pin[Integer(pinOutput)].TransmitStatusChange(SampleClock, OutState);

  inherited;
end;

end.
