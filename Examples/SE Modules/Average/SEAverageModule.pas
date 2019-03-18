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

unit SEAverageModule;

interface

uses
  DAV_Types, DAV_SECommon, DAV_SEModule;

type
  // define some constants to make referencing in/outs clearer
  TSEAveragePins = (pinOutput, pinVarInput);

  TSEAverageModule = class(TSEModuleBase)
  private
    FOutput: PDAVSingleFixedArray;
    FStaticCount: Integer;
    FDynamicPlugsCount: Integer;
    FDynamicPlugs: array of PDAVSingleFixedArray;
  protected
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
    procedure Open; override;
  public
    constructor Create(SEAudioMaster: TSE2AudioMasterCallback;
      HostPtr: Pointer); override;
    destructor Destroy; override;

    class procedure GetModuleProperties(Properties: PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer;
      Properties: PSEPinProperties): Boolean; override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer);
    procedure SubProcessStatic(const BufferOffset, SampleFrames: Integer);
  end;

implementation

constructor TSEAverageModule.Create(SEAudioMaster: TSE2AudioMasterCallback;
  HostPtr: Pointer);
begin
  inherited Create(SEAudioMaster, HostPtr);
  FDynamicPlugs := nil;
end;

destructor TSEAverageModule.Destroy;
var
  i: Integer;
begin
  // This is where you free any memory/resources your module has created
  for i := 0 to Length(FDynamicPlugs) - 1 do
    Dispose(FDynamicPlugs[i]);
  inherited;
end;

procedure TSEAverageModule.Open;
var
  i: Integer;
begin
  // call the base class
  inherited Open;

  // choose which function is used to process audio
  OnProcess := SubProcess;

  // to work out how many 'dynamic' plugs the module has..
  // Ask host how many input plugs this module actually has,
  // then subtract the number of 'regular' plugs
  FDynamicPlugsCount := CallHost(SEAudioMasterGetTotalPinCount) -
    Integer(pinOutput);

  if FDynamicPlugsCount > 0 then
  begin
    // allocate an array to hold pointers to all the input buffers
    SetLength(FDynamicPlugs, FDynamicPlugsCount);

    // ask the host for a pointer to each input buffer
    // store them in the array
    for i := 0 to FDynamicPlugsCount - 1 do
      FDynamicPlugs[i] := Pin[i + Integer(pinOutput)].VariableAddress;
    // (float*)CallHost(SEAudioMasterGetPinVarAddress, i + PN_INPUT1);
  end;
end;

// The most important part, processing the audio
procedure TSEAverageModule.SubProcess(const BufferOffset,
  SampleFrames: Integer);
var
  Total: Single;
  Scaler: Single;
  Sample: Integer;
  PlugNo: Integer;
begin
  // To calculate an average, add up all the inputs, then divide by the total
  // number of inputs dividing by a number is the same as multiplying by
  // 1/number
  // I've pre-calculated this value to make the calculateion a little faster.

  if (FDynamicPlugsCount > 0) then
    Scaler := 1 / FDynamicPlugsCount
  else
    Scaler := 1; // if there are zero inputs, just set scaler to one.

  for Sample := 0 to SampleFrames - 1 do
  begin
    // step though each input, calculating the average
    Total := 0;
    for PlugNo := 0 to FDynamicPlugsCount - 1 do
      Total := Total + FDynamicPlugs[PlugNo, Sample + BufferOffset];

    FOutput[Sample + BufferOffset] := Total * Scaler;
  end;
end;

procedure TSEAverageModule.SubProcessStatic(const BufferOffset,
  SampleFrames: Integer);
begin
  SubProcess(BufferOffset, SampleFrames);
  FStaticCount := FStaticCount - SampleFrames;
  if FStaticCount <= 0 then
    CallHost(SEAudioMasterSleepMode);
end;

// describe your module
class procedure TSEAverageModule.GetModuleProperties
  (Properties: PSEModuleProperties);
begin
  with Properties^ do
  begin
    // describe the plugin, this is the name the end-user will see.
    Name := 'Averager';

    // return a unique string 32 characters max
    // if posible include manufacturer and plugin identity
    // this is used internally by SE to identify the plug.
    // No two plugs may have the same id.
    ID := 'Synthedit Averager';

    // Info, may include Author, Web page whatever
    About := 'Christian-W. Budde';

    SdkVersion := CSeSdkVersion;
  end;
end;

// describe the pins (plugs and parameters)
function TSEAverageModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
  Result := True;
  case TSEAveragePins(index) of // !!TODO!! list your in / out plugs
    pinOutput:
      with Properties^ do
      begin
        Name := 'Output';
        VariableAddress := @FOutput;
        Direction := drOut;
        Datatype := dtFSample;
      end;
    pinVarInput:
      with Properties^ do // this plug automatically duplicates itself
      begin // it must be the last plug in the list
        name := 'Input';
        Direction := drIn;
        Datatype := dtFSample;
        DefaultValue := '0';
        Flags := [iofAutoDuplicate, iofCustomisable];
      end;
  else
    Result := False;
    // host will ask for plugs 0,1,2,3 etc. return false to signal when done
  end;
end;

// An input plug has changed value
procedure TSEAverageModule.PlugStateChange(const CurrentPin: TSEPin);
var
  i: Integer;
  InState: TSEStateType;
  OutState: TSEStateType;
begin
  // query the 'state of the input plugs...
  // ST_RUN     = Normal Streaming Audio        (e.g. from an oscillator)
  // ST_STATIC  = Fixed, unchanging input value (e.g. a slider at rest)

  // we need to pass on the state of this module's output signal
  // it depends on the inputs.  Choose the 'highest'..
  OutState := stStatic;

  for i := 0 to FDynamicPlugsCount - 1 do
  begin
    InState := Pin[i].Status;
    if InState > OutState then
      OutState := InState;
  end;

  // 'transmit' this modules new FOutput status to next module 'downstream'
  Pin[1].TransmitStatusChange(SampleClock, OutState);

  // setup 'sleep mode' or not
  if (OutState < stRun) then
  begin
    FStaticCount := BlockSize;
    OnProcess := SubProcessStatic;
  end
  else
    OnProcess := SubProcess;

  inherited;
end;

end.
