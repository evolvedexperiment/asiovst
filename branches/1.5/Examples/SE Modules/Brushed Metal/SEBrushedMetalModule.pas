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

unit SEBrushedMetalModule;

interface

uses
  Windows, Graphics, DAV_Types, DAV_SECommon, DAV_SEModule;

type
  TSEBlob = record
    Size: Integer;
    Data: Pointer;
  end;

  TSEBrushedMetalModule = class(TSEModuleBase)
  protected
    FHue: Single;
    FLuminance: Single;
    FSaturation: Single;
    FAmount: Single;
    FGradient: Single;
    procedure Open; override;
    procedure GuiNotify(AUserMsgID: Integer; ASize: Integer;
      AData: Pointer); override;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback;
      Reserved: Pointer); override;

    class procedure GetModuleProperties(Properties
      : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer;
      Properties: PSEPinProperties): Boolean; override;

    procedure SubProcessSleep(const BufferOffset, SampleFrames: Integer);
  end;

  TSEBrushedMetalExModule = class(TSEBrushedMetalModule)
  public
    class procedure GetModuleProperties(Properties
      : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer;
      Properties: PSEPinProperties): Boolean; override;
  end;

implementation

uses
  DAV_Common;

constructor TSEBrushedMetalModule.Create(SEAudioMaster: TSE2audioMasterCallback;
  Reserved: Pointer);
begin
  inherited Create(SEAudioMaster, Reserved);
  FHue := 0.1;
  FLuminance := 0.1;
  FSaturation := 0.2;
  FAmount := 0.2;
end;

procedure TSEBrushedMetalModule.Open;
begin
  inherited Open;

  // choose which function is used to process audio
  OnProcess := SubProcessSleep;
end;

procedure TSEBrushedMetalModule.PlugStateChange(const CurrentPin: TSEPin);
begin
  inherited;
  case CurrentPin.PinID of
    0:
      FHue := Limit(FHue, 0, 1);
    1:
      FLuminance := Limit(FLuminance, 0, 1);
    2:
      FSaturation := Limit(FSaturation, 0, 1);
    3:
      FAmount := Limit(FAmount, 0, 1);
  end;
end;

// The most important part, processing the audio
procedure TSEBrushedMetalModule.SubProcessSleep(const BufferOffset,
  SampleFrames: Integer);
begin
  CallHost(SEAudioMasterSleepMode);
end;

// describe your module
class procedure TSEBrushedMetalModule.GetModuleProperties
  (Properties: PSEModuleProperties);
begin
  with Properties^ do
  begin
    // describe the plugin, this is the name the end-user will see.
    Name := 'Brushed Metal';

    // return a unique string 32 characters max
    // if posible include manufacturer and plugin identity
    // this is used internally by SE to identify the plug.
    // No two plugs may have the same id.
    ID := 'Synthedit BrushedMetal Example';

    // Info, may include Author, Web page whatever
    About := 'by Christian-W. Budde';

    Flags := [];
    GuiFlags := [gfControlView];
    SdkVersion := CSeSdkVersion;
  end;
end;

// describe the pins (plugs)
function TSEBrushedMetalModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
  Result := True;
  case Index of
    // typical input plug (inputs are listed first)
    0:
      with Properties^ do
      begin
        Name := 'Hue';
        VariableAddress := @FHue;
        Direction := drParameter;
        Datatype := dtSingle;
        Flags := [iofHideWhenLocked, iofUICommunication, iofPatchStore];
        DefaultValue := '0.1';
      end;
    1:
      with Properties^ do
      begin
        Name := 'Saturation';
        VariableAddress := @FSaturation;
        Direction := drParameter;
        Datatype := dtSingle;
        Flags := [iofHideWhenLocked, iofUICommunication, iofPatchStore];
        DefaultValue := '0.1';
      end;
    2:
      with Properties^ do
      begin
        Name := 'Luminance';
        VariableAddress := @FLuminance;
        Direction := drParameter;
        Datatype := dtSingle;
        Flags := [iofHideWhenLocked, iofUICommunication, iofPatchStore];
        DefaultValue := '0.2';
      end;
    3:
      with Properties^ do
      begin
        Name := 'Amount';
        VariableAddress := @FAmount;
        Direction := drParameter;
        Datatype := dtSingle;
        Flags := [iofHideWhenLocked, iofUICommunication, iofPatchStore];
        DefaultValue := '0.2';
      end;
    4:
      with Properties^ do
      begin
        Name := 'Gradient';
        VariableAddress := @FGradient;
        Direction := drParameter;
        Datatype := dtSingle;
        Flags := [iofHideWhenLocked, iofUICommunication, iofPatchStore];
        DefaultValue := '0.6';
      end;
  else
    Result := False;
    // host will ask for plugs 0,1,2,3 etc. return false to signal when done
  end;
end;

procedure TSEBrushedMetalModule.GuiNotify(AUserMsgID: Integer; ASize: Integer;
  AData: Pointer);
begin
  inherited;
  {
    TODO:
    if AUserMsgID = IdToInt('a','c','k',0)
    then FGuiBusy := False;
  }
end;

(*
  // An input plug has changed value
  procedure TSEBrushedMetalModule.PlugStateChange(Pin: TSEPin);
  var
  InState  : array [0..1] of TSEStateType;
  OutState : TSEStateType;
  begin
  // query the 'state of the input plugs...
  //   stRun    = Normal Streaming Audio        (e.g. from an oscillator)
  //   stStatic = Fixed, unchanging input value (e.g. a slider at rest)
  InState[0] := getPin(Integer(pinInput1)).getStatus;
  InState[1] := getPin(Integer(pinInput2)).getStatus;

  // we need to pass on the state of this module's output signal
  // it depends on the inputs...
  OutState := InState[0];
  if InState[1] > OutState
  then OutState := InState[1];

  // if either input zero, tell downstream modules audio has stopped
  if (InState[0] < stRun) and (getPin(Integer(pinInput1)).getValue = 0)
  then OutState := stStatic;

  if (InState[1] < stRun) and (getPin(Integer(pinInput2)).getValue = 0)
  then OutState := stStatic;

  // 'transmit' new output status to next module 'downstream'
  getPin(Integer(pinOutput)).TransmitStatusChange(SampleClock, OutState);

  inherited;
  end;
*)

{ TSEBrushedMetalExModule }

class procedure TSEBrushedMetalExModule.GetModuleProperties
  (Properties: PSEModuleProperties);
begin
  inherited;
  with Properties^ do
  begin
    // describe the plugin, this is the name the end-user will see.
    Name := 'Brushed Metal Exposed';

    // return a unique string 32 characters max
    // if posible include manufacturer and plugin identity
    // this is used internally by SE to identify the plug.
    // No two plugs may have the same id.
    ID := 'Synthedit Brushed Metal Exposed';
    GuiFlags := [gfControlView, gfStructureView];
  end;
end;

function TSEBrushedMetalExModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
  Result := inherited GetPinProperties(Index, Properties);
  case Index of
    0:
      with Properties^ do
      begin
        Direction := drIn;
        Flags := [iofUICommunication, iofLinearInput, iofPatchStore];
      end;
    1:
      with Properties^ do
      begin
        Direction := drIn;
        Flags := [iofUICommunication, iofLinearInput, iofPatchStore];
      end;
    2:
      with Properties^ do
      begin
        Direction := drIn;
        Flags := [iofUICommunication, iofLinearInput, iofPatchStore];
      end;
    3:
      with Properties^ do
      begin
        Direction := drIn;
        Flags := [iofUICommunication, iofLinearInput, iofPatchStore];
      end;
    4:
      with Properties^ do
      begin
        Direction := drIn;
        Flags := [iofUICommunication, iofLinearInput, iofPatchStore];
      end;
  end;
end;

end.
