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

unit SETransposerModule;

interface

uses
  DAV_Types, DAV_SECommon, DAV_SEModule;

type
  // define some constants to make referencing in/outs clearer
  TSETransposerPins = (pinMidiIn, pinTranspose, pinMidiOut);

  TSETransposerModule = class(TSEModuleBase)
  private
    FTransposeAmount: ShortInt;
  protected
    procedure Open; override;
    procedure MidiData(AClock, AMidiMsg: Cardinal; PinID: Integer); override;
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback;
      Reserved: Pointer); override;
    destructor Destroy; override;

    class procedure GetModuleProperties(Properties: PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer;
      Properties: PSEPinProperties): Boolean; override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer);
  end;

implementation

constructor TSETransposerModule.Create(SEAudioMaster: TSE2audioMasterCallback;
  Reserved: Pointer);
begin
  inherited Create(SEAudioMaster, Reserved);
end;

destructor TSETransposerModule.Destroy;
begin
  // This is where you free any memory/resources your module has created
  inherited;
end;

procedure TSETransposerModule.Open;
begin
  inherited Open;

  // choose which function is used to process audio
  OnProcess := SubProcess;
end;

// The most important part, processing the audio
procedure TSETransposerModule.SubProcess(const BufferOffset,
  SampleFrames: Integer);
begin
  CallHost(seaudioMasterSleepMode);
end;

// describe your module
class procedure TSETransposerModule.GetModuleProperties
  (Properties: PSEModuleProperties);
begin
  with Properties^ do
  begin
    // describe the plugin, this is the name the end-user will see.
    Name := 'MIDI Transposer';

    // return a unique string 32 characters max
    // if posible include manufacturer and plugin identity
    // this is used internally by SE to identify the plug.
    // No two plugs may have the same id.
    ID := 'Synthedit Transposer Example';

    // Info, may include Author, Web page whatever
    About := 'by Christian-W. Budde';

    SdkVersion := CSeSdkVersion;
  end;
end;

procedure TSETransposerModule.MidiData(AClock, AMidiMsg: Cardinal;
  PinID: Integer);
var
  IsSystemMsg: Boolean;
  Chan, Stat, b2, b3: Integer;
begin
  // system messages are for timing info etc
  // they don't have a channel, just pass them right through
  IsSystemMsg := (AMidiMsg and $F0) = $F0;

  if not IsSystemMsg then
  begin
    // The first byte of a midi message contains the status (note on/off etc) and the channel
    Chan := AMidiMsg and $0F;
    Stat := AMidiMsg and $F0;

    // the next two bytes depend on the type of message
    b2 := (AMidiMsg shr 8) and $FF;
    b3 := (AMidiMsg shr 16) and $FF;

    case Stat of
      $80, $90:
        begin
          b2 := b2 + FTransposeAmount; // byte 2 is the note number
          if (b2 > 127) then
            b2 := 127;
          if (b2 < 0) then
            b2 := 0;

          // re-assemble the MIDI message into an 32 bit integer
          AMidiMsg := Stat + Chan + (b2 shl 8) + (b3 shl 16);
        end;
    end;
  end;

  // send the MIDI
  Pin[Integer(pinMidiOut)].TransmitMIDI(AClock, AMidiMsg);

  inherited;
end;

// describe the pins (plugs)
function TSETransposerModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
  Result := True;
  case TSETransposerPins(index) of
    // typical input plug (inputs are listed first)
    pinMidiIn:
      with Properties^ do
      begin
        Name := 'MIDI In';
        Direction := drIn;
        Datatype := dtMIDI2;
      end;
    pinTranspose:
      with Properties^ do
      begin
        Name := 'Transpose';
        VariableAddress := @FTransposeAmount;
        Direction := drIn;
        Datatype := dtEnum;
        DefaultValue := '5';
        DatatypeExtra := 'range -128,128';
      end;
    pinMidiOut:
      with Properties^ do
      begin
        Name := 'MIDI Out';
        Direction := drOut;
        Datatype := dtMIDI2;
      end;
  else
    Result := False;
    // host will ask for plugs 0,1,2,3 etc. return false to signal when done
  end;
end;

end.
