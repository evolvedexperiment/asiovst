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
{  Portions created by Christian-W. Budde are Copyright (C) 2003-2013          }
{  by Christian-W. Budde. All Rights Reserved.                                 }
{                                                                              }
{   SynthEdit is witten by Jef McClintock (see http://www.synthedit.com/       }
{                                                                              }
{******************************************************************************}

unit SECorrelationMeterModule;

interface

uses
  DAV_Types, DAV_SECommon, DAV_SEModule;

type
  TSECorrelationMeterModule = class(TSEModuleBase)
  private
    FDirection: Integer;
    FCorrelation: Single;
  protected
    procedure Open; override;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    function GetPinProperties(const Index: Integer;
      Properties: PSEPinProperties): Boolean; override;
    class procedure GetModuleProperties(Properties
      : PSEModuleProperties); override;

    procedure SubProcessSleep(const BufferOffset, SampleFrames: Integer);
  end;

implementation

procedure TSECorrelationMeterModule.Open;
begin
  inherited Open;

  // choose which function is used to process audio
  OnProcess := SubProcessSleep;
end;

// The most important part, processing the audio
procedure TSECorrelationMeterModule.SubProcessSleep(const BufferOffset,
  SampleFrames: Integer);
begin
  CallHost(SEAudioMasterSleepMode);
end;

// describe your module
class procedure TSECorrelationMeterModule.GetModuleProperties
  (Properties: PSEModuleProperties);
begin
  with Properties^ do
  begin
    // describe the plugin, this is the name the end-user will see.
    Name := 'Correlation Meter';

    // return a unique string 32 characters max
    // if posible include manufacturer and plugin identity
    // this is used internally by SE to identify the plug.
    // No two plugs may have the same id.
    ID := 'Synthedit Correlation Meter';

    // Info, may include Author, Web page whatever
    About := 'by Christian-W. Budde';

    Flags := [];
    GuiFlags := [gfControlView, gfStructureView];
    SdkVersion := CSeSdkVersion;
  end;
end;

// describe the pins (plugs)
function TSECorrelationMeterModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
  Result := True;
  case Index of
    0:
      with Properties^ do
      begin
        Name := 'Direction';
        Direction := drIn;
        Datatype := dtEnum;
        DatatypeExtra := 'Horizontal=0, Vertical=1';
        DefaultValue := '0';
        Flags := [iofUICommunication, iofPatchStore];
        VariableAddress := @FDirection;
      end;
    1:
      with Properties^ do
      begin
        Name := 'Correlation';
        Direction := drIn;
        Datatype := dtSingle;
        DefaultValue := '0';
        Flags := [iofUICommunication, iofPatchStore];
        VariableAddress := @FCorrelation;
      end;
    // typical input plug (inputs are listed first)
  else
    Result := False;
    // host will ask for plugs 0,1,2,3 etc. return false to signal when done
  end;
end;

// An input plug has changed value
procedure TSECorrelationMeterModule.PlugStateChange(const CurrentPin: TSEPin);
begin
  (*
    case Pin.PinID of
    0 :
    end;
  *)
  inherited;
end;

end.
