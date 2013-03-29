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

unit SEIntToListModule;

interface

uses
  DAV_Types, DAV_SECommon, DAV_SEModule;

type
  TSEIntToListModule = class(TSEModuleBase)
  protected
    procedure Open; override;
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback;
      Reserved: Pointer); override;
    function GetPinProperties(const Index: Integer;
      Properties: PSEPinProperties): Boolean; override;
    class procedure GetModuleProperties(Properties
      : PSEModuleProperties); override;
    procedure SubProcessSleep(const BufferOffset, SampleFrames: Integer);
  end;

implementation

constructor TSEIntToListModule.Create(SEAudioMaster: TSE2audioMasterCallback;
  Reserved: Pointer);
begin
  inherited Create(SEAudioMaster, Reserved);
end;

procedure TSEIntToListModule.Open;
begin
  inherited Open;

  // choose which function is used to process audio
  OnProcess := SubProcessSleep;
end;

procedure TSEIntToListModule.SubProcessSleep(const BufferOffset,
  SampleFrames: Integer);
begin
  CallHost(SEAudioMasterSleepMode);
end;

// describe the pins (plugs)
class procedure TSEIntToListModule.GetModuleProperties
  (Properties: PSEModuleProperties);
begin
  with Properties^ do
  begin
    // describe the plugin, this is the name the end-user will see.
    Name := 'Int To List';

    // return a unique string 32 characters max
    // include manufacturer and plugin identity
    // this is used internally by SE to identify the plug.
    // No two plugs may have the same id.
    ID := 'Synthedit Int To List';

    // Info, may include Author, Web page whatever
    About := 'by Christian-W. Budde';

    // only show in structure view
    GuiFlags := [gfStructureView];

    SdkVersion := CSeSdkVersion;
  end;
end;

function TSEIntToListModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
  Result := True;
  case Index of
    // typical input plug (inputs are listed first)
    0:
      with Properties^ do
      begin
        Name := 'In';
        Direction := drIn;
        Datatype := dtInteger;
        Flags := [iofUICommunication];
      end;
    1:
      with Properties^ do
      begin
        Name := 'Out';
        Direction := drOut;
        Datatype := dtEnum;
        Flags := [iofUICommunication];
      end;
    2:
      with Properties^ do
      begin
        Name := 'Mode';
        DatatypeExtra := 'Index,Value';
        Direction := drIn;
        Datatype := dtEnum;
        Flags := [iofUICommunication];
      end
  else
    Result := False;
    // host will ask for plugs 0,1,2,3 etc. return false to signal when done
  end;;
end;

end.
