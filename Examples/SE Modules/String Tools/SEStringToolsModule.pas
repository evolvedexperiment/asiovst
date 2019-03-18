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

unit SEStringToolsModule;

interface

uses
  DAV_Types, DAV_Complex, DAV_SECommon, DAV_SEModule, DAV_DspLfo;

type
  TSEStringToolsModule = class(TSEModuleBase)
  protected
    procedure Open; override;
  public
    procedure SubProcessStatic(const BufferOffset, SampleFrames: Integer);
  end;

  TSEConcatStringModule = class(TSEStringToolsModule)
  protected
    FText: array [0 .. 2] of PAnsiChar;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    class procedure GetModuleProperties(Properties: PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer;
      Properties: PSEPinProperties): Boolean; override;
  end;

implementation

uses
  SysUtils;

{ TSEStringToolsModule }

procedure TSEStringToolsModule.Open;
begin
  inherited Open;

  OnProcess := SubProcessStatic;

  // 'transmit' new output status to next module 'downstream'
  Pin[0].TransmitStatusChange(SampleClock, stStatic);
  Pin[1].TransmitStatusChange(SampleClock, stStatic);
end;

// The most important part, processing the audio
procedure TSEStringToolsModule.SubProcessStatic(const BufferOffset,
  SampleFrames: Integer);
begin
  // empty
end;

{ TSEConcatStringModule }

// describe your module
class procedure TSEConcatStringModule.GetModuleProperties
  (Properties: PSEModuleProperties);
begin
  with Properties^ do
  begin
    // describe the plugin, this is the name the end-user will see.
    Name := 'Concat Text';

    // return a unique string 32 characters max
    // if posible include manufacturer and plugin identity
    // this is used internally by SE to identify the plug.
    // No two plugs may have the same id.
    ID := 'DAV Concat Text';

    // Info, may include Author, Web page whatever
    About := 'by Christian-W. Budde';
    SDKVersion := CSeSdkVersion;
  end;
end;

procedure TSEConcatStringModule.PlugStateChange(const CurrentPin: TSEPin);
begin
  if CurrentPin.PinID = 0 then
    FText[0] := PAnsiChar(StrPas(FText[1]) + StrPas(FText[2]) + #0);
  inherited;
end;

// describe the pins (plugs)
function TSEConcatStringModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
  Result := True;
  case Index of
    // typical input plug (inputs are listed first)
    0:
      with Properties^ do
      begin
        Name := 'Text 1';
        VariableAddress := @FText[1];
        Direction := drIn;
        Datatype := dtText;
        DefaultValue := '';
      end;
    1:
      with Properties^ do
      begin
        Name := 'Text 2';
        VariableAddress := @FText[2];
        Direction := drIn;
        Datatype := dtText;
        DefaultValue := '';
      end;
    2:
      with Properties^ do
      begin
        Name := 'Concatted Text';
        VariableAddress := @FText[0];
        Direction := drOut;
        Datatype := dtText;
        DefaultValue := '';
      end;
  else
    Result := False;
  end;
end;

end.
