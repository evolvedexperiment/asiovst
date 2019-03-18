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

unit SELabelModule;

interface

uses
  Graphics, DAV_Types, DAV_SECommon, DAV_SEModule;

const
  CFontList: array [0 .. 10] of TFontName = ('Arial', 'Comic Sans MS',
    'Courier New', 'Georgia', 'Impact', 'Script', 'Tahoma', 'Times New Roman',
    'Trebuchet MS', 'Verdana', 'WST_Engl');

type
  TCustomSELabelModule = class(TSEModuleBase)
  private
    FText: PAnsiChar;
  protected
    procedure Open; override;
  public
    procedure SubProcess(const BufferOffset, SampleFrames: Integer);
  end;

  TSELabelModuleDSP = class(TCustomSELabelModule)
  protected
    FHiddenText: PAnsiChar;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    function GetPinProperties(const Index: Integer;
      Properties: PSEPinProperties): Boolean; override;
    class procedure GetModuleProperties(Properties
      : PSEModuleProperties); override;
  end;

  TSELabelModuleGUI = class(TCustomSELabelModule)
  private
    FFontColor: Integer;
    FFontName: Integer;
    FFontSize: Integer;
    FFontBold: Boolean;
    FFontItalic: Boolean;
    FFontShadow: Integer;
  public
    function GetPinProperties(const Index: Integer;
      Properties: PSEPinProperties): Boolean; override;
    class procedure GetModuleProperties(Properties
      : PSEModuleProperties); override;
  end;

implementation

{ TCustomSELabelModule }

procedure TCustomSELabelModule.Open;
begin
  inherited Open;

  // choose which function is used to process audio
  OnProcess := SubProcess;
end;

// The most important part, processing the audio
procedure TCustomSELabelModule.SubProcess(const BufferOffset,
  SampleFrames: Integer);
begin
  CallHost(SEAudioMasterSleepMode);
end;

{ TSELabelModuleDSP }

// describe your module
class procedure TSELabelModuleDSP.GetModuleProperties
  (Properties: PSEModuleProperties);
begin
  // describe the plugin, this is the name the end-user will see.
  Properties.Name := 'DSP Label';

  // return a unique string 32 characters max
  // if posible include manufacturer and plugin identity
  // this is used internally by SE to identify the plug.
  // No two plugs may have the same id.
  Properties.ID := 'DAV DSP Label';

  // Info, may include Author, Web page whatever
  Properties.About := 'by Christian-W. Budde';

  Properties.Flags := [];
  Properties.GuiFlags := [gfControlView, gfStructureView];
  Properties.SdkVersion := CSeSdkVersion;
end;

// describe the pins (plugs)
function TSELabelModuleDSP.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
  Result := True;
  case Index of
    // typical input plug (inputs are listed first)
    0:
      with Properties^ do
      begin
        Name := 'Text';
        VariableAddress := @FText;
        Flags := [iofLinearInput];
        Direction := drIn;
        Datatype := dtText;
        DefaultValue := 'Label';
      end;
    1:
      with Properties^ do
      begin
        Name := 'Text';
        VariableAddress := @FHiddenText;
        Flags := [iofUICommunication, iofHidePin];
        Direction := drIn;
        Datatype := dtText;
        DefaultValue := 'Label';
      end;
  else
    Result := False;
    // host will ask for plugs 0,1,2,3 etc. return false to signal when done
  end;
end;

procedure TSELabelModuleDSP.PlugStateChange(const CurrentPin: TSEPin);
begin
  inherited;
  if CurrentPin.PinID = 0 then
  begin
    FHiddenText := PAnsiChar(FText + #0);
  end;
end;

{ TSELabelModuleGUI }

// describe your module
class procedure TSELabelModuleGUI.GetModuleProperties
  (Properties: PSEModuleProperties);
begin
  // describe the plugin, this is the name the end-user will see.
  Properties.Name := 'Label';

  // return a unique string 32 characters max
  // if posible include manufacturer and plugin identity
  // this is used internally by SE to identify the plug.
  // No two plugs may have the same id.
  Properties.ID := 'DAV Label';

  // Info, may include Author, Web page whatever
  Properties.About := 'by Christian-W. Budde';

  Properties.Flags := [];
  Properties.GuiFlags := [gfControlView, gfStructureView];
  Properties.SdkVersion := CSeSdkVersion;
end;

// describe the pins (plugs)
function TSELabelModuleGUI.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
  Result := True;
  case Index of
    // typical input plug (inputs are listed first)
    0:
      with Properties^ do
      begin
        Name := 'Text';
        VariableAddress := @FText;
        Flags := [iofUICommunication, iofLinearInput, iofPatchStore];
        Direction := drIn;
        Datatype := dtText;
        DefaultValue := 'Label';
      end;
    1:
      with Properties^ do
      begin
        Name := 'Font Color';
        VariableAddress := @FFontColor;
        Flags := [iofUICommunication, iofLinearInput, iofPatchStore];
        Datatype := dtInteger;
        Direction := drIn;
      end;
    2:
      with Properties^ do
      begin
        Name := 'Font Name';
        VariableAddress := @FFontName;
        Flags := [iofUICommunication, iofLinearInput, iofPatchStore];
        Datatype := dtEnum;
        Direction := drIn;
        DatatypeExtra :=
          'Arial, Comic Sans MS, Courier New, Georgia, Impact, Script, Tahoma, Times New Roman, Trebuchet MS, Verdana, WST_Engl';
      end;
    3:
      with Properties^ do
      begin
        Name := 'Font Size';
        VariableAddress := @FFontSize;
        Flags := [iofUICommunication, iofLinearInput, iofPatchStore];
        Datatype := dtEnum;
        Direction := drIn;
        DatatypeExtra :=
          '8=8,9=9,10=10,11=11,12=12,14=14,16=16,18=18,20=20,24=24,32=32,48=48,72=72';
        DefaultValue := '24';
      end;
    4:
      with Properties^ do
      begin
        Name := 'Bold';
        VariableAddress := @FFontBold;
        Flags := [iofUICommunication, iofLinearInput, iofPatchStore];
        Datatype := dtBoolean;
        Direction := drIn;
      end;
    5:
      with Properties^ do
      begin
        Name := 'Italic';
        VariableAddress := @FFontItalic;
        Flags := [iofUICommunication, iofLinearInput, iofPatchStore];
        Datatype := dtBoolean;
        Direction := drIn;
      end;
    6:
      with Properties^ do
      begin
        Name := 'Shadow Length';
        VariableAddress := @FFontShadow;
        Flags := [iofUICommunication, iofLinearInput, iofPatchStore];
        Datatype := dtEnum;
        Direction := drIn;
        DatatypeExtra := 'range -32,32';
      end;
  else
    Result := False;
    // host will ask for plugs 0,1,2,3 etc. return false to signal when done
  end;
end;

end.
