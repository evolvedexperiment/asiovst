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

unit SEScopeModule;

interface

uses
  DAV_Types, DAV_SECommon, DAV_SEModule;

const
  CScopeBufferSize = 100;
  CScopeChannels = 2;

type
  TSEBlob = record
    Size: Integer;
    Data: Pointer;
  end;

  TScopeResults = array [0 .. 1, 0 .. 103] of TSEFloatSample;

  // define some constants to make referencing in/outs clearer
  TSEScopePins = (pinInput1, pinInput2);

  TSEScopeModule = class(TSEModuleBase)
  private
    FInput1Buffer: PDAVSingleFixedArray;
    // pointer to circular buffer of samples
    FInput2Buffer: PDAVSingleFixedArray;
    FIndex: Integer;
    FTimeOut: Integer;
    FGuiBusy: Boolean;
    FScopeResults: TScopeResults;
    FChannelActive: array [0 .. CScopeChannels - 1] of Boolean;
    FResultsBlob: TSEBlob;
    procedure SendStringToGui(AMsgID, ALength: Integer; AData: PAnsiChar);
    procedure ForceTrigger;
    procedure SendResultToGui;
  protected
    procedure Open; override;
    // procedure PlugStateChange(Pin: TSEPin); override;
    procedure GuiNotify(AUserMsgID: Integer; ASize: Integer;
      AData: Pointer); override;
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback;
      Reserved: Pointer); override;
    destructor Destroy; override;

    class procedure GetModuleProperties(Properties: PSEModuleProperties); override;
    // function GetPinProperties(Index: Integer; Properties: PSEPinProperties): Boolean; override;
    function GetPinProperties(const Index: Integer;
      Properties: PSEPinProperties): Boolean; override;

    procedure SubProcess(const BufferOffset, SampleFrames: Integer);
    procedure SubProcessCruise(const BufferOffset, SampleFrames: Integer);
    procedure WaitForTrigger1(const BufferOffset, SampleFrames: Integer);
    procedure WaitForTrigger2(const BufferOffset, SampleFrames: Integer);
  end;

implementation

constructor TSEScopeModule.Create(SEAudioMaster: TSE2audioMasterCallback;
  Reserved: Pointer);
begin
  inherited Create(SEAudioMaster, Reserved);
  FGuiBusy := False;
  FIndex := 0;
  FResultsBlob.Data := @FScopeResults;
  FResultsBlob.Size := SizeOf(TScopeResults);
end;

destructor TSEScopeModule.Destroy;
begin
  // This is where you free any memory/resources your module has created
  inherited;
end;

procedure TSEScopeModule.ForceTrigger;
begin
  FIndex := 0;
  if not FGuiBusy then
    OnProcess := SubProcess
  else
    OnProcess := SubProcessCruise;
end;

procedure TSEScopeModule.Open;
var
  ch, j: Integer;
begin
  inherited Open;

  // choose which function is used to process audio
  OnProcess := SubProcess;

  for ch := 0 to CScopeChannels - 1 do
  begin
    FChannelActive[ch] := Pin[ch].IsConnected;
    if not FChannelActive[ch] then
    begin
      // if a channel is not active, don't want to draw it.
      // so set it's value really big ( so it draws off-screen )
      for j := 0 to CScopeBufferSize - 1 do
        FScopeResults[ch][j] := 1000;
    end;
  end;
end;

// The most important part, processing the audio
procedure TSEScopeModule.SendResultToGui;
var
  ChunkName: TChunkName;
begin
  if not FGuiBusy then
  begin
    FGuiBusy := True;
    ChunkName := 'data';
    SendStringToGui(Integer(ChunkName), FResultsBlob.Size, FResultsBlob.Data);
  end;

  OnProcess := WaitForTrigger1;
  FTimeOut := Round(SampleRate / 3);
end;

procedure TSEScopeModule.SendStringToGui(AMsgID, ALength: Integer;
  AData: PAnsiChar);
begin
  CallHost(seAudioMasterSendStringToGui, ALength, AMsgID, AData);
end;

procedure TSEScopeModule.SubProcess(const BufferOffset, SampleFrames: Integer);
var
  Input: PDAVSingleFixedArray;
  Count, c: Integer;
  Chan, i: Integer;
  Remain: Integer;
begin
  Input := @FInput1Buffer^[BufferOffset];
  Count := CScopeBufferSize + 2 - FIndex;

  if (Count > SampleFrames) then
    Count := SampleFrames;

  Remain := SampleFrames - Count;

  for Chan := 0 to CScopeChannels - 1 do
  begin
    if FChannelActive[Chan] then
    begin
      i := FIndex;
      for c := Count - 1 downto 0 do
      begin
        FScopeResults[Chan][i] := Input[0];
        inc(Input);
        inc(i);
      end;
    end;
    Input := @FInput2Buffer^[BufferOffset];
  end;

  FIndex := FIndex + Count;

  if FIndex > CScopeBufferSize + 1 then
  begin
    SendResultToGui;
    OnProcess(BufferOffset + SampleFrames - Remain, Remain);
  end;
end;

// same as sub process, but don't record samples (because UI redrawing)
procedure TSEScopeModule.SubProcessCruise(const BufferOffset,
  SampleFrames: Integer);
var
  Count: Integer;
  Remain: Integer;
begin
  Count := CScopeBufferSize + 2 - FIndex;
  if Count > SampleFrames then
    Count := SampleFrames;

  FIndex := FIndex + Count;

  if FIndex > CScopeBufferSize + 1 then
  begin
    Remain := SampleFrames - Count;

    OnProcess := WaitForTrigger1;
    FTimeOut := Round(SampleRate / 3);
    OnProcess(BufferOffset + SampleFrames - Remain, Remain);
  end;
end;

procedure TSEScopeModule.WaitForTrigger1(const BufferOffset,
  SampleFrames: Integer);
var
  Input: PSingle;
  s: Integer;
begin
  Input := @FInput1Buffer^[BufferOffset];
  for s := SampleFrames - 1 downto 0 do
  begin
    if Input^ <= 0 then
    begin
      FIndex := 0;
      OnProcess := WaitForTrigger2;
      WaitForTrigger2(BufferOffset + SampleFrames - s, s);
      exit;
    end;
    inc(Input);
  end;

  FTimeOut := FTimeOut - SampleFrames;
  if FTimeOut < 0 then
    OnProcess := WaitForTrigger2
end;

procedure TSEScopeModule.WaitForTrigger2(const BufferOffset,
  SampleFrames: Integer);
var
  Input: PSingle;
  s: Integer;
begin
  Input := @FInput1Buffer^[BufferOffset];

  for s := SampleFrames - 1 downto 0 do
  begin
    if Input^ > 0 then
    begin
      ForceTrigger;

      // simple check that upsteam module is behaving
      // PCtl_Scope(GetCUG).InputStateErrorFlag := s < SampleFrames and GetPlug(0).getState = stStop;

      OnProcess(BufferOffset + SampleFrames - s, s);
      exit;
    end;
    inc(Input);
  end;

  FTimeOut := FTimeOut - SampleFrames;
  if FTimeOut < 0 then
    ForceTrigger;
end;

// describe your module
class procedure TSEScopeModule.GetModuleProperties
  (Properties: PSEModuleProperties);
begin
  with Properties^ do
  begin
    // describe the plugin, this is the name the end-user will see.
    Name := 'Scope Example';

    // return a unique string 32 characters max
    // if posible include manufacturer and plugin identity
    // this is used internally by SE to identify the plug.
    // No two plugs may have the same id.
    ID := 'Synthedit Scope Example';

    // Info, may include Author, Web page whatever
    About := 'by Christian-W. Budde';

    Flags := [ugfPolyphonicAgregator, ugfVoiceMonIgnore];
    GuiFlags := [gfControlView, gfStructureView];

    SdkVersion := CSeSdkVersion;
  end;
end;

// describe the pins (plugs)
function TSEScopeModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
  Result := True;
  case TSEScopePins(index) of
    // typical input plug (inputs are listed first)
    pinInput1:
      with Properties^ do
      begin
        Name := 'Signal A';
        VariableAddress := @FInput1Buffer;
        Direction := drIn;
        Datatype := dtFSample;
        DefaultValue := '0';
      end;
    pinInput2:
      with Properties^ do
      begin
        Name := 'Signal B';
        VariableAddress := @FInput2Buffer;
        Direction := drIn;
        Datatype := dtFSample;
        DefaultValue := '0';
      end;
  else
    Result := False;
    // host will ask for plugs 0,1,2,3 etc. return false to signal when done
  end;
end;

procedure TSEScopeModule.GuiNotify(AUserMsgID: Integer; ASize: Integer;
  AData: Pointer);
begin
  inherited;
end;

end.
