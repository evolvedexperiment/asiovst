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

unit SEFibonacciModule;

interface

uses
  DAV_Types, DAV_SECommon, DAV_SEModule;

type
  TSEFibonacciModule = class(TSEModuleBase)
  private
    FOutputBuffer: PDAVSingleFixedArray;
    FLastState: Single;
    FOrder: Integer;
    FDownsample: Integer;
    FFiboLength: Integer;
    FFiboIndex: Integer;
    FFiboStates: Array [0 .. 1] of Integer;
    FSqr5, FA, fB: Double;
    procedure OrderChanged;
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

uses
  Math;

constructor TSEFibonacciModule.Create(SEAudioMaster: TSE2audioMasterCallback;
  Reserved: Pointer);
begin
  inherited Create(SEAudioMaster, Reserved);
  FSqr5 := Sqrt(5);
  FA := (1 + FSqr5) * 0.5;
  fB := (1 - FSqr5) * 0.5;
  FOrder := 8;
  FDownsample := 1;
  OrderChanged;
  FFiboIndex := 0;
  FFiboStates[0] := 1;
  FFiboStates[1] := 1;
end;

destructor TSEFibonacciModule.Destroy;
begin
  // This is where you free any memory/resources your module has created
  inherited;
end;

procedure TSEFibonacciModule.Open;
begin
  inherited Open;

  // choose which function is used to process audio
  OnProcess := SubProcess;
end;

procedure TSEFibonacciModule.OrderChanged;
begin
  FFiboLength := FDownsample * Round((Power(FA, FOrder) - Power(fB, FOrder)
    ) / FSqr5);
end;

procedure TSEFibonacciModule.PlugStateChange(const CurrentPin: TSEPin);
begin
  case CurrentPin.PinID of
    1:
      begin
        OrderChanged;
        if FFiboIndex > FFiboLength then
        begin
          FFiboIndex := 0;
          FFiboStates[0] := 1;
          FFiboStates[1] := 1;
        end;
      end;
  end;
  inherited;
end;

// The most important part, processing the audio
procedure TSEFibonacciModule.SubProcess(const BufferOffset,
  SampleFrames: Integer);
var
  Out1: PDAVSingleFixedArray;
  Sample: Integer;
  spd: Double;
begin
  Out1 := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);
  for Sample := 0 to SampleFrames - 1 do
  begin
    if (FFiboIndex mod FDownsample) = 0 then
    begin
      spd := FFiboStates[1] / FFiboStates[0];
      if spd * spd > spd + 1 then
      begin
        FLastState := -1;
        Inc(FFiboStates[0]);
      end
      else
      begin
        FLastState := 1;
        Inc(FFiboStates[1]);
      end;
    end;

    Out1^[Sample] := FLastState;
    Inc(FFiboIndex);
    if FFiboIndex >= FFiboLength then
    begin
      FFiboIndex := 0;
      FFiboStates[0] := 1;
      FFiboStates[1] := 1;
    end;
  end;
end;

// describe your module
class procedure TSEFibonacciModule.GetModuleProperties
  (Properties: PSEModuleProperties);
begin
  with Properties^ do
  begin
    // describe the plugin, this is the name the end-user will see.
    Name := 'Fibonacci Example';

    // return a unique string 32 characters max
    // if posible include manufacturer and plugin identity
    // this is used internally by SE to identify the plug.
    // No two plugs may have the same id.
    ID := 'Synthedit Fibonacci Example';

    // Info, may include Author, Web page whatever
    About := 'by Christian-W. Budde';
    SDKVersion := CSeSdkVersion;
  end;
end;

// describe the pins (plugs)
function TSEFibonacciModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
  Result := True;
  case Index of
    // typical input plug (inputs are listed first)
    // typical output plug
    0:
      with Properties^ do
      begin
        Name := 'Output';
        VariableAddress := @FOutputBuffer;
        Direction := drOut;
        Datatype := dtFSample;
      end;
    1:
      with Properties^ do
      begin
        Name := 'Order';
        VariableAddress := @FOrder;
        Direction := drIn;
        Datatype := dtEnum;
        DefaultValue := '8';
        DatatypeExtra := 'range 2, 64'
      end;
    2:
      with Properties^ do
      begin
        Name := 'Downsample';
        VariableAddress := @FDownsample;
        Direction := drIn;
        Datatype := dtEnum;
        DefaultValue := '1';
        DatatypeExtra := 'range 1, 1024'
      end;
  else
    Result := False;
    // host will ask for plugs 0,1,2,3 etc. return false to signal when done
  end;;
end;

end.
