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

unit SEWhiteNoiseModule;

interface

uses
  DAV_Types, DAV_Complex, DAV_SECommon, DAV_SEModule;

type
  // define some constants to make referencing in/outs clearer
  TSEWhiteNoisePins = (pinDistribution, pinOutput);

  TNoiseDistribution = (ndRectangle, ndTriangle, ndGauss, ndFastGauss);

  TSEWhiteNoiseModule = class(TSEModuleBase)
  private
    FOutputBuffer: PDAVSingleFixedArray;
    FDistribution: TNoiseDistribution;
    procedure ChooseProcess;
  protected
    procedure Open; override;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    class procedure GetModuleProperties(Properties
      : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer;
      Properties: PSEPinProperties): Boolean; override;
    procedure SubProcessRectangle(const BufferOffset, SampleFrames: Integer);
    procedure SubProcessTriangle(const BufferOffset, SampleFrames: Integer);
    procedure SubProcessGauss(const BufferOffset, SampleFrames: Integer);
    procedure SubProcessFastGauss(const BufferOffset, SampleFrames: Integer);
  end;

implementation

uses
  SysUtils, DAV_Math, DAV_Approximations;

procedure TSEWhiteNoiseModule.Open;
begin
  inherited Open;
  // choose which function is used to process audio
  OnProcess := SubProcessRectangle;

  // 'transmit' new output status to next module 'downstream'
  Pin[Integer(pinOutput)].TransmitStatusChange(SampleClock, stRun);
end;

procedure TSEWhiteNoiseModule.PlugStateChange(const CurrentPin: TSEPin);
begin
  inherited;
  case TSEWhiteNoisePins(CurrentPin.PinID) of
    pinDistribution:
      ChooseProcess;
  end;
end;

procedure TSEWhiteNoiseModule.ChooseProcess;
begin
  case FDistribution of
    ndRectangle:
      OnProcess := SubProcessRectangle;
    ndTriangle:
      OnProcess := SubProcessTriangle;
    ndGauss:
      OnProcess := SubProcessGauss;
    ndFastGauss:
      OnProcess := SubProcessFastGauss;
  end;
end;

// describe your module
class procedure TSEWhiteNoiseModule.GetModuleProperties
  (Properties: PSEModuleProperties);
begin
  with Properties^ do
  begin
    // describe the plugin, this is the name the end-user will see.
    Name := 'White Noise Generator';

    // return a unique string 32 characters max
    // if posible include manufacturer and plugin identity
    // this is used internally by SE to identify the plug.
    // No two plugs may have the same id.
    ID := 'DAV White Noise Generator';

    // Info, may include Author, Web page whatever
    About := 'by Christian-W. Budde';
    SDKVersion := CSeSdkVersion;
  end;
end;

// describe the pins (plugs)
function TSEWhiteNoiseModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
  Result := True;
  case TSEWhiteNoisePins(index) of
    pinDistribution:
      with Properties^ do
      begin
        Name := 'Distribution';
        VariableAddress := @FDistribution;
        Direction := drIn;
        Datatype := dtEnum;
        DatatypeExtra := 'Rectangle, Triangle, Gauss, Fast Gauss'
      end;
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
  end;
end;

// The most important part, processing the audio
procedure TSEWhiteNoiseModule.SubProcessRectangle(const BufferOffset,
  SampleFrames: Integer);
var
  Sample: Integer;
begin
  for Sample := 0 to SampleFrames - 1 do
    FOutputBuffer[BufferOffset + Sample] := FastRandom;
end;

procedure TSEWhiteNoiseModule.SubProcessTriangle(const BufferOffset,
  SampleFrames: Integer);
var
  Sample: Integer;
begin
  for Sample := 0 to SampleFrames - 1 do
    FOutputBuffer[BufferOffset + Sample] := random - random;
end;

procedure TSEWhiteNoiseModule.SubProcessGauss(const BufferOffset,
  SampleFrames: Integer);
var
  Sample: Integer;
begin
  for Sample := 0 to SampleFrames - 1 do
    FOutputBuffer[BufferOffset + Sample] := FastRandomGauss;
end;

procedure TSEWhiteNoiseModule.SubProcessFastGauss(const BufferOffset,
  SampleFrames: Integer);
var
  Sample: Integer;
begin
  for Sample := 0 to SampleFrames - 1 do
    FOutputBuffer[BufferOffset + Sample] := FastRandomGauss;
end;

end.
