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

unit SEPascalScriptModule;

interface

uses
  Classes, DAV_Types, DAV_SECommon, DAV_SEModule, uPSCompiler, uPSRuntime,
  uPSUtils;

type
  // define some constants to make referencing in/outs clearer
  TSEPascalScriptPins = (pinFilename, pinInput, pinOutput);
  TSEProcessSample = procedure(Channel: Integer; var Data: Single) of object;
  TSEProcessBlock = procedure(Channel: Integer) of object;

  TSEPascalScriptModule = class(TSEModuleBase)
  private
    FByteCode: AnsiString;
    FFileName: PAnsiChar;
    FInputBuffer: PDAVSingleFixedArray; // pointer to circular buffer of samples
    FOutputBuffer: PDAVSingleFixedArray;
    FPSCompiler: TPSPascalCompiler;
    FPSExecuter: TPSExec;
    FSEProcessSample: TSEProcessSample;
    FSEProcessBlock: TSEProcessBlock;
    procedure SetByteCode(const Value: AnsiString);
    procedure ByteCodeLoaded;
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

    property ByteCode: AnsiString read FByteCode write SetByteCode;
  end;

implementation

uses
  SysUtils;

function ScriptOnExportCheck(Sender: TPSPascalCompiler;
  Proc: TPSInternalProcedure; const ProcDecl: string): Boolean;
begin
  if Proc.Name = 'SEPROCESSSAMPLE' then
  begin
    if not ExportCheck(Sender, Proc, [btReturnAddress, btS32, btSingle],
      [pmIn, pmInOut]) then // Check if the proc has the correct params.
    begin
      Sender.MakeError('', ecTypeMismatch, '');
      Result := False;
      Exit;
    end;
    Result := True;
  end
  else if Proc.Name = 'SEPROCESSBLOCK' then
  begin
    if not ExportCheck(Sender, Proc, [btReturnAddress, btS32], [pmIn]) then
    // Check if the proc has the correct params.
    begin
      Sender.MakeError('', ecTypeMismatch, '');
      Result := False;
      Exit;
    end;
    Result := True;
  end
  else
    Result := True;
end;

constructor TSEPascalScriptModule.Create(SEAudioMaster: TSE2audioMasterCallback;
  Reserved: Pointer);
begin
  inherited Create(SEAudioMaster, Reserved);
  FPSExecuter := TPSExec.Create;
  FPSCompiler := TPSPascalCompiler.Create;
  // create an instance of the compiler.
  with FPSCompiler do
  begin
    OnExportCheck := ScriptOnExportCheck; // Assign the onExportCheck event.
    AllowNoBegin := True;
    AllowNoEnd := True;
    // AllowNoBegin and AllowNoEnd allows it that begin and end are not required in a script.
  end;
end;

destructor TSEPascalScriptModule.Destroy;
begin
  // This is where you free any memory/resources your module has created
  FreeAndNil(FPSExecuter);
  FreeAndNil(FPSCompiler);
  inherited;
end;

procedure TSEPascalScriptModule.Open;
begin
  inherited Open;

  // choose which function is used to process audio
  OnProcess := SubProcess;
end;

procedure TSEPascalScriptModule.SetByteCode(const Value: AnsiString);
begin
  if FByteCode <> Value then
  begin
    FByteCode := Value;
    ByteCodeLoaded;
  end;
end;

procedure TSEPascalScriptModule.ByteCodeLoaded;
begin
  try
    if FPSExecuter.LoadData(FByteCode) then
    begin
      FSEProcessSample := TSEProcessSample
        (FPSExecuter.GetProcAsMethodN('SEProcessSample'));
      FSEProcessBlock := TSEProcessBlock
        (FPSExecuter.GetProcAsMethodN('SEProcessBlock'));
    end;
  except
    FSEProcessSample := nil;
    FSEProcessBlock := nil;
  end;
end;

// The most important part, processing the audio
procedure TSEPascalScriptModule.SubProcess(const BufferOffset,
  SampleFrames: Integer);
var
  Input: PDAVSingleFixedArray;
  Output: PDAVSingleFixedArray;
  Sample: Integer;
  d: Single;
begin
  // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
  Input := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
  Output := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);
  if Assigned(FSEProcessBlock) then
    FSEProcessBlock(0);
  if Assigned(FSEProcessSample) then
    for Sample := 0 to SampleFrames - 1 do
    begin
      d := Input^[Sample] + cDenorm64;
      FSEProcessSample(0, d);
      Output^[Sample] := d;
    end;
end;

// describe your module
class procedure TSEPascalScriptModule.GetModuleProperties
  (Properties: PSEModuleProperties);
begin
  with Properties^ do
  begin
    // describe the plugin, this is the name the end-user will see.
    Name := 'PascalScript';

    // return a unique string 32 characters max
    // if posible include manufacturer and plugin identity
    // this is used internally by SE to identify the plug.
    // No two plugs may have the same id.
    ID := 'DAV PascalScript';

    // Info, may include Author, Web page whatever
    About := 'by Christian-W. Budde';

    SDKVersion := CSeSdkVersion;
  end;
end;

// describe the pins (plugs)
function TSEPascalScriptModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
  Result := True;
  case TSEPascalScriptPins(index) of
    pinFilename:
      with Properties^ do
      begin
        Name := 'Filename';
        VariableAddress := @FFileName;
        Direction := drIn;
        Datatype := dtText;
        DefaultValue := '';
        Flags := [iofFilename];
        DatatypeExtra := 'ps'; // file extension
      end;
    // typical input plug (inputs are listed first)
    pinInput:
      with Properties^ do
      begin
        Name := 'Input';
        VariableAddress := @FInputBuffer;
        Direction := drIn;
        Datatype := dtFSample;
        DefaultValue := '0';
      end;
    // typical output plug
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

// An input plug has changed value
procedure TSEPascalScriptModule.PlugStateChange(const CurrentPin: TSEPin);
var
  StrLst: TStringList;
  FN: TFileName;
begin
  case TSEPascalScriptPins(CurrentPin.PinID) of
    pinFilename:
      begin
        FN := ResolveFileName(0);

        if FileExists(FN) then
        begin
          StrLst := TStringList.Create;
          try
            StrLst.LoadFromFile(FN);
            if not FPSCompiler.Compile(StrLst.Text) then
              raise Exception.Create('could not compile script')
            else
            begin
              FPSCompiler.GetOutput(FByteCode);
              ByteCodeLoaded;
            end;
          finally
            FreeAndNil(StrLst);
          end;
        end;
      end;
  end;

  (*
    if (Pin[1].Status <> stRun) or (Pin[2].Status <> stRun)
    then SEAudioMasterSleepMode
  *)

  inherited;
end;

end.
