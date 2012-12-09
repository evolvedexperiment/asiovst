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
{******************************************************************************}

unit DAV_FileFormat;

interface

{$I ..\DAV_Compiler.Inc}

uses
  Classes, Contnrs, SysUtils, DAV_Types, DAV_ChunkClasses;

type
  TDavCustomFileFormat = class(TInterfacedPersistent, IStreamPersist)
  public
    procedure LoadFromStream(Stream: TStream); virtual; abstract;
    procedure SaveToStream(Stream: TStream); virtual; abstract;
    procedure LoadFromFile(FileName: TFileName); virtual;
    procedure SaveToFile(FileName: TFileName); virtual;
  end;

  TDavCustomFileFormatFile = class(TInterfacedPersistent, IStreamPersist)
  private
    FStream: TStream;
    FOwnsStream: Boolean;
  protected
    procedure LoadFromStream(Stream: TStream); virtual; abstract;
    procedure SaveToStream(Stream: TStream); virtual; abstract;
    class function DefaultExtension: TFileName; virtual; abstract;
  public
    constructor Create; overload; virtual;
    constructor Create(FileName: TFileName); overload; virtual;
    constructor Create(Stream: TStream); overload; virtual;
    destructor Destroy; override;

    procedure LoadFromFile(FileName: TFileName); virtual;
    procedure SaveToFile(FileName: TFileName); virtual;
  end;

  TDavAudioFile = class(TDavCustomFileFormatFile)

  end;

implementation

{ TDavCustomFileFormat }

procedure TDavCustomFileFormat.LoadFromFile(FileName: TFileName);
var
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(FileName, fmOpenRead);
  with FileStream do
    try
      LoadFromStream(FileStream);
    finally
      Free;
    end;
end;

procedure TDavCustomFileFormat.SaveToFile(FileName: TFileName);
var
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(FileName, fmCreate);
  with FileStream do
    try
      SaveToStream(FileStream);
    finally
      Free;
    end;
end;

{ TDavCustomFileFormatFile }

constructor TDavCustomFileFormatFile.Create;
begin
  inherited Create;
  FOwnsStream := False;
end;

constructor TDavCustomFileFormatFile.Create(FileName: TFileName);
begin
  Create;

  if FileName <> '' then
    if FileExists(FileName) then
      FStream := TFileStream.Create(FileName, fmOpenReadWrite)
    else
      FStream := TFileStream.Create(FileName, fmCreate);

  FOwnsStream := True;

  if Assigned(FStream) then
    LoadFromStream(FStream);
end;

constructor TDavCustomFileFormatFile.Create(Stream: TStream);
begin
  Create;

  FStream := Stream;

  if Assigned(FStream) then
    LoadFromStream(FStream);
end;

destructor TDavCustomFileFormatFile.Destroy;
begin
  if FOwnsStream then
    FreeAndNil(FStream);

  inherited;
end;

procedure TDavCustomFileFormatFile.LoadFromFile(FileName: TFileName);
var
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(FileName, fmOpenRead);
  with FileStream do
    try
      LoadFromStream(FileStream);
    finally
      Free;
    end;
end;

procedure TDavCustomFileFormatFile.SaveToFile(FileName: TFileName);
var
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(FileName, fmCreate);
  with FileStream do
    try
      SaveToStream(FileStream);
    finally
      Free;
    end;
end;

end.
