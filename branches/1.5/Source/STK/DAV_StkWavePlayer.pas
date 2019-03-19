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
{******************************************************************************}

unit DAV_StkWavePlayer;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

{ TStkWavePlayer }

interface

{$I ..\DAV_Compiler.inc}

uses
  Windows, SysUtils, DAV_Common, DAV_StkCommon, DAV_StkLfo, DAV_AudioFileWAV,
  DAV_AudioFileAIFF, DAV_AudioFileAU, DAV_AudioData;

type
  TStkWavePlayer = class(TStkLFO)
  private
    FOneShot: Boolean;
    FSampleData: PSingle;
    FLoopstart: Integer;
    FLoopend: Integer;
    FStart: Single;
    FEnd: Single;

    procedure SetOneShot(const Value: Boolean);
  protected
    FSize: Longint;
    FInvSize: Single;
    FLength: Single;
    FIsFinished: Boolean;

    procedure OneShotChanged; virtual;

    // Sets the playback rate
    procedure SetRate(const Value: Single);

    // Set the loop points
    procedure SetLoop(StartPosition, EndPosition: Integer);

    // Seek to position in file
    procedure SetPos(const Value: Longint);
  public
    // Class constructor, taking the desired number of modes to create.
    constructor Create(const SampleRate: Single); overload; override;

    // Overloaded constructor, load file on create
    constructor Create(const SampleRate: Single; const FileName: String);
      reintroduce; overload; virtual;

    // Class destructor.
    destructor Destroy; override;

    // Reset and clear all internal state.
    procedure Reset; virtual;

    // Loads a WAV audio file
    procedure LoadFile(const FileName: TFileName);

    // Compute one output sample.
    function Tick: Single; override;

    property OneShot: Boolean read FOneShot write SetOneShot;
    property Size: Longint read FSize;
    property Length: Single read FLength;
    property IsFinished: Boolean read FIsFinished;
  end;

implementation

constructor TStkWavePlayer.Create(const SampleRate: Single);
begin
  inherited Create(SampleRate);
  FSize := 0;
  FSampleData := nil;
  FLoopstart := 0;
  FLoopend := 0;
  FStart := 0;
  FEnd := 0;
end;

constructor TStkWavePlayer.Create(const SampleRate: Single;
  const FileName: String);
begin
  if SampleRate <= 0 then
    raise Exception.Create('Samplerate must be larger than zero');
  inherited Create(SampleRate);
  FSize := 0;
  FSampleData := nil;
  FLoopstart := 0;
  FLoopend := 0;
  FStart := 0;
  FEnd := 0;
  LoadFile(FileName);
end;

destructor TStkWavePlayer.Destroy;
begin
  inherited Destroy;
  if Assigned(FSampleData) then
    Dispose(FSampleData);
end;

procedure TStkWavePlayer.Reset;
begin
  SetPos(0);
end;

procedure TStkWavePlayer.LoadFile(const FileName: TFileName);
var
  ADC: TAudioDataCollection32;
begin
  ADC := TAudioDataCollection32.Create(nil);
  with ADC do
    try
      LoadFromFile(FileName);
      FSize := ADC.SampleFrames;
      FInvSize := 1 / FSize;
      GetMem(FSampleData, FSize * SizeOf(Single));
      Move(ADC[0].ChannelDataPointer^[0], FSampleData^, FSize * SizeOf(Single));
      FLength := FSize * FSampleRateInv;
      FLoopstart := 0;
      FLoopend := FSize - 1;
      FStart := 0;
      FEnd := (FSize - 1) * FInvSize;
      FOneShot := True;
      FIsFinished := False;
    finally
      FreeAndNil(ADC);
    end;
end;

function TStkWavePlayer.Tick: Single;
var
  x, y: Longint;
  q: Single;
  s1, s2: PSingle;
begin
  Phase := Phase + (FFreq * FSampleRateInv);
  if FOneShot then
  begin
    FIsFinished := (Phase >= 1);
    if FIsFinished then
    begin
      Result := 0;
      exit;
    end;
  end
  else
    while (Phase >= 1) do
      Phase := Phase - 1;
  q := (Phase * (FEnd - FStart) + FStart);
  if q > 1 then
    q := 1
  else if q < 0 then
    q := 0;
  q := q * FSize;
  x := Round(q);
  q := q - x;
  s1 := PSingle(Longint(FSampleData) + 4 * x);
  y := x + 1;
  if y > FLoopend then
    y := FLoopstart;
  s2 := PSingle(Longint(FSampleData) + 4 * y);
  Result := s1^ * (1 - q) + s2^ * q;
end;

procedure TStkWavePlayer.SetLoop(StartPosition, EndPosition: Integer);
begin
  if StartPosition < 0 then
    StartPosition := 0
  else if StartPosition > FSize - 1 then
    StartPosition := FSize - 1;
  if EndPosition < 1 then
    EndPosition := 1
  else if EndPosition > FSize - 1 then
    EndPosition := FSize - 1;
  if StartPosition > EndPosition then
    StartPosition := 0;
  FLoopstart := StartPosition;
  FLoopend := EndPosition;
  FStart := FLoopstart * FInvSize;
  FEnd := FLoopend * FInvSize;
end;

procedure TStkWavePlayer.SetPos(const Value: Longint);
begin
  Phase := Value * FInvSize;
end;

procedure TStkWavePlayer.SetRate(const Value: Single);
begin
  Frequency := Value;
end;

procedure TStkWavePlayer.SetOneShot(const Value: Boolean);
begin
  if FOneShot <> Value then
  begin
    FOneShot := Value;
    OneShotChanged;
  end;
end;

procedure TStkWavePlayer.OneShotChanged;
begin
  // nothing todo yet!
end;

end.
