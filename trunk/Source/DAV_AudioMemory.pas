unit DAV_AudioMemory;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  Version: MPL 1.1 or LGPL 2.1 with linking exception                       //
//                                                                            //
//  The contents of this file are subject to the Mozilla Public License       //
//  Version 1.1 (the "License"); you may not use this file except in          //
//  compliance with the License. You may obtain a copy of the License at      //
//  http://www.mozilla.org/MPL/                                               //
//                                                                            //
//  Software distributed under the License is distributed on an "AS IS"       //
//  basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the   //
//  License for the specific language governing rights and limitations under  //
//  the License.                                                              //
//                                                                            //
//  Alternatively, the contents of this file may be used under the terms of   //
//  the Free Pascal modified version of the GNU Lesser General Public         //
//  License Version 2.1 (the "FPC modified LGPL License"), in which case the  //
//  provisions of this license are applicable instead of those above.         //
//  Please see the file LICENSE.txt for additional information concerning     //
//  this license.                                                             //
//                                                                            //
//  The code is part of the Delphi ASIO & VST Project                         //
//                                                                            //
//  The initial developer of this code is Christian-W. Budde                  //
//                                                                            //
//  Portions created by Christian-W. Budde are Copyright (C) 2008-2009        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I DAV_Compiler.inc}

uses
  {$IFDEF FPC}LCLIntf, {$ELSE}Windows, {$ENDIF} Classes, SysUtils,
  DAV_Classes, DAV_Types;

type
  TCustomAudioMemory = class(TPersistent)
  private
    FSampleCount : Cardinal;
    procedure SetSampleCount(const Value: Cardinal);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure SampleCountChanged(NewSampleCount: Cardinal); virtual;
  public
    constructor Create(SampleCount: Cardinal = 0); virtual;
    procedure Clear; virtual; abstract;

    property SampleCount: Cardinal read FSampleCount write SetSampleCount;
  end;

  TAudioMemory32 = class(TCustomAudioMemory)
  private
    FData : PDAVSingleFixedArray;
    function GetData(Sample: Cardinal): Single;
    procedure SetData(Sample: Cardinal; const Value: Single);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure SampleCountChanged(NewSampleCount: Cardinal); override;
  public
    constructor Create(SampleCount: Cardinal = 0); override;
    destructor Destroy; override;
    procedure Clear; override;

    // data access properties
    property Data[Sample: Cardinal]: Single read GetData write SetData;
    property DataPointer: PDAVSingleFixedArray read FData;
  published
    property SampleCount;
  end;

  TAudioMemory64 = class(TCustomAudioMemory)
  private
    FData  : PDAVDoubleFixedArray;
    function GetData(Sample: Cardinal): Double;
    procedure SetData(Sample: Cardinal; const Value: Double);
  protected
    procedure SampleCountChanged(NewSampleCount: Cardinal); override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(SampleCount: Cardinal = 0); override;
    destructor Destroy; override;
    procedure Clear; override;

    // data access properties
    property Data[Sample: Cardinal]: Double read GetData write SetData;
    property DataPointer: PDAVDoubleFixedArray read FData;
  published
    property SampleCount;
  end;

implementation

uses
  DAV_BlockProcessing;

{ TCustomAudioMemory }

constructor TCustomAudioMemory.Create(SampleCount: Cardinal = 0);
begin
 inherited Create;
 FSampleCount := SampleCount;
end;

procedure TCustomAudioMemory.AssignTo(Dest: TPersistent);
begin
 inherited;
 if Dest is TCustomAudioMemory then
  begin
   TCustomAudioMemory(Dest).FSampleCount  := FSampleCount;
  end;
end;

procedure TCustomAudioMemory.SampleCountChanged(NewSampleCount: Cardinal);
begin
 FSampleCount := NewSampleCount;
end;

procedure TCustomAudioMemory.SetSampleCount(const Value: Cardinal);
begin
 if (FSampleCount <> Value) then
  begin
   SampleCountChanged(Value);
  end;
end;

{ TAudioMemory32 }

constructor TAudioMemory32.Create(SampleCount: Cardinal = 0);
begin
 FData := nil;
 inherited Create(SampleCount);
end;

destructor TAudioMemory32.Destroy;
begin
 Dispose(FData);
 FData := nil;
 inherited;
end;

procedure TAudioMemory32.AssignTo(Dest: TPersistent);
begin
 inherited;
 if Dest is TAudioMemory32
  then Move(FData, TAudioMemory32(Dest).FData, FSampleCount * SizeOf(Single))
  else
 if Dest is TAudioMemory64
  then ConvertSingleToDouble(FData, TAudioMemory64(Dest).FData, FSampleCount);
end;

function TAudioMemory32.GetData(Sample: Cardinal): Single;
begin
 if (Sample < SampleCount)
  then Result := FData[Sample]
  else raise Exception.CreateFmt('Sample out of range', [Sample]);
end;

procedure TAudioMemory32.SampleCountChanged(NewSampleCount: Cardinal);
begin
 ReallocMem(FData, SampleCount * SizeOf(Single));

 // check if new length is longer than the old length and fill with zeroes if necessary
 if SampleCount > Self.SampleCount
  then FillChar(FData^[Self.SampleCount], (SampleCount - Self.SampleCount) * SizeOf(Single), 0);

 inherited;
end;

procedure TAudioMemory32.SetData(Sample: Cardinal; const Value: Single);
begin
 if (Sample < SampleCount)
  then FData[Sample] := Value
  else raise Exception.CreateFmt('Sample out of range', [Sample]);
end;

procedure TAudioMemory32.Clear;
begin
 FillChar(FData^, SampleCount * SizeOf(Single), 0);
end;

{ TAudioMemory64 }

constructor TAudioMemory64.Create(SampleCount: Cardinal = 0);
begin
 FData := nil;
 inherited Create(SampleCount);
end;

destructor TAudioMemory64.Destroy;
begin
 Dispose(FData);
 FData := nil;
 inherited;
end;

procedure TAudioMemory64.AssignTo(Dest: TPersistent);
begin
 inherited;
 if Dest is TAudioMemory64
  then Move(FData, TAudioMemory64(Dest).FData, FSampleCount * SizeOf(Double))
  else
 if Dest is TAudioMemory32
  then ConvertDoubleToSingle(FData, TAudioMemory32(Dest).FData, FSampleCount);
end;

function TAudioMemory64.GetData(Sample: Cardinal): Double;
begin
 if (Sample < SampleCount)
  then Result := FData[Sample]
  else raise Exception.CreateFmt('Sample out of range', [Sample]);
end;

procedure TAudioMemory64.SampleCountChanged(NewSampleCount: Cardinal);
begin
 ReallocMem(FData, SampleCount * SizeOf(Double));

 // check if new length is longer than the old length and fill with zeroes if necessary
 if SampleCount > Self.SampleCount
  then FillChar(FData^[Self.SampleCount], (SampleCount - Self.SampleCount) * SizeOf(Double), 0);

 inherited;
end;

procedure TAudioMemory64.SetData(Sample: Cardinal; const Value: Double);
begin
 if (Sample < SampleCount)
  then FData[Sample] := Value
  else raise Exception.CreateFmt('Sample out of range', [Sample]);
end;

procedure TAudioMemory64.Clear;
begin
 FillChar(FData^, SampleCount * SizeOf(Single), 0);
end;

end.
