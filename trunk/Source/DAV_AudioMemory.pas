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
  Classes, SysUtils, DAV_Classes, DAV_Types;

type
  TCustomAudioMemory = class(TPersistent)
  private
    FSampleCount  : Cardinal;
    FExternalData : Boolean;
    procedure SetSampleCount(const Value: Cardinal);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure SampleCountChanged(NewSampleCount: Cardinal); virtual;
  public
    constructor Create(SampleCount: Cardinal = 0; DataPointer: Pointer = nil); virtual;
    procedure Clear; virtual; abstract;

    // some simple signal processing functions
    procedure Scale32(Value: Single); virtual; abstract;
    procedure Scale64(Value: Double); virtual; abstract;
    procedure Offset32(Value: Single); virtual; abstract;
    procedure Offset64(Value: Double); virtual; abstract;

    property SampleCount: Cardinal read FSampleCount write SetSampleCount;
  end;

  TAudioMemory32 = class(TCustomAudioMemory)
  private
    FData: PDAVSingleFixedArray;
    function GetData(Sample: Cardinal): Single;
    procedure SetData(Sample: Cardinal; const Value: Single);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure SampleCountChanged(NewSampleCount: Cardinal); override;
  public
    constructor Create(SampleCount: Cardinal = 0; DataPointer: Pointer = nil); override;
    destructor Destroy; override;
    procedure Clear; override;

    // some signal processing functions
    procedure Scale32(Value: Single); override;
    procedure Scale64(Value: Double); override;
    procedure Offset32(Value: Single); override;
    procedure Offset64(Value: Double); override;

    // data access properties
    property Data[Sample: Cardinal]: Single read GetData write SetData;
    property DataPointer: PDAVSingleFixedArray read FData;
  published
    property SampleCount;
  end;

  TAudioMemory64 = class(TCustomAudioMemory)
  private
    FData: PDAVDoubleFixedArray;
    function GetData(Sample: Cardinal): Double;
    procedure SetData(Sample: Cardinal; const Value: Double);
  protected
    procedure SampleCountChanged(NewSampleCount: Cardinal); override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(SampleCount: Cardinal = 0; DataPointer: Pointer = nil); override;
    destructor Destroy; override;
    procedure Clear; override;

    // some signal processing functions
    procedure Scale32(Value: Single); override;
    procedure Scale64(Value: Double); override;
    procedure Offset32(Value: Single); override;
    procedure Offset64(Value: Double); override;

    // data access properties
    property Data[Sample: Cardinal]: Double read GetData write SetData;
    property DataPointer: PDAVDoubleFixedArray read FData;
  published
    property SampleCount;
  end;

implementation

uses
  DAV_BlockConvert, DAV_BlockArithmetrics;

resourcestring
  RCStrExternalDataSampleCount = 'Sample count can not be changed for external data!';
  RCStrSampleOutOfRange = 'Sample out of range';

{ TCustomAudioMemory }

constructor TCustomAudioMemory.Create(SampleCount: Cardinal = 0; DataPointer: Pointer = nil);
begin
 inherited Create;
 FExternalData := DataPointer <> nil;
 FSampleCount := SampleCount;
end;

procedure TCustomAudioMemory.AssignTo(Dest: TPersistent);
begin
 inherited;
 if Dest is TCustomAudioMemory then
  with TCustomAudioMemory(Dest)
   do SampleCount := Self.SampleCount;
end;

procedure TCustomAudioMemory.SampleCountChanged(NewSampleCount: Cardinal);
begin
 // store new sample count
 FSampleCount := NewSampleCount;
end;

procedure TCustomAudioMemory.SetSampleCount(const Value: Cardinal);
begin
 if (FSampleCount <> Value) then
  begin
   // check if data is external data (fixed sample count)
   if FExternalData
    then raise Exception.Create(RCStrExternalDataSampleCount);

   // actually change sample count
   SampleCountChanged(Value);
  end;
end;


{ TAudioMemory32 }

constructor TAudioMemory32.Create(SampleCount: Cardinal = 0; DataPointer: Pointer = nil);
begin
 FData := DataPointer;
 inherited Create(SampleCount, DataPointer);
end;

destructor TAudioMemory32.Destroy;
begin
 // eventually dispose allocated memory
 if not FExternalData
  then Dispose(FData);

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
  then BlockConvert32To64(PDouble(TAudioMemory64(Dest).FData), PSingle(FData),
    FSampleCount);
end;

function TAudioMemory32.GetData(Sample: Cardinal): Single;
begin
 if (Sample < SampleCount)
  then Result := FData[Sample]
  else raise Exception.CreateFmt(RCStrSampleOutOfRange, [Sample]);
end;

procedure TAudioMemory32.Offset32(Value: Single);
begin
 BlockOffsetInplace32(PSingle(FData), Value, FSampleCount);
end;

procedure TAudioMemory32.Offset64(Value: Double);
begin
 BlockOffsetInplace32(PSingle(FData), Value, FSampleCount);
end;

procedure TAudioMemory32.Scale32(Value: Single);
begin
 BlockScaleInplace32(PSingle(FData), Value, FSampleCount);
end;

procedure TAudioMemory32.Scale64(Value: Double);
begin
 BlockScaleInplace32(PSingle(FData), Value, FSampleCount);
end;

procedure TAudioMemory32.SampleCountChanged(NewSampleCount: Cardinal);
begin
 Assert(not FExternalData);
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
  else raise Exception.CreateFmt(RCStrSampleOutOfRange, [Sample]);
end;

procedure TAudioMemory32.Clear;
begin
 FillChar(FData^, SampleCount * SizeOf(Single), 0);
end;


{ TAudioMemory64 }

constructor TAudioMemory64.Create(SampleCount: Cardinal = 0; DataPointer: Pointer = nil);
begin
 FData := DataPointer;
 inherited Create(SampleCount, DataPointer);
end;

destructor TAudioMemory64.Destroy;
begin
 // eventually dispose allocated memory
 if not FExternalData
  then Dispose(FData);

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
  then BlockConvert64To32(PSingle(TAudioMemory32(Dest).FData), PDouble(FData),
    FSampleCount);
end;

function TAudioMemory64.GetData(Sample: Cardinal): Double;
begin
 if (Sample < SampleCount)
  then Result := FData[Sample]
  else raise Exception.CreateFmt(RCStrSampleOutOfRange, [Sample]);
end;

procedure TAudioMemory64.Offset32(Value: Single);
begin
 BlockOffsetInplace64(PDouble(FData), Value, FSampleCount);
end;

procedure TAudioMemory64.Offset64(Value: Double);
begin
 BlockOffsetInplace64(PDouble(FData), Value, FSampleCount);
end;

procedure TAudioMemory64.Scale32(Value: Single);
begin
 BlockScaleInplace64(PDouble(FData), Value, FSampleCount);
end;

procedure TAudioMemory64.Scale64(Value: Double);
begin
 BlockScaleInplace64(PDouble(FData), Value, FSampleCount);
end;

procedure TAudioMemory64.SampleCountChanged(NewSampleCount: Cardinal);
begin
 Assert(not FExternalData);
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
  else raise Exception.CreateFmt(RCStrSampleOutOfRange, [Sample]);
end;

procedure TAudioMemory64.Clear;
begin
 FillChar(FData^, SampleCount * SizeOf(Single), 0);
end;

end.
