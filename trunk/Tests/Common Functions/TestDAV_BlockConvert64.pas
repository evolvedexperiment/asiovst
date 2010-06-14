unit TestDAV_BlockConvert64;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2010             //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////


interface

uses
  Classes, SysUtils, TestFramework, DAV_AudioMemory, DAV_Classes, DAV_Types,
  DAV_Bindings;

type
  TCustomTestConvertToFloat64 = class(TTestCase)
  strict protected
    FAudioMemory64 : TAudioMemory64;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TCustomTestConvertInt16ToFloat64 = class(TCustomTestConvertToFloat64)
  protected
    FData            : PWord;
    FFunctionBinding : TFunctionBinding;
    procedure FillData; virtual; abstract;
    procedure PerformSimpleTest;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure BasicTest;
    procedure NativeTest;
    procedure UnalignedBufferTest;
    procedure SpeedTestNative;
    procedure SpeedTestSSE2;
  end;

  TTestConvertInt16LSBToFloat64 = class(TCustomTestConvertInt16ToFloat64)
  protected
    procedure FillData; override;
  public
    procedure SetUp; override;
  end;

  TTestConvertInt16MSBToFloat64 = class(TCustomTestConvertInt16ToFloat64)
  protected
    procedure FillData; override;
  public
    procedure SetUp; override;
  end;


  TCustomTestConvertInt24LSBToFloat64 = class(TCustomTestConvertToFloat64)
  protected
    FData            : Pointer;
    FFunctionBinding : TFunctionBinding;
    procedure FillData; virtual; abstract;
    procedure PerformSimpleTest;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure BasicTest;
    procedure NativeTest;
    procedure UnalignedBufferTest;
    procedure SpeedTestNative;
    procedure SpeedTestSSE2;
  end;

  TTestConvertInt24LSBToFloat64 = class(TCustomTestConvertInt24LSBToFloat64)
  protected
    procedure FillData; override;
  public
    procedure SetUp; override;
  end;

  TTestConvertInt24MSBToFloat64 = class(TCustomTestConvertInt24LSBToFloat64)
  protected
    procedure FillData; override;
  public
    procedure SetUp; override;
  end;


  TTestFunction = procedure(Destination, Source: Pointer; Count: Integer);

  TCustomTestConvertInt32ToFloat64 = class(TCustomTestConvertToFloat64)
  protected
    FData            : PInteger;
    FTestMaxValue    : Integer;
    FScale           : Double;
    FFunctionBinding : TFunctionBinding;
    FDelta           : Single;
    procedure FillData; virtual; abstract;
    procedure PerformSimpleTest;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure BasicTest;
    procedure NativeTest;
    procedure UnalignedBufferTest;
    procedure SpeedTestNative;
    procedure SpeedTestSSE2;
  end;

  TCustomTestConvertInt32LSBToFloat64 = class(TCustomTestConvertInt32ToFloat64)
  protected
    procedure FillData; override;
  end;

  TCustomTestConvertInt32MSBToFloat64 = class(TCustomTestConvertInt32ToFloat64)
  protected
    procedure FillData; override;
  end;

  TTestConvertInt32LSBToFloat64 = class(TCustomTestConvertInt32LSBToFloat64)
  public
    procedure SetUp; override;
  end;

  TTestConvertInt32LSB16ToFloat64 = class(TCustomTestConvertInt32LSBToFloat64)
  public
    procedure SetUp; override;
  end;

  TTestConvertInt32LSB18ToFloat64 = class(TCustomTestConvertInt32LSBToFloat64)
  public
    procedure SetUp; override;
  end;

  TTestConvertInt32LSB20ToFloat64 = class(TCustomTestConvertInt32LSBToFloat64)
  public
    procedure SetUp; override;
  end;

  TTestConvertInt32LSB24ToFloat64 = class(TCustomTestConvertInt32LSBToFloat64)
  public
    procedure SetUp; override;
  end;

  TTestConvertInt32MSBToFloat64 = class(TCustomTestConvertInt32MSBToFloat64)
  public
    procedure SetUp; override;
  end;

  TTestConvertInt32MSB16ToFloat64 = class(TCustomTestConvertInt32MSBToFloat64)
  public
    procedure SetUp; override;
  end;

  TTestConvertInt32MSB18ToFloat64 = class(TCustomTestConvertInt32MSBToFloat64)
  public
    procedure SetUp; override;
  end;

  TTestConvertInt32MSB20ToFloat64 = class(TCustomTestConvertInt32MSBToFloat64)
  public
    procedure SetUp; override;
  end;

  TTestConvertInt32MSB24ToFloat64 = class(TCustomTestConvertInt32MSBToFloat64)
  public
    procedure SetUp; override;
  end;

implementation

uses
  DAV_Common, DAV_BlockConvert64;

const
  CTestSampleCount = 1 shl 15;
  CSpeedTestCount  = 1 shl 13;


{ TCustomTestConvertToFloat64 }

procedure TCustomTestConvertToFloat64.SetUp;
begin
 inherited;
 FAudioMemory64 := TAudioMemory64.Create;
 FAudioMemory64.SampleCount := CTestSampleCount;
end;

procedure TCustomTestConvertToFloat64.TearDown;
begin
 FreeAndNil(FAudioMemory64);
 inherited;
end;


{ TCustomTestConvertInt16ToFloat64 }

procedure TCustomTestConvertInt16ToFloat64.SetUp;
begin
 inherited;

 // allocate data memory
 GetMem(FData, FAudioMemory64.SampleCount * SizeOf(Word));

 // fill data memory
 FillData;
end;

procedure TCustomTestConvertInt16ToFloat64.TearDown;
begin
 Dispose(FData);
 inherited;
end;

procedure TCustomTestConvertInt16ToFloat64.PerformSimpleTest;
var
  SampleIndex  : Integer;
  TestFunction : TTestFunction;
begin
 // clear audio memory
 FAudioMemory64.Clear;

 // assign function under test
 TestFunction := Pointer(FFunctionBinding.Prototype^);

 // convert integer data to float
 TestFunction(PDouble(FAudioMemory64.DataPointer), FData,
   FAudioMemory64.SampleCount);

 // check if data has been converted correctly
 for SampleIndex := 0 to FAudioMemory64.SampleCount - 1
  do CheckEquals((SampleIndex / $7FFF), FAudioMemory64.DataPointer^[SampleIndex], 1E-7);
end;

procedure TCustomTestConvertInt16ToFloat64.BasicTest;
var
  TestFunction : TTestFunction;
begin
 // use processor specific binding
 FFunctionBinding.RebindProcessorSpecific;

 // assign function under test
 TestFunction := Pointer(FFunctionBinding.Prototype^);

 // check if a sample count of 0 can be passed
 TestFunction(PDouble(FAudioMemory64.DataPointer), FData, 0);

 PerformSimpleTest;
end;

procedure TCustomTestConvertInt16ToFloat64.NativeTest;
var
  TestFunction : TTestFunction;
begin
 // use processor specific binding
 FFunctionBinding.RebindProcessorSpecific;

 // assign function under test
 TestFunction := Pointer(FFunctionBinding.Prototype^);

 // check if a sample count of 0 can be passed
 TestFunction(PDouble(FAudioMemory64.DataPointer), FData, 0);

 PerformSimpleTest;
end;

procedure TCustomTestConvertInt16ToFloat64.UnalignedBufferTest;
var
  SampleIndex  : Integer;
  DataPointer  : PWord;
  FloatPointer : PDouble;
  TestFunction : TTestFunction;
begin
 // use processor specific binding
 FFunctionBinding.RebindProcessorSpecific;

 // clear audio memory
 FAudioMemory64.Clear;

 // get unaligned pointers
 DataPointer := FData;
 Inc(DataPointer);
 FloatPointer := PDouble(FAudioMemory64.DataPointer);
 Inc(FloatPointer);

 // assign function under test
 TestFunction := Pointer(FFunctionBinding.Prototype^);

 // convert integer data to float
 TestFunction(FloatPointer, DataPointer, 1);

 // check if sample has been converted correctly
 CheckEquals(1 / $7FFF, FAudioMemory64.Data[1], 1E-7);

 Inc(DataPointer);

 // convert integer data to float
 TestFunction(FloatPointer, DataPointer,
   FAudioMemory64.SampleCount - 17);

 // check if data has been converted correctly
 for SampleIndex := 1 to FAudioMemory64.SampleCount - 17
  do CheckEquals(((SampleIndex + 1) / $7FFF), FAudioMemory64.DataPointer^[SampleIndex], 1E-7);
end;

procedure TCustomTestConvertInt16ToFloat64.SpeedTestNative;
var
  LoopIndex    : Integer;
  SampleIndex  : Integer;
  TestFunction : TTestFunction;
begin
 // use compatibility binding
 FFunctionBinding.Rebind([]);

 // assign function under test
 TestFunction := Pointer(FFunctionBinding.Prototype^);

 // process a sequence of block converts
 for LoopIndex := 1 to CSpeedTestCount
  do TestFunction(PDouble(FAudioMemory64.DataPointer), FData,
       FAudioMemory64.SampleCount);

 // check if data has been converted correctly
 for SampleIndex := 0 to FAudioMemory64.SampleCount - 1
  do CheckEquals(SampleIndex / $7FFF, FAudioMemory64.DataPointer^[SampleIndex], 1E-7);
end;

procedure TCustomTestConvertInt16ToFloat64.SpeedTestSSE2;
var
  LoopIndex    : Integer;
  SampleIndex  : Integer;
  TestFunction : TTestFunction;
begin
 // use SSE/SSE2 binding
 FFunctionBinding.Rebind([pfSSE, pfSSE2]);

 // assign function under test
 TestFunction := Pointer(FFunctionBinding.Prototype^);

 // process a sequence of block converts
 for LoopIndex := 1 to CSpeedTestCount
  do TestFunction(PDouble(FAudioMemory64.DataPointer), FData,
       FAudioMemory64.SampleCount);

 // check if data has been converted correctly
 for SampleIndex := 0 to FAudioMemory64.SampleCount - 1
  do CheckEquals(SampleIndex / $7FFF, FAudioMemory64.DataPointer^[SampleIndex], 1E-7);
end;


{ TTestConvertInt16LSBToFloat64 }

procedure TTestConvertInt16LSBToFloat64.FillData;
var
  SampleIndex : Integer;
  Data        : PWord;
begin
 inherited;

 Data := FData;

 // fill data with an increasing sequence
 for SampleIndex := 0 to FAudioMemory64.SampleCount - 1 do
  begin
   Data^ := SampleIndex;
   Inc(Data);
  end;
end;

procedure TTestConvertInt16LSBToFloat64.SetUp;
begin
 FFunctionBinding := BindingBlockConvertInt16LSBToFloat64;

 inherited;
end;


{ TTestConvertInt16MSBToFloat64 }

procedure TTestConvertInt16MSBToFloat64.FillData;
var
  SampleIndex : Integer;
  Data        : PWord;
begin
 inherited;

 Data := FData;

 // fill data with an increasing sequence
 for SampleIndex := 0 to FAudioMemory64.SampleCount - 1 do
  begin
   Data^ := Swap16(SampleIndex);
   Inc(Data);
  end;
end;

procedure TTestConvertInt16MSBToFloat64.SetUp;
begin
 FFunctionBinding := BindingBlockConvertInt16MSBToFloat64;

 inherited;
end;


{ TCustomTestConvertInt24LSBToFloat64 }

procedure TCustomTestConvertInt24LSBToFloat64.SetUp;
begin
 inherited;

 // allocate data memory
 GetMem(FData, FAudioMemory64.SampleCount * 3);

 // fill data memory
 FillData;
end;

procedure TCustomTestConvertInt24LSBToFloat64.TearDown;
begin
 Dispose(FData);
 inherited;
end;

procedure TCustomTestConvertInt24LSBToFloat64.PerformSimpleTest;
var
  SampleIndex  : Integer;
  TestFunction : TTestFunction;
begin
 // clear audio memory
 FAudioMemory64.Clear;

 // assign function under test
 TestFunction := Pointer(FFunctionBinding.Prototype^);

 // convert integer data to float
 TestFunction(PDouble(FAudioMemory64.DataPointer), FData,
   FAudioMemory64.SampleCount);

 // check if data has been converted correctly
 for SampleIndex := 0 to FAudioMemory64.SampleCount - 1
  do CheckEquals((SampleIndex / $7FFFFF), FAudioMemory64.DataPointer^[SampleIndex], 1E-7);
end;

procedure TCustomTestConvertInt24LSBToFloat64.BasicTest;
var
  TestFunction : TTestFunction;
begin
 // use processor specific binding
 FFunctionBinding.RebindProcessorSpecific;

 // assign function under test
 TestFunction := Pointer(FFunctionBinding.Prototype^);

 // check if a sample count of 0 can be passed
 TestFunction(PDouble(FAudioMemory64.DataPointer), FData, 0);

 PerformSimpleTest;
end;

procedure TCustomTestConvertInt24LSBToFloat64.NativeTest;
var
  TestFunction : TTestFunction;
begin
 // use processor specific binding
 FFunctionBinding.RebindProcessorSpecific;

 // assign function under test
 TestFunction := Pointer(FFunctionBinding.Prototype^);

 // check if a sample count of 0 can be passed
 TestFunction(PDouble(FAudioMemory64.DataPointer), FData, 0);

 PerformSimpleTest;
end;

procedure TCustomTestConvertInt24LSBToFloat64.UnalignedBufferTest;
var
  SampleIndex  : Integer;
  DataPointer  : PByte;
  FloatPointer : PDouble;
  TestFunction : TTestFunction;
begin
 // use processor specific binding
 FFunctionBinding.RebindProcessorSpecific;

 // clear audio memory
 FAudioMemory64.Clear;

 // get unaligned pointers
 DataPointer := FData;
 Inc(DataPointer, 3);
 FloatPointer := PDouble(FAudioMemory64.DataPointer);
 Inc(FloatPointer);

 // assign function under test
 TestFunction := Pointer(FFunctionBinding.Prototype^);

 // convert integer data to float
 TestFunction(FloatPointer, DataPointer, 1);

 // check if sample has been converted correctly
 CheckEquals(1 / $7FFFFF, FAudioMemory64.Data[1], 1E-9);

 Inc(DataPointer, 3);

 // convert integer data to float
 TestFunction(FloatPointer, DataPointer,
   FAudioMemory64.SampleCount - 17);

 // check if data has been converted correctly
 for SampleIndex := 1 to FAudioMemory64.SampleCount - 17
  do CheckEquals(((SampleIndex + 1) / $7FFFFF), FAudioMemory64.DataPointer^[SampleIndex], 1E-7);
end;

procedure TCustomTestConvertInt24LSBToFloat64.SpeedTestNative;
var
  LoopIndex    : Integer;
  TestFunction : TTestFunction;
begin
 // use compatibility binding
 FFunctionBinding.Rebind([]);

 // assign function under test
 TestFunction := Pointer(FFunctionBinding.Prototype^);

 // process a sequence of block converts
 for LoopIndex := 1 to CSpeedTestCount
  do TestFunction(PDouble(FAudioMemory64.DataPointer),
       FData, FAudioMemory64.SampleCount);
end;

procedure TCustomTestConvertInt24LSBToFloat64.SpeedTestSSE2;
var
  LoopIndex    : Integer;
  TestFunction : TTestFunction;
begin
 // use SSE/SSE2 binding
 FFunctionBinding.Rebind([pfSSE, pfSSE2]);

 // assign function under test
 TestFunction := Pointer(FFunctionBinding.Prototype^);

 // process a sequence of block converts
 for LoopIndex := 1 to CSpeedTestCount
  do TestFunction(PDouble(FAudioMemory64.DataPointer),
       FData, FAudioMemory64.SampleCount);
end;


{ TTestConvertInt24LSBToFloat64 }

procedure TTestConvertInt24LSBToFloat64.FillData;
var
  SampleIndex : Integer;
  Data        : PByte;
begin
 Data := FData;

 // fill data with an increasing sequence (little endian)
 for SampleIndex := 0 to FAudioMemory64.SampleCount - 1 do
  begin
   Data^ :=  SampleIndex         and $FF; Inc(Data);
   Data^ := (SampleIndex shr  8) and $FF; Inc(Data);
   Data^ := (SampleIndex shr 16) and $FF; Inc(Data);
  end;
end;

procedure TTestConvertInt24LSBToFloat64.SetUp;
begin
 FFunctionBinding := BindingBlockConvertInt24LSBToFloat64;

 inherited;
end;


{ TTestConvertInt24MSBToFloat64 }

procedure TTestConvertInt24MSBToFloat64.FillData;
var
  SampleIndex : Integer;
  Data        : PByte;
begin
 Data := FData;

 // fill data with an increasing sequence (big endian)
 for SampleIndex := 0 to FAudioMemory64.SampleCount - 1 do
  begin
   Data^ := (SampleIndex shr 16) and $FF; Inc(Data);
   Data^ := (SampleIndex shr  8) and $FF; Inc(Data);
   Data^ :=  SampleIndex         and $FF; Inc(Data);
  end;
end;

procedure TTestConvertInt24MSBToFloat64.SetUp;
begin
 FFunctionBinding := BindingBlockConvertInt24MSBToFloat64;

 inherited;
end;


{ TCustomTestConvertInt32ToFloat64 }

procedure TCustomTestConvertInt32ToFloat64.SetUp;
begin
 inherited;

 // define test delta
 FDelta := 1E-9;

 // calculate scale
 Assert(FTestMaxValue <> 0);
 FScale := 1 / FTestMaxValue;

 // allocate data memory
 GetMem(FData, FAudioMemory64.SampleCount * SizeOf(Integer));

 // fill data memory
 FillData;
end;

procedure TCustomTestConvertInt32ToFloat64.TearDown;
begin
 Dispose(FData);
 inherited;
end;

procedure TCustomTestConvertInt32ToFloat64.PerformSimpleTest;
var
  SampleIndex  : Integer;
  TestFunction : TTestFunction;
begin
 // clear audio memory
 FAudioMemory64.Clear;

 // assign function under test
 TestFunction := Pointer(FFunctionBinding.Prototype^);

 // convert integer data to float
 TestFunction(PDouble(FAudioMemory64.DataPointer), FData,
   FAudioMemory64.SampleCount);

 // check if data has been converted correctly
 for SampleIndex := 0 to FAudioMemory64.SampleCount - 1
  do CheckEquals(SampleIndex * FScale, FAudioMemory64.DataPointer^[SampleIndex], FDelta);
end;

procedure TCustomTestConvertInt32ToFloat64.BasicTest;
var
  TestFunction : TTestFunction;
begin
 // use processor specific binding
 FFunctionBinding.RebindProcessorSpecific;

 // assign function under test
 TestFunction := Pointer(FFunctionBinding.Prototype^);

 // check if a sample count of 0 can be passed
 TestFunction(PDouble(FAudioMemory64.DataPointer), FData, 0);

 PerformSimpleTest;
end;

procedure TCustomTestConvertInt32ToFloat64.NativeTest;
var
  TestFunction : TTestFunction;
begin
 // use processor specific binding
 FFunctionBinding.Rebind([]);

 // assign function under test
 TestFunction := Pointer(FFunctionBinding.Prototype^);

 // check if a sample count of 0 can be passed
 TestFunction(PDouble(FAudioMemory64.DataPointer), FData, 0);

 PerformSimpleTest;
end;

procedure TCustomTestConvertInt32ToFloat64.UnalignedBufferTest;
var
  SampleIndex  : Integer;
  DataPointer  : PInteger;
  FloatPointer : PDouble;
  TestFunction : TTestFunction;
begin
 // use processor specific binding
 FFunctionBinding.RebindProcessorSpecific;

 // clear audio memory
 FAudioMemory64.Clear;

 // get unaligned pointers
 DataPointer := FData;
 Inc(DataPointer);
 FloatPointer := PDouble(FAudioMemory64.DataPointer);
 Inc(FloatPointer);

 // assign function under test
 TestFunction := Pointer(FFunctionBinding.Prototype^);

 // convert integer data to float
 TestFunction(FloatPointer, DataPointer, 1);

 // check if sample has been converted correctly
 CheckEquals(1 * FScale, FAudioMemory64.Data[1], FDelta);

 Inc(DataPointer);

 // convert integer data to float
 TestFunction(FloatPointer, DataPointer, FAudioMemory64.SampleCount - 17);

 // check if data has been converted correctly
 for SampleIndex := 1 to FAudioMemory64.SampleCount - 17
  do CheckEquals(((SampleIndex + 1) * FScale), FAudioMemory64.DataPointer^[SampleIndex], FDelta);
end;

procedure TCustomTestConvertInt32ToFloat64.SpeedTestNative;
var
  LoopIndex    : Integer;
  SampleIndex  : Integer;
  TestFunction : TTestFunction;
begin
 // use compatibility binding
 FFunctionBinding.Rebind([]);

 // assign function under test
 TestFunction := Pointer(FFunctionBinding.Prototype^);

 // process a sequence of block converts
 for LoopIndex := 1 to CSpeedTestCount
  do TestFunction(PDouble(FAudioMemory64.DataPointer),
       FData, FAudioMemory64.SampleCount);

 // check if data has been converted correctly
 for SampleIndex := 0 to FAudioMemory64.SampleCount - 1
  do CheckEquals(SampleIndex * FScale, FAudioMemory64.DataPointer^[SampleIndex], FDelta);
end;

procedure TCustomTestConvertInt32ToFloat64.SpeedTestSSE2;
var
  LoopIndex    : Integer;
  SampleIndex  : Integer;
  TestFunction : TTestFunction;
begin
 // use SSE/SSE2 binding
 FFunctionBinding.Rebind([pfSSE, pfSSE2]);

 // assign function under test
 TestFunction := Pointer(FFunctionBinding.Prototype^);

 // process a sequence of block converts
 for LoopIndex := 1 to CSpeedTestCount
  do TestFunction(PDouble(FAudioMemory64.DataPointer),
       FData, FAudioMemory64.SampleCount);

 // check if data has been converted correctly
 for SampleIndex := 0 to FAudioMemory64.SampleCount - 1
  do CheckEquals(SampleIndex * FScale, FAudioMemory64.DataPointer^[SampleIndex], FDelta);
end;


{ TCustomTestConvertInt32LSBToFloat64 }

procedure TCustomTestConvertInt32LSBToFloat64.FillData;
var
  SampleIndex : Integer;
  Data        : PInteger;
begin
 Data := FData;

 // fill data with an increasing sequence
 for SampleIndex := 0 to FAudioMemory64.SampleCount - 1 do
  begin
   Data^ := SampleIndex;
   Inc(Data);
  end;
end;

{ TCustomTestConvertInt32MSBToFloat64 }

procedure TCustomTestConvertInt32MSBToFloat64.FillData;
var
  SampleIndex : Integer;
  Data        : PInteger;
begin
 Data := FData;

 // fill data with an increasing sequence
 for SampleIndex := 0 to FAudioMemory64.SampleCount - 1 do
  begin
   Data^ := Swap32(SampleIndex);
   Inc(Data);
  end;
end;


{ TTestConvertInt32LSBToFloat64 }

procedure TTestConvertInt32LSBToFloat64.SetUp;
begin
 FFunctionBinding := BindingBlockConvertInt32LSBToFloat64;
 FTestMaxValue    := $7FFFFFFF;
 inherited;
end;


{ TTestConvertInt32LSB16ToFloat64 }

procedure TTestConvertInt32LSB16ToFloat64.SetUp;
begin
 FFunctionBinding := BindingBlockConvertInt32LSB16ToFloat64;
 FTestMaxValue    := $7FFF;

 inherited;

 // use custom delta
 FDelta := 1E-7;
end;


{ TTestConvertInt32LSB18ToFloat64 }

procedure TTestConvertInt32LSB18ToFloat64.SetUp;
begin
 FFunctionBinding := BindingBlockConvertInt32LSB18ToFloat64;
 FTestMaxValue    := $1FFFF;

 inherited;

 // use custom delta
 FDelta := 1E-7;
end;


{ TTestConvertInt32LSB20ToFloat64 }

procedure TTestConvertInt32LSB20ToFloat64.SetUp;
begin
 FFunctionBinding := BindingBlockConvertInt32LSB20ToFloat64;
 FTestMaxValue    := $7FFFF;

 inherited;

 // use custom delta
 FDelta := 1E-7;
end;


{ TTestConvertInt32LSB24ToFloat64 }

procedure TTestConvertInt32LSB24ToFloat64.SetUp;
begin
 FFunctionBinding := BindingBlockConvertInt32LSB24ToFloat64;
 FTestMaxValue    := $7FFFFF;

 inherited;

 // use custom delta
 FDelta := 1E-7;
end;


{ TTestConvertInt32MSBToFloat64 }

procedure TTestConvertInt32MSBToFloat64.SetUp;
begin
 FFunctionBinding := BindingBlockConvertInt32MSBToFloat64;
 FTestMaxValue    := $7FFFFFFF;
 inherited;
end;


{ TTestConvertInt32MSB16ToFloat64 }

procedure TTestConvertInt32MSB16ToFloat64.SetUp;
begin
 FFunctionBinding := BindingBlockConvertInt32MSB16ToFloat64;
 FTestMaxValue    := $7FFF;

 inherited;

 // use custom delta
 FDelta := 1E-7;
end;


{ TTestConvertInt32MSB18ToFloat64 }

procedure TTestConvertInt32MSB18ToFloat64.SetUp;
begin
 FFunctionBinding := BindingBlockConvertInt32MSB18ToFloat64;
 FTestMaxValue    := $1FFFF;

 inherited;

 // use custom delta
 FDelta := 1E-7;
end;


{ TTestConvertInt32MSB20ToFloat64 }

procedure TTestConvertInt32MSB20ToFloat64.SetUp;
begin
 FFunctionBinding := BindingBlockConvertInt32MSB20ToFloat64;
 FTestMaxValue    := $7FFFF;

 inherited;

 // use custom delta
 FDelta := 1E-7;
end;


{ TTestConvertInt32MSB24ToFloat64 }

procedure TTestConvertInt32MSB24ToFloat64.SetUp;
begin
 FFunctionBinding := BindingBlockConvertInt32MSB24ToFloat64;
 FTestMaxValue    := $7FFFFF;

 inherited;

 // use custom delta
 FDelta := 1E-7;
end;


procedure RegisterTests;
var
  TestSuiteFloat: TTestSuite;
begin
  TestSuiteFloat := TTestSuite.Create('Float 64-bit (LSB)');
  with TestSuiteFloat do
   begin
    AddTest(TTestConvertInt16LSBToFloat64.Suite);
    AddTest(TTestConvertInt24LSBToFloat64.Suite);
    AddTest(TTestConvertInt32LSBToFloat64.Suite);
    AddTest(TTestConvertInt32LSB16ToFloat64.Suite);
    AddTest(TTestConvertInt32LSB18ToFloat64.Suite);
    AddTest(TTestConvertInt32LSB20ToFloat64.Suite);
    AddTest(TTestConvertInt32LSB24ToFloat64.Suite);
   end;
  RegisterTest(TestSuiteFloat);

  TestSuiteFloat := TTestSuite.Create('Float 64-bit (MSB)');
  with TestSuiteFloat do
   begin
    AddTest(TTestConvertInt16MSBToFloat64.Suite);
    AddTest(TTestConvertInt24MSBToFloat64.Suite);
    AddTest(TTestConvertInt32MSBToFloat64.Suite);
    AddTest(TTestConvertInt32MSB16ToFloat64.Suite);
    AddTest(TTestConvertInt32MSB18ToFloat64.Suite);
    AddTest(TTestConvertInt32MSB20ToFloat64.Suite);
    AddTest(TTestConvertInt32MSB24ToFloat64.Suite);
   end;
  RegisterTest(TestSuiteFloat);
end;


initialization
  RegisterTests;

end.
