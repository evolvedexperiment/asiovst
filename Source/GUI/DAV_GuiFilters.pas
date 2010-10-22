unit DAV_GuiFilters;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2008-2010        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I ..\DAV_Compiler.inc}

uses
  Graphics, Classes, SysUtils, DAV_Common, DAV_GuiCommon, DAV_GuiBlend,
  DAV_GuiPixelMap, DAV_GuiByteMap, DAV_DspFilter, DAV_DspFilterSimple,
  DAV_DspDelayLines;

type
  TGuiCustomFilter = class(TPersistent)
  public
    constructor Create; virtual; abstract;

    procedure Filter(ByteMap: TGuiCustomByteMap); overload; virtual; abstract;
    procedure Filter(PixelMap: TGuiCustomPixelMap); overload; virtual; abstract;
  end;

  TGuiCustomBlurFilter = class(TGuiCustomFilter)
  private
    FRadius           : Single;
    FRadiusReciprocal : Single;
    procedure SetRadius(const Value: Single);
  protected
    procedure RadiusChanged; virtual;
  public
    constructor Create; override;

    property Radius: Single read FRadius write SetRadius;
  end;

  TGuiBlurIIRFilter = class(TGuiCustomBlurFilter)
  private
    FIIRFilter : TFirstOrderLowpassFilter;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Filter(ByteMap: TGuiCustomByteMap); override;
    procedure Filter(PixelMap: TGuiCustomPixelMap); override;
  end;

  TGuiBlurFIRFilter = class(TGuiCustomBlurFilter)
  private
    FDelayLine : TDelayLineFractional32;
  protected
    procedure RadiusChanged; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Filter(ByteMap: TGuiCustomByteMap); override;
    procedure Filter(PixelMap: TGuiCustomPixelMap); override;
  end;

implementation

{ TGuiCustomBlurFilter }

constructor TGuiCustomBlurFilter.Create;
begin
 inherited;
 FRadius := 1;
end;

procedure TGuiCustomBlurFilter.RadiusChanged;
begin
 FRadiusReciprocal := 1 / FRadius;
end;

procedure TGuiCustomBlurFilter.SetRadius(const Value: Single);
begin
 if FRadius <= 0
  then raise Exception.Create('Radius must be larger than zero!');

 if FRadius <> Value then
  begin
   FRadius := Value;
   RadiusChanged;
  end;
end;


{ TGuiBlurIIRFilter }

constructor TGuiBlurIIRFilter.Create;
begin
 FIIRFilter := TFirstOrderLowpassFilter.Create;
end;

destructor TGuiBlurIIRFilter.Destroy;
begin
 FreeAndNil(FIIRFilter);
 inherited;
end;

procedure TGuiBlurIIRFilter.Filter(ByteMap: TGuiCustomByteMap);
var
  X, Y : Integer;
  Data : PByteArray;
begin
 with ByteMap do
  begin
   for Y := 0 to Height - 1 do
    begin
     Data := ScanLine[Y];
     FIIRFilter.Reset;
     for X := 0 to Width - 1
      do Data^[X] := Round(FIIRFilter.ProcessSample32(Data^[X]));
     for X := Width - 1 downto 0
      do Data^[X] := Round(FIIRFilter.ProcessSample32(Data^[X]));
    end;
  end;
end;

procedure TGuiBlurIIRFilter.Filter(PixelMap: TGuiCustomPixelMap);
var
  X, Y : Integer;
  Data : PPixel32Array;
begin
 raise Exception.Create('not yet implemented');

 with PixelMap do
  begin
   for Y := 0 to Height - 1 do
    begin
     Data := DataPointer;
     for X := 0 to Width - 1 do
      begin
//       Data^[X] := Round(FIIRFilter.ProcessSample32(Data^[X]);

      end;
    end;
  end;
end;


{ TGuiBlurFIRFilter }

constructor TGuiBlurFIRFilter.Create;
begin
 inherited;
 FDelayLine := TDelayLineFractional32.Create;
 FDelayLine.FractionalBufferSize := FRadius;
end;

destructor TGuiBlurFIRFilter.Destroy;
begin
 FreeAndNil(FDelayLine);
 inherited;
end;

procedure TGuiBlurFIRFilter.Filter(ByteMap: TGuiCustomByteMap);
var
  X, Y    : Integer;
  Data    : PByteArray;
  Current : Single;
  Sum     : Single;
begin
 with ByteMap do
  begin
   for Y := 0 to Height - 1 do
    begin
     Data := ScanLine[Y];
     FDelayLine.ClearBuffer;
     Sum := 0;
     for X := 0 to Width - 1 do
      begin
       Current := Data^[X] * COne255th;
       Sum := Sum + Current;
       Current := FDelayLine.ProcessSample32(Current);
       Sum := Sum - Current;
       if Sum > 0
        then Data^[X] := Round($FF * Sum * FRadiusReciprocal)
        else Data^[X] := 0;
      end;
    end;

   Data := DataPointer;
   for X := 0 to Width - 1 do
    begin
     FDelayLine.ClearBuffer;
     Sum := 0;
     for Y := 0 to Height - 1 do
      begin
       Current := Data^[Y * Width + X] * COne255th;
       Sum := Sum + Current;
       Current := FDelayLine.ProcessSample32(Current);
       Sum := Sum - Current;
       if Sum > 0
        then Data^[Y * Width + X] := Round($FF * Sum * FRadiusReciprocal)
        else Data^[Y * Width + X] := 0;
      end;
    end;
  end;
end;

procedure TGuiBlurFIRFilter.Filter(PixelMap: TGuiCustomPixelMap);
begin
 raise Exception.Create('not yet implemented');
end;

procedure TGuiBlurFIRFilter.RadiusChanged;
begin
 inherited;
 FDelayLine.FractionalBufferSize := FRadius;
end;

end.
