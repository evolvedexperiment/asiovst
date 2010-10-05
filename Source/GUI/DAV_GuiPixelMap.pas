unit DAV_GuiPixelMap;

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
  {$IFDEF FPC} LCLIntf, LCLType, LResources, LMessages,
  {$IFDEF Windows} Windows, {$ENDIF}
  {$ELSE} Windows, Messages, {$ENDIF}
  Graphics, Classes, SysUtils, DAV_Common, DAV_MemoryUtils, DAV_GuiCommon,
  DAV_GuiBlend;

type
  TGuiCustomPixelMap = class(TInterfacedPersistent, IStreamPersist)
  private
    function GetDataPointer: PPixel32Array;
    function GetPixel(X, Y: Integer): TPixel32;
    function GetScanLine(Y: Integer): PPixel32Array;
    procedure SetHeight(const Value: Integer);
    procedure SetPixel(X, Y: Integer; const Value: TPixel32);
    procedure SetWidth(const Value: Integer);
    function GetPixelPointer(X, Y: Integer): PPixel32;
  protected
    FDataPointer : PPixel32Array;
    FDataSize    : Integer;
    FBitmapInfo  : TBitmapInfo;
    FWidth       : Integer;
    FHeight      : Integer;
    FOnChange    : TNotifyEvent;
    FOnResize    : TNotifyEvent;
    procedure HeightChanged(UpdateBitmap: Boolean = True); virtual;
    procedure WidthChanged(UpdateBitmap: Boolean = True); virtual;
    procedure SizeChangedAtOnce; virtual;
    procedure Changed; virtual;
    procedure Resized; virtual;

    procedure AssignTo(Dest: TPersistent); override;
    function Equal(PixelMap: TGuiCustomPixelMap): Boolean;
    function Empty: Boolean;

    procedure ReadData(Stream: TStream); virtual;
    procedure WriteData(Stream: TStream); virtual;
    procedure DefineProperties(Filer: TFiler); override;
  public
    constructor Create; virtual;

    procedure Clear; overload; virtual;
    procedure Clear(Color: TColor); overload; virtual;
    procedure Clear(Color: TPixel32); overload; virtual;

    procedure Draw(Bitmap: TBitmap); overload; virtual;
    procedure Draw(Bitmap: TBitmap; X, Y: Integer); overload; virtual; abstract;
    procedure Draw(PixelMap: TGuiCustomPixelMap; Alpha: Byte = $FF); overload; virtual;
    procedure Draw(PixelMap: TGuiCustomPixelMap; X, Y: Integer; Alpha: Byte = $FF); overload; virtual;
    procedure DrawTransparent(PixelMap: TGuiCustomPixelMap); overload; virtual;
    procedure DrawTransparent(PixelMap: TGuiCustomPixelMap; X, Y: Integer); overload; virtual;
    procedure PaintTo(Canvas: TCanvas); overload; virtual;
    procedure PaintTo(Canvas: TCanvas; X, Y: Integer); overload; virtual; abstract;
    procedure PaintTo(Canvas: TCanvas; Rect: TRect; X: Integer = 0; Y: Integer = 0); overload; virtual; abstract;

    procedure LoadFromFile(const Filename: TFileName); virtual;
    procedure SaveToFile(const Filename: TFileName); virtual;
    procedure LoadFromStream(Stream: TStream); virtual;
    procedure SaveToStream(Stream: TStream); virtual;

    procedure SetSize(Width, Height: Integer); virtual;
    procedure ResetAlpha; virtual;

    // simple Painting functions
    procedure FillRect(Rect: TRect; Color: TPixel32);
    procedure FrameRect(Rect: TRect; Color: TPixel32);
    procedure Line(FromX, FromY, ToX, ToY: Integer; Color: TPixel32);
    procedure HorizontalLine(FromX, ToX, Y: Integer; Color: TPixel32);
    procedure VerticalLine(X, FromY, ToY: Integer; Color: TPixel32);
    procedure Assign(Source: TPersistent); override;

    property Width: Integer read FWidth write SetWidth;
    property Height: Integer read FHeight write SetHeight;
    property DataPointer: PPixel32Array read GetDataPointer;
    property Pixel[X, Y: Integer]: TPixel32 read GetPixel write SetPixel;
    property PixelPointer[X, Y: Integer]: PPixel32 read GetPixelPointer;
    property ScanLine[Y: Integer]: PPixel32Array read GetScanLine;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnResize: TNotifyEvent read FOnResize write FOnResize;
  end;

  TGuiPixelMapMemory = class(TGuiCustomPixelMap)
  protected
    procedure AllocateDataPointer; virtual;
    procedure HeightChanged(UpdateBitmap: Boolean = True); override;
    procedure WidthChanged(UpdateBitmap: Boolean = True); override;
    procedure SizeChangedAtOnce; override;
  public
    destructor Destroy; override;

    procedure PaintTo(Canvas: TCanvas; X, Y: Integer); override;
    procedure PaintTo(Canvas: TCanvas; Rect: TRect; X: Integer = 0; Y: Integer = 0); override;
    procedure Draw(Bitmap: TBitmap; X, Y: Integer); override;
  published
    property Width;
    property Height;
    property OnChange;
    property OnResize;
  end;

  TGuiPixelMapDIB = class(TGuiCustomPixelMap)
  protected
    FDC            : HDC;
    FBitmapHandle  : HBITMAP;
    FDeviceContext : HDC;
    procedure AllocateDeviceIndependentBitmap;
    procedure DisposeDeviceIndependentBitmap;
    procedure HeightChanged(UpdateBitmap: Boolean = True); override;
    procedure WidthChanged(UpdateBitmap: Boolean = True); override;
    procedure SizeChangedAtOnce; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure PaintTo(Canvas: TCanvas; X, Y: Integer); override;
    procedure PaintTo(Canvas: TCanvas; Rect: TRect; X: Integer = 0; Y: Integer = 0); override;
    procedure Draw(Bitmap: TBitmap; X, Y: Integer); override;
  published
    property Width;
    property Height;
    property OnChange;
    property OnResize;
  end;

implementation

uses
  Math, DAV_GuiFileFormats;

{ TGuiCustomPixelMap }

constructor TGuiCustomPixelMap.Create;
begin
 inherited;
 FDataPointer := nil;

 // initialize header
 FillChar(FBitmapInfo.bmiHeader, SizeOf(TBitmapInfoHeader), 0);
 with FBitmapInfo.bmiHeader do
  begin
   biSize := SizeOf(TBitmapInfoHeader);
   biBitCount := 32;
   biPlanes := 1;
   biCompression := BI_RGB;
  end;
end;

procedure TGuiCustomPixelMap.Draw(PixelMap: TGuiCustomPixelMap; X, Y: Integer;
  Alpha: Byte = $FF);
var
  ClipRect : TRect;
  Index    : Integer;
begin
 with ClipRect do
  begin
   Left := X;
   if Left < 0 then Left := 0;
   Top := Y;
   if Top < 0 then Top := 0;
   Right := X + PixelMap.Width;
   if Right > Self.Width then Right := Self.Width;
   Bottom := Y + PixelMap.Height;
   if Bottom > Self.Height then Bottom := Self.Height;

   // blend scanlines
   for Index := Top to Bottom - 1
     do CombineLine(PixelMap.PixelPointer[Left - X, Top - Y + Index],
       PixelPointer[Left, Top + Index], Right - Left, Alpha);
  end;
end;

procedure TGuiCustomPixelMap.DrawTransparent(PixelMap: TGuiCustomPixelMap);
begin
 DrawTransparent(PixelMap, 0, 0);
end;

procedure TGuiCustomPixelMap.DrawTransparent(PixelMap: TGuiCustomPixelMap; X,
  Y: Integer);
var
  ClipRect : TRect;
  Index    : Integer;
begin
 with ClipRect do
  begin
   Left := X;
   if Left < 0 then Left := 0;
   Top := Y;
   if Top < 0 then Top := 0;
   Right := X + PixelMap.Width;
   if Right > Self.Width then Right := Self.Width;
   Bottom := Y + PixelMap.Height;
   if Bottom > Self.Height then Bottom := Self.Height;

   // blend scanlines
   for Index := Top to Bottom - 1
     do BlendLine(PixelMap.PixelPointer[Left - X, Top - Y + Index],
       PixelPointer[Left, Top + Index], Right - Left);
  end;
end;

procedure TGuiCustomPixelMap.Draw(PixelMap: TGuiCustomPixelMap;
  Alpha: Byte = $FF);
begin
 Draw(PixelMap, 0, 0, Alpha);
end;

procedure TGuiCustomPixelMap.Assign(Source: TPersistent);
var
  TempBitmap : TBitmap;
begin
 if Source is TGuiCustomPixelMap then
  with TGuiCustomPixelMap(Source) do
   begin
    Self.SetSize(Width, Height);
    Self.FBitmapInfo := FBitmapInfo;

    Assert(Self.FDataSize = FDataSize);
    Move(FDataPointer^, Self.FDataPointer^, FDataSize);

    Self.FOnChange := FOnChange;
    Self.FOnResize := FOnResize;
   end else
 if Source is TBitmap then
  with TBitmap(Source) do
   begin
    Self.SetSize(Width, Height);
    Draw(TBitmap(Source));
    if Assigned(FOnChange) then FOnChange(Self);
   end else
 if Source is TGraphic then
  with TGraphic(Source) do
   begin
    Self.SetSize(Width, Height);
    TempBitmap := TBitmap.Create;
    try
     TempBitmap.Assign(Source);
     Draw(TempBitmap);
     if Assigned(FOnChange) then FOnChange(Self);
    finally
     if Assigned(TempBitmap)
      then FreeAndNil(TempBitmap);
    end;
   end
 else inherited;
end;

procedure TGuiCustomPixelMap.AssignTo(Dest: TPersistent);
begin
 if Dest is TGuiCustomPixelMap then
  with TGuiCustomPixelMap(Dest) do
   begin
    SetSize(Self.Width, Self.Height);
    FBitmapInfo := Self.FBitmapInfo;

    Assert(FDataSize = Self.FDataSize);
    Move(Self.FDataPointer^, FDataPointer^, FDataSize);

    FOnChange := Self.FOnChange;
    FOnResize := Self.FOnResize;
   end
 else inherited;
end;

procedure TGuiCustomPixelMap.Draw(Bitmap: TBitmap);
begin
 Draw(Bitmap, 0, 0);
end;

procedure TGuiCustomPixelMap.PaintTo(Canvas: TCanvas);
begin
 PaintTo(Canvas, 0, 0);
end;

procedure TGuiCustomPixelMap.Clear;
begin
 FillChar(FDataPointer^, FDataSize, 0);
end;

procedure TGuiCustomPixelMap.Changed;
begin
 if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TGuiCustomPixelMap.Clear(Color: TPixel32);
var
  Index : Integer;
begin
 for Index := 0 to FWidth * FHeight - 1
  do FDataPointer^[Index] := Color;
end;

procedure TGuiCustomPixelMap.Clear(Color: TColor);
begin
 Clear(ConvertColor(Color));
end;

function TGuiCustomPixelMap.GetDataPointer: PPixel32Array;
begin
 Result := FDataPointer;
end;

function TGuiCustomPixelMap.GetPixel(X, Y: Integer): TPixel32;
begin
 Result := FDataPointer^[Y * Width + X];
end;

function TGuiCustomPixelMap.GetPixelPointer(X, Y: Integer): PPixel32;
begin
 Result := @FDataPointer^[Y * Width + X];
end;

function TGuiCustomPixelMap.GetScanLine(Y: Integer): PPixel32Array;
begin
 Result := @FDataPointer^[Y * Width];
end;

procedure TGuiCustomPixelMap.SetPixel(X, Y: Integer; const Value: TPixel32);
begin
 BlendPixelInplace(Value, FDataPointer[Y * Width + X]);
end;

procedure TGuiCustomPixelMap.SetSize(Width, Height: Integer);
begin
 if Width < 0
  then raise Exception.Create('Width may not be negative!');
 if Height < 0
  then raise Exception.Create('Height may not be negative!');

 if (FWidth <> Width) or (FHeight <> Height) then
  begin
   FWidth := Width;
   FHeight := Height;
   SizeChangedAtOnce;
  end;
end;

procedure TGuiCustomPixelMap.SizeChangedAtOnce;
begin
 HeightChanged(False);
 WidthChanged(False);
 Resized;
end;

procedure TGuiCustomPixelMap.SetHeight(const Value: Integer);
begin
 if Value < 0
  then raise Exception.Create('Height may not be negative!');

 if FHeight <> Value then
  begin
   FHeight := Value;
   HeightChanged;
  end;
end;

procedure TGuiCustomPixelMap.SetWidth(const Value: Integer);
begin
 if Value < 0
  then raise Exception.Create('Width may not be negative!');

 if FWidth <> Value then
  begin
   FWidth := Value;
   WidthChanged;
  end;
end;

procedure TGuiCustomPixelMap.HeightChanged(UpdateBitmap: Boolean = True);
begin
 FBitmapInfo.bmiHeader.biHeight := -FHeight;
 if UpdateBitmap then Resized;
end;

procedure TGuiCustomPixelMap.WidthChanged(UpdateBitmap: Boolean = True);
begin
 FBitmapInfo.bmiHeader.biWidth := FWidth;
 if UpdateBitmap then Resized;
end;

procedure TGuiCustomPixelMap.LoadFromFile(const Filename: TFileName);
var
  Stream: TStream;
begin
 Stream := TFileStream.Create(Filename, fmOpenRead or fmShareDenyWrite);
 try
  LoadFromStream(Stream);
 finally
  FreeAndNil(Stream);
 end;
end;

procedure TGuiCustomPixelMap.SaveToFile(const Filename: TFileName);
var
  Stream          : TStream;
  FileFormatClass : TGuiCustomFileFormatClass;
begin
 FileFormatClass := FindGraphicFileFormatByExtension(ExtractFileExt(Filename));
 if Assigned(FileFormatClass) then
  begin
   with FileFormatClass.Create do
    try
     Assign(Self);
     SaveToFile(Filename);
     Exit;
    finally
     Free;
    end;
  end;

 // if no file format was found use the default method
 Stream := TFileStream.Create(Filename, fmCreate);
 try
  SaveToStream(Stream);
 finally
  FreeAndNil(Stream);
 end;
end;

procedure TGuiCustomPixelMap.LoadFromStream(Stream: TStream);
var
  BitmapFileHeader : TBitmapFileHeader;
  FileFormatClass  : TGuiCustomFileFormatClass;
  BitmapInfo       : TBitmapInfo;
  Bitmap           : TBitmap;
begin
 FileFormatClass := FindGraphicFileFormatByStream(Stream);
 if Assigned(FileFormatClass) then
  begin
   with FileFormatClass.Create do
    try
     LoadFromStream(Stream);
     AssignTo(Self);
     Exit;
    finally
     Free;
    end;
  end;

 // if no file format was found use the default method
 with Stream do
  begin
   if Size < SizeOf(TBitmapFileHeader)
    then raise Exception.Create('Invalid bitmap header found!');

   Read(BitmapFileHeader, SizeOf(TBitmapFileHeader));

   if BitmapFileHeader.bfType <> $4D42
    then raise Exception.Create('Invalid bitmap header found!');

   Read(BitmapInfo, SizeOf(TBitmapInfo));

   if BitmapInfo.bmiHeader.biBitCount = 32 then
    begin

    end
   else
    begin
     Stream.Seek(-(SizeOf(TBitmapFileHeader) + SizeOf(TBitmapInfo)), soFromCurrent);
     Bitmap := TBitmap.Create;
     try
      Bitmap.LoadFromStream(Stream);
      Self.Assign(Bitmap);
     finally
      FreeAndNil(Bitmap);
     end;
    end;
  end;
end;

procedure TGuiCustomPixelMap.SaveToStream(Stream: TStream);
var
  BitmapFileHeader : TBitmapFileHeader;
begin
 with Stream do
  begin
   // initialize bitmap file header
   FillChar(BitmapFileHeader, SizeOf(BitmapFileHeader), 0);
   with BitmapFileHeader do
    begin
     bfType := $4D42;
     bfSize := SizeOf(TBitmapInfo) + Width * Height * SizeOf(Cardinal);
     bfOffBits := SizeOf(BitmapFileHeader) + SizeOf(TBitmapInfo);
    end;

   // write bitmap file header to stream
   Write(BitmapFileHeader, SizeOf(TBitmapFileHeader));

   // write bitmap file header to stream
   Write(FBitmapInfo, SizeOf(TBitmapInfo));

   Write(FDataPointer^, Width * Height * SizeOf(Cardinal));
  end;
end;

function TGuiCustomPixelMap.Empty: Boolean;
begin
 Result := FDataSize = 0;
end;

function TGuiCustomPixelMap.Equal(PixelMap: TGuiCustomPixelMap): Boolean;
begin
 Result := (PixelMap.Width = FWidth) and (PixelMap.Height = FHeight);

 if Result
  then Result := CompareMem(FDataPointer, PixelMap.FDataPointer, FDataSize);
end;

procedure TGuiCustomPixelMap.ReadData(Stream: TStream);
var
  TempWidth, TempHeight: Integer;
begin
 with Stream do
  try
   ReadBuffer(TempWidth, 4);
   ReadBuffer(TempHeight, 4);
   SetSize(TempWidth, TempHeight);
   Assert(FDataSize = FWidth * FHeight * SizeOf(TPixel32));
   ReadBuffer(FDataPointer^, FDataSize);
  finally
   Changed;
  end;
end;

procedure TGuiCustomPixelMap.WriteData(Stream: TStream);
begin
 with Stream do
  begin
   WriteBuffer(FWidth, 4);
   WriteBuffer(FHeight, 4);
   Assert(FDataSize = FWidth * FHeight * SizeOf(TPixel32));
   WriteBuffer(FDataPointer^, FDataSize);
  end;
end;

procedure TGuiCustomPixelMap.DefineProperties(Filer: TFiler);
var
  HasData : Boolean;
begin
 HasData := (FDataSize > 0);
 if HasData and (Filer.Ancestor <> nil)
  then HasData := not ((Filer.Ancestor is TGuiCustomPixelMap) and
    Equal(TGuiCustomPixelMap(Filer.Ancestor)));

 Filer.DefineBinaryProperty('Data', ReadData, WriteData, HasData);
end;

procedure TGuiCustomPixelMap.FillRect(Rect: TRect; Color: TPixel32);
var
  X, Y : Integer;
begin
 if Color.A = $FF then
  for Y := Rect.Top to Rect.Bottom - 1 do
   for X := Rect.Left to Rect.Right - 1
    do FDataPointer[Y * Width + X] := Color
 else
  try
   for Y := Rect.Top to Rect.Bottom - 1 do
    for X := Rect.Left to Rect.Right - 1
     do BlendPixelInplace(Color, FDataPointer[Y * Width + X]);
  finally
   EMMS;
  end;
end;

procedure TGuiCustomPixelMap.FrameRect(Rect: TRect; Color: TPixel32);
begin
 // top & bottom
 HorizontalLine(Rect.Left, Rect.Right, Rect.Top, Color);
 HorizontalLine(Rect.Left, Rect.Right, Rect.Bottom - 1, Color);

 // left & right
 VerticalLine(Rect.Left, Rect.Top + 1, Rect.Bottom - 1, Color);
 VerticalLine(Rect.Right - 1, Rect.Top + 1, Rect.Bottom - 1, Color);
end;

procedure TGuiCustomPixelMap.VerticalLine(X, FromY, ToY: Integer; Color: TPixel32);
var
  Y : Integer;
begin
 try
  if ToY < FromY  then
   for Y := ToY to FromY - 1
    do BlendPixelInplace(Color, FDataPointer[Y * Width + X])
  else
   for Y := FromY to ToY - 1
    do BlendPixelInplace(Color, FDataPointer[Y * Width + X]);
 finally
  EMMS;
 end;
end;

procedure TGuiCustomPixelMap.HorizontalLine(FromX, ToX, Y: Integer;
  Color: TPixel32);
var
  X : Integer;
begin
 try
  if ToX < FromX  then
   for X := ToX to FromX - 1
    do BlendPixelInplace(Color, FDataPointer[Y * Width + X])
  else
   for X := FromX to ToX - 1
    do BlendPixelInplace(Color, FDataPointer[Y * Width + X])
 finally
  EMMS;
 end;
end;

procedure TGuiCustomPixelMap.Line(FromX, FromY, ToX, ToY: Integer; Color: TPixel32);
var
  x, y, t     : Integer;
  dx, dy      : Integer;
  incx, incy  : Integer;
  pdx, pdy    : Integer;
  ddx, ddy    : Integer;
  es, el, err : Integer;
begin
 if FromY = ToY then HorizontalLine(FromX, ToX, FromY, Color) else
 if FromX = ToX then VerticalLine(FromX, FromY, ToY, Color) else
  try
   dx := ToX - FromX;
   dy := ToY - FromY;

   incx := Sign(dx);
   incy := Sign(dy);
   if (dx < 0) then dx := -dx;
   if (dy < 0) then dy := -dy;

   if (dx > dy) then
    begin
     pdx := incx;
     pdy := 0;
     ddx := incx;
     ddy := incy;
     es  := dy;
     el  := dx;
    end
   else
    begin
     pdx := 0;
     pdy := incy;
     ddx := incx;
     ddy := incy;
     es  := dx;
     el  := dy;
    end;

   x := FromX;
   y := FromY;
   err := el shr 1;
   BlendPixelInplace(Color, FDataPointer[Y * Width + X]);

   for t := 1 to el - 1 do
    begin
     err := err - es;
     if (err < 0) then
      begin
       err := err + el;
       x := x + ddx;
       y := y + ddy;
      end
     else
      begin
       x := x + pdx;
       y := y + pdy;
      end;
     BlendPixelInplace(Color, FDataPointer[Y * Width + X]);
    end;
  finally
   EMMS;
  end;
end;

procedure TGuiCustomPixelMap.ResetAlpha;
var
  Index : Integer;
begin
 for Index := 0 to FWidth * FHeight - 1
  do FDataPointer^[Index].A := $FF;
end;

procedure TGuiCustomPixelMap.Resized;
begin
 if Assigned(FOnResize) then FOnResize(Self);
 Changed;
end;


{ TGuiPixelMapMemory }

destructor TGuiPixelMapMemory.Destroy;
begin
 Dispose(FDataPointer);
 inherited;
end;

procedure TGuiPixelMapMemory.Draw(Bitmap: TBitmap; X, Y: Integer);
begin
 if (Bitmap.Height <> 0) and (FDataPointer <> nil) then
  begin
   if GetDIBits(Bitmap.Canvas.Handle, Bitmap.Handle, 0, Bitmap.Height,
     FDataPointer, FBitmapInfo, DIB_RGB_COLORS) = 0
    then raise Exception.Create('Error');
  end;

(*
var
  CompDC     : HDC;
  CompBitmap : HBITMAP;

 CompDC := CreateCompatibleDC(Canvas.Handle);
 try
  CompBitmap := CreateCompatibleBitmap(CompDC, Width, Height);
  SelectObject(CompDC, CompBitmap);
  if CompBitmap <> 0 then
   try
    BitBlt(CompDC, 0, 0, Width, Height, Canvas.Handle, 0, 0, SRCCOPY);

    if GetDIBits(Canvas.Handle, CompBitmap, 0, Height, FDataPointer, FBitmapInfo,
      DIB_RGB_COLORS) = 0
      then raise Exception.Create('Error');
   finally
    DeleteObject(CompBitmap);
   end;
 finally
  DeleteDC(CompDC);
 end;
*)
end;

procedure TGuiPixelMapMemory.PaintTo(Canvas: TCanvas; X, Y: Integer);
var
  Bitmap        : HBITMAP;
  DeviceContext : HDC;
  Buffer        : Pointer;
  OldObject     : HGDIOBJ;
begin
 if SetDIBitsToDevice(Canvas.Handle, X, Y, Width, Height, 0, 0, 0, Height,
   FDataPointer, FBitmapInfo, DIB_RGB_COLORS) = 0 then
  begin
   // create compatible device context
   DeviceContext := CreateCompatibleDC(Canvas.Handle);
   if DeviceContext <> 0 then
    try
     Bitmap := CreateDIBSection(DeviceContext, FBitmapInfo,
       DIB_RGB_COLORS, Buffer, 0, 0);

     if Bitmap <> 0 then
      begin
       OldObject := SelectObject(DeviceContext, Bitmap);
       try
        Move(FDataPointer^, Buffer^, Width * Height * SizeOf(Cardinal));
        BitBlt(Canvas.Handle, X, Y, Width, Height, DeviceContext,
          0, 0, SRCCOPY);
       finally
        if OldObject <> 0
         then SelectObject(DeviceContext, OldObject);
        DeleteObject(Bitmap);
       end;
      end;
    finally
     DeleteDC(DeviceContext);
    end;
  end;
end;

procedure TGuiPixelMapMemory.PaintTo(Canvas: TCanvas; Rect: TRect;
  X: Integer = 0; Y: Integer = 0);
var
  Bitmap        : HBITMAP;
  DeviceContext : HDC;
  Buffer        : Pointer;
  OldObject     : HGDIOBJ;
  H, W          : Integer;
begin
 W := Min(Width, Rect.Right - Rect.Left);
 H := Min(Height, Rect.Bottom - Rect.Top);
 if SetDIBitsToDevice(Canvas.Handle, X, Y, W, H, Rect.Left, Rect.Top, 0, Height,
   FDataPointer, FBitmapInfo, DIB_RGB_COLORS) = 0 then
  begin
   // create compatible device context
   DeviceContext := CreateCompatibleDC(Canvas.Handle);
   if DeviceContext <> 0 then
    try
     Bitmap := CreateDIBSection(DeviceContext, FBitmapInfo,
       DIB_RGB_COLORS, Buffer, 0, 0);

     if Bitmap <> 0 then
      begin
       OldObject := SelectObject(DeviceContext, Bitmap);
       try
        Move(FDataPointer^, Buffer^, Width * Height * SizeOf(Cardinal));
        BitBlt(Canvas.Handle, X, Y, W, H, DeviceContext, Rect.Left, Rect.Top,
          SRCCOPY);
       finally
        if OldObject <> 0
         then SelectObject(DeviceContext, OldObject);
        DeleteObject(Bitmap);
       end;
      end;
    finally
     DeleteDC(DeviceContext);
    end;
  end;
end;

procedure TGuiPixelMapMemory.HeightChanged(UpdateBitmap: Boolean);
begin
 inherited;
 if UpdateBitmap then AllocateDataPointer;
end;

procedure TGuiPixelMapMemory.WidthChanged(UpdateBitmap: Boolean);
begin
 inherited;
 if UpdateBitmap then AllocateDataPointer;
end;

procedure TGuiPixelMapMemory.SizeChangedAtOnce;
begin
 inherited;
 AllocateDataPointer;
end;

procedure TGuiPixelMapMemory.AllocateDataPointer;
begin
 FDataSize := FWidth * FHeight * SizeOf(Cardinal);
 ReallocateAlignedMemory(Pointer(FDataPointer), FDataSize);
 Clear;
end;


{ TGuiPixelMapDIB }

constructor TGuiPixelMapDIB.Create;
begin
 inherited;
end;

destructor TGuiPixelMapDIB.Destroy;
begin
 DisposeDeviceIndependentBitmap;
 inherited;
end;

procedure TGuiPixelMapDIB.AllocateDeviceIndependentBitmap;
begin
 FBitmapHandle := CreateDIBSection(0, FBitmapInfo, DIB_RGB_COLORS,
   Pointer(FDataPointer), 0, 0);

 if FDataPointer = nil
  then raise Exception.Create('Can''t allocate the DIB handle');

 FDC := CreateCompatibleDC(0);
 if FDC = 0 then
  begin
   DeleteObject(FBitmapHandle);
   FBitmapHandle := 0;
   FDataPointer := nil;
   raise Exception.Create('Can''t create compatible DC');
  end;

 if SelectObject(FDC, FBitmapHandle) = 0 then
  begin
   DeleteDC(FDC);
   DeleteObject(FBitmapHandle);
   FDC := 0;
   FBitmapHandle := 0;
   FDataPointer := nil;
   raise Exception.Create('Can''t select an object into DC');
  end;
end;

procedure TGuiPixelMapDIB.DisposeDeviceIndependentBitmap;
begin
 if FDC <> 0 then DeleteDC(FDC);
 FDC := 0;
 if FBitmapHandle <> 0
  then DeleteObject(FBitmapHandle);
 FBitmapHandle := 0;

 FDataPointer := nil;
end;

procedure TGuiPixelMapDIB.Draw(Bitmap: TBitmap; X, Y: Integer);
(*
var
  CompBitmap : HBITMAP;
*)
begin
(*
 CompBitmap := CreateCompatibleBitmap(Canvas.Handle, Width, Height);
 if CompBitmap <> 0 then
  try
   GetDIBits(Canvas.Handle, CompBitmap, 0, Height, FDataPointer, FBitmapInfo,
     DIB_RGB_COLORS)
  finally
   DeleteObject(CompBitmap);
  end;
*)
end;

procedure TGuiPixelMapDIB.PaintTo(Canvas: TCanvas; X, Y: Integer);
begin
 BitBlt(Canvas.Handle, X, Y, Width, Height, FDC, X, Y, SRCCOPY);
end;

procedure TGuiPixelMapDIB.PaintTo(Canvas: TCanvas; Rect: TRect; X: Integer = 0;
  Y: Integer = 0);
begin
 BitBlt(Canvas.Handle, X, Y, Min(Width, Rect.Right - Rect.Left),
   Min(Height, Rect.Bottom - Rect.Top), FDC, Rect.Left, Rect.Top, SRCCOPY);
end;

procedure TGuiPixelMapDIB.SizeChangedAtOnce;
begin
 inherited;
 DisposeDeviceIndependentBitmap;
 if Width * Height <> 0
  then AllocateDeviceIndependentBitmap;
end;

procedure TGuiPixelMapDIB.HeightChanged(UpdateBitmap: Boolean);
begin
 inherited;
 if UpdateBitmap then
  begin
   DisposeDeviceIndependentBitmap;
   if Width * Height <> 0
    then AllocateDeviceIndependentBitmap;
  end;
end;

procedure TGuiPixelMapDIB.WidthChanged(UpdateBitmap: Boolean);
begin
 inherited;
 if UpdateBitmap then
  begin
   DisposeDeviceIndependentBitmap;
   if Width * Height <> 0
    then AllocateDeviceIndependentBitmap;
  end;
end;

end.
