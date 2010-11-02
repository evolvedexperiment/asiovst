unit DAV_GuiFont;

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
  {$IFDEF Windows} Windows, {$ENDIF} {$ELSE} Windows, Messages, {$ENDIF}
  Graphics, Classes, SysUtils, DAV_Common, DAV_Classes, DAV_GuiCommon,
  DAV_GuiBlend, DAV_GuiPixelMap, DAV_GuiByteMap, DAV_GuiFilters, DAV_GuiShadow;

{$DEFINE UseShadowBuffer}

type
  TGuiCustomFont = class(TPersistent)
  private
    FAntiAliasing : Boolean;
    FShadow       : TGuiShadow;
    FShadowColor  : TPixel32;
    FSaturation   : TGuiSaturationFilter;
    FBlurFilter   : TGuiStackBlurFilter;
    FOnChange     : TNotifyEvent;
    procedure SetAntialiasing(const Value: Boolean);
    procedure SetShadow(const Value: TGuiShadow);
  protected
    procedure AntiAliasingChanged; virtual;
    procedure ShadowChangedHandler(Sender: TObject); virtual;
    procedure Changed; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function TextExtend(Text: string): TSize; virtual; abstract;
    procedure TextOut(Text: string; PixelMap: TGuiCustomPixelMap;
      X: Integer = 0; Y: Integer = 0); virtual; abstract;

    property Shadow: TGuiShadow read FShadow write SetShadow;
    property Antialiasing: Boolean read FAntiAliasing write SetAntialiasing;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;
  TGuiCustomFontClass = class of TGuiCustomFont;

  TGuiCustomGDIFont = class(TGuiCustomFont)
  private
    FFont         : TFont;
    FBuffer       : TGuiByteMapDIB;
    {$IFDEF UseShadowBuffer}
    FShadowBuffer : TGuiCustomByteMap;
    {$ENDIF}
    FOldHandle    : HDC;
    FFontHandle   : HFont;
    procedure SetFont(const Value: TFont);
  protected
    procedure FontChanged(Sender: TObject); virtual; abstract;
    procedure AntiAliasingChanged; override;
    procedure AssignByteMapFont; virtual; abstract;
  public
    constructor Create; override;
    destructor Destroy; override;

    property Font: TFont read FFont write SetFont;
  end;

  TGuiCustomSimpleGDIFont = class(TGuiCustomGDIFont)
  protected
    procedure FontChanged(Sender: TObject); override;
    procedure AssignByteMapFont; override;
  public
    function TextExtend(Text: string): TSize; override;
    procedure TextOut(Text: string; PixelMap: TGuiCustomPixelMap;
      X: Integer = 0; Y: Integer = 0); override;
  end;

  TFontOversampling = (foNone, fo2x, fo3x, fo4x, fo6x, fo8x, fo12x, fo16x);

  TGuiCustomOversampledGDIFont = class(TGuiCustomGDIFont)
  private
    FFontOversampling : TFontOversampling;
    FOSFactor         : Integer;
    FScaledFont       : TFont;
    procedure SetFontOversampling(const Value: TFontOversampling);
  protected
    procedure AssignByteMapFont; override;
    procedure DownsampleByteMap(Buffer: TGuiCustomByteMap); virtual;
    procedure FontChanged(Sender: TObject); override;
    procedure FontOversamplingChanged; virtual;
    procedure UpdateScaledFont; virtual;
  public
    constructor Create; override;
    destructor Destroy; override;

    function TextExtend(Text: string): TSize; override;
    procedure TextOut(Text: string; PixelMap: TGuiCustomPixelMap;
      X: Integer = 0; Y: Integer = 0); override;

    property FontOversampling: TFontOversampling read FFontOversampling write SetFontOversampling;
  end;

  TGuiSimpleGDIFont = class(TGuiCustomSimpleGDIFont)
  published
    property Font;
    property Shadow;
  end;

  TGuiOversampledGDIFont = class(TGuiCustomOversampledGDIFont)
  published
    property Font;
    property FontOversampling;
    property Shadow;
  end;

var
  FontClassList: TClassList;

implementation

uses
  DAV_Approximations;

procedure SetFontAntialiasing(const Font: TFont; Quality: Cardinal);
var
  LogFont: TLogFont;
begin
 with LogFont do
  begin
   lfHeight := Font.Height;
   lfWidth := 0;

   {$IFDEF DELPHI9_UP}
   lfEscapement := Font.Orientation;
   lfOrientation := Font.Orientation;
   {$ELSE}
   lfEscapement := 0;
   lfOrientation := 0;
   {$ENDIF}

   if fsBold in Font.Style
    then lfWeight := FW_BOLD
    else lfWeight := FW_NORMAL;

   lfItalic    := Byte(fsItalic in Font.Style);
   lfUnderline := Byte(fsUnderline in Font.Style);
   lfStrikeOut := Byte(fsStrikeOut in Font.Style);
   lfCharSet   := Byte(Font.Charset);

   if AnsiCompareText(Font.Name, 'Default') = 0
    then StrPCopy(lfFaceName, string(DefFontData.Name))
    else StrPCopy(lfFaceName, Font.Name);

   lfQuality := Quality;

   if lfOrientation <> 0
    then lfOutPrecision := OUT_TT_ONLY_PRECIS
    else lfOutPrecision := OUT_DEFAULT_PRECIS;

   lfClipPrecision := CLIP_DEFAULT_PRECIS;

   case Font.Pitch of
    fpVariable : lfPitchAndFamily := VARIABLE_PITCH;
    fpFixed    : lfPitchAndFamily := FIXED_PITCH;
    else lfPitchAndFamily := DEFAULT_PITCH;
   end;
  end;
 Font.Handle := CreateFontIndirect(LogFont);
end;

procedure DownsampleByteMap2x(var ByteMap: TGuiCustomByteMap);
var
  x, y  : Integer;
  ScnLn : array [0..2] of PByteArray;
begin
 with ByteMap do
  begin
   for y := 0 to (Height div 2) - 1 do
    begin
     ScnLn[0] := ScanLine[y];
     ScnLn[1] := ScanLine[y * 2];
     ScnLn[2] := ScanLine[y * 2 + 1];
     for x := 0 to (Width div 2) - 1 do
      begin
       ScnLn[0, x] := (ScnLn[1, 2 * x] + ScnLn[2, 2 * x] +
         ScnLn[1, 2 * x + 1] + ScnLn[2, 2 * x + 1]) div 4;
      end;
    end;
  end;
end;

procedure DownsampleByteMap3x(var ByteMap: TGuiCustomByteMap);
var
  x, y  : Integer;
  x3    : Integer;
  ScnLn : array [0..3] of PByteArray;
begin
 with ByteMap do
  begin
   for y := 0 to (Height div 3) - 1 do
    begin
     ScnLn[0] := ScanLine[y];
     ScnLn[1] := ScanLine[3 * y];
     ScnLn[2] := ScanLine[3 * y + 1];
     ScnLn[3] := ScanLine[3 * y + 2];
     for x := 0 to (Width  div 3) - 1 do
      begin
       x3 := 3 * x;
       ScnLn[0, x] := (ScnLn[1, x3] + ScnLn[2, x3] + ScnLn[3, x3] +
         ScnLn[1, x3 + 1] + ScnLn[2, x3 + 1] + ScnLn[3, x3 + 1] +
         ScnLn[1, x3 + 2] + ScnLn[2, x3 + 2] + ScnLn[3, x3 + 2]) div 9;
      end;
    end;
  end;
end;

procedure DownsampleByteMap4x(var ByteMap: TGuiCustomByteMap);
var
  x, y  : Integer;
  x4    : Integer;
  ScnLn : array [0..4] of PByteArray;
begin
 with ByteMap do
  begin
   for y := 0 to (Height div 4) - 1 do
    begin
     ScnLn[0] := ScanLine[y];
     ScnLn[1] := ScanLine[y * 4];
     ScnLn[2] := ScanLine[y * 4 + 1];
     ScnLn[3] := ScanLine[y * 4 + 2];
     ScnLn[4] := ScanLine[y * 4 + 3];
     for x := 0 to (Width div 4) - 1 do
      begin
       x4 := 4 * x;
       ScnLn[0, x] := (ScnLn[1, x4] + ScnLn[1, x4 + 1] + ScnLn[1, x4 + 2] + ScnLn[1, x4 + 3] +
         ScnLn[2, x4] + ScnLn[2, x4 + 1] + ScnLn[2, x4 + 2] + ScnLn[2, x4 + 3] +
         ScnLn[3, x4] + ScnLn[3, x4 + 1] + ScnLn[3, x4 + 2] + ScnLn[3, x4 + 3] +
         ScnLn[4, x4] + ScnLn[4, x4 + 1] + ScnLn[4, x4 + 2] + ScnLn[4, x4 + 3]) div 16;
      end;
    end;
  end;
end;


{ TGuiCustomFont }

constructor TGuiCustomFont.Create;
begin
 inherited;
 FShadow          := TGuiShadow.Create;
 FShadow.OnChange := ShadowChangedHandler;
 FBlurFilter      := TGuiStackBlurFilter.Create;
 FSaturation      := TGuiSaturationFilter.Create;
 FShadowColor     := pxBlack32;
end;

destructor TGuiCustomFont.Destroy;
begin
 FreeAndNil(FShadow);
 FreeAndNil(FBlurFilter);
 FreeAndNil(FSaturation);

 inherited;
end;

procedure TGuiCustomFont.Changed;
begin
 if Assigned(FOnChange)
  then FOnChange(Self);
end;

procedure TGuiCustomFont.AntiAliasingChanged;
begin
 Changed;
end;

procedure TGuiCustomFont.SetAntialiasing(const Value: Boolean);
begin
 if FAntiAliasing <> Value then
  begin
   FAntiAliasing := Value;
   AntiAliasingChanged;
  end;
end;

procedure TGuiCustomFont.SetShadow(const Value: TGuiShadow);
begin
 FShadow.Assign(Value);
 Changed;
end;

procedure TGuiCustomFont.ShadowChangedHandler(Sender: TObject);
begin
 FShadowColor := ConvertColor(FShadow.Color);
 FShadowColor.A := FShadow.Opacity;
 FSaturation.Value := FShadow.Saturation;
 Changed;
end;


{ TGuiCustomGDIFont }

constructor TGuiCustomGDIFont.Create;
begin
 inherited;
 FFont          := TFont.Create;
 FFont.OnChange := FontChanged;
 FBuffer        := TGuiByteMapDIB.Create;
 FBuffer.SetSize(8, 8);
 FOldHandle     := FBuffer.Handle;

 AssignByteMapFont;
end;

destructor TGuiCustomGDIFont.Destroy;
begin
 FreeAndNil(FFont);
 FreeAndNil(FBuffer);

 {$IFDEF UseShadowBuffer}
 if Assigned(FShadowBuffer)
  then FreeAndNil(FShadowBuffer);
 {$ENDIF}

 inherited;
end;

procedure TGuiCustomGDIFont.AntiAliasingChanged;
begin
 inherited;

 if FAntiAliasing
  then SetFontAntialiasing(FFont, ANTIALIASED_QUALITY)
  else SetFontAntialiasing(FFont, NONANTIALIASED_QUALITY);
end;

procedure TGuiCustomGDIFont.SetFont(const Value: TFont);
begin
 FFont.Assign(Value);
 FontChanged(nil);
end;


{ TGuiCustomSimpleGDIFont }

procedure TGuiCustomSimpleGDIFont.AssignByteMapFont;
begin
 Assert(FBuffer.Handle <> 0);
 if (FFontHandle = 0) then
  begin
   SelectObject(FBuffer.Handle, Font.Handle);
   SetTextColor(FBuffer.Handle, ColorToRGB(clWhite));
   SetBkMode(FBuffer.Handle, Windows.TRANSPARENT);

   FFontHandle := Font.Handle;
  end
 else
  begin
   SelectObject(FBuffer.Handle, FFontHandle);
   SetTextColor(FBuffer.Handle, ColorToRGB(clWhite));
   SetBkMode(FBuffer.Handle, Windows.TRANSPARENT);
  end;
end;

procedure TGuiCustomSimpleGDIFont.FontChanged(Sender: TObject);
begin
 Assert(FBuffer.Handle <> 0);
 SelectObject(FBuffer.Handle, Font.Handle);
 SetTextColor(FBuffer.Handle, ColorToRGB(clWhite));
 SetBkMode(FBuffer.Handle, Windows.TRANSPARENT);
 FFontHandle := Font.Handle;
 Changed;
end;

function TGuiCustomSimpleGDIFont.TextExtend(Text: string): TSize;
begin
 Result.cx := 0;
 Result.cy := 0;
 if FBuffer.Handle <> 0
  then GetTextExtentPoint32(FBuffer.Handle, PChar(Text), Length(Text), Result);
end;

procedure TGuiCustomSimpleGDIFont.TextOut(Text: string; PixelMap: TGuiCustomPixelMap;
  X, Y: Integer);
var
  TextExtend : TSize;
  BlurOffset : Integer;
begin
 if FBuffer.Handle <> 0 then
  begin
   TextExtend.cx := 0;
   TextExtend.cy := 0;
   GetTextExtentPoint32(FBuffer.Handle, PChar(Text), Length(Text), TextExtend);

   if (TextExtend.cx > 0) and (TextExtend.cy > 0) then
    begin
     if FShadow.Visible then
      begin
       {$IFDEF UseShadowBuffer}
       if not Assigned(FShadowBuffer)
        then FShadowBuffer := TGuiByteMapMemory.Create;
       {$ENDIF}

       BlurOffset := Round(FShadow.Blur + 0.5);
       Inc(TextExtend.cx, 2 * BlurOffset);
       Inc(TextExtend.cy, 2 * BlurOffset);

       TextExtend.cx := (TextExtend.cx and $FFFFFFFC) + $4;
       TextExtend.cy := (TextExtend.cy and $FFFFFFFC) + $4;

       FBuffer.SetSize(TextExtend.cx, TextExtend.cy);
       if FBuffer.Handle <> FOldHandle then
        begin
         AssignByteMapFont;
         FOldHandle := FBuffer.Handle;
        end
       else FBuffer.Clear;

       Windows.TextOut(FBuffer.Handle, BlurOffset, BlurOffset, PChar(Text),
         Length(Text));

       {$IFDEF UseShadowBuffer}
       FShadowBuffer.Assign(FBuffer);

       if FShadow.Blur > 0 then
        begin
         FBlurFilter.Radius := FShadow.Blur;
         FBlurFilter.Filter(FShadowBuffer);
        end;

       if PixelMap <> nil
        then PixelMap.DrawByteMap(FShadowBuffer, FShadowColor,
          X + FShadow.Offset.X - BlurOffset,
          Y + FShadow.Offset.Y - BlurOffset);
       {$ELSE}
       if FShadow.Blur > 0 then
        begin
         FBlurFilter.Radius := FShadow.Blur;
         FBlurFilter.Filter(FBuffer);
        end;

       if PixelMap <> nil
        then PixelMap.DrawByteMap(FBuffer, FShadowColor,
          X + FShadow.Offset.X - BlurOffset,
          Y + FShadow.Offset.Y - BlurOffset);

       FBuffer.Clear;
       Windows.TextOut(FBuffer.Handle, BlurOffset, BlurOffset, PChar(Text),
         Length(Text));
       {$ENDIF}

       if PixelMap <> nil
        then PixelMap.DrawByteMap(FBuffer, ConvertColor(Font.Color),
          X - BlurOffset, Y - BlurOffset);
      end
     else
      begin
       TextExtend.cx := (TextExtend.cx and $FFFFFFFC) + $4;
       TextExtend.cy := (TextExtend.cy and $FFFFFFFC) + $4;

       FBuffer.SetSize(TextExtend.cx, TextExtend.cy);
       if FBuffer.Handle <> FOldHandle then
        begin
         AssignByteMapFont;
         FOldHandle := FBuffer.Handle;
        end
       else FBuffer.Clear;

       Windows.TextOut(FBuffer.Handle, 0, 0, PChar(Text), Length(Text));

       if PixelMap <> nil
        then PixelMap.DrawByteMap(FBuffer, ConvertColor(Font.Color), X, Y);
      end;
    end;
  end;
end;


{ TGuiCustomOversampledGDIFont }

constructor TGuiCustomOversampledGDIFont.Create;
begin
 FScaledFont := TFont.Create;
 FOSFactor := 1;
 inherited;
 FFontOversampling := foNone;
end;

destructor TGuiCustomOversampledGDIFont.Destroy;
begin
 FreeAndNil(FScaledFont);
 inherited;
end;

procedure TGuiCustomOversampledGDIFont.DownsampleByteMap(Buffer: TGuiCustomByteMap);
begin
 case FFontOversampling of
  fo2x : DownsampleByteMap2x(Buffer);
  fo3x : DownsampleByteMap3x(Buffer);
  fo4x : DownsampleByteMap4x(Buffer);
  fo6x : begin
          DownsampleByteMap3x(Buffer);
          DownsampleByteMap2x(Buffer);
         end;
  fo8x : begin
          DownsampleByteMap4x(Buffer);
          DownsampleByteMap2x(Buffer);
         end;
  fo12x : begin
          DownsampleByteMap4x(Buffer);
          DownsampleByteMap3x(Buffer);
         end;
  fo16x : begin
          DownsampleByteMap4x(Buffer);
          DownsampleByteMap4x(Buffer);
         end;
 end;
end;

procedure TGuiCustomOversampledGDIFont.FontChanged(Sender: TObject);
begin
 UpdateScaledFont;

 SelectObject(FBuffer.Handle, FScaledFont.Handle);
 SetTextColor(FBuffer.Handle, ColorToRGB(clWhite));
 SetBkMode(FBuffer.Handle, Windows.TRANSPARENT);
 FFontHandle := FScaledFont.Handle;
 Changed;
end;

procedure TGuiCustomOversampledGDIFont.FontOversamplingChanged;
begin
 case FFontOversampling of
  foNone : FOSFactor := 1;
  fo2x   : FOSFactor := 2;
  fo3x   : FOSFactor := 3;
  fo4x   : FOSFactor := 4;
  fo6x   : FOSFactor := 6;
  fo8x   : FOSFactor := 8;
  fo12x  : FOSFactor := 12;
  fo16x  : FOSFactor := 16;
 end;

 UpdateScaledFont;
 Changed;
end;

procedure TGuiCustomOversampledGDIFont.UpdateScaledFont;
begin
 FScaledFont.Assign(FFont);
 FScaledFont.Size := FOSFactor * FFont.Size;
 FFontHandle := 0;
 AssignByteMapFont;
end;

procedure TGuiCustomOversampledGDIFont.AssignByteMapFont;
begin
 if (FFontHandle = 0) then
  begin
   SelectObject(FBuffer.Handle, FScaledFont.Handle);
   SetTextColor(FBuffer.Handle, ColorToRGB(clWhite));
   SetBkMode(FBuffer.Handle, Windows.TRANSPARENT);

   FFontHandle := FScaledFont.Handle;
  end
 else
  begin
   SelectObject(FBuffer.Handle, FFontHandle);
   SetTextColor(FBuffer.Handle, ColorToRGB(clWhite));
   SetBkMode(FBuffer.Handle, Windows.TRANSPARENT);
  end;
end;

procedure TGuiCustomOversampledGDIFont.SetFontOversampling(
  const Value: TFontOversampling);
begin
 if FFontOversampling <> Value then
  begin
   FFontOversampling := Value;
   FontOversamplingChanged;
  end;
end;

function TGuiCustomOversampledGDIFont.TextExtend(Text: string): TSize;
begin
 Result.cx := 0;
 Result.cy := 0;
 if FBuffer.Handle <> 0
  then GetTextExtentPoint32(FBuffer.Handle, PChar(Text), Length(Text), Result);

 Result.cx := Result.cx div FOSFactor;
 Result.cy := Result.cy div FOSFactor;
end;

procedure TGuiCustomOversampledGDIFont.TextOut(Text: string;
  PixelMap: TGuiCustomPixelMap; X, Y: Integer);
var
  TextExtend : TSize;
  BlurOffset : Integer;
begin
 if FBuffer.Handle <> 0 then
  begin
   TextExtend.cx := 0;
   TextExtend.cy := 0;
   GetTextExtentPoint32(FBuffer.Handle, PChar(Text), Length(Text), TextExtend);

   if (TextExtend.cx > 0) and (TextExtend.cy > 0) then
    begin
     if FShadow.Visible then
      begin
       {$IFDEF UseShadowBuffer}
       if not Assigned(FShadowBuffer)
        then FShadowBuffer := TGuiByteMapMemory.Create;
       {$ENDIF}

       BlurOffset := FOSFactor * Round(0.5 * FShadow.Blur);
       TextExtend.cx := TextExtend.cx + 2 * BlurOffset;
       TextExtend.cy := TextExtend.cy + 2 * BlurOffset;

       // align byte map size
       TextExtend.cx := (TextExtend.cx and $FFFFFFFC) + $4;
       TextExtend.cy := (TextExtend.cy and $FFFFFFFC) + $4;

       FBuffer.SetSize(TextExtend.cx, TextExtend.cy);
       if FBuffer.Handle <> FOldHandle then
        begin
         AssignByteMapFont;
         FOldHandle := FBuffer.Handle;
        end
       else FBuffer.Clear;

       Windows.TextOut(FBuffer.Handle, BlurOffset, BlurOffset, PChar(Text),
         Length(Text));

       {$IFDEF UseShadowBuffer}
       FShadowBuffer.Assign(FBuffer);

       if FShadow.Blur > 0 then
        begin
         FBlurFilter.Radius := FOSFactor * FShadow.Blur;
         FBlurFilter.Filter(FShadowBuffer);

         // eventually apply saturation
         if FShadow.Saturation <> 0
          then FSaturation.Filter(FShadowBuffer);
        end;

       DownsampleByteMap(FShadowBuffer);

       if PixelMap <> nil
        then PixelMap.DrawByteMap(FShadowBuffer, FShadowColor, Rect(
          X + FShadow.Offset.X - BlurOffset div FOSFactor,
          Y + FShadow.Offset.Y - BlurOffset div FOSFactor,
          X + FShadow.Offset.X + (FBuffer.Width - BlurOffset) div FOSFactor,
          Y + FShadow.Offset.Y + (FBuffer.Height - BlurOffset) div FOSFactor));
       {$ELSE}
       if FShadow.Blur > 0 then
        begin
         FBlurFilter.Radius := FOSFactor * FShadow.Blur;
         FBlurFilter.Filter(FBuffer);

         // eventually apply saturation
         if FShadow.Saturation <> 0
          then FSaturation.Filter(FBuffer);
        end;

       DownsampleByteMap(FBuffer);

       if PixelMap <> nil
        then PixelMap.DrawByteMap(FBuffer, FShadowColor, Rect(
          X + FShadow.Offset.X - BlurOffset div FOSFactor,
          Y + FShadow.Offset.Y - BlurOffset div FOSFactor,
          X + FShadow.Offset.X + (FBuffer.Width - BlurOffset) div FOSFactor,
          Y + FShadow.Offset.Y + (FBuffer.Height - BlurOffset) div FOSFactor));

       FBuffer.Clear;
       Windows.TextOut(FBuffer.Handle, BlurOffset, BlurOffset, PChar(Text),
         Length(Text));

       {$ENDIF}

       DownsampleByteMap(FBuffer);

       if PixelMap <> nil
        then PixelMap.DrawByteMap(FBuffer, ConvertColor(Font.Color),
          Rect(X - BlurOffset div FOSFactor, Y - BlurOffset div FOSFactor,
            X + (FBuffer.Width - BlurOffset) div FOSFactor,
            Y + (FBuffer.Height - BlurOffset) div FOSFactor));
      end
     else
      begin
       TextExtend.cx := (TextExtend.cx and $FFFFFFFC) + $4;
       TextExtend.cy := (TextExtend.cy and $FFFFFFFC) + $4;

       FBuffer.SetSize(TextExtend.cx, TextExtend.cy);
       if FBuffer.Handle <> FOldHandle then
        begin
         AssignByteMapFont;
         FOldHandle := FBuffer.Handle;
        end
       else FBuffer.Clear;

       Windows.TextOut(FBuffer.Handle, 0, 0, PChar(Text), Length(Text));

       DownsampleByteMap(FBuffer);

       if PixelMap <> nil
        then PixelMap.DrawByteMap(FBuffer, ConvertColor(Font.Color),
          Rect(X, Y, X + FBuffer.Width div FOSFactor,
            Y + FBuffer.Height div FOSFactor));
      end;
    end;
  end;
end;

procedure RegisterFontClass(FontClass: TGuiCustomFontClass);
begin
 if not Assigned(FontClassList) then FontClassList := TClassList.Create;
 FontClassList.Add(FontClass);
end;

initialization
  // register font classes
  RegisterFontClass(TGuiSimpleGDIFont);
  RegisterFontClass(TGuiOversampledGDIFont);

finalization
  FreeAndNil(FontClassList);

end.
