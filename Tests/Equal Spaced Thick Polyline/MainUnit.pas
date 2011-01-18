unit MainUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, DAV_GuiByteMap, DAV_GuiPixelMap, DAV_GuiGraphicControl,
  DAV_GuiLabel, DAV_GuiSlider, StdCtrls, Menus;

type
  TFmESTP = class(TForm)
    PaintBox: TPaintBox;
    SlLineWidth: TGuiSlider;
    Label1: TLabel;
    Label2: TLabel;
    PuMenu: TPopupMenu;
    MiPositionA: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure PaintBoxPaint(Sender: TObject);
    procedure SlLineWidthChange(Sender: TObject);
    procedure PaintBoxClick(Sender: TObject);
    procedure SlLineWidthDblClick(Sender: TObject);
    procedure MiPositionAClick(Sender: TObject);
  private
    FPixelMap       : TGuiCustomPixelMap;
    FLineWidth      : Single;
    FPaintBoxUpdate : Boolean;
    FPointArray     : array of Double;
    procedure SetLineWidth(const Value: Single);
  protected
    procedure LineWidthChanged; virtual;
    procedure RenderPolyline;
  public
    property LineWidth: Single read FLineWidth write SetLineWidth;
    property PixelMap: TGuiCustomPixelMap read FPixelMap;
  end;

var
  FmESTP: TFmESTP;

implementation

{$R *.dfm}

uses
  Math, DAV_Types, DAV_Common, DAV_GuiCommon, DAV_GuiBlend, DAV_MemoryUtils,
  Magnifier;

procedure TFmESTP.FormCreate(Sender: TObject);
begin
 FPixelMap := TGuiPixelMapMemory.Create;
 FPaintBoxUpdate := True;
 PaintBox.ControlStyle := PaintBox.ControlStyle + [csOpaque];
 FLineWidth := 2;
end;

procedure TFmESTP.FormDestroy(Sender: TObject);
begin
 FreeAndNil(FPixelMap);
end;

procedure TFmESTP.FormResize(Sender: TObject);
var
  x : Integer;
begin
 with FPixelMap do
  begin
   SetSize(PaintBox.Width, PaintBox.Height);
   SetLength(FPointArray, PaintBox.Width);

   for x := 0 to Length(FPointArray) - 1
    do FPointArray[x] := Round(0.5 * Height) + 0.001;
   FPointArray[1] := 0.1 * Height;
   FPointArray[2] := 0.1 * Height;
   FPointArray[3] := 0.1 * Height;
   FPointArray[4] := 0.1 * Height;

   FPointArray[24] := 0.1 * Height;
   FPointArray[25] := 0.1 * Height;
   FPointArray[26] := 0.1 * Height;
   FPointArray[27] := 0.1 * Height;
   FPointArray[28] := 0.1 * Height;

   FPointArray[42] := 0.9 * Height;


   FPointArray[56] := 0.9 * Height;
   FPointArray[57] := 0.9 * Height;
   FPointArray[58] := 0.9 * Height;
   FPointArray[59] := 0.9 * Height;
   FPointArray[60] := 0.9 * Height;

   FPointArray[76] := 0.6 * Height;
   FPointArray[79] := 0.9 * Height;

   FPointArray[77] := FPointArray[79];
   FPointArray[78] := FPointArray[79];
   FPointArray[80] := FPointArray[79];

   FPointArray[84] := Round(FPointArray[84]);
   for x := 85 to 130
    do FPointArray[x] := FPointArray[x - 1] + 1.5;
(*
   FPointArray[5] := 0.1 * Height;
   FPointArray[6] := 0.1 * Height;
   FPointArray[7] := 0.1 * Height;
   FPointArray[8] := 0.1 * Height;
*)

(*
   FPointArray[0] := 0.5 * Height;
   FPointArray[1] := FPointArray[0]; // + 0.0001;
   FPointArray[2] := FPointArray[0];
   FPointArray[3] := FPointArray[0];
   FPointArray[4] := 0.1 * Height;
   for x := 5 to 19
    do FPointArray[x] := (1 + 0.1 * (x - 4)) * 0.1 * Height;
   for x := 20 to 27
    do FPointArray[x] := FPointArray[19];
   for x := 30 to 32
    do FPointArray[x] := 0.7 * Height;
   for x := 33 to 59
    do FPointArray[x] := 0.5 * Height;
   for x := 60 to Length(FPointArray) - 1
    do FPointArray[x] := Height * (0.5 * (1 + 0.5 * (Random - Random)));

   FPointArray[28] := 0.3 * Height;
   FPointArray[29] := 0.5 * Height;
   FPointArray[44] := 0.1 * Height;
   FPointArray[45] := 0.9 * Height;
*)

   FPaintBoxUpdate := True;
  end;
end;

procedure TFmESTP.LineWidthChanged;
begin
 FPaintBoxUpdate := True;
 PaintBox.Invalidate
end;

procedure TFmESTP.PaintBoxClick(Sender: TObject);
begin
// SlLineWidth.Value := 1;
// SlLineWidth.Value := 9;
// SlLineWidth.Value := 2.999999;
// SlLineWidth.Value := 7.076;
 FmMagnifier.Show;
end;

procedure TFmESTP.SlLineWidthDblClick(Sender: TObject);
begin
 SlLineWidth.Value := 7.076;
end;

procedure TFmESTP.MiPositionAClick(Sender: TObject);
begin
 SlLineWidth.Value := 7.076;
end;

procedure TFmESTP.PaintBoxPaint(Sender: TObject);
begin
 if FPaintBoxUpdate
  then RenderPolyline;

 if Assigned(FPixelMap)
  then FPixelMap.PaintTo(PaintBox.Canvas);
end;

procedure MergeBytesInplace(Foreground: Byte; var Background: Byte);
begin
 Background := Round($FF - ($FF - Foreground) * (1 - Background * COne255th));
end;


{$DEFINE DrawSolid}
{$DEFINE DrawAntialiasedLines}
{$DEFINE DrawInnerHalfLines}
{$DEFINE DrawOuterHalfLines}

procedure TFmESTP.RenderPolyline;
var
  YRange         : array [0..2] of Integer;
  SolidRange     : array [0..1] of Integer;
  IntegerRadius  : Integer;
  NewSolid       : Integer;
  x, y, YBase    : Integer;
  PtIndex        : Integer;
  ByteValue      : Byte;

  YValues        : array of Double;
  Distance       : Double;
  YDistance      : Double;
  IntLineWdth    : Double;
  RadiusMinusOne : Double;
  CurrentValue   : Double;
  YStartPos      : Double;
  YEndPos        : Double;
  YWSSplitPos    : Double;
  YWSStartPos    : Double;
  Delta          : Double;
  WidthScale     : Double;
  TempDouble     : Double;
  VertLine       : PByteArray;
  PointPtr       : PDAVDoubleFixedArray;
  PxColor        : TPixel32;
  LeftRightIdx   : Integer;
begin
 FPaintBoxUpdate := False;
 with FPixelMap do
  begin
   FillRect(ClientRect, pxBlack32);

   PxColor := pxWhite32;
   IntLineWdth := Max(FLineWidth - 1, 0);
   RadiusMinusOne := 0.5 * IntLineWdth;

   GetAlignedMemory(VertLine, Height);
   try
    IntegerRadius := 1 + Ceil(RadiusMinusOne);
    SetLength(YValues, 1 + 2 * IntegerRadius);
    Assert(Length(YValues) mod 2 = 1);
    PointPtr := @YValues[IntegerRadius];

    for PtIndex := 0 to IntegerRadius - 1
     do YValues[PtIndex] := 0.5 * Height;

    for PtIndex := IntegerRadius to Length(YValues) - 1
     do YValues[PtIndex] := FPointArray[PtIndex - IntegerRadius];

    for x := 0 to Width - 1 do
     begin
      // get next value
      if IntegerRadius + x < Length(FPointArray)
       then YValues[Length(YValues) - 1] := FPointArray[x + IntegerRadius]
       else YValues[Length(YValues) - 1] := 0;

      // clear vertical line array
      FillChar(VertLine^, Height, 0);

      // calculate solid range
      CurrentValue := PointPtr^[0];
      YRange[0] := Trunc(CurrentValue - RadiusMinusOne);
      YRange[1] := Ceil(CurrentValue + RadiusMinusOne);
      SolidRange[0] := YRange[0] + 1;
      SolidRange[1] := YRange[1] - 1;


      // check for the solid range
      for PtIndex := 1 to IntegerRadius - 2 do
       for LeftRightIdx := 0 to 1 do
        begin
         CurrentValue := PointPtr^[(2 * LeftRightIdx - 1) * PtIndex];

         NewSolid := Trunc(CurrentValue - RadiusMinusOne) + 1;
         if NewSolid < SolidRange[0] then
          begin
           Distance := Sqrt(Sqr(RadiusMinusOne) - Sqr(PtIndex));
           NewSolid := Trunc(CurrentValue - Distance) + 1;
           if NewSolid < SolidRange[0]
            then SolidRange[0] := NewSolid;
          end
         else
          begin
           NewSolid := Ceil(CurrentValue + RadiusMinusOne) - 1;
           if NewSolid > SolidRange[1] then
            begin
             Distance := Sqrt(Sqr(RadiusMinusOne) - Sqr(PtIndex));
             NewSolid := Ceil(CurrentValue + Distance) - 1;
             if NewSolid > SolidRange[1]
              then SolidRange[1] := NewSolid;
            end;
          end;
        end;



(*
      // draw round borders
      for PtIndex := 1 to IntegerRadius - 2 do
       for LeftRightIdx := 0 to 1 do
        begin
         CurrentValue := PointPtr^[(2 * LeftRightIdx - 1) * PtIndex];

         YDistance := Sqrt(Sqr(RadiusMinusOne) - Sqr(PtIndex));
         YBase := Trunc(CurrentValue - YDistance);
         repeat
          TempDouble := Sqr(Y) + Sqr(PtIndex);
          if TempDouble <= 0
           then Assert(TempDouble >= 0);
          Distance := Sqrt(TempDouble);
          TempDouble := Distance - RadiusMinusOne + 0.5;
          if TempDouble > 1
           then ByteValue := 0 else
          if TempDouble < 0
           then ByteValue := $FF
           else ByteValue := Round($FF * TempDouble);

          if ByteValue > VertLine[Y] then VertLine[Y] := ByteValue;
          Dec(Y);
         until ByteValue = 0;
        end;
(*
*)


      // draw antialiased border
      CurrentValue := PointPtr^[0];
      if (YRange[0] < SolidRange[0]) or (YRange[0] >= SolidRange[1]) then
       begin
        Distance := 1 + RadiusMinusOne - CurrentValue + YRange[0];
        if VertLine^[YRange[0]] < $FF
         then VertLine^[YRange[0]] := Round($FF * Distance);
       end;

      if (YRange[1] < SolidRange[0]) or (YRange[1] >= SolidRange[1]) then
       begin
        Distance := 1 + RadiusMinusOne - YRange[1] + CurrentValue;
        if VertLine^[YRange[1]] < $FF
         then VertLine^[YRange[1]] := Round($FF * Distance);
       end;





      {$IFDEF DrawAntialiasedLines}

      // calculate width scale
      WidthScale := 2 + RadiusMinusOne - IntegerRadius;


(*
      if (IntegerRadius > 1) then
       for LeftRightIdx := 0 to 1 do
        begin
         // set start/end values (left/right)
         YStartPos := PointPtr^[(2 * LeftRightIdx - 1) * (IntegerRadius - 2)];
         YEndPos := PointPtr^[(2 * LeftRightIdx - 1) * (IntegerRadius - 1)];

         // eventually skip drawing if inside the solid range
         if YStartPos < YEndPos then
          if (YStartPos > SolidRange[0]) and (YEndPos < SolidRange[1])
           then Continue else else
          if (YEndPos > SolidRange[0]) and (YStartPos < SolidRange[1])
           then Continue;

         WidthScale := 2 + RadiusMinusOne - IntegerRadius;
         Assert(WidthScale >= 0);
         if WidthScale > 1
          then Assert(WidthScale <= 1);

         YWSStartPos := YEndPos + (1 - WidthScale) * (YStartPos - YEndPos);

         if YEndPos <> YWSStartPos then
          begin
           // eventually exchange start and end point

           Delta := 1 / Abs(YEndPos - YStartPos);

           if YStartPos < YEndPos then
            begin
             YRange[0] := Round(YStartPos);
             YRange[1] := Round(YEndPos);

             if YRange[1] > YRange[0] then
              begin
{
               for Y := YRange[0] to Round(YWSStartPos + RadiusMinusOne) - 1
                do VertLine^[y] := $FF;
}

               for Y := Round(YWSStartPos + RadiusMinusOne) to YRange[1] do
                begin
                 TempDouble := WidthScale + (YEndPos - Y) * Delta;
                 if VertLine^[y] < $FF
                  then MergeBytesInplace(Round(Limit(TempDouble, 0, 1) * $FF), VertLine^[y]);
                end;
              end;
            end
           else
            begin
             YRange[0] := Round(YEndPos);
             YRange[1] := Round(YStartPos);

             if YRange[1] > YRange[0] then
              begin
               for Y := YRange[0] to Round(YWSStartPos + RadiusMinusOne) - 1 do
                begin
                 TempDouble := WidthScale + (Y - YEndPos) * Delta;
                 if VertLine^[y] < $FF
                  then MergeBytesInplace(Round(Limit(TempDouble, 0, 1) * $FF), VertLine^[y]);
                end;
{
               for Y := Round(YWSStartPos + RadiusMinusOne) to YRange[1]
                do VertLine^[y] := $FF;
}
              end;
            end;
          end;
        end;
*)



      {$IFDEF DrawInnerHalfLines}
      if (IntegerRadius > 1) then
       for LeftRightIdx := 0 to 1 do
        begin
         // set start/end values (left/right)
         YStartPos := PointPtr^[(2 * LeftRightIdx - 1) * (IntegerRadius - 2)];
         YEndPos := PointPtr^[(2 * LeftRightIdx - 1) * (IntegerRadius - 1)];

         // eventually skip drawing if inside the solid range
         if YStartPos < YEndPos then
          if (YStartPos > SolidRange[0]) and (YEndPos < SolidRange[1])
           then Continue else else
          if (YEndPos > SolidRange[0]) and (YStartPos < SolidRange[1])
           then Continue;

         // calculate split point
         YWSSplitPos := YStartPos + WidthScale * (YEndPos - YStartPos);

         if YEndPos <> YWSSplitPos then
          begin
           Delta := (1 - WidthScale) / Abs(YEndPos - YStartPos);

           if YStartPos < YEndPos then
            begin
             YRange[0] := Round(YWSSplitPos);
             YRange[1] := Round(YEndPos);
             for Y := Round(YStartPos) to YRange[0] - 1
              do VertLine^[y] := $FF;
            end
           else
            begin
             YRange[0] := Round(YEndPos);
             YRange[1] := Round(YWSSplitPos);
             for Y := YRange[1] to Round(YStartPos) - 1
              do VertLine^[y] := $FF;
            end;

           Delta := 1  / (YWSSplitPos - YEndPos);

           if YRange[1] > YRange[0] then
            begin
             for Y := YRange[0] to YRange[1] - 1 do
              begin
               TempDouble := WidthScale + (Y - YEndPos) * Delta;

               if VertLine^[y] < $FF
                then MergeBytesInplace(Round(Limit(TempDouble, 0, 1) * $FF), VertLine^[y]);
              end;
            end;
          end;
        end;
      {$ENDIF}



      {$IFDEF DrawOuterHalfLines}
      for LeftRightIdx := 0 to 1 do
       begin
        // set start/end values (left/right)
        YStartPos := PointPtr^[(2 * LeftRightIdx - 1) * (IntegerRadius - 1)];
        YEndPos := PointPtr^[(2 * LeftRightIdx - 1) * IntegerRadius];

        // calculate split point
        YWSSplitPos := YStartPos + WidthScale * (YEndPos - YStartPos);

        if YWSSplitPos <> YStartPos then
         begin
          if YStartPos < YEndPos then
           begin
            YStartPos := YStartPos + RadiusMinusOne;
            YWSSplitPos := YWSSplitPos - RadiusMinusOne;
            YRange[0] := Ceil(YStartPos);
            YRange[1] := Trunc(YWSSplitPos) + 1;
           end
          else
           begin
            YStartPos := YStartPos - RadiusMinusOne;
            YWSSplitPos := YWSSplitPos + RadiusMinusOne;
            YRange[0] := Ceil(YWSSplitPos);
            YRange[1] := Trunc(YStartPos) + 1;
           end;

          Delta := WidthScale / (YStartPos - YWSSplitPos);

          // exclude solid range
          if (YRange[0] >= SolidRange[0]) and (YRange[0] < SolidRange[1])
           then YRange[0] := SolidRange[1];
          if (YRange[1] < SolidRange[1]) and (YRange[1] >= SolidRange[0])
           then YRange[1] := SolidRange[0];

          for Y := YRange[0] to YRange[1] - 1 do
           begin
            TempDouble := (Y - YWSSplitPos) * Delta;
            if VertLine^[Y] < $FF
             then MergeBytesInplace(Round(Limit(TempDouble, 0, 1) * $FF), VertLine^[Y]);
           end;
         end;
       end;
      {$ENDIF}

      {$ENDIF}



      {$IFDEF DrawSolid}
      // fill solid
      for y := Max(0, SolidRange[0]) to Min(Height - 1, SolidRange[1])
       do VertLine^[y] := $FF;
      {$ENDIF}


      // copy line to pixel map
      for y := 0 to Height - 1 do
       if VertLine^[y] > 0
        then CombinePixelInplace(PxColor, PixelPointer[x, y]^, VertLine^[y]);
      EMMS;

      // shift y-values
      Move(YValues[1], YValues[0], (Length(YValues) - 1) * SizeOf(Double));
     end;
   finally
    FreeAlignedMemory(VertLine);
   end;

   MakeOpaque;
  end;
end;

procedure TFmESTP.SetLineWidth(const Value: Single);
begin
 if FLineWidth <> Value then
  begin
   FLineWidth := Value;
   LineWidthChanged;
  end;
end;

procedure TFmESTP.SlLineWidthChange(Sender: TObject);
var
  CenterValue  : Integer;
begin
 LineWidth := SlLineWidth.Value;

 if FmMagnifier.Visible
  then FmMagnifier.PaintBox.Invalidate;


(*
 Label1.Visible := True;
 Label2.Visible := True;

 CenterValue := 1 + Ceil(0.5 * Max(SlLineWidth.Value - 1, 0));
 Label1.Caption := IntToStr(1 + 2 * CenterValue);
 Label2.Caption := FloatToStrF(2 + 0.5 * (FLineWidth - 1) - CenterValue, ffGeneral, 4, 4);
*)
end;

end.
