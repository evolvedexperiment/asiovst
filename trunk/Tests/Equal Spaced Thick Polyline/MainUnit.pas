unit MainUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, Menus, DAV_GuiByteMap, DAV_GuiPixelMap,
  DAV_GuiGraphicControl, DAV_GuiLabel, DAV_GuiSlider, DAV_GuiFilters, ComCtrls,
  ActnList;

type
  TFmESTP = class(TForm)
    PaintBox: TPaintBox;
    SlLineWidth: TGuiSlider;
    PuLinePreset: TPopupMenu;
    MiWidthG: TMenuItem;
    MiWidthA: TMenuItem;
    MiWidthB: TMenuItem;
    MiWidthH: TMenuItem;
    MiWidthF: TMenuItem;
    N1: TMenuItem;
    MiAddTinyValue: TMenuItem;
    MiSubtractTinyValue: TMenuItem;
    PuScenario: TPopupMenu;
    MiScenarioStandard: TMenuItem;
    MiScenarioPeakLineI: TMenuItem;
    MiScenarioRandom: TMenuItem;
    MiScenarioSmallIncrease: TMenuItem;
    MiScenarioExceedBorders: TMenuItem;
    MiWidthC: TMenuItem;
    MiWidthD: TMenuItem;
    MiWidthE: TMenuItem;
    MiUpDown: TMenuItem;
    StatusBar: TStatusBar;
    ActionList1: TActionList;
    ACAddTinyValue: TAction;
    ACSubtractTinyValue: TAction;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure PaintBoxPaint(Sender: TObject);
    procedure SlLineWidthChange(Sender: TObject);
    procedure PaintBoxClick(Sender: TObject);
    procedure MiWidthGClick(Sender: TObject);
    procedure MiWidthAClick(Sender: TObject);
    procedure MiWidthBClick(Sender: TObject);
    procedure MiWidthHClick(Sender: TObject);
    procedure MiWidthFClick(Sender: TObject);
    procedure MiScenarioStandardClick(Sender: TObject);
    procedure MiScenarioRandomClick(Sender: TObject);
    procedure MiScenarioPeakLineIClick(Sender: TObject);
    procedure MiScenarioSmallIncreaseClick(Sender: TObject);
    procedure MiScenarioExceedBordersClick(Sender: TObject);
    procedure MiWidthCClick(Sender: TObject);
    procedure MiWidthDClick(Sender: TObject);
    procedure MiWidthEClick(Sender: TObject);
    procedure MiUpDownClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ACAddTinyValueExecute(Sender: TObject);
    procedure ACSubtractTinyValueExecute(Sender: TObject);
  private
    FPixelMap       : TGuiCustomPixelMap;
    FLineWidth      : Single;
    FPaintBoxUpdate : Boolean;
    FPointArray     : array of Double;
    procedure SetLineWidth(const Value: Single);
    procedure ScenarioPeakLine1;
    procedure ScenarioRandom;
    procedure ScenarioStandard;
    procedure ScenarioSmallIncrease;
    procedure ScenarioExceedBorders;
    procedure ScenarioUpDown;
    procedure UpdateStatusInformation;
  protected
    procedure LineWidthChanged; virtual;
    procedure UpdateGui; virtual;
    procedure RenderPolyline;
    procedure RenderPolylineDraft;
  public
    property LineWidth: Single read FLineWidth write SetLineWidth;
    property PixelMap: TGuiCustomPixelMap read FPixelMap;
  end;

var
  FmESTP: TFmESTP;

implementation

{$R *.dfm}

uses
  Math, DAV_Types, DAV_Common, DAV_GuiCommon, DAV_GuiFixedPoint, DAV_GuiBlend,
  DAV_MemoryUtils, Magnifier;

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

procedure TFmESTP.FormShow(Sender: TObject);
begin
 UpdateStatusInformation;
end;

procedure TFmESTP.FormResize(Sender: TObject);
begin
 with FPixelMap do
  begin
   SetSize(PaintBox.Width, PaintBox.Height);
   SetLength(FPointArray, PaintBox.Width);

   ScenarioStandard;
   FPaintBoxUpdate := True;
  end;
end;

procedure TFmESTP.ScenarioStandard;
var
  x : Integer;
begin
 with FPixelMap do
  begin
   FPointArray[0] := Round(0.5 * Height);
   FPointArray[1] := FPointArray[0]; // + 0.0001;
   FPointArray[2] := FPointArray[0];
   FPointArray[3] := FPointArray[0];
   FPointArray[4] := 0.1 * Height;
   for x := 5 to 19
    do FPointArray[x] := (1 + 0.1 * (x - 4)) * 0.1 * Height;

   for x := 20 to 27
    do FPointArray[x] := FPointArray[19];
   FPointArray[28] := 0.3 * Height;
   FPointArray[29] := 0.5 * Height;
   for x := 30 to 32
    do FPointArray[x] := 0.7 * Height;

   for x := 33 to 59
    do FPointArray[x] := Round(0.5 * Height);
   FPointArray[44] := 0.1 * Height;
   FPointArray[45] := 0.9 * Height;

   for x := 60 to 90
    do FPointArray[x] := 0.5 * Height + x;

   for x := 91 to 199
    do FPointArray[x] := 0.5 * Height + 0.008 * Sqr(x - 91);

   for x := 200 to Length(FPointArray) - 1
    do FPointArray[x] := Height * (0.5 * (1 + 0.5 * (Random - Random)));

  end;
end;

procedure TFmESTP.ScenarioUpDown;
var
  x : Integer;
begin
 with FPixelMap do
  begin
   for x := 0 to Length(FPointArray) - 1
    do FPointArray[x] := 0.1 + 0.5 * Height + (2 * ((x div 32) mod 2) - 1) *
      (x mod 32) + 0.1 * (x div 32) - 32 * ((x div 32) mod 2);
  end;
end;

procedure TFmESTP.ScenarioPeakLine1;
var
  x : Integer;
begin
 with FPixelMap do
  begin
   for x := 0 to Length(FPointArray) - 1
    do FPointArray[x] := Round(0.5 * Height) + 0.001;
   FPointArray[1] := Round(0.1 * Height);
   FPointArray[2] := Round(0.1 * Height);
   FPointArray[3] := Round(0.1 * Height);
   FPointArray[4] := Round(0.1 * Height);

   FPointArray[24] := Round(0.1 * Height) + 0.5;
   FPointArray[25] := Round(0.1 * Height) + 0.5;
   FPointArray[26] := Round(0.1 * Height) + 0.5;
   FPointArray[27] := Round(0.1 * Height) + 0.5;
   FPointArray[28] := Round(0.1 * Height) + 0.5;

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
  end;
end;

procedure TFmESTP.ScenarioRandom;
var
  x : Integer;
begin
 with FPixelMap do
  for x := 0 to Length(FPointArray) - 1
   do FPointArray[x] := Height * (0.5 * (1 + 0.5 * (Random - Random)));
end;

procedure TFmESTP.ScenarioSmallIncrease;
var
  x : Integer;
begin
 with FPixelMap do
  for x := 0 to Length(FPointArray) - 1
   do FPointArray[x] := Height * 0.5 - 4 * x / Length(FPointArray);
end;

procedure TFmESTP.ScenarioExceedBorders;
var
  x : Integer;
begin
 with FPixelMap do
  for x := 0 to Length(FPointArray) - 1
   do FPointArray[x] := (2 * Random - 0.5) * Height;
end;

procedure TFmESTP.LineWidthChanged;
begin
 UpdateGui;
end;

procedure TFmESTP.PaintBoxClick(Sender: TObject);
begin
 with FmMagnifier do
  begin
   Show;
   Magnify(4);
  end;
end;

procedure TFmESTP.UpdateGui;
begin
 FPaintBoxUpdate := True;
 PaintBox.Invalidate
end;

procedure TFmESTP.MiWidthAClick(Sender: TObject);
begin
 SlLineWidth.Value := 2.0;
end;

procedure TFmESTP.MiWidthBClick(Sender: TObject);
begin
 SlLineWidth.Value := 2.99999;
end;

procedure TFmESTP.MiWidthCClick(Sender: TObject);
begin
 SlLineWidth.Value := 3.0;
end;

procedure TFmESTP.MiWidthDClick(Sender: TObject);
begin
 SlLineWidth.Value := 3.5;
end;

procedure TFmESTP.MiWidthEClick(Sender: TObject);
begin
 SlLineWidth.Value := 4.5;
end;

procedure TFmESTP.MiWidthFClick(Sender: TObject);
begin
 SlLineWidth.Value := 7.0;
end;

procedure TFmESTP.MiWidthGClick(Sender: TObject);
begin
 SlLineWidth.Value := 7.076;
end;

procedure TFmESTP.MiWidthHClick(Sender: TObject);
begin
 SlLineWidth.Value := 9.0;
end;

procedure TFmESTP.MiScenarioExceedBordersClick(Sender: TObject);
begin
 MiScenarioExceedBorders.Checked := True;
 ScenarioExceedBorders;
 UpdateGui;
end;

procedure TFmESTP.MiScenarioPeakLineIClick(Sender: TObject);
begin
 MiScenarioPeakLineI.Checked := True;
 ScenarioPeakLine1;
 UpdateGui;
end;

procedure TFmESTP.MiScenarioRandomClick(Sender: TObject);
begin
 MiScenarioRandom.Checked := True;
 ScenarioRandom;
 UpdateGui;
end;

procedure TFmESTP.MiScenarioSmallIncreaseClick(Sender: TObject);
begin
 MiScenarioSmallIncrease.Checked := True;
 ScenarioSmallIncrease;
 UpdateGui;
end;

procedure TFmESTP.MiScenarioStandardClick(Sender: TObject);
begin
 MiScenarioStandard.Checked := True;
 ScenarioStandard;
 UpdateGui;
end;

procedure TFmESTP.ACAddTinyValueExecute(Sender: TObject);
begin
 SlLineWidth.Value := SlLineWidth.Value + 0.0001;
end;

procedure TFmESTP.ACSubtractTinyValueExecute(Sender: TObject);
begin
 SlLineWidth.Value := SlLineWidth.Value - 0.0001;
end;

procedure TFmESTP.MiUpDownClick(Sender: TObject);
begin
 MiUpDown.Checked := True;
 ScenarioUpDown;
 UpdateGui;
end;

procedure TFmESTP.PaintBoxPaint(Sender: TObject);
begin
 if FPaintBoxUpdate then
  begin
   RenderPolylineDraft;
   with FmMagnifier do
    if Visible then
     begin
      Magnify(4);
      PaintBox.Invalidate;
     end;
  end;

 if Assigned(FPixelMap)
  then FPixelMap.PaintTo(PaintBox.Canvas);
end;

procedure MergeBytesInplace(Foreground: Byte; var Background: Byte);
begin
 Background := Round($FF - ($FF - Foreground) * (1 - Background * COne255th));
end;


{$DEFINE DrawSolid}
{$DEFINE DrawAntialiasedBorder}
{$DEFINE DrawAntialiasedLines}
{$DEFINE DrawInnerHalfLines}
{$DEFINE DrawOuterHalfLines}
{$DEFINE DrawHalo}

{-$DEFINE ShowCenter}


procedure TFmESTP.RenderPolyline;
var
  Distance        : Double;
  IntLineWdth     : Double;
  RadiusMinusHalf : Double;
  CurrentValue    : Double;
  YStartPos       : Double;
  YEndPos         : Double;
  YWSSplitPos     : Double;
  Delta           : Double;
  WidthScale      : Double;
  TempDouble      : Double;

  YRange          : array [0..1] of Integer;
  SolidRange      : array [0..1] of Integer;
  NewYRange       : array [0..1] of Integer;
  IntegerRadiusX  : Integer;
  IntegerRadiusY  : Integer;
  NewSolid        : Integer;
  x, y            : Integer;
  PtIndex         : Integer;

  YValues         : array of Double;
  VertLine        : PByteArray;
  PointPtr        : PDAVDoubleFixedArray;
  PxColor         : TPixel32;
  LeftRightIdx    : Integer;

  procedure AddToSolidRange(Lower, Upper: Integer);
  begin
   if Lower < Upper then
    begin
     if Lower < SolidRange[0] then SolidRange[0] := Lower;
     if Upper > SolidRange[1] then SolidRange[1] := Upper;
    end;
  end;

  function CalculateCircleYOffset(Value: Single; XOffset: Integer): Single;
  begin
   Result := Sqr(Value) - Sqr(XOffset);
   if Result > 0
    then Result := Sqrt(Result)
    else Result := 0;
  end;

  function CalculateMinCircleYOffset(Value: Single; XOffset: Integer): Single;
  begin
   Result := Sqr(Value - 0.5) - Sqr(XOffset);
   if Result > 0
    then Result := Sqrt(Result)
    else Result := 0;
  end;

  function CalculateMaxCircleYOffset(Value: Single; XOffset: Integer): Single;
  begin
   Result := Sqr(Value + 0.5) - Sqr(XOffset);
   if Result > 0
    then Result := Sqrt(Result)
    else Result := 0;
  end;

begin
 FPaintBoxUpdate := False;
 with FPixelMap do
  begin
   FillRect(ClientRect, pxBlack32);

   PxColor := pxWhite32;
   IntLineWdth := Max(FLineWidth - 1, 0);
   RadiusMinusHalf := 0.5 * IntLineWdth;

   GetAlignedMemory(VertLine, Height);
   try
    IntegerRadiusY := 2 + Trunc(RadiusMinusHalf);
    SetLength(YValues, 1 + 2 * IntegerRadiusY);
    Assert(Length(YValues) mod 2 = 1);
    PointPtr := @YValues[IntegerRadiusY];

    for PtIndex := 0 to IntegerRadiusY - 1
     do YValues[PtIndex] := 0.5 * Height;

    for PtIndex := IntegerRadiusY to Length(YValues) - 1
     do YValues[PtIndex] := FPointArray[PtIndex - IntegerRadiusY];

    for x := 0 to Width - 1 do
     begin
      // get next value
      if IntegerRadiusY + x < Length(FPointArray)
       then YValues[Length(YValues) - 1] := FPointArray[x + IntegerRadiusY]
       else YValues[Length(YValues) - 1] := 0;

      // clear vertical line array
      FillChar(VertLine^, Height, 0);

      // calculate solid range
      CurrentValue := PointPtr^[0];
      YRange[0] := Trunc(CurrentValue - RadiusMinusHalf);
      YRange[1] := Ceil(CurrentValue + RadiusMinusHalf);
      SolidRange[0] := YRange[0] + 1;
      SolidRange[1] := YRange[1] - 1;


      // check for the solid range
      for PtIndex := 1 to IntegerRadiusY - 2 do
       for LeftRightIdx := 0 to 1 do
        begin
         CurrentValue := PointPtr^[(2 * LeftRightIdx - 1) * PtIndex];

         NewSolid := Trunc(CurrentValue - RadiusMinusHalf) + 1;
         if NewSolid < SolidRange[0] then
          begin
           Distance := Sqr(RadiusMinusHalf) - Sqr(PtIndex);
           if Distance > 0
            then Distance := Sqrt(Distance)
            else Distance := 0;
           NewSolid := Trunc(CurrentValue - Distance) + 1;
           if NewSolid < SolidRange[0]
            then SolidRange[0] := NewSolid;
          end
         else
          begin
           NewSolid := Ceil(CurrentValue + RadiusMinusHalf) - 1;
           if NewSolid > SolidRange[1] then
            begin
             Distance := Sqr(RadiusMinusHalf) - Sqr(PtIndex);
             if Distance > 0
              then Distance := Sqrt(Distance)
              else Distance := 0;
             NewSolid := Trunc(CurrentValue + Distance);
             if NewSolid > SolidRange[1]
              then SolidRange[1] := NewSolid;
            end;
          end;
        end;





      {$IFDEF DrawAntialiasedBorder}
      // draw antialiased border
      if (YRange[0] < SolidRange[0]) or (YRange[0] >= SolidRange[1]) then
       begin
        Distance := 1 + RadiusMinusHalf - PointPtr^[0] + YRange[0];
        if (YRange[0] >= 0) and (YRange[0] < Height) and (VertLine^[YRange[0]] < $FF)
         then MergeBytesInplace(Round($FF * Distance), VertLine^[YRange[0]]);
       end;

      if (YRange[1] < SolidRange[0]) or (YRange[1] >= SolidRange[1]) then
       begin
        Distance := 1 + RadiusMinusHalf - YRange[1] + PointPtr^[0];
        if (YRange[1] >= 0) and (YRange[1] < Height) and (VertLine^[YRange[1]] < $FF)
         then MergeBytesInplace(Round($FF * Distance), VertLine^[YRange[1]])
       end;
      {$ENDIF}



      {$IFDEF DrawAntialiasedLines}

      // calculate width scale
      WidthScale := 2 + RadiusMinusHalf - IntegerRadiusY;


      {$IFDEF DrawInnerHalfLines}
       for LeftRightIdx := 0 to 1 do
        begin
         // set start/end values (left/right)
         YStartPos := PointPtr^[(2 * LeftRightIdx - 1) * (IntegerRadiusY - 2)];
         YEndPos := PointPtr^[(2 * LeftRightIdx - 1) * (IntegerRadiusY - 1)];

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
           if YStartPos < YEndPos then
            begin
             YRange[0] := Round(YWSSplitPos);
             YRange[1] := Round(YEndPos);

             AddToSolidRange(Round(YStartPos), YRange[0]);
            end
           else
            begin
             YRange[0] := Round(YEndPos);
             YRange[1] := Round(YWSSplitPos);

             AddToSolidRange(YRange[1], Round(YStartPos));
            end;

           Delta := 1  / (YWSSplitPos - YEndPos);

           for Y := YRange[0] + 1 to YRange[1] - 1 do
            begin
             TempDouble := WidthScale + (Y - YEndPos) * Delta;

             if (y >= 0) and (y < Height) and (VertLine^[y] < $FF)
              then MergeBytesInplace(Round(Limit(TempDouble, 0, 1) * $FF), VertLine^[y]);
            end;
          end;
        end;
      {$ENDIF}



      {$IFDEF DrawOuterHalfLines}
      for LeftRightIdx := 0 to 1 do
       begin
        // set start/end values (left/right)
        YStartPos := PointPtr^[(2 * LeftRightIdx - 1) * (IntegerRadiusY - 1)];
        YEndPos := PointPtr^[(2 * LeftRightIdx - 1) * IntegerRadiusY];

        // calculate split point
        YWSSplitPos := YStartPos + WidthScale * (YEndPos - YStartPos);

        if YWSSplitPos <> YStartPos then
         begin
          if YStartPos < YEndPos then
           begin
            YRange[0] := Round(YStartPos);
            YRange[1] := Round(YWSSplitPos);
           end
          else
           begin
            YRange[0] := Round(YWSSplitPos);
            YRange[1] := Round(YStartPos);
           end;

          Delta := WidthScale / (YStartPos - YWSSplitPos);

(*
          // exclude solid range
          if (YRange[0] >= SolidRange[0]) and (YRange[0] <= SolidRange[1])
           then YRange[0] := SolidRange[1] + 1;
          if (YRange[1] < SolidRange[1]) and (YRange[1] >= SolidRange[0])
           then YRange[1] := SolidRange[0];
*)

(*
          if (YRange[0] < SolidRange[0]) and (YRange[1] > SolidRange[1])
           then YRange[0] := SolidRange[1];
*)

          for Y := YRange[0] + 1 to YRange[1] - 1 do
           begin
            TempDouble := (Y - YWSSplitPos) * Delta;
            if (y >= 0) and (y < Height) and (VertLine^[Y] < $FF)
             then MergeBytesInplace(Round(Limit(TempDouble, 0, 1) * $FF), VertLine^[Y]);
           end;
         end;
       end;
      {$ENDIF}

      {$ENDIF}


      // draw round borders
      {$IFDEF DrawHalo}
(*
      for PtIndex := 0 to IntegerRadiusY - 1 do
       for LeftRightIdx := 0 to 1 do
        begin
         CurrentValue := PointPtr^[(2 * LeftRightIdx - 1) * PtIndex];
         TempDouble := CalculateMaxCircleYOffset(RadiusMinusHalf, PtIndex);

         YRange[0] := Round(CurrentValue - TempDouble);
         YRange[1] := Round(CurrentValue + TempDouble);


         TempDouble := CalculateMinCircleYOffset(RadiusMinusHalf, PtIndex);
         NewYRange[0] := Round(CurrentValue - TempDouble) - 1;
         NewYRange[1] := Round(CurrentValue + TempDouble) + 1;

         for Y := YRange[0] to NewYRange[0] do
          begin
           Distance := Sqrt(Sqr(Y - CurrentValue) + Sqr(PtIndex)) - 0.5;

           if (y >= 0) and (y < Height) then
//            if (VertLine^[Y] = 0) then
            begin
//             Assert(RadiusMinusHalf + 0.5 - Distance <= 1);
             MergeBytesInplace(Round(Limit(RadiusMinusHalf + 0.5 - Distance, 0, 1) * $FF), VertLine^[Y]);
            end;
          end;

         for Y := NewYRange[1] to YRange[1] do
          begin
           Distance := Sqrt(Sqr(Y - CurrentValue) + Sqr(PtIndex)) - 0.5;

           if (y >= 0) and (y < Height) then
//            if (VertLine^[Y] = 0) then
            begin
//             Assert(RadiusMinusHalf + 0.5 - Distance <= 1);
             MergeBytesInplace(Round(Limit(RadiusMinusHalf + 0.5 - Distance, 0, 1) * $FF), VertLine^[Y]);
            end;
          end;


{
         for Y := YRange[0] to YRange[1] do
          begin
           Distance := Sqrt(Sqr(Y - CurrentValue) + Sqr(PtIndex)) - 0.5;

           if (y >= 0) and (y < Height) then
//            if (VertLine^[Y] = 0) then
             MergeBytesInplace(Round(Limit(RadiusMinusHalf + 0.5 - Distance, 0, 1) * $FF), VertLine^[Y]);
          end;
}
        end;
*)

      for PtIndex := 1 to IntegerRadiusY - 1 do
       for LeftRightIdx := 0 to 1 do
        begin
         CurrentValue := PointPtr^[(2 * LeftRightIdx - 1) * PtIndex];
         TempDouble := Sqr(RadiusMinusHalf + 0.5) - Sqr(PtIndex);
         if TempDouble > 0
          then TempDouble := Sqrt(TempDouble)
          else TempDouble := 0;

         YRange[0] := Round(CurrentValue - TempDouble);
         YRange[1] := Round(CurrentValue + TempDouble);

         for Y := YRange[0] to YRange[1] do
          begin
           Distance := Sqrt(Sqr(Y - CurrentValue) + Sqr(PtIndex)) - 0.5;
           if ((y >= SolidRange[0]) and (y <= SolidRange[1]))
            then Continue;

           if (y >= 0) and (y < Height) and (VertLine[Y] < $FF) then
            if (RadiusMinusHalf + 0.5 - Distance < 1) // and (VertLine^[Y] = 0)
             then MergeBytesInplace(Round(Limit(RadiusMinusHalf + 0.5 - Distance, 0, 1) * $FF), VertLine^[Y]);
          end;

        end;
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

      {$IFDEF ShowCenter}
      if (PointPtr[0] >= 0) and (PointPtr[0] < Height) then
       begin
        CurrentValue := PointPtr[0];
        DontRaiseExceptionsAndSetFPUcodeword;
        Y := Round(CurrentValue);
        BlendPixelInplace(pxRed32, PixelPointer[x, Y]^);
       end;
      EMMS;
      {$ENDIF}

      // shift y-values
      Move(YValues[1], YValues[0], (Length(YValues) - 1) * SizeOf(Double));
     end;
   finally
    FreeAlignedMemory(VertLine);
   end;

   MakeOpaque;
  end;
end;

procedure TFmESTP.RenderPolylineDraft;
var
  SolidRange      : array [0..1] of Integer;
  IntegerRadiusX  : Integer;
  IntegerRadiusY  : Integer;
  NewSolid        : Integer;
  x, y            : Integer;
  PtIndex         : Integer;

  YValues         : array of TFixed24Dot8Point;
  Distance        : TFixed24Dot8Point;
  IntLineWdth     : TFixed24Dot8Point;
  RadiusMinusHalf : TFixed24Dot8Point;
  CurrentValue    : TFixed24Dot8Point;
  YStartPos       : TFixed24Dot8Point;
  YEndPos         : TFixed24Dot8Point;
  WidthScale      : TFixed24Dot8Point;
  PointPtr        : PFixed24Dot8PointArray;
  PxColor         : TPixel32;
  LeftRightIdx    : Integer;


  procedure AddToSolidRange(Lower, Upper: Integer);
  begin
   if Lower < Upper then
    begin
     if Lower < SolidRange[0] then SolidRange[0] := Lower;
     if Upper > SolidRange[1] then SolidRange[1] := Upper;
    end;
  end;

begin
 FPaintBoxUpdate := False;
 with FPixelMap do
  begin
   FillRect(ClientRect, pxBlack32);

   PxColor := pxWhite32;
   IntLineWdth := ConvertToFixed24Dot8Point(Max(FLineWidth - 1, 0));
   RadiusMinusHalf := FixedMul(IntLineWdth, CFixed24Dot8Half);

   // initialize temporaty variables
   IntegerRadiusX := 1 + FixedCeil(RadiusMinusHalf);
   IntegerRadiusY := 2 + FixedFloor(RadiusMinusHalf);
   SetLength(YValues, 1 + 2 * IntegerRadiusX);
   Assert(Length(YValues) mod 2 = 1);
   PointPtr := @YValues[IntegerRadiusX];

   // fill additional points
   for PtIndex := 0 to IntegerRadiusX - 1
    do YValues[PtIndex] := ConvertToFixed24Dot8Point(0.5 * Height);

   for PtIndex := IntegerRadiusX to Length(YValues) - 1
    do YValues[PtIndex] := ConvertToFixed24Dot8Point(FPointArray[PtIndex - IntegerRadiusX]);


   for x := 0 to Width - 1 do
    begin
     // get next value
     if IntegerRadiusX + x < Length(FPointArray)
      then YValues[Length(YValues) - 1] := ConvertToFixed24Dot8Point(FPointArray[x + IntegerRadiusX])
      else YValues[Length(YValues) - 1].Fixed := 0;

     // calculate solid range
     CurrentValue := PointPtr^[0];
     SolidRange[0] := FixedRound(FixedSub(CurrentValue, RadiusMinusHalf));
     SolidRange[1] := FixedRound(FixedAdd(CurrentValue, RadiusMinusHalf));

     // check for the solid range
     for PtIndex := 1 to IntegerRadiusY - 2 do
      begin
       // calculate distance
       Distance := FixedSqrt(FixedSub(FixedSqr(RadiusMinusHalf),
         FixedSqr(ConvertToFixed24Dot8Point(PtIndex))));

       for LeftRightIdx := 0 to 1 do
        begin
         CurrentValue := PointPtr^[(2 * LeftRightIdx - 1) * PtIndex];

         // quick check for rectangle
         NewSolid := FixedRound(FixedSub(CurrentValue, Distance));
         if NewSolid < SolidRange[0]
          then SolidRange[0] := NewSolid
          else
           begin
            NewSolid := FixedRound(FixedAdd(CurrentValue, Distance));
            if NewSolid > SolidRange[1]
             then SolidRange[1] := NewSolid;
           end;
        end;
      end;
     {$IFDEF DrawAntialiasedLines}

     // calculate width scale (0 < x <= 1)
     WidthScale := FixedSub(RadiusMinusHalf, ConvertToFixed24Dot8Point(IntegerRadiusX - 2));

     {$IFDEF DrawInnerHalfLines}
     if IntegerRadiusY = IntegerRadiusX then
      for LeftRightIdx := 0 to 1 do
       begin
        // set start/end values (left/right)
        YStartPos := PointPtr^[(2 * LeftRightIdx - 1) * (IntegerRadiusX - 2)];
        YEndPos := PointPtr^[(2 * LeftRightIdx - 1) * (IntegerRadiusX - 1)];

        // calculate split point
        Distance := FixedSub(YStartPos, FixedMul(WidthScale, FixedSub(YStartPos, YEndPos)));
        CurrentValue := FixedAdd(YEndPos,
          ((FixedMul(FixedSub(CFixed24Dot8Half, WidthScale), FixedSub(Distance, YEndPos)))));

        Y := FixedRound(YStartPos);
        if YStartPos.Fixed <= YEndPos.Fixed then
         if FixedRound(CurrentValue) <= FixedRound(YEndPos)
          then AddToSolidRange(Y, FixedRound(CurrentValue))
          else AddToSolidRange(Y, FixedRound(YEndPos) - 1)
        else
         if FixedRound(CurrentValue) > FixedRound(YEndPos)
          then AddToSolidRange(FixedRound(CurrentValue) + 1, Y)
          else AddToSolidRange(FixedRound(YEndPos), Y);

       end;
     {$ENDIF}

     {$IFDEF DrawOuterHalfLines}
      for LeftRightIdx := 0 to 1 do
       begin
        // set start/end values (left/right)
        YStartPos := PointPtr^[(2 * LeftRightIdx - 1) * (IntegerRadiusX - 1)];
        YEndPos := PointPtr^[(2 * LeftRightIdx - 1) * IntegerRadiusX];

        // calculate split point
        CurrentValue := FixedSub(YStartPos, YEndPos);
        Distance := FixedSub(YStartPos, FixedMul(WidthScale, CurrentValue));
        CurrentValue := FixedAdd(Distance, FixedMul(CFixed24Dot8Half, CurrentValue));

        Y := FixedRound(YStartPos);
        if YStartPos.Fixed <= YEndPos.Fixed then
         if FixedRound(CurrentValue) < FixedRound(Distance)
          then AddToSolidRange(Y, FixedRound(CurrentValue))
          else AddToSolidRange(Y, FixedRound(Distance) - 1)
        else
         if FixedRound(CurrentValue) >= FixedRound(Distance)
          then AddToSolidRange(FixedRound(CurrentValue) + 1, Y)
          else AddToSolidRange(FixedRound(Distance), Y);
       end;
     {$ENDIF}

     {$ENDIF}

     // copy line to pixel map
     for y := Max(0, SolidRange[0]) to Min(Height - 1, SolidRange[1])
      do BlendPixelInplace(PxColor, PixelPointer[x, y]^);

     // shift y-values
     Move(YValues[1], YValues[0], (Length(YValues) - 1) * SizeOf(TFixed24Dot8Point));
    end;

   EMMS;
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
begin
 LineWidth := SlLineWidth.Value;
 UpdateStatusInformation;
end;

procedure TFmESTP.UpdateStatusInformation;
var
  TempValue    : Double;
  CenterValue  : Integer;
begin
 TempValue := 0.5 * Max(SlLineWidth.Value - 1, 0);
 CenterValue := 1 + Ceil(TempValue);
 StatusBar.Panels[0].Text := 'X Pixel: ' + IntToStr(1 + 2 * CenterValue);
 StatusBar.Panels[1].Text := 'Fractional: ' + FloatToString(2 + 0.5 * (FLineWidth - 1) - CenterValue, 4);
end;

end.
