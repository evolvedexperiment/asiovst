unit SmoothMultibandCompressorGUI;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, Types, Controls, ExtCtrls, GR32,
  GR32_Image, Graphics, GraphicEx, DAV_Common, DAV_VSTModule;

const
  cSweepRange = 10;
  cFadeSpeed = 11;
  cFadeSpeedH = cFadeSpeed div 2;
  cAboutScrollLength = 700;

type
  TVerticallyStitchedBitmap32 = class(TBitmap32)
  private
    FNumGlyphs: Integer;
    function GetRealHeight: Integer;
    procedure SetNumGlyphs(const Value: Integer);
  public
    constructor Create; override;
  published
    property RealWidth : Integer read FWidth;
    property RealHeight : Integer read GetRealHeight;
    property NumGlyphs : Integer read FNumGlyphs write SetNumGlyphs;
  end;

  TEditValue = (edNone, edLow, edHigh, edAttack, edRelease, edThreshold,
    edRatio, edKnee, edMakeUpGain, edVolume);

  TGraph = (gLow, gLowMid, gHighMid, gHigh);
  TGraphs = set of TGraph;
  TFmSmoothMultibandCompressor = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure TimerTimer(Sender: TObject);
  private
    FBackground   : Array [0..1] of TBitmap32;

    FStage        : Integer;
    FTimer        : TTimer;
    FEditValue    : TEditValue;
    FAnimateFrame : Integer;
    procedure RestitchPNG2Bitmap32(const PNG: TPNGGraphic;
      const BMP: TVerticallyStitchedBitmap32);

    procedure DrawAll;
    procedure DrawLowFrequency;
    procedure DrawHighFrequency;
    procedure DrawOutputGain;
    procedure DrawAttack(const Stage: Integer);
    procedure DrawRelease(const Stage: Integer);
    procedure DrawThreshold(const Stage: Integer);
    procedure DrawRatio(const Stage: Integer);
    procedure DrawKnee(const Stage: Integer);
    procedure DrawMakeUpGain(const Stage: Integer);
    procedure DrawAutoGain(const Stage: Integer);
    procedure DrawSoftClip;
    procedure DrawGraph(const Stage: Integer);
  public
    procedure UpdateLowFrequency;
    procedure UpdateOutputGain;
    procedure UpdateHighFrequency;
    procedure UpdateLimit;
    procedure UpdateLowAttack;
    procedure UpdateLowAutoMakeUpGain;
    procedure UpdateLowKnee;
    procedure UpdateLowMakeUp;
    procedure UpdateLowRatio;
    procedure UpdateLowRelease;
    procedure UpdateLowThreshold;
    procedure UpdateMidAttack;
    procedure UpdateMidAutoMakeUpGain;
    procedure UpdateMidKnee;
    procedure UpdateMidMakeUp;
    procedure UpdateMidRatio;
    procedure UpdateMidRelease;
    procedure UpdateMidThreshold;
    procedure UpdateHighAttack;
    procedure UpdateHighAutoMakeUpGain;
    procedure UpdateHighKnee;
    procedure UpdateHighMakeUp;
    procedure UpdateHighRatio;
    procedure UpdateHighRelease;
    procedure UpdateHighThreshold;
  end;

var
  GBG           : TVerticallyStitchedBitmap32;
  GKnob         : TVerticallyStitchedBitmap32;
  GSoftClip     : TVerticallyStitchedBitmap32;
  GAutoGain     : TVerticallyStitchedBitmap32;
  GRectStage    : Array [0..2, 0..7] of TRect;
  GRectTop      : Array [0..3] of TRect;

implementation

uses
  Math, GR32_Backends, SmoothMultibandCompressorDM, DAV_VSTModuleWithPrograms;

{$R *.DFM}

procedure TFmSmoothMultibandCompressor.FormCreate(Sender: TObject);
var
  tmp   : TPNGGraphic;
  RS    : TResourceStream;
  Stage : Integer;
  Param : Integer;
begin
 FTimer := TTimer.Create(Self);
 with FTimer do
  begin
   Interval := 28;
   OnTimer := TimerTimer;
  end;

 ControlStyle    := ControlStyle + [csOpaque];
 FBackground[1]  := TBitmap32.Create;
 FBackground[0]  := TBitmap32.Create;
 FBackground[0].MasterAlpha := $20;
 FBackground[0].DrawMode := dmBlend;
 FAnimateFrame := 0;

 if not Assigned(GBG) then
  begin
   tmp := TPNGGraphic.Create;
   try
    RS  := TResourceStream.Create(hInstance, 'LPMCBackground', 'PNG');
    try
     tmp.LoadFromStream(RS);
     GBG := TVerticallyStitchedBitmap32.Create;
     GBG.Assign(tmp);
    finally
     RS.Free;
    end;
   finally
    tmp.Free;
   end;
  end;

 if not Assigned(GKnob) then
  begin
   tmp := TPNGGraphic.Create;
   try
    RS  := TResourceStream.Create(hInstance, 'LPMCKnob', 'PNG');
    try
     tmp.LoadFromStream(RS);
     GKnob := TVerticallyStitchedBitmap32.Create;
     GKnob.DrawMode := dmBlend;
     GKnob.NumGlyphs := 64;
     GKnob.Assign(tmp);
//     RestitchPNG2Bitmap32(tmp, GKnob);
     GKnob.Font.Assign(tmp.Canvas.Font);
    finally
     RS.Free;
    end;
   finally
    tmp.Free;
   end;

   // Calculate Rects
   GRectTop[0].TopLeft := Point(416, 25);
   GRectTop[1].TopLeft := Point(520, 25);
   GRectTop[2].TopLeft := Point(628, 25);

   GRectTop[0].Right  := GRectTop[0].Left + GKnob.RealWidth;
   GRectTop[0].Bottom := GRectTop[0].Top + GKnob.RealHeight;
   GRectTop[1].Right  := GRectTop[1].Left + GKnob.RealWidth;
   GRectTop[1].Bottom := GRectTop[1].Top + GKnob.RealHeight;
   GRectTop[2].Right  := GRectTop[2].Left + GKnob.RealWidth;
   GRectTop[2].Bottom := GRectTop[2].Top + GKnob.RealHeight;

   GRectStage[0, 0].TopLeft := Point(144, 124);
   GRectStage[0, 1].TopLeft := Point(220, 124);
   GRectStage[0, 2].TopLeft := Point(313, 124);
   GRectStage[0, 3].TopLeft := Point(402, 124);
   GRectStage[0, 4].TopLeft := Point(480, 124);
   GRectStage[0, 5].TopLeft := Point(558, 124);

   GRectStage[1, 0].TopLeft := Point(144, 230);
   GRectStage[1, 1].TopLeft := Point(220, 230);
   GRectStage[1, 2].TopLeft := Point(313, 230);
   GRectStage[1, 3].TopLeft := Point(402, 230);
   GRectStage[1, 4].TopLeft := Point(480, 230);
   GRectStage[1, 5].TopLeft := Point(558, 230);

   GRectStage[2, 0].TopLeft := Point(142, 332);
   GRectStage[2, 1].TopLeft := Point(220, 332);
   GRectStage[2, 2].TopLeft := Point(313, 332);
   GRectStage[2, 3].TopLeft := Point(402, 332);
   GRectStage[2, 4].TopLeft := Point(480, 332);
   GRectStage[2, 5].TopLeft := Point(558, 332);

   for Stage := 0 to Length(GRectStage) - 1 do
    for Param := 0 to Length(GRectStage[Stage]) - 3 do
     begin
      GRectStage[Stage, Param].Right  := GRectStage[Stage, Param].Left + GKnob.RealWidth;
      GRectStage[Stage, Param].Bottom := GRectStage[Stage, Param].Top + GKnob.RealHeight;
     end;

   GRectStage[0, 6].TopLeft := Point(618, 107);
   GRectStage[1, 6].TopLeft := Point(618, 217);
   GRectStage[2, 6].TopLeft := Point(618, 317);
  end;

  if not Assigned(GSoftClip) then
   begin
    tmp := TPNGGraphic.Create;
    try
     RS  := TResourceStream.Create(hInstance, 'LPMCSoftClip', 'PNG');
     try
      tmp.LoadFromStream(RS);
      GSoftClip := TVerticallyStitchedBitmap32.Create;
      GSoftClip.DrawMode := dmBlend;
      GSoftClip.NumGlyphs := 2;
      RestitchPNG2Bitmap32(tmp, GSoftClip);
      GSoftClip.Font.Assign(tmp.Canvas.Font);
     finally
      RS.Free;
     end;
    finally
     tmp.Free;
    end;

    // Calculate Rects
    GRectTop[3].TopLeft := Point(713, 21);
    GRectTop[3].Right   := GRectTop[3].Left + GSoftClip.RealWidth;
    GRectTop[3].Bottom  := GRectTop[3].Top + GSoftClip.RealHeight;
   end;

  if not Assigned(GAutoGain) then
   begin
    tmp := TPNGGraphic.Create;
    try
     RS  := TResourceStream.Create(hInstance, 'LPMCAutoGain', 'PNG');
     try
      tmp.LoadFromStream(RS);
      GAutoGain := TVerticallyStitchedBitmap32.Create;
      GAutoGain.DrawMode := dmBlend;
      GAutoGain.NumGlyphs := 2;
      RestitchPNG2Bitmap32(tmp, GAutoGain);
      GAutoGain.Font.Assign(tmp.Canvas.Font);
     finally
      RS.Free;
     end;
    finally
     tmp.Free;
    end;

    // Calculate Rects
    GRectStage[0, 7].TopLeft := Point(717, 125);
    GRectStage[1, 7].TopLeft := Point(717, 230);
    GRectStage[2, 7].TopLeft := Point(717, 334);

    GRectStage[0, 7].Right   := GRectStage[0, 7].Left + GAutoGain.RealWidth;
    GRectStage[0, 7].Bottom  := GRectStage[0, 7].Top + GAutoGain.RealHeight;
    GRectStage[1, 7].Right   := GRectStage[1, 7].Left + GAutoGain.RealWidth;
    GRectStage[1, 7].Bottom  := GRectStage[1, 7].Top + GAutoGain.RealHeight;
    GRectStage[2, 7].Right   := GRectStage[2, 7].Left + GAutoGain.RealWidth;
    GRectStage[2, 7].Bottom  := GRectStage[2, 7].Top + GAutoGain.RealHeight;
   end;

 FEditValue := edNone;



end;

procedure TFmSmoothMultibandCompressor.FormDestroy(Sender: TObject);
begin
 FreeAndNil(FTimer);
 FreeAndNil(FBackground[0]);
 FreeAndNil(FBackground[1]);
end;

procedure TFmSmoothMultibandCompressor.FormMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  pml   : Integer;
  Pt    : TPoint;
  Stage : Integer;
begin
 with TSmoothMultibandCompressorDataModule(Owner) do
  begin
   if Button = mbLeft
    then MidiLearnParameter := -1;
   pml := MidiLearnParameter;
   Pt := Point(X, Y);
   SetFocus;

   if PtInRect(GRectTop[0], Pt) then
    if ssShift in Shift
     then Parameter[0] := 500
     else FEditValue := edLow
    else
   if PtInRect(GRectTop[1], Pt) then
    if ssShift in Shift
     then Parameter[1] := 2500
     else FEditValue := edHigh
    else
   if PtInRect(GRectTop[2], Pt) then
    if ssShift in Shift
     then Parameter[2] := 0
     else FEditValue := edVolume
    else
   if PtInRect(GRectTop[3], Pt)  then
    begin
     Parameter[2] := 1 - round(Parameter[2]);
     FAnimateFrame := -$20;
    end else
   for Stage := 0 to Length(GRectStage) - 1 do
    begin
     FStage := Stage;
     if PtInRect(GRectStage[FStage, 0], Pt)  then
      begin
       if ssShift in Shift
        then Parameter[5 + Stage * 6] := -10
        else FEditValue := edThreshold;
       break;
      end else
     if PtInRect(GRectStage[FStage, 1], Pt)  then
      begin
       if ssShift in Shift
        then Parameter[6 + Stage * 6] := 4
        else FEditValue := edRatio;
       break;
      end else
     if PtInRect(GRectStage[FStage, 2], Pt)  then
      begin
       if ssShift in Shift
        then Parameter[3 + Stage * 6] := 5
        else FEditValue := edAttack;
       break;
      end else
     if PtInRect(GRectStage[FStage, 3], Pt)  then
      begin
       if ssShift in Shift
        then Parameter[4 + Stage * 6] := 50
        else FEditValue := edRelease;
       break;
      end else
     if PtInRect(GRectStage[FStage, 4], Pt)  then
      begin
       if ssShift in Shift
        then Parameter[7 + Stage * 6] := 1
        else FEditValue := edKnee;
       break;
      end else
     if PtInRect(GRectStage[FStage, 5], Pt)  then
      begin
       if ssShift in Shift
        then Parameter[8 + Stage * 6] := 3
        else FEditValue := edMakeUpGain;
       break;
      end;
    end;
(*
   if PtInRect(FRectSwitch[0], Pt, GRatios[0]) then
    begin
     if y <  55 then Parameter[4] := 3 else
     if y <  82 then Parameter[4] := 2 else
     if y < 108 then Parameter[4] := 1
      else Parameter[4] := 0;
     FBackground[0].DrawMode := dmOpaque;
     FBackground[0].DrawTo(FBackground[1]);
     FBackground[0].DrawMode := dmBlend;
     FAnimateValue := avRatio;
    end else
   if PtInRect(FRectSwitch[1], Pt, GRatios[0]) then
    begin
     pml := Round(Parameter[5]);
     FBackground[0].DrawMode := dmOpaque;
     FBackground[0].DrawTo(FBackground[1]);
     FBackground[0].DrawMode := dmBlend;
     FAnimateValue := avMeter;
     if y <  55 then pml := (pml and $FFFFFFFE) else
     if y <  82 then pml := 1 or (pml and $FFFFFFFE) else
     if y < 108
      then ChangeSkin
      else pml := ((not pml) and $2) or (pml and $FFFFFFFD);
     Parameter[5] := pml;
    end else
   if PtInRect(FRectAbout, Pt) then
    begin
     FBackground[0].DrawMode := dmOpaque;
     FBackground[0].DrawTo(FBackground[1]);
     FBackground[0].DrawMode := dmBlend;
     FBackground[0].Draw(0, 0, GAbout);
     FAnimateFrame := 0;
     FAnimateValue := avAbout;
     OnMouseDown := AboutMouseDown;
     OnMouseMove := nil;
    end;
*)
   ;

   if FEditValue in [edLow, edHigh, edVolume, edAttack, edRelease, edThreshold,
     edRatio, edKnee, edMakeUpGain]
    then Screen.Cursor := crNone;
 end;
(*
 FBackground[0].RenderText(x, y, IntToStr(X) +', ' + IntToStr(Y), 0, clWhite32);
 FBackground[1].RenderText(x, y, IntToStr(X) +', ' + IntToStr(Y), 0, clWhite32);
 Invalidate;
*)
end;

procedure TFmSmoothMultibandCompressor.FormMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  s  : Single;
  Pt : TPoint;
begin
 with TSmoothMultibandCompressorDataModule(Owner) do
  case FEditValue of
   edLow: with GRectTop[0] do
             begin
              Pt := Point((3 * Left + Right) div 4, (3 * Top + Bottom) div 4);
              s := Parameter[0];
              if ssCtrl in Shift
               then s := s * Power(2, 5E-3 * (Pt.Y - Y))
               else s := s * Power(2, 0.01 * (Pt.Y - Y)) * Power(2, 5E-3 * (X - Pt.X));
              with ParameterProperties[0]
               do Parameter[0] := Limit(s, Min, Max);
             end;
   edHigh: with GRectTop[1] do
              begin
               Pt := Point((3 * Left + Right) div 4, (3 * Top + Bottom) div 4);
               s := Parameter[1];
               if ssCtrl in Shift
                then s := s * Power(2, 5E-3 * (Pt.Y - Y))
                else s := s * Power(2, 0.01 * (Pt.Y - Y)) * Power(2, 5E-3 * (X - Pt.X));
               with ParameterProperties[1]
                do Parameter[1] := Limit(s, Min, Max);
              end;
   edVolume: with GRectTop[2] do
              begin
               Pt := Point((3 * Left + Right) div 4, (3 * Top + Bottom) div 4);
               s := Parameter[21];
               if ssCtrl in Shift
                then s := s + 0.002 * (Pt.Y - Y)
                else s := s + 0.016 * (Pt.Y - Y) - 0.002 * (Pt.X - X);
               with ParameterProperties[21]
                do Parameter[21] := Limit(s, Min, Max);
              end;
   edThreshold: with GRectStage[FStage, 0] do
                 begin
                  Pt := Point((3 * Left + Right) div 4, (3 * Top + Bottom) div 4);
                  s := Parameter[5 + FStage * 6];
                  if ssCtrl in Shift
                   then s := s + 0.01 * (Pt.Y - Y)
                   else s := s + 0.05 * (Pt.Y - Y) - 0.01 * (Pt.X - X);
                  with ParameterProperties[5 + FStage * 6]
                   do Parameter[5 + FStage * 6] := Limit(s, Min, Max);
                 end;
   edRatio: with GRectStage[FStage, 1] do
             begin
              Pt := Point((3 * Left + Right) div 4, (3 * Top + Bottom) div 4);
              s := Parameter[6 + FStage * 6];
              if ssCtrl in Shift
               then s := s * Power(2, 2E-3 * (Pt.Y - Y))
               else s := s * Power(2, 0.006 * (Pt.Y - Y)) * Power(2, 2E-3 * (Pt.X - X));
              with ParameterProperties[6 + FStage * 6]
               do Parameter[6 + FStage * 6] := Limit(s, Min, Max);
             end;
   edAttack: with GRectStage[FStage, 2] do
              begin
               Pt := Point((3 * Left + Right) div 4, (3 * Top + Bottom) div 4);
               s := Parameter[3 + FStage * 6];
               if ssCtrl in Shift
                then s := s * Power(2, 5E-3 * (Pt.Y - Y))
                else s := s * Power(2, 0.01 * (Pt.Y - Y)) * Power(2, 5E-3 * (Pt.X - X));
               with ParameterProperties[3 + FStage * 6]
                do Parameter[3 + FStage * 6] := Limit(s, Min, Max);
              end;
   edRelease: with GRectStage[FStage, 3] do
                begin
                 Pt := Point((3 * Left + Right) div 4, (3 * Top + Bottom) div 4);
                 s := Parameter[4 + FStage * 6];
                 if ssCtrl in Shift
                  then s := s * Power(2, 5E-3 * (Pt.Y - Y))
                  else s := s * Power(2, 0.01 * (Pt.Y - Y)) * Power(2, 5E-3 * (Pt.X - X));
                 with ParameterProperties[4 + FStage * 6]
                  do Parameter[4 + FStage * 6] := Limit(s, Min, Max);
                end;
   edKnee: with GRectStage[FStage, 4] do
              begin
               Pt := Point((3 * Left + Right) div 4, (3 * Top + Bottom) div 4);
               s := Parameter[7 + FStage * 6];
               if ssCtrl in Shift
                then s := s + 0.002 * (Pt.Y - Y)
                else s := s + 0.016 * (Pt.Y - Y) - 0.002 * (Pt.X - X);
               with ParameterProperties[7 + FStage * 6]
                do Parameter[7 + FStage * 6] := Limit(s, Min, Max);
              end;
   edMakeUpGain: with GRectStage[FStage, 5] do
                begin
                 Pt := Point((3 * Left + Right) div 4, (3 * Top + Bottom) div 4);
                 s := Parameter[8 + FStage * 6];
                 if ssCtrl in Shift
                  then s := s + 0.01 * (Pt.Y - Y)
                  else s := s + 0.05 * (Pt.Y - Y) - 0.01 * (Pt.X - X);
                 with ParameterProperties[8 + FStage * 6]
                  do Parameter[8 + FStage * 6] := Limit(s, Min, Max);
                end;
  end;

 if FEditValue in [edLow, edHigh, edVolume, edThreshold, edRatio, edAttack,
   edRelease, edKnee, edMakeUpGain, edVolume] then
  begin
   Pt := ClientToScreen(Pt);
   OnMouseMove := nil;
   SetCursorPos(Pt.X, Pt.Y);
   FAnimateFrame := -$10;
   Invalidate;
   Sleep(5);
   Application.ProcessMessages;
   OnMouseMove := FormMouseMove;
  end;

end;

procedure TFmSmoothMultibandCompressor.FormMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 Screen.Cursor := crDefault;
 FEditValue := edNone;
end;

procedure TFmSmoothMultibandCompressor.FormMouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
begin
 MousePos := ScreenToClient(MousePos);
 if PtInRect(GRectTop[0], MousePos) then FEditValue := edLow else
 if PtInRect(GRectTop[1], MousePos) then FEditValue := edHigh else
 if PtInRect(GRectTop[2], MousePos) then FEditValue := edVolume;

 if WheelDelta > 0
  then FormMouseMove(Sender, Shift + [ssAlt, ssLeft], MousePos.X, MousePos.Y - 10)
  else FormMouseMove(Sender, Shift + [ssAlt, ssLeft], MousePos.X, MousePos.Y + 10);
 FEditValue := edNone;
end;

procedure TFmSmoothMultibandCompressor.FormPaint(Sender: TObject);
begin
 if FAnimateFrame >= 0
  then FBackground[0].DrawTo(Canvas.Handle, 0, 0)
  else
   begin
    FBackground[0].MasterAlpha := $80 + 2 * FAnimateFrame;
    FBackground[0].DrawTo(FBackground[1]);
    FBackground[1].DrawTo(Canvas.Handle, 0, 0);
    inc(FAnimateFrame);
   end;
end;

procedure TFmSmoothMultibandCompressor.FormResize(Sender: TObject);
begin
 with FBackground[0] do
  begin
   BeginUpdate;
   SetSize(ClientWidth, ClientHeight);
   DrawAll;
   EndUpdate;
  end;
 FBackground[1].Assign(FBackground[0]);
end;

procedure TFmSmoothMultibandCompressor.FormShow(Sender: TObject);
begin
 UpdateLowAttack;
 UpdateLowAutoMakeUpGain;
 UpdateLowKnee;
 UpdateLowMakeUp;
 UpdateLowRatio;
 UpdateLowRelease;
 UpdateLowThreshold;
 UpdateMidAttack;
 UpdateMidAutoMakeUpGain;
 UpdateMidKnee;
 UpdateMidMakeUp;
 UpdateMidRatio;
 UpdateMidRelease;
 UpdateMidThreshold;
 UpdateHighAttack;
 UpdateHighAutoMakeUpGain;
 UpdateHighKnee;
 UpdateHighMakeUp;
 UpdateHighRatio;
 UpdateHighRelease;
 UpdateHighThreshold;
 UpdateLimit;
end;

procedure TFmSmoothMultibandCompressor.TimerTimer(Sender: TObject);
begin
 Invalidate;
end;

procedure TFmSmoothMultibandCompressor.RestitchPNG2Bitmap32(const PNG : TPNGGraphic; const BMP : TVerticallyStitchedBitmap32);
var i : Integer;
begin
 BMP.Width  := PNG.Width div BMP.numGlyphs;
 BMP.Height := PNG.Height * BMP.numGlyphs;
 PNG.Canvas.Lock;
 try
  for i := 0 to BMP.numGlyphs - 1
   do BitBlt(BMP.Handle, 0, i * PNG.Height, PNG.Width, PNG.Height, PNG.Canvas.Handle, i * BMP.Width, 0, SRCCOPY);
 finally
  PNG.Canvas.UnLock;
 end;
end;

procedure TFmSmoothMultibandCompressor.DrawAll;
var
  Stage: Integer;
begin
 with FBackground[0], TSmoothMultibandCompressorDataModule(Owner) do
  try
   BeginUpdate;
   Draw(0, 0, GBG);
   DrawLowFrequency;
   DrawHighFrequency;
   DrawOutputGain;
   DrawSoftClip;
   for Stage := 0 to 2 do
    begin
     DrawThreshold(Stage);
     DrawRatio(Stage);
     DrawAttack(Stage);
     DrawRelease(Stage);
     DrawKnee(Stage);
     DrawMakeUpGain(Stage);
     DrawAutoGain(Stage);
     DrawGraph(Stage);
    end;
  finally
   EndUpdate;
  end;
end;

procedure TFmSmoothMultibandCompressor.DrawLowFrequency;
var
  i : Integer;
  r : TRect;
  t : string;
begin
 with FBackground[0], TSmoothMultibandCompressorDataModule(Owner) do
  try
   BeginUpdate;
   with ParameterProperties[0]
    do i := Round((GKnob.NumGlyphs - 1) * Parameter2VSTParameter(Parameter[0]));
   if i < 0 then i := 0
    else if i >= GKnob.NumGlyphs then i := GKnob.NumGlyphs - 1;
   r := GRectTop[0];
   Draw(r, r, GBG);
   Draw(r, Rect(0, i * GKnob.RealHeight, GKnob.RealWidth, (i + 1) * GKnob.RealHeight), GKnob);
   r.Bottom := r.Top;
   r.Top    := r.Bottom - 17;
   r.Left   := (3 * r.Left + r.Right) div 4;
   r.Right  := r.Left + 32;
   r.Left   := r.Left - 32;
   Draw(r, r, GBG);
   t := ParameterDisplay[0] + ' ' + ParameterLabel[0];
   RenderText((r.Left + r.Right - TextWidth(t)) div 2, R.Top, t, 0, $FFC1CAD1);
  finally
   EndUpdate;
  end;
end;

procedure TFmSmoothMultibandCompressor.DrawHighFrequency;
var
  i : Integer;
  r : TRect;
  t : string;
begin
 with FBackground[0], TSmoothMultibandCompressorDataModule(Owner) do
  try
   BeginUpdate;
   with ParameterProperties[1]
    do i := Round((GKnob.NumGlyphs - 1) * Parameter2VSTParameter(Parameter[1]));
   if i < 0 then i := 0
    else if i >= GKnob.NumGlyphs then i := GKnob.NumGlyphs - 1;
   r := GRectTop[1];
   Draw(r, r, GBG);
   Draw(r, Rect(0, i * GKnob.RealHeight, GKnob.RealWidth, (i + 1) * GKnob.RealHeight), GKnob);
   r.Bottom := r.Top;
   r.Top    := r.Bottom - 17;
   r.Left   := (3 * r.Left + r.Right) div 4;
   r.Right  := r.Left + 32;
   r.Left   := r.Left - 32;
   Draw(r, r, GBG);
   t := ParameterDisplay[1] + ' ' + ParameterLabel[1];
   RenderText((r.Left + r.Right - TextWidth(t)) div 2, R.Top, t, 4, $FFC1CAD1);
  finally
   EndUpdate;
  end;
end;

procedure TFmSmoothMultibandCompressor.DrawOutputGain;
var
  i : Integer;
  r : TRect;
  t : string;
begin
 with FBackground[0], TSmoothMultibandCompressorDataModule(Owner) do
  try
   BeginUpdate;
   with ParameterProperties[21]
    do i := Round((GKnob.NumGlyphs - 1) * Parameter2VSTParameter(Parameter[21]));
   i := Limit(i, 0, GKnob.NumGlyphs - 1);
   r := GRectTop[2];
   Draw(r, r, GBG);
   Draw(r, Rect(0, i * GKnob.RealHeight, GKnob.RealWidth, (i + 1) * GKnob.RealHeight), GKnob);
   r.Bottom := r.Top;
   r.Top    := r.Bottom - 17;
   r.Left   := (3 * r.Left + r.Right) div 4;
   r.Right  := r.Left + 32;
   r.Left   := r.Left - 32;
   Draw(r, r, GBG);
   t := ParameterDisplay[21] + ' ' + ParameterLabel[21];
   RenderText((r.Left + r.Right - TextWidth(t)) div 2, R.Top, t, 4, $FFC1CAD1);
  finally
   EndUpdate;
  end;
end;

procedure TFmSmoothMultibandCompressor.DrawSoftClip;
var
  i : Integer;
  r : TRect;
begin
 with FBackground[0], TSmoothMultibandCompressorDataModule(Owner) do
  try
   BeginUpdate;
   with ParameterProperties[2]
    do i := Round((GSoftClip.NumGlyphs - 1) * Parameter2VSTParameter(Parameter[2]));
   if i < 0 then i := 0
    else if i >= GSoftClip.NumGlyphs then i := GSoftClip.NumGlyphs - 1;
   r := GRectTop[3];
   Draw(r, r, GBG);
   Draw(r, Rect(0, i * GSoftClip.RealHeight, GSoftClip.RealWidth, (i + 1) * GSoftClip.RealHeight), GSoftClip);
  finally
   EndUpdate;
  end;
end;

procedure TFmSmoothMultibandCompressor.DrawThreshold(const Stage: Integer);
var
  i : Integer;
  r : TRect;
  t : string;
begin
 with FBackground[0], TSmoothMultibandCompressorDataModule(Owner) do
  try
   BeginUpdate;
   with ParameterProperties[5 + Stage * 6]
    do i := Round((GKnob.NumGlyphs - 1) * Parameter2VSTParameter(Parameter[5 + Stage * 6]));
   if i < 0 then i := 0
    else if i >= GKnob.NumGlyphs then i := GKnob.NumGlyphs - 1;
   r := GRectStage[Stage, 0];
   Draw(r, r, GBG);
   Draw(r, Rect(0, i * GKnob.RealHeight, GKnob.RealWidth, (i + 1) * GKnob.RealHeight), GKnob);
   r.Bottom := r.Top;
   r.Top    := r.Bottom - 19;
   r.Left   := (3 * r.Left + r.Right) div 4;
   r.Right  := r.Left + 32;
   r.Left   := r.Left - 32;
   Draw(r, r, GBG);
   t := ParameterDisplay[5 + Stage * 6] + ' ' + ParameterLabel[5 + Stage * 6];
   RenderText((r.Left + r.Right - TextWidth(t)) div 2, R.Top, t, 0, $FF4C4847);
  finally
   EndUpdate;
  end;
end;

procedure TFmSmoothMultibandCompressor.DrawRatio(const Stage: Integer);
var
  i : Integer;
  r : TRect;
  t : string;
begin
 with FBackground[0], TSmoothMultibandCompressorDataModule(Owner) do
  try
   BeginUpdate;
   with ParameterProperties[6 + Stage * 6]
    do i := Round((GKnob.NumGlyphs - 1) * Parameter2VSTParameter(Parameter[6 + Stage * 6]));
   if i < 0 then i := 0
    else if i >= GKnob.NumGlyphs then i := GKnob.NumGlyphs - 1;
   r := GRectStage[Stage, 1];
   Draw(r, r, GBG);
   Draw(r, Rect(0, i * GKnob.RealHeight, GKnob.RealWidth, (i + 1) * GKnob.RealHeight), GKnob);
   r.Bottom := r.Top;
   r.Top    := r.Bottom - 19;
   r.Left   := (3 * r.Left + r.Right) div 4;
   r.Right  := r.Left + 32;
   r.Left   := r.Left - 32;
   Draw(r, r, GBG);
   t := ParameterDisplay[6 + Stage * 6] + ' : 1';
   RenderText((r.Left + r.Right - TextWidth(t)) div 2, R.Top, t, 1, $FF4C4847);
  finally
   EndUpdate;
  end;
end;

procedure TFmSmoothMultibandCompressor.DrawAttack(const Stage: Integer);
var
  i : Integer;
  r : TRect;
  t : string;
begin
 with FBackground[0], TSmoothMultibandCompressorDataModule(Owner) do
  try
   BeginUpdate;
   with ParameterProperties[3 + Stage * 6]
    do i := Round((GKnob.NumGlyphs - 1) * Parameter2VSTParameter(Parameter[3 + Stage * 6]));
   if i < 0 then i := 0
    else if i >= GKnob.NumGlyphs then i := GKnob.NumGlyphs - 1;
   r := GRectStage[Stage, 2];
   Draw(r, r, GBG);
   Draw(r, Rect(0, i * GKnob.RealHeight, GKnob.RealWidth, (i + 1) * GKnob.RealHeight), GKnob);
   r.Bottom := r.Top;
   r.Top    := r.Bottom - 19;
   r.Left   := (3 * r.Left + r.Right) div 4;
   r.Right  := r.Left + 32;
   r.Left   := r.Left - 32;
   Draw(r, r, GBG);
   t := ParameterDisplay[3 + Stage * 6] + ' ' + ParameterLabel[3 + Stage * 6];
   RenderText((r.Left + r.Right - TextWidth(t)) div 2, R.Top, t, 2, $FF4C4847);
  finally
   EndUpdate;
  end;
end;

procedure TFmSmoothMultibandCompressor.DrawRelease(const Stage: Integer);
var
  i : Integer;
  r : TRect;
  t : string;
begin
 with FBackground[0], TSmoothMultibandCompressorDataModule(Owner) do
  try
   BeginUpdate;
   with ParameterProperties[4 + Stage * 6]
    do i := Round((GKnob.NumGlyphs - 1) * Parameter2VSTParameter(Parameter[4 + Stage * 6]));
   if i < 0 then i := 0
    else if i >= GKnob.NumGlyphs then i := GKnob.NumGlyphs - 1;
   r := GRectStage[Stage, 3];
   Draw(r, r, GBG);
   Draw(r, Rect(0, i * GKnob.RealHeight, GKnob.RealWidth, (i + 1) * GKnob.RealHeight), GKnob);
   r.Bottom := r.Top;
   r.Top    := r.Bottom - 19;
   r.Left   := (3 * r.Left + r.Right) div 4;
   r.Right  := r.Left + 32;
   r.Left   := r.Left - 32;
   Draw(r, r, GBG);
   t := ParameterDisplay[4 + Stage * 6] + ' ' + ParameterLabel[4 + Stage * 6];
   RenderText((r.Left + r.Right - TextWidth(t)) div 2, R.Top, t, 3, $FF4C4847);
  finally
   EndUpdate;
  end;
end;

procedure TFmSmoothMultibandCompressor.DrawKnee(const Stage: Integer);
var
  i : Integer;
  r : TRect;
  t : string;
begin
 with FBackground[0], TSmoothMultibandCompressorDataModule(Owner) do
  try
   BeginUpdate;
   with ParameterProperties[7 + Stage * 6]
    do i := Round((GKnob.NumGlyphs - 1) * Parameter2VSTParameter(Parameter[7 + Stage * 6]));
   if i < 0 then i := 0
    else if i >= GKnob.NumGlyphs then i := GKnob.NumGlyphs - 1;
   r := GRectStage[Stage, 4];
   Draw(r, r, GBG);
   Draw(r, Rect(0, i * GKnob.RealHeight, GKnob.RealWidth, (i + 1) * GKnob.RealHeight), GKnob);
   r.Bottom := r.Top;
   r.Top    := r.Bottom - 19;
   r.Left   := (3 * r.Left + r.Right) div 4;
   r.Right  := r.Left + 32;
   r.Left   := r.Left - 32;
   Draw(r, r, GBG);
   t := ParameterDisplay[7 + Stage * 6] + ' ' + ParameterLabel[7 + Stage * 6];
   RenderText((r.Left + r.Right - TextWidth(t)) div 2, R.Top, t, 4, $FF4C4847);
  finally
   EndUpdate;
  end;
end;

procedure TFmSmoothMultibandCompressor.DrawMakeUpGain(const Stage: Integer);
var
  i : Integer;
  r : TRect;
  t : string;
begin
 with FBackground[0], TSmoothMultibandCompressorDataModule(Owner) do
  try
   BeginUpdate;
   with ParameterProperties[8 + Stage * 6]
    do i := Round((GKnob.NumGlyphs - 1) * Parameter2VSTParameter(Parameter[8 + Stage * 6]));
   if i < 0 then i := 0
    else if i >= GKnob.NumGlyphs then i := GKnob.NumGlyphs - 1;
   r := GRectStage[Stage, 5];
   Draw(r, r, GBG);
   Draw(r, Rect(0, i * GKnob.RealHeight, GKnob.RealWidth, (i + 1) * GKnob.RealHeight), GKnob);
   r.Bottom := r.Top;
   r.Top    := r.Bottom - 19;
   r.Left   := (3 * r.Left + r.Right) div 4;
   r.Right  := r.Left + 32;
   r.Left   := r.Left - 32;
   Draw(r, r, GBG);
   t := ParameterDisplay[8 + Stage * 6] + ' ' + ParameterLabel[8 + Stage * 6];
   RenderText((r.Left + r.Right - TextWidth(t)) div 2, R.Top, t, -1, $FF4C4847);
  finally
   EndUpdate;
  end;
end;

procedure TFmSmoothMultibandCompressor.DrawGraph(const Stage: Integer);
begin

end;

procedure TFmSmoothMultibandCompressor.DrawAutoGain(const Stage: Integer);
var
  i : Integer;
  r : TRect;
begin
 with FBackground[0], TSmoothMultibandCompressorDataModule(Owner) do
  try
   BeginUpdate;
   i := random(2);
(*
   with ParameterProperties[2]
    do i := Round((GSoftClip.NumGlyphs - 1) * Parameter2VSTParameter(Parameter[2]));
*)
   if i < 0 then i := 0
    else if i >= GAutoGain.NumGlyphs then i := GAutoGain.NumGlyphs - 1;
   r := GRectStage[Stage, 7];
   Draw(r, r, GBG);
   Draw(r, Rect(0, i * GAutoGain.RealHeight, GAutoGain.RealWidth, (i + 1) * GAutoGain.RealHeight), GAutoGain);
  finally
   EndUpdate;
  end;
end;

////////////
// Update //
////////////

procedure TFmSmoothMultibandCompressor.UpdateLowFrequency;
begin
 DrawLowFrequency;
end;

procedure TFmSmoothMultibandCompressor.UpdateHighFrequency;
begin
 DrawHighFrequency;
end;

procedure TFmSmoothMultibandCompressor.UpdateLowAttack;
begin
 DrawAttack(0);
end;

procedure TFmSmoothMultibandCompressor.UpdateLowRelease;
begin
 DrawRelease(0);
end;

procedure TFmSmoothMultibandCompressor.UpdateLowThreshold;
begin
 DrawThreshold(0);
end;

procedure TFmSmoothMultibandCompressor.UpdateLowRatio;
begin
 DrawRatio(0);
end;

procedure TFmSmoothMultibandCompressor.UpdateLowKnee;
begin
 DrawKnee(0);
end;

procedure TFmSmoothMultibandCompressor.UpdateLowMakeUp;
begin
 DrawMakeUpGain(0);
end;

procedure TFmSmoothMultibandCompressor.UpdateLowAutoMakeUpGain;
begin
 // todo
end;

procedure TFmSmoothMultibandCompressor.UpdateMidAttack;
begin
 DrawAttack(1);
end;

procedure TFmSmoothMultibandCompressor.UpdateMidRelease;
begin
 DrawRelease(1);
end;

procedure TFmSmoothMultibandCompressor.UpdateMidThreshold;
begin
 DrawThreshold(1);
end;

procedure TFmSmoothMultibandCompressor.UpdateOutputGain;
begin
 DrawOutputGain;
end;

procedure TFmSmoothMultibandCompressor.UpdateMidRatio;
begin
 DrawRatio(1);
end;

procedure TFmSmoothMultibandCompressor.UpdateMidKnee;
begin
 DrawKnee(1);
end;

procedure TFmSmoothMultibandCompressor.UpdateMidMakeUp;
begin
 DrawMakeUpGain(1);
end;

procedure TFmSmoothMultibandCompressor.UpdateMidAutoMakeUpGain;
begin
 // todo
end;

procedure TFmSmoothMultibandCompressor.UpdateHighAttack;
begin
 DrawAttack(2);
end;

procedure TFmSmoothMultibandCompressor.UpdateHighRelease;
begin
 DrawRelease(2);
end;

procedure TFmSmoothMultibandCompressor.UpdateHighThreshold;
begin
 DrawThreshold(2);
end;

procedure TFmSmoothMultibandCompressor.UpdateHighRatio;
begin
 DrawRatio(2);
end;

procedure TFmSmoothMultibandCompressor.UpdateHighKnee;
begin
 DrawKnee(2);
end;

procedure TFmSmoothMultibandCompressor.UpdateHighMakeUp;
begin
 DrawMakeUpGain(2);
end;

procedure TFmSmoothMultibandCompressor.UpdateHighAutoMakeUpGain;
begin
 // todo
end;

procedure TFmSmoothMultibandCompressor.UpdateLimit;
begin
 DrawSoftClip;
end;

{ TVerticallyStitchedBitmap32 }

constructor TVerticallyStitchedBitmap32.Create;
begin
 inherited;
 FNumGlyphs := 1;
end;

function TVerticallyStitchedBitmap32.GetRealHeight: Integer;
begin
 result := FHeight div FNumGlyphs;
end;

procedure TVerticallyStitchedBitmap32.SetNumGlyphs(const Value: Integer);
begin
 FNumGlyphs := Value;
end;

end.
