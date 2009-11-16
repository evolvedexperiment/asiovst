unit DAV_GuiEQGraph;

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

{$I ..\DAV_Compiler.inc}

uses
  {$IFDEF FPC} LCLIntf, LMessages, {$ELSE} Windows, Messages, {$ENDIF}
  Classes, Graphics, Forms, Types, SysUtils, Controls, DAV_GuiCommon,
  DAV_GuiBaseControl;

type
  TGuiEQGraph = class;
  TXAxisLabelStyle = (xlsNone, xlsTop, xlsBottom);
  TYAxisLabelStyle = (ylsNone, ylsLeft, ylsRight);

  TGetFilterGainEvent = function(Sender: TObject; const Frequency: Single): Single of object;

  TCustomGuiEQGraph = class;

  TCustomGuiEQGraphAxis = class(TPersistent)
  protected
    FOwner : TCustomGuiEQGraph;
    procedure Changed;
  public
    constructor Create(AOwner: TCustomGuiEQGraph); virtual;
  end;

  TCustomGuiEQGraphXAxis = class(TCustomGuiEQGraphAxis)
  private
    FLabelStyle     : TXAxisLabelStyle;
    FUpperFrequency : Single;
    FLowerFrequency : Single;
    procedure SetLabelStyle(const Value: TXAxisLabelStyle);
    procedure SetLowerFrequency(const Value: Single);
    procedure SetUpperFrequency(const Value: Single);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure LabelStyleChanged; virtual;
    procedure LowerFrequencyChanged; virtual;
    procedure UpperFrequencyChanged; virtual;
  public
    constructor Create(AOwner: TCustomGuiEQGraph); override;

    property UpperFrequency: Single read FUpperFrequency write SetUpperFrequency;
    property LowerFrequency: Single read FLowerFrequency write SetLowerFrequency;
    property LabelStyle: TXAxisLabelStyle read FLabelStyle write SetLabelStyle default xlsNone;
  end;

  TGuiEQGraphXAxis = class(TCustomGuiEQGraphXAxis)
  published
    property LabelStyle;
  end;


  TCustomGuiEQGraphYAxis = class(TCustomGuiEQGraphAxis)
  private
    FUpperLevel : Single;
    FLowerLevel : Single;
    FLabelStyle : TYAxisLabelStyle;
    procedure SetUpperLevel(const Value: Single);
    procedure SetLowerLevel(const Value: Single);
    procedure SetLabelStyle(const Value: TYAxisLabelStyle);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure LabelStyleChanged; virtual;
    procedure UpperLevelChanged; virtual;
    procedure LowerLevelChanged; virtual;
  public
    constructor Create(AOwner: TCustomGuiEQGraph); override;

    property UpperLevel: Single read FUpperLevel write SetUpperLevel;
    property LowerLevel: Single read FLowerLevel write SetLowerLevel;
    property LabelStyle: TYAxisLabelStyle read FLabelStyle write SetLabelStyle default ylsNone;
  end;

  TGuiEQGraphYAxis = class(TCustomGuiEQGraphYAxis)
  published
    property LabelStyle;
    property LowerLevel;
    property UpperLevel;
  end;


  TCustomGuiEQGraph = class(TCustomControl)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TGuiEQGraph = class(TCustomGuiEQGraph)
  private
    FAutoColor               : Boolean;
    FBuffer                  : TBitmap;
    FChartBuffer             : TBitmap;
    FChartColor              : TColor;
    FBorderRadius            : Integer;
    FBorderWidth             : Integer;
    FGraphColorDark          : TColor;
    FGraphColorLight         : TColor;
    FChartBufferNeedsRepaint : Boolean;
    FBufferNeedsRepaint      : Boolean;
    FAutoUpdate              : Boolean;
    FAntiAlias               : TGuiAntiAlias;
    FOSFactor                : Integer;
    FTransparent             : Boolean;

    FYAxis                   : TGuiEQGraphYAxis;
    FXAxis                   : TGuiEQGraphXAxis;

    FOnPaint                 : TNotifyEvent;
    FOnGetFilterGain         : TGetFilterGainEvent;

    procedure SetAutoColor(const Value: Boolean);
    procedure SetChartColor(const Value: TColor);
    procedure SetBorderRadius(const Value: Integer);
    procedure SetGraphColorDark(const Value: TColor);
    procedure SetGraphColorLight(const Value: TColor);
    procedure SetBorderWidth(const Value: Integer);
    procedure SetYAxis(const Value: TGuiEQGraphYAxis);
    procedure SetXAxis(const Value: TGuiEQGraphXAxis);
    procedure SetAntiAlias(const Value: TGuiAntiAlias);
    procedure SetTransparent(const Value: Boolean);
    procedure TransparentChanged;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure Paint; override;
    procedure Resize; override;

    procedure AntiAliasChanged;
    procedure RenderBuffer;
    procedure DownsampleBitmap(Bitmap: TBitmap);
    procedure UpsampleBitmap(Bitmap: TBitmap);

    procedure RenderToBitmap(Bitmap: TBitmap); virtual;
    {$IFDEF FPC}
    procedure CMFontChanged(var Message: TLMessage); message CM_FONTCHANGED;
    {$ELSE}
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    {$ENDIF}

    {$IFNDEF FPC}
    procedure DrawParentImage(Dest: TCanvas); virtual;
    {$ENDIF}

    property ChartBufferNeedsRepaint: Boolean read FChartBufferNeedsRepaint write FChartBufferNeedsRepaint;
    property BufferNeedsRepaint: Boolean read FBufferNeedsRepaint write FBufferNeedsRepaint;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property AntiAlias: TGuiAntiAlias read FAntiAlias write SetAntiAlias default gaaNone;
    property AutoColor: Boolean read FAutoColor write SetAutoColor default true;
    property AutoUpdate: Boolean read FAutoUpdate write FAutoUpdate;
    property GraphColorDark: TColor read FGraphColorDark write SetGraphColorDark default $303030;
    property GraphColorLight: TColor read FGraphColorLight write SetGraphColorLight default $606060;
    property ColorChart: TColor read FChartColor write SetChartColor;
    property BorderRadius: Integer read FBorderRadius write SetBorderRadius default 0;
    property BorderWidth: Integer read FBorderWidth write SetBorderWidth default 1;
    property Transparent: Boolean read FTransparent write SetTransparent default False;


    property YAxis: TGuiEQGraphYAxis read FYAxis write SetYAxis;
    property XAxis: TGuiEQGraphXAxis read FXAxis write SetXAxis;

    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
    property OnGetFilterGain: TGetFilterGainEvent read FOnGetFilterGain write FOnGetFilterGain;
    property Align;
    property Anchors;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Font;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property ParentColor;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
    {$IFNDEF FPC}
    property BiDiMode;
    property OnCanResize;
    {$ENDIF}
  end;

implementation

uses
  Math, DAV_Common;

{ TCustomGuiEQGraphAxis }

constructor TCustomGuiEQGraphAxis.Create(AOwner: TCustomGuiEQGraph);
begin
 FOwner := AOwner;
end;

procedure TCustomGuiEQGraphAxis.Changed;
begin
(*
 if FOwner is TGuiEQGraph then
  with TGuiEQGraph(FOwner) do
   begin
    RepaintChartBuffer;
   end;
*)
 FOwner.Invalidate;
end;


{ TCustomGuiEQGraphXAxis }

constructor TCustomGuiEQGraphXAxis.Create(AOwner: TCustomGuiEQGraph);
begin
 inherited;
 FLabelStyle  := xlsNone;
end;

procedure TCustomGuiEQGraphXAxis.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomGuiEQGraphXAxis then
  with TCustomGuiEQGraphXAxis(Dest) do
   begin
    FLabelStyle := Self.FLabelStyle;
   end
 else inherited;
end;

procedure TCustomGuiEQGraphXAxis.SetLabelStyle(const Value: TXAxisLabelStyle);
begin
 if FLabelStyle <> Value then
  begin
   FLabelStyle := Value;
   LabelStyleChanged;
  end;
end;

procedure TCustomGuiEQGraphXAxis.SetLowerFrequency(const Value: Single);
begin
 if FLowerFrequency <> Value then
  begin
   FLowerFrequency := Value;
   LowerFrequencyChanged;
  end;
end;

procedure TCustomGuiEQGraphXAxis.SetUpperFrequency(const Value: Single);
begin
 if FUpperFrequency <> Value then
  begin
   FUpperFrequency := Value;
   UpperFrequencyChanged;
  end;
end;

procedure TCustomGuiEQGraphXAxis.LowerFrequencyChanged;
begin
 Changed;
end;

procedure TCustomGuiEQGraphXAxis.UpperFrequencyChanged;
begin
 Changed;
end;

procedure TCustomGuiEQGraphXAxis.LabelStyleChanged;
begin
 Changed;
end;


{ TCustomGuiEQGraphYAxis }

constructor TCustomGuiEQGraphYAxis.Create(AOwner: TCustomGuiEQGraph);
begin
 inherited Create(AOwner);
 FUpperLevel :=  15;
 FLowerLevel := -15;
 FLabelStyle := ylsNone;
end;

procedure TCustomGuiEQGraphYAxis.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomGuiEQGraphYAxis then
  with TCustomGuiEQGraphYAxis(Dest) do
   begin
    FUpperLevel := Self.FUpperLevel;
    FLowerLevel := Self.FLowerLevel;
    FLabelStyle := Self.FLabelStyle;
   end
 else inherited;
end;

procedure TCustomGuiEQGraphYAxis.SetLowerLevel(const Value: Single);
begin
 if FLowerLevel <> Value then
  begin
   FLowerLevel := Value;
   LowerLevelChanged;
  end;
end;

procedure TCustomGuiEQGraphYAxis.SetUpperLevel(const Value: Single);
begin
 if FUpperLevel <> Value then
  begin
   FUpperLevel := Value;
   UpperLevelChanged;
  end;
end;

procedure TCustomGuiEQGraphYAxis.SetLabelStyle(const Value: TYAxisLabelStyle);
begin
 if FLabelStyle <> Value then
  begin
   FLabelStyle := Value;
   LabelStyleChanged;
  end;
end;

procedure TCustomGuiEQGraphYAxis.LabelStyleChanged;
begin
 Changed;
end;

procedure TCustomGuiEQGraphYAxis.UpperLevelChanged;
begin
 Changed;
end;

procedure TCustomGuiEQGraphYAxis.LowerLevelChanged;
begin
 Changed;
end;


{ TCustomGuiEQGraph }

constructor TCustomGuiEQGraph.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
 ControlStyle := [csAcceptsControls, csCaptureMouse, csClickEvents,
   csDoubleClicks, csReplicatable, csOpaque];
 TabStop      := False; // Ensure we're not a tab-stop
 Color        := clBtnFace;
end;


{ TGuiEQGraph }

constructor TGuiEQGraph.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);

 FAutoColor       := False;
 FXAxis           := TGuiEQGraphXAxis.Create(Self);
 FYAxis           := TGuiEQGraphYAxis.Create(Self);
 FBuffer          := TBitmap.Create;
 FChartBuffer     := TBitmap.Create;
 FGraphColorLight := $606060;
 FGraphColorDark  := $303030;
 FBorderWidth     := 1;
 FChartColor      := Color;
end;

destructor TGuiEQGraph.Destroy;
begin
 FreeAndNil(FBuffer);
 FreeAndNil(FChartBuffer);
 FreeAndNil(FXAxis);
 FreeAndNil(FYAxis);
 inherited Destroy;
end;

procedure TGuiEQGraph.AssignTo(Dest: TPersistent);
begin
 inherited;
 if Dest is TGuiEQGraph then
  with TGuiEQGraph(Dest) do
   begin
    FAutoColor               := Self.FAutoColor;
    FChartColor              := Self.FChartColor;
    FBorderRadius            := Self.FBorderRadius;
    FBorderWidth             := Self.FBorderWidth;
    FGraphColorDark          := Self.FGraphColorDark;
    FGraphColorLight         := Self.FGraphColorLight;
    FChartBufferNeedsRepaint := Self.FChartBufferNeedsRepaint;
    FBufferNeedsRepaint      := Self.FBufferNeedsRepaint;
    FOnPaint                 := Self.FOnPaint;
    FOnGetFilterGain         := Self.FOnGetFilterGain;

    FYAxis.Assign(Self.FYAxis);
    FXAxis.Assign(Self.FXAxis);
    FBuffer.Assign(Self.FBuffer);
    FChartBuffer.Assign(Self.FChartBuffer);
   end;
end;

{$IFNDEF FPC}
procedure TGuiEQGraph.CMFontChanged(var Message: TMessage);
{$ELSE}
procedure TGuiEQGraph.CMFontChanged(var Message: TLMessage);
{$ENDIF}
begin
 FChartBuffer.Canvas.Font.Assign(Font);
 FBuffer.Canvas.Font.Assign(Font);
 ChartBufferNeedsRepaint := True;
end;

// Drawing stuff

procedure TGuiEQGraph.UpsampleBitmap(Bitmap: TBitmap);
begin
 case FAntiAlias of
   gaaLinear2x: Upsample2xBitmap32(Bitmap);
   gaaLinear3x: Upsample3xBitmap32(Bitmap);
   gaaLinear4x: Upsample4xBitmap32(Bitmap);
   gaaLinear8x: begin
                 Upsample4xBitmap32(Bitmap);
                 Upsample2xBitmap32(Bitmap);
                end;
  gaaLinear16x: begin
                 Upsample4xBitmap32(Bitmap);
                 Upsample4xBitmap32(Bitmap);
                end;
  else raise Exception.Create('not yet supported');
 end;
end;

procedure TGuiEQGraph.DownsampleBitmap(Bitmap: TBitmap);
begin
 case FAntiAlias of
   gaaLinear2x: Downsample2xBitmap32(Bitmap);
   gaaLinear3x: Downsample3xBitmap32(Bitmap);
   gaaLinear4x: Downsample4xBitmap32(Bitmap);
   gaaLinear8x: begin
                 Downsample4xBitmap32(Bitmap);
                 Downsample2xBitmap32(Bitmap);
                end;
  gaaLinear16x: begin
                 Downsample4xBitmap32(Bitmap);
                 Downsample4xBitmap32(Bitmap);
                end;
  else raise Exception.Create('not yet supported');
 end;
end;

{$IFNDEF FPC}
procedure TGuiEQGraph.DrawParentImage(Dest: TCanvas);
var
  SaveIndex : Integer;
  DC        : THandle;
  Position  : TPoint;
begin
  if Parent = nil then Exit;
  DC := Dest.Handle;
  SaveIndex := SaveDC(DC);
  GetViewportOrgEx(DC, Position);
  SetViewportOrgEx(DC, Position.X - Left, Position.Y - Top, nil);
  IntersectClipRect(DC, 0, 0, Parent.ClientWidth, Parent.ClientHeight);
  Parent.Perform(WM_ERASEBKGND, Longint(DC), 0);
  Parent.Perform(WM_PAINT, Longint(DC), 0);
  RestoreDC(DC, SaveIndex);
end;
{$ENDIF}

procedure TGuiEQGraph.Paint;
begin
 RenderBuffer;
 Canvas.Draw(0, 0, FBuffer);
 inherited;
(*
 if FAutoUpdate or FChartBufferNeedsRepaint then RepaintChartBuffer;
 if FAutoUpdate or FBufferNeedsRepaint then RepaintBuffer;
 Canvas.Draw(0, 0, FBuffer);
*)
 if assigned(FOnPaint)
  then FOnPaint(Self);
end;

procedure TGuiEQGraph.SetChartColor(const Value: TColor);
begin
 if not FAutoColor and (FChartColor <> Value) then
  begin
   FChartColor := Value;
   ChartBufferNeedsRepaint := True;
   Invalidate;
  end;
end;

procedure TGuiEQGraph.SetBorderRadius(const Value: Integer);
begin
 if FBorderRadius <> Value then
  begin
   FBorderRadius := Value;
   ChartBufferNeedsRepaint := True;
   Invalidate;
  end;
end;

procedure TGuiEQGraph.SetBorderWidth(const Value: Integer);
begin
 if FBorderWidth <> Value then
  begin
   FBorderWidth := Value;
   ChartBufferNeedsRepaint := True;
   Invalidate;
  end;
end;

procedure TGuiEQGraph.SetAntiAlias(const Value: TGuiAntiAlias);
begin
 if FAntiAlias <> Value then
  begin
   FAntiAlias := Value;
   AntiAliasChanged;
  end;
end;

procedure TGuiEQGraph.AntiAliasChanged;
begin
 case FAntiAlias of
       gaaNone : FOSFactor :=  1;
   gaaLinear2x : FOSFactor :=  2;
   gaaLinear3x : FOSFactor :=  3;
   gaaLinear4x : FOSFactor :=  4;
   gaaLinear8x : FOSFactor :=  8;
  gaaLinear16x : FOSFactor := 16;
 end;
 Invalidate;
end;

procedure TGuiEQGraph.SetAutoColor(const Value: Boolean);
begin
 if FAutoColor <> Value then
  begin
   FAutoColor := Value;
   if FAutoColor then
    begin
(*
     FChartColor32 := Lighten(Color32(Color),60);
     FChartColor := WinColor(FChartColor32);
     RepaintChartBuffer;
*)
    end;
  end;
end;

procedure TGuiEQGraph.RenderBuffer;
var
  Bmp: TBitmap;
begin
 if (Width > 0) and (Height > 0) then
  with FBuffer.Canvas do
   begin
    Lock;
    Brush.Assign(Canvas.Brush);

    case FAntiAlias of
     gaaNone:
      begin
       // draw background
       {$IFNDEF FPC}
       if FTransparent
        then DrawParentImage(FBuffer.Canvas)
        else
       {$ENDIF}
        begin
         Brush.Color := Self.Color;
         FillRect(ClipRect);
        end;
       RenderToBitmap(FBuffer);
      end;
     else
      begin
       Bmp := TBitmap.Create;
       with Bmp do
        try
         PixelFormat := pf32bit;
         Width       := FOSFactor * FBuffer.Width;
         Height      := FOSFactor * FBuffer.Height;
         {$IFNDEF FPC}
         if FTransparent then
          begin
           CopyParentImage(Self, Bmp.Canvas);
//           DrawParentImage(Bmp.Canvas);
           UpsampleBitmap(Bmp);
          end
         else
         {$ENDIF}
          with Bmp.Canvas do
           begin
            Brush.Color := Self.Color;
            FillRect(ClipRect);
           end;
         RenderToBitmap(Bmp);
         DownsampleBitmap(Bmp);
         FBuffer.Canvas.Draw(0, 0, Bmp);
        finally
         Free;
        end;
      end;
    end;
    Unlock;
   end;
end;

(*
procedure TGuiEQGraph.RepaintBuffer;
begin
 with FBuffer, Canvas do
  begin
   Lock;
   FBufferNeedsRepaint := False;
   Draw(0, 0, FChartBuffer);
   Unlock;
  end;
end;
*)

procedure TGuiEQGraph.RenderToBitmap(Bitmap: TBitmap);
var
  i, j, w, h : Integer;
  rct        : TRect;
  HHOffs     : Integer;
  HalfHeight : Single;
  NormFactor : Single;
  Temp       : array [0..1] of Single;
  Wdth       : Integer;
begin
 if ((csLoading in ComponentState) and not (csDesigning in ComponentState))
   or (csDestroying in ComponentState) then exit;
 with Bitmap, Canvas do
  begin
   Lock;

   FChartBufferNeedsRepaint := False;
   {$IFDEF Delphi10_Up}
   with Margins
    do rct := Rect(Left, Top, Width - Right, Height - Bottom);
   {$ELSE}
   rct := Rect(0, 0, Width, Height);
   {$ENDIF}

   Brush.Color := Color;
   Brush.Style := bsSolid;
   FillRect(ClientRect);
   Brush.Color := FChartColor;

   Pen.Width := FOSFactor;
   InflateRect(rct, -1, -1);

   // calculate the real half height of the visible area (without the margin)
   HalfHeight := (rct.Bottom - rct.Top) * 0.5;
   NormFactor := HalfHeight / FYAxis.UpperLevel;

   // add top margin to half height as offset
   HHOffs := round(rct.Top + HalfHeight);
   Wdth := round(rct.Right - rct.Left);

   // draw middle line
   Pen.Color := FGraphColorDark;
   MoveTo(rct.Left, HHOffs);
   LineTo(rct.Right, HHOffs);

   Pen.Color := FGraphColorLight;
   if FYAxis.UpperLevel <=  5 then w := 1 else
   if FYAxis.UpperLevel <= 10 then w := 5 else
   if FYAxis.UpperLevel >= 180 then w := 45 else w := 10;
   i := w;
   while i * NormFactor < HalfHeight do
    begin
     MoveTo(rct.Left,  round(rct.Top + HalfHeight - i * NormFactor));
     LineTo(rct.Right, round(rct.Top + HalfHeight - i * NormFactor));

     MoveTo(rct.Left,  round(rct.Top + HalfHeight + i * NormFactor));
     LineTo(rct.Right, round(rct.Top + HalfHeight + i * NormFactor));

     inc(i, w);
    end;

   i := 10; j := 3;
   while j * i < 20000 do
    begin
     w := round(rct.Left + FreqLogToLinear(j * i)    * Wdth);
     MoveTo(w, rct.Top); LineTo(w, rct.Bottom);
     inc(j);
     if j >= 10 then
      begin
       i := i * 10;
       j := 1;
      end;
    end;

   Font.Assign(Self.Font);
   case FXAxis.LabelStyle of
    xlsBottom :
     begin
      h := rct.Bottom + Font.Height - 2 - FBorderWidth div 2;
      TextOut(Round(rct.Left + FreqLogToLinear(30)    * Wdth) - 12, h, '30Hz');
      TextOut(Round(rct.Left + FreqLogToLinear(50)    * Wdth) - 12, h, '50Hz');
      TextOut(Round(rct.Left + FreqLogToLinear(100)   * Wdth) - 12, h, '100Hz');
      TextOut(Round(rct.Left + FreqLogToLinear(200)   * Wdth) - 12, h, '200Hz');
      TextOut(Round(rct.Left + FreqLogToLinear(500)   * Wdth) - 12, h, '500Hz');
      TextOut(Round(rct.Left + FreqLogToLinear(1000)  * Wdth) - 10, h, '1kHz');
      TextOut(Round(rct.Left + FreqLogToLinear(2000)  * Wdth) - 10, h, '2kHz');
      TextOut(Round(rct.Left + FreqLogToLinear(5000)  * Wdth) - 10, h, '5kHz');
      TextOut(Round(rct.Left + FreqLogToLinear(10000) * Wdth) - 12, h, '10kHz');
     end;
    xlsTop:
     begin
      TextOut(Round(rct.Left + FreqLogToLinear(100) * Wdth) - 12, Round(rct.Top),'100Hz');
      TextOut(Round(rct.Left + FreqLogToLinear(1000) * Wdth) - 10, Round(rct.Top),'1kHz');
     end;
   end;

   case FYAxis.LabelStyle of
    ylsLeft:
     begin
      if FYAxis.UpperLevel <=  5 then w := 1 else
      if FYAxis.UpperLevel <= 10 then w := 5 else
      if FYAxis.UpperLevel >= 180 then w := 45 else w := 10;
      i := w;
      while i * NormFactor < HalfHeight do
       begin
        TextOut(rct.Left, round(rct.Top + HalfHeight - i * NormFactor - 5) + Font.Height div 2 + 2, '+' + IntToStr(i) + 'dB');
        TextOut(rct.Left, round(rct.Top + HalfHeight + i * NormFactor - 5) + Font.Height div 2 + 2, '-' + IntToStr(i) + 'dB');
        inc(i,w);
       end;
     end;
    ylsRight:
     begin
      if FYAxis.UpperLevel <=  5 then w := 1 else
      if FYAxis.UpperLevel <= 10 then w := 5 else 
      if FYAxis.UpperLevel >= 180 then w := 45 else w := 10;
      i := w;
      while i * NormFactor < HalfHeight do
       begin
        TextOut(Round(rct.Right - 27), round(HalfHeight - i * NormFactor - 5), '+' + IntToStr(i) + 'dB');
        TextOut(Round(rct.Right - 27), round(HalfHeight + i * NormFactor - 5), '-' + IntToStr(i) + 'dB');
        inc(i, w);
       end;
     end;
   end;

   Pen.Color := GraphColorDark;
   Pen.Width := 2 * FOSFactor;
   if Assigned(FOnGetFilterGain) then
    begin
     MoveTo(rct.Left, round(HHOffs - FOnGetFilterGain(Self, 20) * NormFactor));
     Temp[0] := 1 / (rct.Right - rct.Left);
     for w := rct.Left + 1 to rct.Right - 1
      do LineTo(w, round(HHOffs - FOnGetFilterGain(Self, FreqLinearToLog((w - rct.Left) * Temp[0])) * NormFactor));
    end;


   InflateRect(rct, 1, 1);

   if FBorderWidth > 0 then
    begin
     Pen.Color := Font.Color;
     Pen.Width := FBorderWidth;
     Brush.Style := bsClear;
     RoundRect(rct.Left, rct.Top, rct.Right, rct.Bottom,
       FBorderRadius, FBorderRadius);
    end;

   FBufferNeedsRepaint := True;
   Unlock;
  end;
end;

procedure TGuiEQGraph.Resize;
begin
 inherited;
 FBuffer.Canvas.Brush.Color := Self.Color;
 FBuffer.Width := Self.Width;
 FBuffer.Height := Self.Height;
 FChartBuffer.Canvas.Brush.Color := Self.Color;
 FChartBuffer.Width := Self.Width;
 FChartBuffer.Height := Self.Height;
 ChartBufferNeedsRepaint := True;
end;

procedure TGuiEQGraph.SetGraphColorDark(const Value: TColor);
begin
 if FGraphColorDark <> Value then
  begin
   FGraphColorDark := Value;
   Invalidate;
  end;
end;

procedure TGuiEQGraph.SetGraphColorLight(const Value: TColor);
begin
 if FGraphColorLight <> Value then
  begin
   FGraphColorLight := Value;
   Invalidate;
  end;
end;

procedure TGuiEQGraph.SetTransparent(const Value: Boolean);
begin
 if FTransparent <> Value then
  begin
   FTransparent := Value;
   TransparentChanged;
  end;
end;

procedure TGuiEQGraph.TransparentChanged;
begin
 Invalidate;
end;

procedure TGuiEQGraph.SetXAxis(const Value: TGuiEQGraphXAxis);
begin
 FXAxis.Assign(Value);
end;

procedure TGuiEQGraph.SetYAxis(const Value: TGuiEQGraphYAxis);
begin
 FYAxis.Assign(Value);
end;

end.
