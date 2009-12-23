unit DAV_GuiMediaButton;

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

{$I ..\DAV_Compiler.Inc}

uses
  {$IFDEF FPC} LCLIntf, LMessages, {$ELSE} Windows, Messages, {$ENDIF}
  Classes, Graphics, Forms, Types, SysUtils, Controls, DAV_GuiCommon,
  DAV_GuiBaseControl;

type
  TMediaButtonState = (mbsPlay, mbsPause, mbsStop, mbsRecord, mbsFastBackward,
    mbsFastForward, mbsPrevious, mbsNext);
  TCustomGuiMediaButton = class(TCustomControl)
  private
    FBorderRadius : Integer;
    FBorderWidth  : Integer;
    FBorderColor  : TColor;
    FButtonColor  : TColor;
    FButtonState  : TMediaButtonState;
    FGlyphOffset  : Integer;
    FOnPaint      : TNotifyEvent;
    procedure SetBorderColor(const Value: TColor);
    procedure SetBorderRadius(const Value: Integer);
    procedure SetBorderWidth(const Value: Integer);
    procedure SetButtonColor(const Value: TColor);
    procedure SetButtonState(const Value: TMediaButtonState);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure BorderRadiusChanged; virtual;
    procedure BorderWidthChanged; virtual;
    procedure BorderColorChanged; virtual;
    procedure ButtonColorChanged; virtual;
    procedure ButtonStateChanged; virtual;
    procedure ControlChanged; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property BorderColor: TColor read FBorderColor write SetBorderColor default $202020;
    property BorderRadius: Integer read FBorderRadius write SetBorderRadius default 0;
    property BorderWidth: Integer read FBorderWidth write SetBorderWidth default 1;
    property ButtonColor: TColor read FButtonColor write SetButtonColor default $303030;
    property ButtonState: TMediaButtonState read FButtonState write SetButtonState default mbsPlay;

    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
  end;

  TGuiMediaButton = class(TCustomGuiMediaButton)
  private
    FBuffer      : TBitmap;
    FAntiAlias   : TGuiAntiAlias;
    FOSFactor    : Integer;
    FTransparent : Boolean;

    procedure SetAntiAlias(const Value: TGuiAntiAlias);
    procedure SetTransparent(const Value: Boolean);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure Paint; override;
    procedure Resize; override;
    procedure Loaded; override;

    procedure AntiAliasChanged; virtual;
    procedure ControlChanged; override;
    procedure RenderBuffer; virtual;
    procedure TransparentChanged; virtual;
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

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    property AntiAlias: TGuiAntiAlias read FAntiAlias write SetAntiAlias default gaaNone;
    property Transparent: Boolean read FTransparent write SetTransparent default False;
    property BorderColor;
    property BorderRadius;
    property BorderWidth;
    property ButtonColor;
    property ButtonState;

    property OnPaint;

    property Align;
    property Anchors;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
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
  Math, {$IFNDEF FPC}Consts, {$ENDIF} DAV_Common, DAV_Approximations;


{ TCustomGuiMediaButton }

constructor TCustomGuiMediaButton.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
 ControlStyle := [csAcceptsControls, csCaptureMouse, csClickEvents,
   csDoubleClicks, csReplicatable, csOpaque];
 TabStop := False; // Ensure we're not a tab-stop
 Color := clBtnFace;

 FButtonColor     := $606060;
 FBorderColor     := $202020;
 FBorderWidth     := 1;
 FGlyphOffset     := 3;
 FButtonState     := mbsPlay;
 FBorderRadius    := 0;
end;

destructor TCustomGuiMediaButton.Destroy;
begin
 inherited Destroy;
end;

procedure TCustomGuiMediaButton.AssignTo(Dest: TPersistent);
begin
 inherited;
 if Dest is TCustomGuiMediaButton then
  with TCustomGuiMediaButton(Dest) do
   begin
    FBorderRadius := Self.FBorderRadius;
    FBorderWidth  := Self.FBorderWidth;
    FBorderColor  := Self.FBorderColor;
    FButtonColor  := Self.FButtonColor;
    FButtonState  := Self.FButtonState;
    FOnPaint      := Self.FOnPaint;
   end;
end;

procedure TCustomGuiMediaButton.SetBorderColor(const Value: TColor);
begin
 if FBorderColor <> Value then
  begin
   FBorderColor := Value;
   BorderColorChanged;
  end;
end;

procedure TCustomGuiMediaButton.SetBorderRadius(const Value: Integer);
begin
 if FBorderRadius <> Value then
  begin
   FBorderRadius := Value;
   BorderRadiusChanged;
  end;
end;

procedure TCustomGuiMediaButton.SetBorderWidth(const Value: Integer);
begin
 if FBorderWidth <> Value then
  begin
   FBorderWidth := Value;
   BorderWidthChanged;
  end;
end;

procedure TCustomGuiMediaButton.SetButtonState(const Value: TMediaButtonState);
begin
 if FButtonState <> Value then
  begin
   FButtonState := Value;
   ButtonStateChanged;
  end;
end;

procedure TCustomGuiMediaButton.ButtonStateChanged;
begin
 ControlChanged;
end;

procedure TCustomGuiMediaButton.SetButtonColor(const Value: TColor);
begin
 if FButtonColor <> Value then
  begin
   FButtonColor := Value;
   ButtonColorChanged;
  end;
end;

procedure TCustomGuiMediaButton.BorderWidthChanged;
begin
 ControlChanged;
end;

procedure TCustomGuiMediaButton.ControlChanged;
begin
 Invalidate;
end;

procedure TCustomGuiMediaButton.BorderColorChanged;
begin
 ControlChanged;
end;

procedure TCustomGuiMediaButton.BorderRadiusChanged;
begin
 ControlChanged;
end;

procedure TCustomGuiMediaButton.ButtonColorChanged;
begin
 ControlChanged;
end;


{ TGuiMediaButton }

constructor TGuiMediaButton.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);

 FBuffer := TBitmap.Create;
 with FBuffer do
  begin
   Canvas.Brush.Color := Self.Color;
   Width := Self.Width;
   Height := Self.Height;
  end;

 FOSFactor := 1;
 FTransparent := False;
end;

destructor TGuiMediaButton.Destroy;
begin
 FreeAndNil(FBuffer);
 inherited Destroy;
end;

procedure TGuiMediaButton.AssignTo(Dest: TPersistent);
begin
 inherited;
 if Dest is TGuiMediaButton then
  with TGuiMediaButton(Dest) do
   begin
    FAntiAlias       := Self.FAntiAlias;
    FOSFactor        := Self.FOSFactor;
    FTransparent     := Self.FTransparent;

    FBuffer.Assign(Self.FBuffer);
   end;
end;

procedure TGuiMediaButton.ControlChanged;
begin
 inherited;
end;

{$IFNDEF FPC}
procedure TGuiMediaButton.CMFontChanged(var Message: TMessage);
{$ELSE}
procedure TGuiMediaButton.CMFontChanged(var Message: TLMessage);
{$ENDIF}
begin
 inherited;
 FBuffer.Canvas.Font.Assign(Font);
end;



// Drawing stuff

procedure TGuiMediaButton.UpsampleBitmap(Bitmap: TBitmap);
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

procedure TGuiMediaButton.DownsampleBitmap(Bitmap: TBitmap);
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
procedure TGuiMediaButton.DrawParentImage(Dest: TCanvas);
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

procedure TGuiMediaButton.Paint;
begin
 if Assigned(FBuffer) then
  begin
   RenderBuffer;
   Canvas.Draw(0, 0, FBuffer);
  end;

 inherited;

 if Assigned(FOnPaint)
  then FOnPaint(Self);
end;

procedure TGuiMediaButton.SetAntiAlias(const Value: TGuiAntiAlias);
begin
 if FAntiAlias <> Value then
  begin
   FAntiAlias := Value;
   AntiAliasChanged;
  end;
end;

procedure TGuiMediaButton.AntiAliasChanged;
begin
 case FAntiAlias of
       gaaNone : FOSFactor :=  1;
   gaaLinear2x : FOSFactor :=  2;
   gaaLinear3x : FOSFactor :=  3;
   gaaLinear4x : FOSFactor :=  4;
   gaaLinear8x : FOSFactor :=  8;
  gaaLinear16x : FOSFactor := 16;
 end;
 ControlChanged;
end;

procedure TGuiMediaButton.RenderBuffer;
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
         Canvas.Font.Assign(Font);
         Canvas.Font.Size := FOSFactor * Font.Size;
         {$IFNDEF FPC}
         if FTransparent then
          begin
           CopyParentImage(Self, Bmp.Canvas);
//           DrawParentImage(Bmp.Canvas);
           UpsampleBitmap(Bmp);
          end
         else
         {$ENDIF}
          with Canvas do
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

procedure TGuiMediaButton.RenderToBitmap(Bitmap: TBitmap);
begin
 with Bitmap, Canvas do
  begin
   Lock;

   if FBorderWidth > 0 then
    begin
     Pen.Color := FBorderColor;
     Pen.Width := FOSFactor * FBorderWidth;
     Brush.Style := bsSolid;
     Brush.Color := FButtonColor;
     RoundRect((FOSFactor * FBorderWidth) div 2,
       (FOSFactor * FBorderWidth) div 2,
       Width - (FOSFactor * FBorderWidth) div 2,
       Height - (FOSFactor * FBorderWidth) div 2,
       FOSFactor * FBorderRadius, FOSFactor * FBorderRadius);
    end;

   case FButtonState of
    mbsPlay :
     begin
      Brush.Color := clLime;
      Polygon([Point((FBorderWidth + FGlyphOffset) * FOSFactor, (FBorderWidth + FGlyphOffset) * FOSFactor),
               Point(Width - (FBorderWidth + FGlyphOffset) * FOSFactor, Height div 2),
               Point((FBorderWidth + FGlyphOffset) * FOSFactor, Height - (FBorderWidth + FGlyphOffset) * FOSFactor)]);
     end;
    mbsPause :
     begin
      Brush.Color := clBlack;
      Rectangle((FBorderWidth + FGlyphOffset) * FOSFactor,
                (FBorderWidth + FGlyphOffset) * FOSFactor,
                Width div 2 - ((FGlyphOffset + FBorderWidth) div 2) * FOSFactor,
                Height - (FBorderWidth + FGlyphOffset) * FOSFactor);

      Rectangle(Width div 2 + ((FGlyphOffset + FBorderWidth) div 2) * FOSFactor,
                (FBorderWidth + FGlyphOffset) * FOSFactor,
                Width - (FBorderWidth + FGlyphOffset) * FOSFactor,
                Height - (FBorderWidth + FGlyphOffset) * FOSFactor);
     end;
    mbsStop :
     begin
      Brush.Color := clBlack;
      Rectangle((FBorderWidth + FGlyphOffset) * FOSFactor,
                (FBorderWidth + FGlyphOffset) * FOSFactor,
                Width  - (FBorderWidth + FGlyphOffset) * FOSFactor,
                Height - (FBorderWidth + FGlyphOffset) * FOSFactor);
     end;

   end;

(*
   Offset := FOSFactor * FBorderWidth;
   Pen.Color := FButtonColor;
   Brush.Color := FButtonColor;
   Scale := (FPosition - FMin) / (FMax - FMin);
   Scale := UnmapValue(Scale);
   Rectangle(Offset, Offset, Round(Offset + Scale * (Width - 2 * Offset)), Height - Offset);

   if ShowText then
    begin
     Text := FloatToStrF(FPosition, ffGeneral, 5, 5);
     TextSize := TextExtent(Text);
     Brush.Style := bsClear;
     TextOut((Width - TextSize.cx) div 2, (Height - TextSize.cy) div 2, Text);
    end;

*)

   Unlock;
  end;
end;

procedure TGuiMediaButton.Resize;
begin
 inherited;
 if Assigned(FBuffer) then
  with FBuffer do
   begin
    Canvas.Brush.Color := Self.Color;
    Width := Self.Width;
    Height := Self.Height;
   end;
 ControlChanged;
end;

procedure TGuiMediaButton.Loaded;
begin
 inherited;
 Resize;
end;

procedure TGuiMediaButton.SetTransparent(const Value: Boolean);
begin
 if FTransparent <> Value then
  begin
   FTransparent := Value;
   TransparentChanged;
  end;
end;

procedure TGuiMediaButton.TransparentChanged;
begin
 ControlChanged;
end;

end.
