{******************************************************************************}
{                                                                              }
{  Version: MPL 1.1 or LGPL 2.1 with linking exception                         }
{                                                                              }
{  The contents of this file are subject to the Mozilla Public License         }
{  Version 1.1 (the "License"); you may not use this file except in            }
{  compliance with the License. You may obtain a copy of the License at        }
{  http://www.mozilla.org/MPL/                                                 }
{                                                                              }
{  Software distributed under the License is distributed on an "AS IS"         }
{  basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the     }
{  License for the specific language governing rights and limitations under    }
{  the License.                                                                }
{                                                                              }
{  Alternatively, the contents of this file may be used under the terms of     }
{  the Free Pascal modified version of the GNU Lesser General Public           }
{  License Version 2.1 (the "FPC modified LGPL License"), in which case the    }
{  provisions of this license are applicable instead of those above.           }
{  Please see the file LICENSE.txt for additional information concerning       }
{  this license.                                                               }
{                                                                              }
{  The code is part of the Delphi ASIO & VST Project                           }
{                                                                              }
{  The initial developer of this code is Christian-W. Budde                    }
{                                                                              }
{  Portions created by Christian-W. Budde are Copyright (C) 2003-2019          }
{  by Christian-W. Budde. All Rights Reserved.                                 }
{                                                                              }
{******************************************************************************}

unit DAV_GuiModular;

interface

{$I ..\DAV_Compiler.inc}

uses
  Windows, Classes, Graphics, Forms, Messages, SysUtils, Controls,
  DAV_GuiBaseControl;

type
  TCustomGuiModular = class(TCustomControl)
  private
    FBuffer: TBitmap;
    FPinSize: Integer;
    FModuleManager: TModularManager;
    procedure SetModuleManager(const Value: TModularManager);
    procedure SetPinSize(const Value: Integer);
    procedure RenderModule(ModuleItem: TCustomModularItem);
  protected
    procedure Resize; override;
    procedure Loaded; override;
    procedure Paint; override;

    procedure RenderBuffer; virtual;
    (*
      procedure RenderInputPins; virtual;
      procedure RenderOutputPins; virtual;
    *)

    procedure PinSizeChanged; virtual;
    procedure ModuleManagerChanged; virtual;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property PinSize: Integer read FPinSize write SetPinSize;
    property ModuleManager: TModularManager read FModuleManager
      write SetModuleManager;
  end;

  TGuiModular = class(TCustomGuiModular)
  published
    property Align;
    property Anchors;
    property Color;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ModuleManager;
    property OnClick;
    property OnDblClick;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
{$IFDEF DELPHICOMPILER8_UP}
    property OnMouseEnter;
    property OnMouseLeave;
{$ENDIF}
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnStartDock;
    property OnStartDrag;
    property ParentColor;
    property ParentFont;
    property PinSize;
    property PopupMenu;
    property Visible;
  end;

implementation

uses
  Math;

{ TCustomGuiModular }

constructor TCustomGuiModular.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  ControlStyle := ControlStyle + [csOpaque];
  FBuffer := TBitmap.Create;
  FBuffer.PixelFormat := pf24bit;
  FPinSize := 8;
end;

destructor TCustomGuiModular.Destroy;
begin
  FreeAndNil(FBuffer);
  inherited;
end;

procedure TCustomGuiModular.Loaded;
begin
  inherited;
  FBuffer.Width := Width;
  FBuffer.Height := Height;
end;

procedure TCustomGuiModular.Paint;
begin
  RenderBuffer;
  Canvas.Draw(0, 0, FBuffer);
  inherited;
end;

procedure TCustomGuiModular.RenderBuffer;
var
  ModuleNo: Integer;
begin
  with FBuffer, Canvas do
    if (Width > 0) and (Height > 0) then
    begin
      Brush.Color := Self.Color;
      FillRect(ClipRect);
      // Font.Assign(Self.Font);

      // check whether a module manager is assigned
      if not Assigned(FModuleManager) then
        Exit;

      // render modules
      for ModuleNo := 0 to FModuleManager.ModuleCount - 1 do
        RenderModule(FModuleManager.ModuleItem[ModuleNo]);
    end;
end;

procedure TCustomGuiModular.RenderModule(ModuleItem: TCustomModularItem);
var
  DefaultHeight: Integer;
  InputHeight: Integer;
  OutputHeight: Integer;
  CurrentWidth: Integer;
  MaximumWidth: Integer;
  PinNo: Integer;
  ModuleRect: TRect;
  ModuleWidth: Integer;
  ModuleHeight: Integer;
  PinCenter: Integer;
begin
  with FBuffer, Canvas do
  begin
    // estimate default height
    DefaultHeight := (TextHeight('Pin') + 2);

    // estimate height of the input pins
    InputHeight := 2 + (ModuleItem.Module.PinCountInput + 1) * DefaultHeight;

    // estimate height of the output pins
    OutputHeight := 2 + (ModuleItem.Module.PinCountOutput + 1) * DefaultHeight;

    // estimate pin width
    MaximumWidth := TextWidth(ModuleItem.Module.Name);
    with ModuleItem.Module do
      for PinNo := 0 to min(PinCountInput, PinCountOutput) - 1 do
      begin
        CurrentWidth := 2 * PinSize + TextWidth(PinInput[PinNo].DisplayName +
          ' - ' + PinOutput[PinNo].DisplayName);
        if CurrentWidth > MaximumWidth then
          MaximumWidth := CurrentWidth;
      end;

    // estimate pin width for single pins
    with ModuleItem.Module do
      if PinCountInput > PinCountOutput then
        for PinNo := PinCountOutput to PinCountInput - 1 do
        begin
          CurrentWidth := 4 + PinSize + TextWidth(PinInput[PinNo].DisplayName);
          if CurrentWidth > MaximumWidth then
            MaximumWidth := CurrentWidth;
        end
      else
        for PinNo := PinCountOutput to PinCountInput - 1 do
        begin
          CurrentWidth := 4 + PinSize + TextWidth(PinOutput[PinNo].DisplayName);
          if CurrentWidth > MaximumWidth then
            MaximumWidth := CurrentWidth;
        end;

    ModuleWidth := MaximumWidth;
    ModuleHeight := Max(InputHeight, OutputHeight);
    if ModuleItem.Module is TModularIO then
    begin
      ModuleHeight := ModuleHeight + DefaultHeight;
      ModuleWidth := Max(ModuleWidth, 4 + 2 * PinSize + TextWidth('Spare'));
    end;

    ModuleRect := Rect(ModuleItem.Left, ModuleItem.Top,
      ModuleItem.Left + ModuleWidth, ModuleItem.Top + ModuleHeight);

    // draw panel
    Brush.Color := clSilver;
    Brush.Style := bsSolid;
    Pen.Color := clBlack;
    Rectangle(ModuleRect);

    // draw name
    Brush.Color := $6A240A;
    Pen.Color := $6A240A;
    Font.Color := clWhite;

    FillRect(Rect(ModuleRect.Left, ModuleRect.Top, ModuleRect.Right,
      ModuleRect.Top + DefaultHeight));

    // render caption
    with ModuleItem.Module do
      TextOut(ModuleItem.Left + (ModuleWidth - TextWidth(Name)) div 2,
        ModuleRect.Top + 1, ModuleItem.Module.Name);

    Font.Color := Self.Font.Color;
    Pen.Color := clBlack;
    Brush.Style := bsClear;

    // render input pins
    with ModuleItem.Module do
      for PinNo := 0 to PinCountInput - 1 do
      begin
        case PinInput[PinNo].Datatype of
          mdtDouble:
            Canvas.Font.Color := $960032;
          mdtSingle:
            Canvas.Font.Color := $8C5000;
          mdtInteger:
            Canvas.Font.Color := clOlive;
        else
          Canvas.Font.Color := clBlack;
        end;

        PinCenter := ModuleRect.Top + 1 + (PinNo + 1) * DefaultHeight +
          DefaultHeight div 2;
        Rectangle(Rect(ModuleRect.Left, PinCenter - PinSize div 2,
          ModuleRect.Left + PinSize - 1, PinCenter + PinSize div 2));

        TextOut(PinSize + ModuleRect.Left + 2, ModuleRect.Top + 1 + (PinNo + 1)
          * DefaultHeight, PinInput[PinNo].DisplayName);
      end;

    // render output pins
    with ModuleItem.Module do
      for PinNo := 0 to PinCountOutput - 1 do
      begin
        case PinOutput[PinNo].Datatype of
          mdtDouble:
            Canvas.Font.Color := $960032;
          mdtSingle:
            Canvas.Font.Color := $8C5000;
          mdtInteger:
            Canvas.Font.Color := clOlive;
        else
          Canvas.Font.Color := clBlack;
        end;

        PinCenter := ModuleRect.Top + 1 + (PinNo + 1) * DefaultHeight +
          DefaultHeight div 2;
        Rectangle(Rect(ModuleRect.Right - PinSize + 1,
          PinCenter - PinSize div 2, ModuleRect.Right,
          PinCenter + PinSize div 2));

        TextOut(ModuleRect.Right - PinSize -
          TextWidth(PinOutput[PinNo].DisplayName),
          ModuleRect.Top + 1 + (PinNo + 1) * DefaultHeight,
          PinOutput[PinNo].DisplayName);
      end;

    if ModuleItem.Module is TModularIO then
    begin
      PinCenter := ModuleRect.Bottom - 2 - DefaultHeight div 2;
      Rectangle(Rect(ModuleRect.Right - PinSize + 1, PinCenter - PinSize div 2,
        ModuleRect.Right, PinCenter + PinSize div 2));

      Rectangle(Rect(ModuleRect.Left, PinCenter - PinSize div 2,
        ModuleRect.Left + PinSize - 1, PinCenter + PinSize div 2));

      TextOut((ModuleRect.Left + ModuleRect.Right - TextWidth('Spare')) div 2,
        ModuleRect.Bottom - 2 - DefaultHeight, 'Spare');
    end;
  end;
end;

procedure TCustomGuiModular.Resize;
begin
  inherited;
  FBuffer.Width := Width;
  FBuffer.Height := Height;
end;

procedure TCustomGuiModular.CMFontChanged(var Message: TMessage);
begin
  FBuffer.Canvas.Font.Assign(Self.Font);
end;

procedure TCustomGuiModular.SetModuleManager(const Value: TModularManager);
begin
  if FModuleManager <> Value then
  begin
    FModuleManager := Value;
    ModuleManagerChanged;
  end;
end;

procedure TCustomGuiModular.SetPinSize(const Value: Integer);
begin
  if FPinSize <> Value then
  begin
    FPinSize := Value;
    PinSizeChanged;
  end;
end;

procedure TCustomGuiModular.PinSizeChanged;
begin
  // nothing yet in here
end;

procedure TCustomGuiModular.ModuleManagerChanged;
begin
  Invalidate;
end;

end.
