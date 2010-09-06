unit DAV_GuiStitchedControls;

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
  {$IFDEF FPC} LCLIntf, LResources, LMessages,
  {$ELSE} Windows, Messages, {$ENDIF}
  Classes, Graphics, Forms, SysUtils, Controls, Contnrs, DAV_GuiCommon,
  DAV_GuiPixelMap;

type
  TGuiStitchKind = (skHorizontal, skVertical);

  TCustomGuiStitchedControl = class(TCustomControl)
  private
    function GetDialImageIndex: Integer;
    procedure DoAutoSize;
    procedure SetAutoSize(const Value: Boolean); reintroduce;
    procedure SetPixelMap(const Value: TGuiCustomPixelMap);
    procedure SetGlyphCount(const Value: Integer);
    procedure SetStitchKind(const Value: TGuiStitchKind);
    procedure SetDialImageIndex(Value: Integer);
(*
    procedure SetImageList(const Value: TImageList);
    procedure SetDialImageList(const Value: TGuiDialImageList);
*)
  protected
    FAutoSize      : Boolean;
    FDialPixelMap  : TGuiCustomPixelMap;
    FImageList     : TImageList;
    FGlyphCount    : Integer;
    FOnChange      : TNotifyEvent;
    FStitchKind    : TGuiStitchKind;
(*
    FDialImageList : TGuiDialImageList;
    FDialImageItem : TGuiDialImageCollectionItem;
*)

    function GetGlyphNr: Integer; virtual; abstract;
    procedure GlyphCountChanged; virtual;
    procedure StitchKindChanged; virtual;
//    procedure UpdateBuffer; override;

    procedure Render(const Pixmap: TGuiCustomPixelMap); virtual; abstract;
    procedure Paint; override;

    property GlyphCount: Integer read FGlyphCount write SetGlyphCount default 1;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property AutoSize: Boolean read FAutoSize write SetAutoSize default False;
    property PixelMap: TGuiCustomPixelMap read FDialPixelMap write SetPixelMap;
    property DialImageIndex: Integer read GetDialImageIndex write SetDialImageIndex;
(*
    property DialImageList: TGuiDialImageList read FDialImageList write SetDialImageList;
    property ImageList: TImageList read FImageList write SetImageList;
*)
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property StitchKind: TGuiStitchKind read FStitchKind write SetStitchKind;
  end;

implementation

{ TCustomGuiStitchedControl }

constructor TCustomGuiStitchedControl.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
 FDialPixelMap             := TGuiCustomPixelMap.Create;
(*
 FLineColor              := clRed;
 FLineWidth              := 2;
 FDialPixelMap.PixelFormat := pf24bit;
*)
 FGlyphCount               := 1;
 FStitchKind               := skHorizontal;
end;

destructor TCustomGuiStitchedControl.Destroy;
begin
 FreeAndNil(FDialPixelMap);
 inherited Destroy;
end;

(*
procedure TCustomGuiStitchedControl.UpdateBuffer;
var
  theRect   : TRect;
  GlyphNr   : Integer;
  Bmp       : TBitmap;
  OwnerDraw : Boolean;
begin
 if [csLoading..csDestroying] * ComponentState <> [] then exit;

 if (Width > 0) and (Height > 0) then with FBuffer.Canvas do
  begin
   Lock;
   Brush.Color := Self.Color;
   OwnerDraw := FDialPixelMap.Empty and not Assigned(FImageList);
   if OwnerDraw and Assigned(FDialImageList) and Assigned(FDialImageItem)
    then OwnerDraw := FDialImageItem.FDialPixelMap.Empty;

   if OwnerDraw then
    if AntiAlias = gaaNone then
     begin
      // draw background
      {$IFNDEF FPC}if FTransparent then CopyParentImage(Self, FBuffer.Canvas) else{$ENDIF}
      FillRect(ClipRect);

      RenderBitmap(FBuffer);
     end
    else
     begin
      Bmp := TBitmap.Create;
      with Bmp do
       try
        PixelFormat := pf32bit;
        Width       := OversamplingFactor * FBuffer.Width;
        Height      := OversamplingFactor * FBuffer.Height;
        Canvas.Brush.Style := bsSolid;
        Canvas.Brush.Color := Self.Color;
        {$IFNDEF FPC}
        if FTransparent then
         begin
          CopyParentImage(Self, Bmp.Canvas);
          UpsampleBitmap(Bmp);
         end else
        {$ENDIF}
        Canvas.FillRect(Canvas.ClipRect);
        RenderBitmap(Bmp);
        DownsampleBitmap(Bmp);
        FBuffer.Canvas.Draw(0, 0, Bmp);
       finally
        FreeAndNil(Bmp);
       end;
    end
   else
    begin
     // draw background
     Brush.Color := Self.Color;
     {$IFNDEF FPC}if FTransparent then CopyParentImage(Self, FBuffer.Canvas) else{$ENDIF}
     FillRect(ClipRect);

     GlyphNr := GetGlyphNr;
     if (GlyphNr >= FGlyphCount) then GlyphNr := FGlyphCount - 1 else
     if (GlyphNr < 0) then GlyphNr := 0;

     if Assigned(FDialImageItem)
      then Bmp := FDialImageItem.FDialPixelMap
      else Bmp := PixelMap;

     if not Bmp.Empty then
      begin

       theRect := ClientRect;
       if FStitchKind = skVertical then
        begin
         theRect.Top    := Bmp.Height * GlyphNr div FGlyphCount;
         theRect.Bottom := Bmp.Height * (GlyphNr + 1) div FGlyphCount;
        end
       else
        begin
         theRect.Left  := Bmp.Width * GlyphNr div FGlyphCount;
         theRect.Right := Bmp.Width * (GlyphNr + 1) div FGlyphCount;
        end;

       with ClientRect do
        begin
         BitBlt(Handle, Left, Top, Right - Left, Bottom - Top,
           Bmp.Canvas.Handle, theRect.Left, theRect.Top, CopyMode);
        end;
      end else

     if Assigned(ImageList)
      then ImageList.Draw(FBuffer.Canvas, 0, 0, GlyphNr);
    end;
   Unlock;
  end;
end;
*)

function TCustomGuiStitchedControl.GetDialImageIndex: Integer;
begin
(*
 if Assigned(FDialImageItem)
  then Result := FDialImageItem.Index
  else Result := -1;
*)
end;

procedure TCustomGuiStitchedControl.DoAutoSize;
begin
 if Assigned(FImageList) then
  begin
   Width := FImageList.Width;
   Height := FImageList.Height;
   Exit;
  end;
// if FDialPixelMap.Empty or (FGlyphCount = 0) then Exit;

 if FStitchKind = skVertical then
  begin
   Width  := FDialPixelMap.Width;
   Height := FDialPixelMap.Height div FGlyphCount;
  end
 else
  begin
   Width  := FDialPixelMap.Width div FGlyphCount;
   Height := FDialPixelMap.Height;
  end;
end;

procedure TCustomGuiStitchedControl.SetAutoSize(const Value: boolean);
begin
  if FAutoSize <> Value then
  begin
    FAutoSize := Value;
    if Autosize then DoAutoSize;
  end;
end;

procedure TCustomGuiStitchedControl.SetGlyphCount(const Value: Integer);
begin
 if Assigned(FImageList) then exit;
 if FGlyphCount <> Value then
  begin
   FGlyphCount := Value;
   GlyphCountChanged;
  end;
end;

procedure TCustomGuiStitchedControl.GlyphCountChanged;
begin
 DoAutoSize;
end;

procedure TCustomGuiStitchedControl.Paint;
begin
 inherited;
 //
end;

procedure TCustomGuiStitchedControl.SetPixelMap(const Value: TGuiCustomPixelMap);
begin
  FDialPixelMap.Assign(Value);
  DoAutoSize;
end;

procedure TCustomGuiStitchedControl.SetDialImageIndex(Value: Integer);
begin
(*
 // check if dial image list is available
 if not Assigned(FDialImageList) then exit;

 // limit range to existing dial images
 if Value < 0 then Value := 0 else
 if Value >= FDialImageList.Count then Value := FDialImageList.Count - 1;

 if DialImageIndex <> Value then
  begin
   if Value >= 0
    then FDialImageList[Value].LinkStitchedControl(Self)
    else FDialImageItem.UnLinkStitchedControl(Self);
   FDialImageItem := FDialImageList[Value];
   Invalidate;
  end;
*)
end;

(*
procedure TCustomGuiStitchedControl.SetDialImageList(const Value: TGuiDialImageList);
begin
 if FDialImageList <> Value then
  begin
   FDialImageList := Value;
   if not Assigned(FDialImageList)
    then FDialImageItem := nil;
   Invalidate;
  end;
end;

procedure TCustomGuiStitchedControl.SetImageList(const Value: TImageList);
begin
 if FImageList <> Value then
  begin
   FImageList := Value;
   if Assigned(FImageList) then
    begin
     Width := FImageList.Width;
     Height := FImageList.Height;
     FGlyphCount := FImageList.Count;
    end;
   Invalidate;
  end;
end;
*)

procedure TCustomGuiStitchedControl.SetStitchKind(const Value: TGuiStitchKind);
begin
 if FStitchKind <> Value then
  begin
   FStitchKind := Value;
   StitchKindChanged;

  end;
end;

procedure TCustomGuiStitchedControl.StitchKindChanged;
begin
 DoAutoSize;
end;

end.
