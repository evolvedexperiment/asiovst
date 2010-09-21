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

  // forward declarations
  TGuiStitchedImageCollectionItem = class;
  TGuiCustomStitchedControl = class;

  TGuiStitchedImageCollection = class(TOwnedCollection)
  protected
    function GetItem(Index: Integer): TGuiStitchedImageCollectionItem; virtual;
    procedure SetItem(Index: Integer; const Value: TGuiStitchedImageCollectionItem); virtual;
    procedure Notify(Item: TCollectionItem; Action: TCollectionNotification); override;
    property Items[Index: Integer]: TGuiStitchedImageCollectionItem read GetItem write SetItem; default;
  public
    constructor Create(AOwner: TComponent);
    function Add: TGuiStitchedImageCollectionItem;
    function Insert(Index: Integer): TGuiStitchedImageCollectionItem;
    procedure Delete(Index: Integer);
    property Count;
  end;

  TGuiStitchedImageCollectionItem = class(TCollectionItem)
  private
    FStitchedPixelMap : TGuiCustomPixelMap;
    FGlyphCount       : Integer;
    FOnChange         : TNotifyEvent;
    FStitchKind       : TGuiStitchKind;
    FLinkedStitcheds  : TObjectList;
    FDisplayName      : string;
    function GetHeight: Integer;
    function GetWidth: Integer;
    procedure SetStitchedPixelMap(const Value: TGuiCustomPixelMap);
    procedure SetGlyphCount(const Value: Integer);
    procedure SetStitchKind(const Value: TGuiStitchKind);
    procedure SettingsChanged(Sender: TObject);
    procedure SetHeight(const Value: Integer);
    procedure SetWidth(const Value: Integer);
  protected
    procedure GlyphCountChanged; virtual;
    procedure StitchKindChanged; virtual;

    function GetDisplayName: string; override;
    procedure SetDisplayName(const Value: string); override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure LinkStitchedControl(Stitched: TGuiCustomStitchedControl);
    procedure UnLinkStitchedControl(Stitched: TGuiCustomStitchedControl);
  published
    property DisplayName: string read GetDisplayName write SetDisplayName;
    property StitchedPixelMap: TGuiCustomPixelMap read FStitchedPixelMap write SetStitchedPixelMap;
    property GlyphCount: Integer read FGlyphCount write SetGlyphCount default 1;
    property StitchKind: TGuiStitchKind read FStitchKind write SetStitchKind;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Height: Integer read GetHeight write SetHeight;
    property Width: Integer read GetWidth write SetWidth;
  end;

  TGuiCustomStitchedList = class(TComponent)
  protected
    function GetCount: Integer; virtual; abstract;
  public
    property Count: Integer read GetCount;
  end;

  TGuiStitchedImageList = class(TGuiCustomStitchedList)
  private
    FStitchedImageCollection : TGuiStitchedImageCollection;
    function GetItems(Index: Integer): TGuiStitchedImageCollectionItem;
  protected
    function GetCount: Integer; override;
    property Items[Index: Integer]: TGuiStitchedImageCollectionItem read GetItems; default;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property StitchedImages: TGuiStitchedImageCollection read FStitchedImageCollection write FStitchedImageCollection;
  end;

  TGuiStitchedPNGList = class(TGuiCustomStitchedList);


  TGuiCustomStitchedControl = class(TCustomControl)
  private
    function GetStitchedIndex: Integer;
    procedure DoAutoSize;
    procedure SetAutoSize(const Value: Boolean); reintroduce;
    procedure SetGlyphCount(const Value: Integer);
    procedure SetStitchKind(const Value: TGuiStitchKind);
    procedure SetStitchedIndex(Value: Integer);
    procedure SetStitchedList(const Value: TGuiStitchedImageList);
  protected
    FAutoSize     : Boolean;
    FBuffer       : TGuiCustomPixelMap;
    FGlyphCount   : Integer;
    FOnChange     : TNotifyEvent;
    FStitchKind   : TGuiStitchKind;
    FStitchedList : TGuiStitchedImageList;
    FStitchedItem : TGuiStitchedImageCollectionItem;

    function GetGlyphIndex: Integer; virtual; abstract;
    procedure GlyphCountChanged; virtual;
    procedure StitchKindChanged; virtual;
//    procedure UpdateBuffer; override;

    procedure Render(const Pixmap: TGuiCustomPixelMap); virtual; abstract;
    procedure Paint; override;

    property GlyphCount: Integer read FGlyphCount write SetGlyphCount default 1;

    property Buffer: TGuiCustomPixelMap read FBuffer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property AutoSize: Boolean read FAutoSize write SetAutoSize default False;
    property StitchedImageIndex: Integer read GetStitchedIndex write SetStitchedIndex;
    property StitchedImageList: TGuiStitchedImageList read FStitchedList write SetStitchedList;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property StitchKind: TGuiStitchKind read FStitchKind write SetStitchKind;
  end;

implementation

{ TGuiStitchedImageCollection }

constructor TGuiStitchedImageCollection.Create(AOwner: TComponent);
begin
 inherited Create(AOwner, TGuiStitchedImageCollectionItem);
end;

function TGuiStitchedImageCollection.Add: TGuiStitchedImageCollectionItem;
begin
 Result := TGuiStitchedImageCollectionItem(inherited Add);
end;

procedure TGuiStitchedImageCollection.Delete(Index: Integer);
begin
 inherited Delete(Index);
end;

function TGuiStitchedImageCollection.GetItem(
  Index: Integer): TGuiStitchedImageCollectionItem;
begin
 Result := TGuiStitchedImageCollectionItem(inherited GetItem(Index));
end;

function TGuiStitchedImageCollection.Insert(
  Index: Integer): TGuiStitchedImageCollectionItem;
begin
 Result:= TGuiStitchedImageCollectionItem(inherited Insert(Index));
end;

procedure TGuiStitchedImageCollection.Notify(Item: TCollectionItem;
  Action: TCollectionNotification);
begin
 inherited;
 // add things that depend on the order here!
end;

procedure TGuiStitchedImageCollection.SetItem(Index: Integer;
  const Value: TGuiStitchedImageCollectionItem);
begin
 inherited SetItem(Index, Value);
end;

{ TGuiStitchedImageCollectionItem }

constructor TGuiStitchedImageCollectionItem.Create(Collection: TCollection);
begin
 inherited;
 FGlyphCount                := 1;
 FStitchedPixelMap          := TGuiPixelMapMemory.Create;
 FStitchedPixelMap.OnResize := SettingsChanged;
 FLinkedStitcheds           := TObjectList.Create(False);
end;

destructor TGuiStitchedImageCollectionItem.Destroy;
begin
 FreeAndNil(FStitchedPixelMap);
 FreeAndNil(FLinkedStitcheds);
 inherited;
end;

function TGuiStitchedImageCollectionItem.GetDisplayName: string;
begin
 Result := FDisplayName;
end;

function TGuiStitchedImageCollectionItem.GetHeight: Integer;
begin
 Result := FStitchedPixelMap.Height;
end;

function TGuiStitchedImageCollectionItem.GetWidth: Integer;
begin
 Result := FStitchedPixelMap.Width;
end;

procedure TGuiStitchedImageCollectionItem.LinkStitchedControl(Stitched: TGuiCustomStitchedControl);
begin
 if FLinkedStitcheds.IndexOf(Stitched) < 0 then
  begin
   FLinkedStitcheds.Add(Stitched);
   Stitched.GlyphCount := GlyphCount;
   Stitched.StitchKind := StitchKind;
   case StitchKind of
    skHorizontal :
     begin
      Stitched.Width  := Width div GlyphCount;
      Stitched.Height := Height;
     end;
    skVertical :
     begin
      Stitched.Width  := Width;
      Stitched.Height := Height div GlyphCount;
     end;
   end;
  end;
end;

procedure TGuiStitchedImageCollectionItem.UnLinkStitchedControl(Stitched: TGuiCustomStitchedControl);
begin
 FLinkedStitcheds.Remove(Stitched);
end;

procedure TGuiStitchedImageCollectionItem.SettingsChanged(Sender: TObject);
var
  i : Integer;
begin
 for i := 0 to FLinkedStitcheds.Count - 1 do
  with TGuiCustomStitchedControl(FLinkedStitcheds[i]) do
   begin
    GlyphCount := Self.GlyphCount;
    StitchKind := Self.StitchKind;
    Invalidate;
   end;
end;

procedure TGuiStitchedImageCollectionItem.SetWidth(const Value: Integer);
begin
 if Value < 0 then Exit;
 FStitchedPixelMap.Width := Value;
end;

procedure TGuiStitchedImageCollectionItem.SetDisplayName(const Value: string);
begin
 if FDisplayName <> Value then
  begin
   FDisplayName := Value;
   inherited;
  end;
end;

procedure TGuiStitchedImageCollectionItem.SetHeight(const Value: Integer);
begin
 if Value < 0 then Exit;
 FStitchedPixelMap.Height := Value;
end;

procedure TGuiStitchedImageCollectionItem.GlyphCountChanged;
var
  i : Integer;
begin
 for i := 0 to FLinkedStitcheds.Count - 1
  do TGuiCustomStitchedControl(FLinkedStitcheds[i]).GlyphCount := GlyphCount;
end;

procedure TGuiStitchedImageCollectionItem.StitchKindChanged;
var
  i : Integer;
begin
 for i := 0 to FLinkedStitcheds.Count - 1
  do TGuiCustomStitchedControl(FLinkedStitcheds[i]).StitchKind := StitchKind;
end;

procedure TGuiStitchedImageCollectionItem.SetGlyphCount(const Value: Integer);
begin
(*
 if Value <= 0
  then raise Exception.Create(RCStrGlyphCountMustBePositive);
*)

 if FGlyphCount <> Value then
  begin
   FGlyphCount := Value;
   GlyphCountChanged;
  end;
end;

procedure TGuiStitchedImageCollectionItem.SetStitchedPixelMap(
  const Value: TGuiCustomPixelMap);
begin
 //
end;

procedure TGuiStitchedImageCollectionItem.SetStitchKind(const Value: TGuiStitchKind);
begin
 if FStitchKind <> Value then
  begin
   FStitchKind := Value;
   StitchKindChanged;
  end;
end;


{ TGuiStitchedImageList }

constructor TGuiStitchedImageList.Create(AOwner: TComponent);
begin
  inherited;
  FStitchedImageCollection := TGuiStitchedImageCollection.Create(Self);
end;

destructor TGuiStitchedImageList.Destroy;
begin
  FreeAndNil(FStitchedImageCollection);
  inherited;
end;

function TGuiStitchedImageList.GetCount: Integer;
begin
  Result := FStitchedImageCollection.Count;
end;

function TGuiStitchedImageList.GetItems(Index: Integer): TGuiStitchedImageCollectionItem;
begin
 if (Index >= 0) and (Index < FStitchedImageCollection.Count)
  then Result := FStitchedImageCollection[Index]
  else raise Exception.CreateFmt('Index out of bounds (%d)', [Index]);
end;


{ TGuiCustomStitchedControl }

constructor TGuiCustomStitchedControl.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
 FBuffer     := TGuiCustomPixelMap.Create;
 FGlyphCount := 1;
 FStitchKind := skHorizontal;
end;

destructor TGuiCustomStitchedControl.Destroy;
begin
 FreeAndNil(FBuffer);
 inherited Destroy;
end;

(*
procedure TGuiCustomStitchedControl.UpdateBuffer;
var
  theRect   : TRect;
  GlyphIndex   : Integer;
  Bmp       : TGuiCustomPixelMap;
  OwnerDraw : Boolean;
begin
 if [csLoading..csDestroying] * ComponentState <> [] then exit;

 if (Width > 0) and (Height > 0) then with FBuffer.Canvas do
  begin
   Lock;
   Brush.Color := Self.Color;
   OwnerDraw := FBuffer.Empty and not Assigned(FImageList);
   if OwnerDraw and Assigned(FStitchedList) and Assigned(FStitchedItem)
    then OwnerDraw := FStitchedItem.FBuffer.Empty;

   if OwnerDraw then
    if AntiAlias = gaaNone then
     begin
      // draw background
      {$IFNDEF FPC}if FTransparent then CopyParentImage(Self, FBuffer.Canvas) else{$ENDIF}
      FillRect(ClipRect);

      RenderPixelMap(FBuffer);
     end
    else
     begin
      Bmp := TGuiCustomPixelMap.Create;
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
          UpsamplePixelMap(Bmp);
         end else
        {$ENDIF}
        Canvas.FillRect(Canvas.ClipRect);
        RenderPixelMap(Bmp);
        DownsamplePixelMap(Bmp);
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

     GlyphIndex := GetGlyphIndex;
     if (GlyphIndex >= FGlyphCount) then GlyphIndex := FGlyphCount - 1 else
     if (GlyphIndex < 0) then GlyphIndex := 0;

     if Assigned(FStitchedItem)
      then Bmp := FStitchedItem.FBuffer
      else Bmp := PixelMap;

     if not Bmp.Empty then
      begin

       theRect := ClientRect;
       if FStitchKind = skVertical then
        begin
         theRect.Top    := Bmp.Height * GlyphIndex div FGlyphCount;
         theRect.Bottom := Bmp.Height * (GlyphIndex + 1) div FGlyphCount;
        end
       else
        begin
         theRect.Left  := Bmp.Width * GlyphIndex div FGlyphCount;
         theRect.Right := Bmp.Width * (GlyphIndex + 1) div FGlyphCount;
        end;

       with ClientRect do
        begin
         BitBlt(Handle, Left, Top, Right - Left, Bottom - Top,
           Bmp.Canvas.Handle, theRect.Left, theRect.Top, CopyMode);
        end;
      end;
    end;
   Unlock;
  end;
end;
*)

function TGuiCustomStitchedControl.GetStitchedIndex: Integer;
begin
 if Assigned(FStitchedItem)
  then Result := FStitchedItem.Index
  else Result := -1;
end;

procedure TGuiCustomStitchedControl.DoAutoSize;
begin
// if FBuffer.Empty or (FGlyphCount = 0) then Exit;

 if FStitchKind = skVertical then
  begin
   Width  := FBuffer.Width;
   Height := FBuffer.Height div FGlyphCount;
  end
 else
  begin
   Width  := FBuffer.Width div FGlyphCount;
   Height := FBuffer.Height;
  end;
end;

procedure TGuiCustomStitchedControl.SetAutoSize(const Value: boolean);
begin
  if FAutoSize <> Value then
  begin
    FAutoSize := Value;
    if Autosize then DoAutoSize;
  end;
end;

procedure TGuiCustomStitchedControl.SetGlyphCount(const Value: Integer);
begin
 if FGlyphCount <> Value then
  begin
   FGlyphCount := Value;
   GlyphCountChanged;
  end;
end;

procedure TGuiCustomStitchedControl.GlyphCountChanged;
begin
 DoAutoSize;
end;

procedure TGuiCustomStitchedControl.Paint;
begin
 inherited;
 //
end;

(*
procedure TGuiCustomStitchedControl.SetPixelMap(const Value: TGuiCustomPixelMap);
begin
  FBuffer.Assign(Value);
  DoAutoSize;
end;
*)

procedure TGuiCustomStitchedControl.SetStitchedIndex(Value: Integer);
begin
 // check if Stitched image list is available
 if not Assigned(FStitchedList) then exit;

 // limit range to existing Stitched images
 if Value < 0 then Value := 0 else
 if Value >= FStitchedList.Count then Value := FStitchedList.Count - 1;

 if StitchedImageIndex <> Value then
  begin
   if Value >= 0
    then FStitchedList[Value].LinkStitchedControl(Self)
    else FStitchedItem.UnLinkStitchedControl(Self);
   FStitchedItem := FStitchedList[Value];
   Invalidate;
  end;
end;

procedure TGuiCustomStitchedControl.SetStitchedList(const Value: TGuiStitchedImageList);
begin
 if FStitchedList <> Value then
  begin
   FStitchedList := Value;
   if not Assigned(FStitchedList)
    then FStitchedItem := nil;
   Invalidate;
  end;
end;

procedure TGuiCustomStitchedControl.SetStitchKind(const Value: TGuiStitchKind);
begin
 if FStitchKind <> Value then
  begin
   FStitchKind := Value;
   StitchKindChanged;
  end;
end;

procedure TGuiCustomStitchedControl.StitchKindChanged;
begin
 DoAutoSize;
end;

end.
