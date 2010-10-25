unit DAV_GuiFontList;

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
  Graphics, Classes, SysUtils, Contnrs, DAV_Common, DAV_GuiCommon,
  DAV_GuiBlend, DAV_GuiPixelMap, DAV_GuiFont;

type
  // forward declarations
  TGuiCustomFontCollectionItem = class;

  TGuiFontImageCollection = class(TOwnedCollection)
  protected
    function GetItem(Index: Integer): TGuiCustomFontCollectionItem; virtual;
    procedure SetItem(Index: Integer; const Value: TGuiCustomFontCollectionItem); virtual;
    procedure Notify(Item: TCollectionItem; Action: TCollectionNotification); override;
    property Items[Index: Integer]: TGuiCustomFontCollectionItem read GetItem write SetItem; default;
  public
    constructor Create(AOwner: TPersistent; ItemClass: TCollectionItemClass);

    function Add: TGuiCustomFontCollectionItem;
    function Insert(Index: Integer): TGuiCustomFontCollectionItem;
    procedure Delete(Index: Integer);

    property Count;
  end;

  TGuiCustomFontCollectionItem = class(TCollectionItem)
  private
    FOnChange         : TNotifyEvent;
    FDisplayName      : string;
    FFont             : TGuiCustomFont;
    FFontClassChanged : TNotifyEvent;
    FLinkedControls   : TObjectList;
    function GetFontClassName: string;
    procedure SetFont(const Value: TGuiCustomFont);
    procedure SetFontClassName(const Value: string);
    procedure SettingsChanged(Sender: TObject);
  protected
    procedure Changed;
    procedure AssignTo(Dest: TPersistent); override;
    function GetDisplayName: string; override;
    procedure SetDisplayName(const Value: string); override;

(*
    procedure LinkFontControl(Font: TGuiCustomFontControl);
    procedure UnLinkFontControl(Font: TGuiCustomFontControl);
    procedure UnLinkFontControls;
*)
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;

    property DisplayName: string read GetDisplayName write SetDisplayName;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;

    property FontClassName: string read GetFontClassName write SetFontClassName;
    property Font: TGuiCustomFont read FFont write SetFont;
    property FontClassChanged: TNotifyEvent read FFontClassChanged write FFontClassChanged;
  end;

  TGuiFontImageCollectionItem = class(TGuiCustomFontCollectionItem)
  published
    property DisplayName;
    property OnChange;
  end;

  TGuiCustomFontList = class(TComponent)
  private
    function GetItems(Index: Integer): TGuiCustomFontCollectionItem;
  protected
    FFontCollection : TGuiFontImageCollection;
    function GetCount: Integer; virtual; abstract;
    property Items[Index: Integer]: TGuiCustomFontCollectionItem read GetItems; default;
  public
    property Count: Integer read GetCount;
  end;

  TGuiFontImageList = class(TGuiCustomFontList)
  private
    function GetItems(Index: Integer): TGuiFontImageCollectionItem;
  protected
    function GetCount: Integer; override;
    property Items[Index: Integer]: TGuiFontImageCollectionItem read GetItems; default;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property FontImages: TGuiFontImageCollection read FFontCollection write FFontCollection;
  end;

implementation

resourcestring
  RCStrIndexOutOfBounds = 'Index out of bounds (%d)';

{ TGuiFontImageCollection }

constructor TGuiFontImageCollection.Create(AOwner: TPersistent;
  ItemClass: TCollectionItemClass);
begin
 inherited Create(AOwner, ItemClass);
end;

function TGuiFontImageCollection.Add: TGuiCustomFontCollectionItem;
begin
 Result := TGuiCustomFontCollectionItem(inherited Add);
end;

procedure TGuiFontImageCollection.Delete(Index: Integer);
begin
 inherited Delete(Index);
end;

function TGuiFontImageCollection.GetItem(
  Index: Integer): TGuiCustomFontCollectionItem;
begin
 Result := TGuiCustomFontCollectionItem(inherited GetItem(Index));
end;

function TGuiFontImageCollection.Insert(
  Index: Integer): TGuiCustomFontCollectionItem;
begin
 Result:= TGuiCustomFontCollectionItem(inherited Insert(Index));
end;

procedure TGuiFontImageCollection.Notify(Item: TCollectionItem;
  Action: TCollectionNotification);
(*
var
  LinkedControlIndex : Integer;
*)
begin
(*
 if Action in [cnDeleting, cnExtracting] then
  if Item is TGuiCustomFontCollectionItem
   then TGuiCustomFontCollectionItem(Item).UnLinkFontControls;
*)
 inherited;
end;

procedure TGuiFontImageCollection.SetItem(Index: Integer;
  const Value: TGuiCustomFontCollectionItem);
begin
 inherited SetItem(Index, Value);
end;


{ TGuiCustomFontCollectionItem }

constructor TGuiCustomFontCollectionItem.Create(Collection: TCollection);
begin
 inherited;
 FLinkedControls := TObjectList.Create(False);
end;

destructor TGuiCustomFontCollectionItem.Destroy;
begin
// UnLinkFontControls;
 FreeAndNil(FLinkedControls);
 inherited;
end;

procedure TGuiCustomFontCollectionItem.AssignTo(Dest: TPersistent);
begin
 inherited;
 if Dest is TGuiCustomFontCollectionItem then
  with TGuiCustomFontCollectionItem(Dest) do
   begin
    FFont.Assign(Self.FFont);
    FDisplayName := Self.FDisplayName;
    FFontClassChanged := Self.FFontClassChanged;
   end;
end;

procedure TGuiCustomFontCollectionItem.Changed;
begin
 if Assigned(FFontClassChanged)
  then FFontClassChanged(Self);
end;

function TGuiCustomFontCollectionItem.GetDisplayName: string;
begin
 Result := FDisplayName;
end;

function TGuiCustomFontCollectionItem.GetFontClassName: string;
begin
 if Assigned(FFont)
  then Result := FFont.ClassName
  else Result := '';
end;

(*
procedure TGuiCustomFontCollectionItem.LinkFontControl(Font: TGuiCustomFontControl);
begin
 if FLinkedControls.IndexOf(Font) < 0 then
  begin
   FLinkedControls.Add(Font);
   case StitchKind of
    skHorizontal :
     begin
      Font.Width  := Width div GlyphCount;
      Font.Height := Height;
     end;
    skVertical :
     begin
      Font.Width  := Width;
      Font.Height := Height div GlyphCount;
     end;
   end;
  end;
end;

procedure TGuiCustomFontCollectionItem.UnLinkFontControl(Font: TGuiCustomFontControl);
begin
 if Assigned(Font)
  then FLinkedControls.Remove(Font);
end;

procedure TGuiCustomFontCollectionItem.UnLinkFontControls;
begin
 while FLinkedControls.Count > 0 do
  with TGuiCustomFontControl(FLinkedControls[0])
   do FontImageIndex := -1;
end;
*)

procedure TGuiCustomFontCollectionItem.SettingsChanged(Sender: TObject);
var
  Index : Integer;
begin
(*
 for Index := 0 to FLinkedControls.Count - 1 do
  with TGuiCustomFontControl(FLinkedControls[Index])
   do BufferChanged;
*)
end;

procedure TGuiCustomFontCollectionItem.SetDisplayName(const Value: string);
begin
 if FDisplayName <> Value then
  begin
   FDisplayName := Value;
   inherited;
  end;
end;

procedure TGuiCustomFontCollectionItem.SetFont(const Value: TGuiCustomFont);
begin
 FFont.Assign(Value);
end;

procedure TGuiCustomFontCollectionItem.SetFontClassName(const Value: string);
var
  FontClass: TGuiCustomFontClass;
begin
 if (Value <> '') and (FFont.ClassName <> Value) and Assigned(FontClassList) then
  begin
   FontClass := TGuiCustomFontClass(FontClassList.Find(Value));
   if Assigned(FontClass) then
    begin
     FFont.Free;
     FFont := FontClass.Create(nil);
     Changed;
    end;
  end;
end;


{ TGuiFontImageList }

constructor TGuiFontImageList.Create(AOwner: TComponent);
begin
  inherited;
  FFontCollection := TGuiFontImageCollection.Create(Self, TGuiFontImageCollectionItem);
end;

destructor TGuiFontImageList.Destroy;
begin
  FreeAndNil(FFontCollection);
  inherited;
end;

function TGuiFontImageList.GetCount: Integer;
begin
  Result := FFontCollection.Count;
end;

function TGuiFontImageList.GetItems(Index: Integer): TGuiFontImageCollectionItem;
begin
 if (Index >= 0) and (Index < FFontCollection.Count)
  then Result := TGuiFontImageCollectionItem(FFontCollection[Index])
  else raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);
end;


{ TGuiCustomFontList }

function TGuiCustomFontList.GetItems(
  Index: Integer): TGuiCustomFontCollectionItem;
begin
 Assert(Assigned(FFontCollection));
 if (Index >= 0) and (Index < FFontCollection.Count)
  then Result := TGuiCustomFontCollectionItem(FFontCollection[Index])
  else raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);
end;

end.
