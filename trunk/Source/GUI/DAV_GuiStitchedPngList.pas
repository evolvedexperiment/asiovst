unit DAV_GuiStitchedPngList;

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
  {$IFDEF FPC} LCLIntf, LResources, LMessages, {$ELSE} Windows, Messages,
  {$ENDIF} Classes, Graphics, Forms, SysUtils, Controls, Contnrs,
  DAV_GuiCommon, DAV_GuiPixelMap, DAV_GuiStitchedControls, DAV_GuiPng;

type
  TGuiStitchedPNGCollectionItem = class(TGuiCustomStitchedCollectionItem)
  private
    FPng : TPortableNetworkGraphicPixel32;
    procedure SetPng(const Value: TPortableNetworkGraphicPixel32);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
  published
    property PortableNetworkGraphic: TPortableNetworkGraphicPixel32 read FPng write SetPng;
    property DisplayName;
    property GlyphCount;
    property StitchKind;
    property OnChange;
    property Height;
    property Width;
  end;

  TGuiStitchedPNGList = class(TGuiCustomStitchedList)
  private
    function GetItems(Index: Integer): TGuiStitchedPNGCollectionItem;
  protected
    function GetCount: Integer; override;
    property Items[Index: Integer]: TGuiStitchedPNGCollectionItem read GetItems; default;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property StitchedPNGs: TGuiStitchedImageCollection read FStitchedCollection write FStitchedCollection;
  end;


implementation

uses
  DAV_Common, DAV_GuiBlend;

resourcestring
  RCStrIndexOutOfBounds = 'Index out of bounds (%d)';


{ TGuiStitchedPNGCollectionItem }

constructor TGuiStitchedPNGCollectionItem.Create(Collection: TCollection);
begin
 inherited;
 FPng := TPortableNetworkGraphicPixel32.Create;
end;

destructor TGuiStitchedPNGCollectionItem.Destroy;
begin
 FreeAndNil(FPng);
 inherited;
end;

procedure TGuiStitchedPNGCollectionItem.SetPng(
  const Value: TPortableNetworkGraphicPixel32);
begin
 FPng.Assign(Value);
end;


{ TGuiStitchedPNGList }

constructor TGuiStitchedPNGList.Create(AOwner: TComponent);
begin
  inherited;
  FStitchedCollection := TGuiStitchedImageCollection.Create(Self, TGuiStitchedPngCollectionItem);
end;

destructor TGuiStitchedPNGList.Destroy;
begin
  FreeAndNil(FStitchedCollection);
  inherited;
end;

function TGuiStitchedPNGList.GetCount: Integer;
begin
  Result := FStitchedCollection.Count;
end;

function TGuiStitchedPNGList.GetItems(
  Index: Integer): TGuiStitchedPNGCollectionItem;
begin
 if (Index >= 0) and (Index < FStitchedCollection.Count)
  then Result := TGuiStitchedPNGCollectionItem(FStitchedCollection[Index])
  else raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);
end;

end.
