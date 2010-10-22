unit DAV_GuiPaintBox;

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
  Classes, Graphics, Forms, SysUtils, Controls, DAV_GuiCommon, DAV_GuiPixelMap;

type
  TGuiCustomPaintBox = class(TGraphicControl)
  private
    FPixelMap: TGuiCustomPixelMap;
    FTransparent: Boolean;
    procedure SetPixelMap(const Value: TGuiCustomPixelMap);
    procedure SetTransparent(const Value: Boolean);
  protected
    procedure Paint; override;
    procedure TransparentChanged; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property PixelMap: TGuiCustomPixelMap read FPixelMap write SetPixelMap;
    property Transparent: Boolean read FTransparent write SetTransparent;
  end;

  TGuiPaintBox = class(TGuiCustomPaintBox)
  published
    property Anchors;
    property Align;
    property PixelMap;
    property OnClick;
  end;

implementation

{ TGuiCustomPaintBox }

constructor TGuiCustomPaintBox.Create(AOwner: TComponent);
begin
 inherited;
 ControlStyle := ControlStyle + [csOpaque];
 FPixelMap := TGuiPixelMapMemory.Create;
end;

destructor TGuiCustomPaintBox.Destroy;
begin
 FreeAndNil(FPixelMap);
 inherited;
end;

procedure TGuiCustomPaintBox.Paint;
begin
 inherited;
 FPixelMap.PaintTo(Canvas);
end;

procedure TGuiCustomPaintBox.SetPixelMap(const Value: TGuiCustomPixelMap);
begin
 FPixelMap.Assign(Value);
 Invalidate;
end;

procedure TGuiCustomPaintBox.SetTransparent(const Value: Boolean);
begin
 if FTransparent <> Value then
  begin
   FTransparent := Value;
   TransparentChanged;
  end;
end;

procedure TGuiCustomPaintBox.TransparentChanged;
begin
 Invalidate;
end;

end.

