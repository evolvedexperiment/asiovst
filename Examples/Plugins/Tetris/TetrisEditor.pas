unit TetrisEditor;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2009-2019        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I DAV_Compiler.inc}

uses
  {$IFDEF FPC}LCLIntf, LResources, LCLType, {$ELSE} Windows, Types, {$ENDIF}
  SysUtils, Classes, Forms, TetrisUnit, Controls, StdCtrls, ExtCtrls, Graphics,
  DAV_Types, DAV_VSTModule;

type
  TFmTetris = class(TForm)
    TetrisTimer: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormPaint(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure TetrisOnTimer(Sender: TObject);
  private
    FBitmap: TBitmap;
    FTetris: TTetris;
  public
    property Tetris: TTetris read FTetris;
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

procedure TFmTetris.FormCreate(Sender: TObject);
begin
  FTetris := TTetris.Create;
  FBitmap := TBitmap.Create;
end;

procedure TFmTetris.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FTetris);
  FreeAndNil(FBitmap);
end;

procedure TFmTetris.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_SPACE then
    FTetris.StepGame;
  if Key = VK_LEFT then
    FTetris.Left;
  if Key = VK_right then
    FTetris.right;
  if Key = VK_up then
    FTetris.Rotate;
  if Key = VK_DOWN then
    FTetris.StepGame;
  FormPaint(nil);
end;

procedure TFmTetris.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if ssleft in Shift then
    FTetris.Left;
  if ssright in Shift then
    FTetris.right;
  if ssMiddle in Shift then
    FTetris.Rotate;
  FormPaint(nil);
end;

procedure TFmTetris.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  FTetris.Rotate;
  FormPaint(nil);
end;

procedure TFmTetris.FormPaint(Sender: TObject);
begin
  FTetris.DefaultBitmap(FBitmap);
  Canvas.StretchDraw(clientrect, FBitmap);
  Caption := 'lines ' + inttostr(FTetris.Lines);
end;

procedure TFmTetris.TetrisOnTimer(Sender: TObject);
begin
  FTetris.StepGame;
  FormPaint(nil);
  TetrisTimer.Interval := TrimInt(1000 - FTetris.Lines * 10, 100, 1000);
end;

end.
