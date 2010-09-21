unit DAV_GuiPixelMapDesign;

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
  {$IFDEF FPC} LCLIntf, LCLClasses, LCLType, LResources, LMessages, RtlConsts,
  LazIDEIntf, PropEdits, ComponentEditors, FormEditingIntf,
  {$IFDEF Windows} Windows, {$ENDIF}
  {$ELSE} Windows, Messages, DesignIntf, DesignEditors, VCLEditors, Registry,
  Clipbrd, Consts,{$ENDIF}
  Forms, Graphics, Classes, SysUtils, Dialogs, StdCtrls, Controls, ExtCtrls,
  DAV_Common, DAV_GuiCommon, DAV_GuiPixelMap;

type
  TFmPixelMapDialog = class(TForm)
    PaintBox: TPaintBox;
    PnToolbar: TPanel;
    BtOpen: TButton;
    BtSave: TButton;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    procedure PaintBoxPaint(Sender: TObject);
    procedure BtOpenClick(Sender: TObject);
    procedure BtSaveClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    FPixelMap : TGuiPixelMapMemory;
  public
    property PixelMap: TGuiPixelMapMemory read FPixelMap;
  end;

  TPixelMapEditorComponent = class(TComponent)
  private
    FPixelMap       : TGuiCustomPixelMap;
    FPixelMapDialog : TFmPixelMapDialog;
    procedure SetPixelMap(Value: TGuiCustomPixelMap);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute: Boolean;
    property PixelMap: TGuiCustomPixelMap read FPixelMap write SetPixelMap;
  end;

  TPixelMapProperty = class(TClassProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;

  TPixelMapEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

implementation

{$R *.dfm}

procedure TFmPixelMapDialog.FormCreate(Sender: TObject);
begin
 FPixelMap := TGuiPixelMapMemory.Create;
end;

procedure TFmPixelMapDialog.FormDestroy(Sender: TObject);
begin
 FreeAndNil(FPixelMap);
end;

procedure TFmPixelMapDialog.FormResize(Sender: TObject);
begin
 if Assigned(FPixelMap)
  then FPixelMap.SetSize(PaintBox.Width, PaintBox.Height);
end;

procedure TFmPixelMapDialog.BtOpenClick(Sender: TObject);
begin
 with OpenDialog do
  begin

   if Execute
    then FPixelMap.LoadFromFile(FileName);
  end;
end;

procedure TFmPixelMapDialog.BtSaveClick(Sender: TObject);
begin
 with SaveDialog do
  begin

   if Execute
    then FPixelMap.SaveToFile(FileName);
  end;
end;

procedure TFmPixelMapDialog.PaintBoxPaint(Sender: TObject);
begin
 if Assigned(FPixelMap)
  then FPixelMap.PaintTo(PaintBox.Canvas);
end;


{ TPixelMapEditorComponent }

constructor TPixelMapEditorComponent.Create(AOwner: TComponent);
begin
 inherited;
 FPixelMap := TGuiPixelMapMemory.Create;
 FPixelMapDialog := TFmPixelMapDialog.Create(Self);
end;

destructor TPixelMapEditorComponent.Destroy;
begin
 FreeAndNil(FPixelMap);
 inherited;
end;

function TPixelMapEditorComponent.Execute: Boolean;
begin
 FPixelMapDialog.PixelMap.Assign(FPixelMap);
 Result := (FPixelMapDialog.ShowModal = mrOK);
 if Result
  then FPixelMap.Assign(FPixelMapDialog.PixelMap);
end;

procedure TPixelMapEditorComponent.SetPixelMap(Value: TGuiCustomPixelMap);
begin
 try
  FPixelMap.Assign(Value);
 except
  on E: Exception do ShowMessage(E.Message);
 end;
end;


{ TPixelMapProperty }

procedure TPixelMapProperty.Edit;
var
  PixelMapEditor: TPixelMapEditorComponent;
begin
 try
  PixelMapEditor := TPixelMapEditorComponent.Create(nil);
  try
   PixelMapEditor.PixelMap := TGuiCustomPixelMap(Pointer(GetOrdValue));
   if PixelMapEditor.Execute then
    begin
     SetOrdValue(Longint(PixelMapEditor.PixelMap));
     {$IFNDEF FPC} Designer.Modified; {$ENDIF}
    end;
  finally
   PixelMapEditor.Free;
  end;
 except
  on E: Exception do ShowMessage(E.Message);
 end;
end;

function TPixelMapProperty.GetAttributes: TPropertyAttributes;
begin
 Result := [paDialog, paSubProperties];
end;

function TPixelMapProperty.GetValue: string;
var
  PixelMap: TGuiCustomPixelMap;
begin
 try
  PixelMap := TGuiCustomPixelMap(GetOrdValue);
  if (PixelMap = nil) then Result := srNone
  else Result := Format('%s [%d,%d]', [PixelMap.ClassName, PixelMap.Width, PixelMap.Height]);
 except
  on E: Exception do ShowMessage(E.Message);
 end;
end;

procedure TPixelMapProperty.SetValue(const Value: string);
begin
  if Value = '' then SetOrdValue(0);
end;


{ TPixelMapEditor }

procedure TPixelMapEditor.ExecuteVerb(Index: Integer);
begin
 inherited;
 // yet todo
end;

function TPixelMapEditor.GetVerb(Index: Integer): string;
begin
 if Index = 0 then Result := 'PixelMap Editor...';
end;

function TPixelMapEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

end.
