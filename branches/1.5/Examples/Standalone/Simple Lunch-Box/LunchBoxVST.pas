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
{  Portions created by Christian-W. Budde are Copyright (C) 2003-2013          }
{  by Christian-W. Budde. All Rights Reserved.                                 }
{                                                                              }
{******************************************************************************}

unit LunchBoxVST;

interface

{$I DAV_Compiler.inc}

uses
  {$IFDEF FPC}LCLIntf, LMessages, LResources, Buttons,
  {$ELSE} Windows, Messages, {$ENDIF} SysUtils, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls;

type
  TFormVST = class(TForm)
    ButtonOutputEditor: TButton;
    ButtonOutputVST: TButton;
    ButtonRealtimeEditor: TButton;
    ButtonRealtimeVST: TButton;
    EditOutputVST: TEdit;
    EditRealtimeVST: TEdit;
    GroupBoxOutputVST: TGroupBox;
    GroupBoxRealtimeVST: TGroupBox;
    LabelRealtimeVST: TLabel;
    LabelOutputVST: TLabel;
    procedure EditRealtimeVSTChange(Sender: TObject);
    procedure ButtonOutputEditorClick(Sender: TObject);
    procedure ButtonRealtimeEditorClick(Sender: TObject);
    procedure EditOutputVSTChange(Sender: TObject);
    procedure ButtonRealtimeVSTClick(Sender: TObject);
    procedure ButtonOutputVSTClick(Sender: TObject);
  end;

var
  FormVST: TFormVST;

implementation

uses
  LunchBoxMain;

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

procedure TFormVST.ButtonOutputVSTClick(Sender: TObject);
begin
  with TOpenDialog.Create(Self) do
    try
      DefaultExt := 'dll';
      Filter := 'VST Plugin (*.dll)|*.dll';
      Options := [ofHideReadOnly, ofFileMustExist, ofEnableSizing];
      Title := 'Select a VST Plugin';
      if Execute then
      begin
        EditOutputVST.Text := FileName;
      end;
    finally
      Free;
    end;
end;

procedure TFormVST.ButtonRealtimeVSTClick(Sender: TObject);
begin
  with TOpenDialog.Create(Self) do
    try
      DefaultExt := 'dll';
      Filter := 'VST Plugin (*.dll)|*.dll';
      Options := [ofHideReadOnly, ofFileMustExist, ofEnableSizing];
      Title := 'Select a VST Plugin';
      if Execute then
      begin
        EditRealtimeVST.Text := FileName;
      end;
    finally
      Free;
    end;
end;

procedure TFormVST.ButtonRealtimeEditorClick(Sender: TObject);
begin
  if FormLunchBox.VSTHost[0].Active then
    FormLunchBox.VSTHost[0].ShowEdit;
end;

procedure TFormVST.ButtonOutputEditorClick(Sender: TObject);
begin
  if FormLunchBox.VSTHost[1].Active then
    FormLunchBox.VSTHost[1].ShowEdit;
end;

procedure TFormVST.EditRealtimeVSTChange(Sender: TObject);
begin
  with FormLunchBox.VSTHost[0] do
  begin
    Active := False;
    if FileExists(EditRealtimeVST.Text) then
    begin
      DLLFileName := EditRealtimeVST.Text;
      Active := True;
    end;
    ButtonRealtimeEditor.Enabled := Active;
  end;
end;

procedure TFormVST.EditOutputVSTChange(Sender: TObject);
begin
  with FormLunchBox.VSTHost[1] do
  begin
    Active := False;
    if FileExists(EditOutputVST.Text) then
    begin
      DLLFileName := EditOutputVST.Text;
      Active := True;
    end;
    ButtonOutputEditor.Enabled := Active;
  end;
end;

end.
