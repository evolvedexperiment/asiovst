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

unit VAMain;

interface

{$I ..\DAV_Compiler.inc}

uses
  {$IFDEF FPC}LCLIntf, LResources, Buttons, {$ELSE}Windows, Messages, XPMan,
  {$ENDIF}SysUtils, Classes, Graphics, Controls, Forms, ComCtrls, ExtCtrls,
  StdCtrls, Menus, Dialogs, DAV_Types, DAV_VSTHost;

type
  TFormVSTAnalyser = class(TForm)
    MainMenu: TMainMenu;
    MenuItemFile: TMenuItem;
    MenuItemIR: TMenuItem;
    MILoad: TMenuItem;
    MIOpen: TMenuItem;
    MIPlotIR: TMenuItem;
    MenuItemPrograms: TMenuItem;
    MIQuit: TMenuItem;
    MIRenderIR: TMenuItem;
    MISave: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    OpenDialogVst: TOpenDialog;
    VstHost: TVstHost;
    PanelVstPlugin: TPanel;
{$IFNDEF FPC}
    XPManifest: TXPManifest;
{$ENDIF}
    procedure FormActivate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure MIQuitClick(Sender: TObject);
    procedure MIOpenClick(Sender: TObject);
    procedure MIPlotIRClick(Sender: TObject);
    procedure MILoadClick(Sender: TObject);
    procedure MISaveClick(Sender: TObject);
    procedure MIPresetClick(Sender: TObject);
    procedure PanelVstPluginClick(Sender: TObject);
  private
    VSTInBuffer: TDAVArrayOfSingleDynArray;
    VSTOutBuffer: TDAVArrayOfSingleDynArray;
    procedure LoadVSTPlugin(DLLName: TFileName);
  public
  end;

var
  FormVSTAnalyser: TFormVSTAnalyser;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  IniFiles, VAPlotIR;

procedure TFormVSTAnalyser.FormCreate(Sender: TObject);
begin
  if ParamCount > 0 then
    LoadVSTPlugin(ParamStr(1));
  with TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'VSTEditor.INI') do
    try
      Top := ReadInteger('Layout', 'Main Top', Top);
      Left := ReadInteger('Layout', 'Main Left', Left);
    finally
      Free;
    end;
end;

procedure TFormVSTAnalyser.FormActivate(Sender: TObject);
begin
  VstHost[0].EditActivate;
end;

procedure TFormVSTAnalyser.FormDeactivate(Sender: TObject);
begin
  VstHost[0].EditDeActivate;
end;

procedure TFormVSTAnalyser.MIOpenClick(Sender: TObject);
begin
  if OpenDialogVst.Execute then
    LoadVSTPlugin(OpenDialogVst.FileName);
end;

procedure TFormVSTAnalyser.MIPlotIRClick(Sender: TObject);
begin
  with VstHost[0] do
    if Active then
    begin
      VSTInBuffer[0, 0] := 1;
      FillChar(VSTInBuffer[0, 1], (VstHost.BlockSize - 1) * SizeOf(Single), 0);
      Process32Replacing(@VSTInBuffer[0], @VSTOutBuffer[0], VstHost.BlockSize);

      FormPlotIR.Waveform.SetWaveForm(VSTOutBuffer, True);
    end;
  FormPlotIR.ShowModal;
end;

procedure TFormVSTAnalyser.MIQuitClick(Sender: TObject);
begin
  Close;
end;

procedure TFormVSTAnalyser.MILoadClick(Sender: TObject);
begin
  ShowMessage('ToDo');
end;

procedure TFormVSTAnalyser.MISaveClick(Sender: TObject);
begin
  ShowMessage('ToDo');
end;

procedure TFormVSTAnalyser.PanelVstPluginClick(Sender: TObject);
begin
  if not VstHost[0].Active then
    MIOpenClick(Sender);
end;

procedure TFormVSTAnalyser.MIPresetClick(Sender: TObject);
begin
  with Sender as TMenuItem do
    VstHost[0].CurrentProgram := Tag;
end;

procedure TFormVSTAnalyser.LoadVSTPlugin(DLLName: TFileName);
var
  i: Integer;
  s: string;
  temp: AnsiString;
  MenuItem: TMenuItem;
begin
  with VstHost[0] do
  begin
    Active := False;
    DLLFileName := DLLName;
    Active := True;
    Idle;
    ShowEdit(TForm(PanelVstPlugin));
    Idle;
    EditIdle;
    Caption := GetVendorString + ' ' + GetEffectName;
    SetLength(VSTInBuffer, numInputs);
    SetLength(VSTOutBuffer, numOutputs);
    for i := 0 to numInputs - 1 do
      SetLength(VSTInBuffer[i], VstHost.BlockSize);
    for i := 0 to numOutputs - 1 do
      SetLength(VSTOutBuffer[i], VstHost.BlockSize);
  end;

  while MenuItemPrograms.Count > 3 do
    MenuItemPrograms.Delete(3);
  for i := 0 to VstHost[0].numPrograms - 1 do
  begin
    VstHost[0].GetProgramNameIndexed(-1, i, temp);
    s := IntToStr(i);
    if i < 10 then
      s := '00' + s
    else if i < 100 then
      s := '0' + s;
    s := s + ' - ' + temp;
    MenuItem := TMenuItem.Create(MenuItemPrograms);
    with MenuItem do
    begin
      Caption := s;
      Tag := i;
      OnClick := MIPresetClick;
    end;
    MenuItemPrograms.Add(MenuItem);
  end;

  with VstHost[0].GetRect do
  begin
    ClientWidth := Right - Left;
    ClientHeight := Bottom - Top;
  end;
end;

procedure TFormVSTAnalyser.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  with TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'VSTEditor.INI') do
    try
      WriteInteger('Layout', 'Main Top', Top);
      WriteInteger('Layout', 'Main Left', Left);
    finally
      Free;
    end;
end;

{$IFDEF FPC}

initialization

{$I VAMain.lrs}
{$ENDIF}

end.
