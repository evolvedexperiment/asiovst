unit EditorForm;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2007-2011        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I ..\DAV_Compiler.inc}

uses
  {$IFDEF FPC}LCLIntf, LResources, Buttons, {$ELSE}Windows, Messages, XPMan,
  {$ENDIF}SysUtils, Classes, Graphics, Controls, Forms, ComCtrls, ExtCtrls,
  StdCtrls, ToolWin, Dialogs, Menus, DAV_Types, DAV_ASIOHost, DAV_VSTHost;

type
  TFmVSTEditor = class(TForm)
    ASIOHost: TASIOHost;
    BtSetup: TButton;
    BtExit: TButton;
    CBPreset: TComboBox;
    ToolBar: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    LbPreset: TLabel;
    VSTPanel: TPanel;
    VstHost: TVstHost;
    {$IFNDEF FPC}
    MILoadPreset: TMenuItem;
    MISavePreset: TMenuItem;
    OD: TOpenDialog;
    PUPreset: TPopupMenu;
    SD: TSaveDialog;
    {$ENDIF}
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormActivate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure ASIOHostBufferSwitch32(Sender: TObject; const InBuffer, OutBuffer: TDAVArrayOfSingleFixedArray);
    procedure ASIOHostReset(Sender: TObject);
    procedure BtSetupClick(Sender: TObject);
    procedure BtExitClick(Sender: TObject);
    procedure CBPresetChange(Sender: TObject);
    procedure MILoadPresetClick(Sender: TObject);
    procedure MISavePresetClick(Sender: TObject);
  private
    FVSTInBuffer         : array of PDAVSingleFixedArray;
    FVSTOutBuffer        : array of PDAVSingleFixedArray;
    FInputChannelOffset  : Integer;
    FOutputChannelOffset : Integer;
  public
    property InputChannelOffset: Integer read FInputChannelOffset write FInputChannelOffset;
    property OutputChannelOffset: Integer read FOutputChannelOffset write FOutputChannelOffset;
  end;

var
  FmVSTEditor: TFmVSTEditor;

implementation

{$IFNDEF FPC}
{$R *.dfm}
{$ENDIF}

uses
  IniFiles, DAV_VSTEffect, EditorSetup;

function EnumNamesFunc(hModule: THandle; lpType, lpName: PChar; lParam: DWORD): Boolean; stdcall;
begin
 Result := True;
 TStringList(lParam).Add(lpName);
end;

procedure TFmVSTEditor.FormCreate(Sender: TObject);
var
  theRect             : TRect;
  i                   : Integer;
  str                 : string;
  s, p                : AnsiString;
  ContainedVSTPlugins : TStringList;
  RS                  : TResourceStream;
begin
 with VstHost[0] do
  begin
   if ParamCount > 0
    then DLLFileName := ParamStr(1)
    else
     begin
      ContainedVSTPlugins := TStringList.Create;
      try
       EnumResourceNames(HInstance, 'DLL', @EnumNamesFunc, LongWord(ContainedVSTPlugins));

       if ContainedVSTPlugins.Count > 0 then
        begin
         RS := TResourceStream.Create(HInstance, ContainedVSTPlugins[0], 'DLL');
         try
          LoadFromStream(RS);
         finally
          FreeAndNil(RS);
         end;
        end else

       if not FileExists(DLLFileName) then
        with TOpenDialog.Create(Self) do
         try
          DefaultExt := 'dll';
          Filter := 'VST Plugin (*.dll)|*.dll';
          Options := Options + [ofFileMustExist];
          if Execute then DLLFileName := FileName;

          if not FileExists(DLLFileName) then
           begin
            Application.Terminate;
            Exit;
           end;

         finally
          Free;
         end;
      finally
       FreeAndNil(ContainedVSTPlugins);
      end;
     end;

   Active := True;
   Idle;
   ShowEdit(VSTPanel);
   Idle;
   EditIdle;
   Caption := string(GetVendorString + ' ' + GetEffectName);
  end;
 CBPreset.Clear;

 for i := 0 to VstHost[0].numPrograms - 1 do
  begin
   VstHost[0].GetProgramNameIndexed(-1, i, p);
   s := AnsiString(IntToStr(i));
   if i < 10 then s := '00' + s else
   if i < 100 then s := '0' + s;
   s := s + ' - ' + p;
   CBPreset.Items.Add(string(s));
  end;
 CBPreset.ItemIndex := 0;

 str := string(VstHost[0].GetProgramName);
 str := IntToStr(CBPreset.ItemIndex) + ' - ' + str;
 if CBPreset.ItemIndex < 10 then str := '00' + str else
 if CBPreset.ItemIndex < 100 then str := '0' + str;
 if (CBPreset.Text <> str) then
  begin
   CBPreset.Text := str;
   for i := 0 to VstHost[0].numPrograms - 1 do
    begin
     VstHost[0].CurrentProgram := i;
     str := string(VstHost[0].GetProgramName);
     str := IntToStr(i) + ' - ' + str;
     if i < 10 then str := '00' + str else
     if i < 100 then str := '0' + str;
     CBPreset.Items[i] := str;
    end;
   VstHost[0].CurrentProgram := 0;
   CBPreset.ItemIndex := 0;
  end;
 if (effFlagsHasEditor in VstHost[0].EffectOptions) then
  begin
   theRect := VstHost[0].GetRect;
   ClientWidth := theRect.Right - theRect.Left;
   ClientHeight := theRect.Bottom - theRect.Top + ToolBar.Height;
  end;
 SetLength(FVSTInBuffer, 2);
 SetLength(FVSTOutBuffer, 2);
 with TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'VSTEditor.INI') do
  try
   Top  := ReadInteger('Layout', 'Main Top', Top);
   Left := ReadInteger('Layout', 'Main Left', Left);
  finally
   Free;
  end;
end;

procedure TFmVSTEditor.FormActivate(Sender: TObject);
begin
 VstHost[0].EditActivate;
end;

procedure TFmVSTEditor.FormDeactivate(Sender: TObject);
begin
 VstHost[0].EditDeActivate;
end;

procedure TFmVSTEditor.FormDestroy(Sender: TObject);
var
  Channel: Integer;
begin
 for Channel := 0 to Length(FVSTInBuffer) - 1
  do Dispose(FVSTInBuffer[Channel]);
 for Channel := 0 to Length(FVSTOutBuffer) - 1
  do Dispose(FVSTOutBuffer[Channel]);
end;

procedure TFmVSTEditor.MILoadPresetClick(Sender: TObject);
begin
 with OD do
  if Execute then
   case FilterIndex of
    1 : VstHost[0].LoadPreset(FileName);
    2 : VstHost[0].LoadBank(FileName);
   end;
end;

procedure TFmVSTEditor.MISavePresetClick(Sender: TObject);
begin
 with SD do
  if Execute then
   case FilterIndex of
    1 : VstHost[0].SavePreset(FileName);
    2 : VstHost[0].SaveBank(FileName);
   end;
end;

procedure TFmVSTEditor.BtSetupClick(Sender: TObject);
begin
 FmSetup.Visible := not FmSetup.Visible;
end;

procedure TFmVSTEditor.BtExitClick(Sender: TObject);
begin
 Close;
end;

procedure TFmVSTEditor.CBPresetChange(Sender: TObject);
begin
 VstHost[0].CurrentProgram := CBPreset.ItemIndex;
end;

procedure TFmVSTEditor.ASIOHostBufferSwitch32(Sender: TObject; const InBuffer,
  OutBuffer: TDAVArrayOfSingleFixedArray);
begin
 if VSTHost[0].Active
  then VSTHost[0].Process32Replacing(@InBuffer[InputChannelOffset],
                                     @OutBuffer[OutputChannelOffset],
                                     ASIOHost.BufferSize);
end;

procedure TFmVSTEditor.ASIOHostReset(Sender: TObject);
var
  Channel: Integer;
begin
 VSTHost.BlockSize := ASIOHost.BufferSize;
 for Channel := 0 to Length(FVSTInBuffer) - 1
  do ReallocMem(FVSTInBuffer[Channel], VSTHost.BlockSize * SizeOf(Single));
 for Channel := 0 to Length(FVSTOutBuffer) - 1
  do ReallocMem(FVSTOutBuffer[Channel], VSTHost.BlockSize * SizeOf(Single));
end;

procedure TFmVSTEditor.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
 ASIOHost.Active := False;
 VSTHost[0].Active := False;
 Sleep(10);
 Application.ProcessMessages;
 ASIOHOST.Active := False;
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
  {$i EditorForm.lrs}
{$ENDIF}

end.
