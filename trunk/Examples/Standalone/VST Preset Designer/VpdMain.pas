unit VpdMain;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2007-2009        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I DAV_Compiler.inc}

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, ToolWin, ComCtrls, ExtCtrls, DAV_Types, DAV_VSTHost,
  DAV_MidiFile;

type
  TFmVstPresetDesigner = class(TForm)
    MainMenu: TMainMenu;
    MiDesign: TMenuItem;
    MiExit: TMenuItem;
    MiLoad: TMenuItem;
    MiOpen: TMenuItem;
    MiOpenAudio: TMenuItem;
    MiOpenMIDI: TMenuItem;
    MiPlayPreview: TMenuItem;
    MiPreview: TMenuItem;
    MiPrograms: TMenuItem;
    MiRandomize: TMenuItem;
    MiSave: TMenuItem;
    MIShuffle: TMenuItem;
    MIVstPlugin: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    N3: TMenuItem;
    N4: TMenuItem;
    N5: TMenuItem;
    OpenAudio: TOpenDialog;
    OpenDialog: TOpenDialog;
    OpenMidi: TOpenDialog;
    SaveDialog: TSaveDialog;
    ToolBar: TToolBar;
    VstHost: TVstHost;
    VSTPanel: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MiExitClick(Sender: TObject);
    procedure MiOpenClick(Sender: TObject);
    procedure MIPresetClick(Sender: TObject);
    procedure MiSaveClick(Sender: TObject);
    procedure MiRandomizeClick(Sender: TObject);
    procedure MIShuffleClick(Sender: TObject);
    procedure MiOpenAudioClick(Sender: TObject);
    procedure MiOpenMIDIClick(Sender: TObject);
    procedure MiDesignClick(Sender: TObject);
  private
    FIniFile   : TFileName;
    FMidiFile  : TMidiFile;
    FAudioFile : TFileName;
    FInBuffer  : TDAVArrayOfSingleFixedArray;
    FOutBuffer : TDAVArrayOfSingleFixedArray;
    FBufferSize: Integer;
    procedure LoadVSTPlugin(DLLName: TFileName);
    procedure SetBufferSize(const Value: Integer);
  protected
    procedure BufferSizeChanged; virtual;
  public
    property BufferSize: Integer read FBufferSize write SetBufferSize;
  end;

var
  FmVstPresetDesigner: TFmVstPresetDesigner;

implementation

{$R *.dfm}

uses
  Inifiles, DAV_Common; //, VpdModifier;

procedure TFmVstPresetDesigner.FormCreate(Sender: TObject);
var
  VstFileName: TFileName;
begin
 FIniFile := ExtractFilePath(ParamStr(0)) + 'VST Preset Designer.ini';

 with TIniFile.Create(FIniFile) do
  try
   Top := ReadInteger('Layout', 'Main Top', Top);
   Left := ReadInteger('Layout', 'Main Left', Left);
   VstFileName := ReadString('Program', 'Last VST Plugin', '');

   if FileExists(VstFileName)
    then LoadVSTPlugin(VstFileName);
  finally
   Free;
  end;
end;

procedure TFmVstPresetDesigner.FormDestroy(Sender: TObject);
begin
 with TIniFile.Create(FIniFile) do
  try
   WriteString('Program', 'Last VST Plugin', VstHost[0].DLLFileName);
  finally
   Free;
  end;
end;

procedure TFmVstPresetDesigner.MiOpenClick(Sender: TObject);
begin
 with OpenDialog do
  begin
   // restore VST Plugin Directory
   with TIniFile.Create(FIniFile) do
    try
     InitialDir := ReadString('Program', 'VST Plugin Directory', InitialDir);
    finally
     Free;
    end;

   if Execute then
    begin
     LoadVSTPlugin(FileName);

     // Store VST Plugin Directory
     with TIniFile.Create(FIniFile) do
      try
       WriteString('Program', 'VST Plugin Directory',
         ExtractFileDir(OpenDialog.FileName));
      finally
       Free;
      end;
    end;
  end;
end;

procedure TFmVstPresetDesigner.MiOpenMIDIClick(Sender: TObject);
begin
 with OpenDialog do
  begin
   // restore MIDI Directory
   with TIniFile.Create(FIniFile) do
    try
     InitialDir := ReadString('Program', 'Preview MIDI Directory', InitialDir);
    finally
     Free;
    end;

   if Execute then
    begin
     FAudioFile := FileName;
     MiOpen.Caption := 'Open &Audio... (' + ExtractFileName(FileName) + ')';

     // Store MIDI Directory
     with TIniFile.Create(FIniFile) do
      try
       WriteString('Program', 'Preview MIDI Directory',
         ExtractFileDir(OpenDialog.FileName));
      finally
       Free;
      end;
    end;
  end;
end;

procedure TFmVstPresetDesigner.MiOpenAudioClick(Sender: TObject);
begin
 with OpenDialog do
  begin
   // restore Audio Directory
   with TIniFile.Create(FIniFile) do
    try
     InitialDir := ReadString('Program', 'Preview Audio Directory', InitialDir);
    finally
     Free;
    end;

   if Execute then
    begin
     if assigned(FMidiFile)
      then FreeAndNil(FMidiFile);
     
     FMidiFile := TMidiFile.Create(Self);
     FMidiFile.Filename := FileName;

     MiOpen.Caption := 'Open &MIDI... (' + ExtractFileName(FileName) + ')';

     // Store Audio Directory
     with TIniFile.Create(FIniFile) do
      try
       WriteString('Program', 'Preview Audio Directory',
         ExtractFileDir(OpenDialog.FileName));
      finally
       Free;
      end;
    end;
  end;
end;

procedure TFmVstPresetDesigner.MiSaveClick(Sender: TObject);
begin
 with SaveDialog do
  begin
   // restore VST Preset/Bank Directory
   with TIniFile.Create(FIniFile) do
    try
     InitialDir := ReadString('Program', 'VST Preset/Bank Directory', InitialDir);
    finally
     Free;
    end;

   if Execute then
    begin
     case SaveDialog.FilterIndex of
      1 : VstHost[0].SaveBank(SaveDialog.FileName);
      2 : VstHost[0].SavePreset(SaveDialog.FileName);
     end;

     // Store VST Plugin Directory
     with TIniFile.Create(FIniFile) do
      try
       WriteString('Program', 'VST Preset/Bank Directory',
         ExtractFileDir(SaveDialog.FileName));
      finally
       Free;
      end;
    end;
  end;
end;

procedure TFmVstPresetDesigner.MiRandomizeClick(Sender: TObject);
var
  CurrentProg : Integer;
  Prog, Param : Integer;
begin
 CurrentProg := VstHost[0].CurrentProgram;
 for Prog := 0 to VstHost[0].numPrograms - 1 do
  begin
   VstHost[0].CurrentProgram := Prog;
   for Param := 0 to VstHost[0].numParams - 1
    do VstHost[0].Parameter[Param] := Random;
  end;
 VstHost[0].CurrentProgram := CurrentProg;
end;

procedure TFmVstPresetDesigner.MIShuffleClick(Sender: TObject);
var
  CurrentProg : Integer;
  Prog, Param : Integer;
  OldParamVal : Single;
  NewParamVal : Single;
  Trial       : Integer;
begin
 CurrentProg := VstHost[0].CurrentProgram;
 for Prog := 0 to VstHost[0].numPrograms - 1 do
  begin
   VstHost[0].CurrentProgram := Prog;
   for Param := 0 to VstHost[0].numParams - 1 do
    begin
     OldParamVal := VstHost[0].Parameter[Param];
     Trial := 0;
     repeat
      NewParamVal := Limit(VstHost[0].Parameter[Param] + 0.01 * Random, 0, 1);
      inc(Trial);
     until (NewParamVal <> OldParamVal) or (Trial > 15);

     VstHost[0].Parameter[Param] := NewParamVal;
    end;
  end;
 VstHost[0].CurrentProgram := CurrentProg;
end;

procedure TFmVstPresetDesigner.MiDesignClick(Sender: TObject);
begin
(*
 if FmModifier.ShowModal = mrOK then
  begin
  end;
*)
end;

procedure TFmVstPresetDesigner.MiExitClick(Sender: TObject);
begin
 Close;
end;

procedure TFmVstPresetDesigner.MIPresetClick(Sender: TObject);
begin
 with Sender as TMenuItem
  do VstHost[0].CurrentProgram := Tag;
end;

procedure TFmVstPresetDesigner.LoadVSTPlugin(DLLName : TFileName);
var
  i        : Integer;
  s        : string;
  temp     : AnsiString;
  MenuItem : TMenuItem;
begin
  with VstHost[0] do
   begin
    Active := False;
    DLLFileName := DLLName;
    Active := True;
    Idle;
    ShowEdit(TForm(VSTPanel));
    Idle;
    EditIdle;
    Caption :=  string(GetVendorString + ' ' + GetEffectName);

    FBufferSize := 8192;
    SetLength(FInBuffer,  numInputs);
    SetLength(FOutBuffer, numOutputs);
    BufferSizeChanged;
   end;

 while MIPrograms.Count > 7 do MIPrograms.Delete(7);
 for i := 0 to VstHost[0].numPrograms - 1 do
  begin
   VstHost[0].GetProgramNameIndexed(-1, i, temp);
   s := IntToStr(i);
   if i < 10 then s := '00' + s else
   if i < 100 then s := '0' + s;
   s := s + ' - ' + string(temp);
   MenuItem := TMenuItem.Create(MIPrograms);
   with MenuItem do
    begin
     Caption := s;
     Tag := i;
     OnClick := MIPresetClick;
    end;
   MIPrograms.Add(MenuItem);
  end;

 with VstHost[0].GetRect do
  begin
   ClientWidth := Right - Left;
   ClientHeight := Bottom - Top;
  end;
end;

procedure TFmVstPresetDesigner.SetBufferSize(const Value: Integer);
begin
 if FBufferSize <> Value then
  begin
   FBufferSize := Value;
   BufferSizeChanged;
  end;
end;

procedure TFmVstPresetDesigner.BufferSizeChanged;
var
  Channel : Integer;
begin
 VstHost.BlockSize := FBufferSize;

 with VstHost[0] do
  begin
   for Channel := 0 to numInputs  - 1 do ReallocMem(FInBuffer[Channel], FBufferSize);
   for Channel := 0 to numOutputs - 1 do ReallocMem(FOutBuffer[Channel], FBufferSize);
  end;
end;

end.
