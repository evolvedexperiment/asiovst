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
{  Portions created by Christian-W. Budde are Copyright (C) 2003-2019          }
{  by Christian-W. Budde. All Rights Reserved.                                 }
{                                                                              }
{******************************************************************************}

unit LunchBoxMain;

interface

{$I DAV_Compiler.inc}

uses
  {$IFDEF FPC}LCLIntf, LMessages, LResources, Buttons, {$ELSE} Windows,
  Messages, XPMan, {$ENDIF} SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ToolWin, ExtCtrls, StdCtrls, Menus, Types, Spin,
  DAV_Types, DAV_Complex, DAV_VSTHost, DAV_ASIOHost, DAV_AudioData,
  LunchBoxEvent, LunchBoxEventList, LunchBoxInputFilter;

type
  TSampleRec = record
    Data: TDAVSingleDynArray;
    SampleRate: Double;
  end;

  TFormLunchBox = class(TForm)
    ASIOHost: TASIOHost;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    ButtonClear: TButton;
    ButtonFlange: TButton;
    ButtonRecRev: TButton;
    ButtonRobotize: TButton;
    CheckBoxDelay: TCheckBox;
    ComboBoxKit: TComboBox;
    CheckBoxMetronome: TCheckBox;
    CheckBoxOverdrive: TCheckBox;
    CheckBoxQuantize: TComboBox;
    ComboBoxStyle: TComboBox;
    LabelBar: TLabel;
    LabelBPM: TLabel;
    LabelKit: TLabel;
    LabelQuantize: TLabel;
    LabelStyle: TLabel;
    MainMenu: TMainMenu;
    MenuItemAbout: TMenuItem;
    MenuItemExportMID: TMenuItem;
    MenuItemExportWAV: TMenuItem;
    MenuItemFile: TMenuItem;
    MenuItemHelp: TMenuItem;
    MenuItemLoadBeat: TMenuItem;
    MenuItemMoreSettings: TMenuItem;
    MenuItemNewBeat: TMenuItem;
    MenuItemOptions: TMenuItem;
    MenuItemQuit: TMenuItem;
    MenuItemSaveBeat: TMenuItem;
    MenuItemSaveBeatAs: TMenuItem;
    MenuItemSettings: TMenuItem;
    MenuItemShowKeys: TMenuItem;
    MenuItemVST: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    N3: TMenuItem;
    N4: TMenuItem;
    OpenDialog: TOpenDialog;
    SaveMIDIDialog: TSaveDialog;
    SaveWAVDialog: TSaveDialog;
    SpinEditBar: TSpinEdit;
    SpinEditTempo: TSpinEdit;
    TrackBarVolume: TTrackBar;
    ToolBar: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    VstHost: TVstHost;
    procedure FormActivate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ASIOHostBufferSwitch32(Sender: TObject;
      const InBuffer, OutBuffer: TDAVArrayOfSingleFixedArray);
    procedure ASIOHostReset(Sender: TObject);
    procedure ASIOHostSampleRateChanged(Sender: TObject);
    procedure ButtonClearClick(Sender: TObject);
    procedure BtExitClick(Sender: TObject);
    procedure ButtonFlangeMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ButtonFlangeMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ButtonMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ButtonRecRevMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ButtonRecRevMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ButtonRobotizeMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ButtonRobotizeMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure CheckBoxDelayClick(Sender: TObject);
    procedure ComboBoxKitChange(Sender: TObject);
    procedure CheckBoxMetronomeClick(Sender: TObject);
    procedure DrumPadClick(Sender: TObject);
    procedure MenuItemAboutClick(Sender: TObject);
    procedure MenuItemExportMIDClick(Sender: TObject);
    procedure MenuItemExportWAVClick(Sender: TObject);
    procedure MenuItemLoadBeatClick(Sender: TObject);
    procedure MenuItemNewBeatClick(Sender: TObject);
    procedure MenuItemQuitClick(Sender: TObject);
    procedure MenuItemSaveBeatAsClick(Sender: TObject);
    procedure MenuItemSaveBeatClick(Sender: TObject);
    procedure MenuItemSettingsClick(Sender: TObject);
    procedure MenuItemShowKeysClick(Sender: TObject);
    procedure MenuItemVSTClick(Sender: TObject);
    procedure SpinEditBarChange(Sender: TObject);
    procedure SpinEditTempoChange(Sender: TObject);
    procedure TrackBarVolumeChange(Sender: TObject);
  private
    FMetAngle: TComplex64;
    FMetPosition: TComplex64;
    FFlangeAngle: TComplex64;
    FFlangePosition: TComplex64;
    FFlange: Boolean;
    FRobotize: Boolean;
    FRecRev: Boolean;
    FRealtimeVST: Boolean;
    FBeatPos: Integer;
    FVolume: Single;
    FSamplesPerBeat: Single;
    FSamplesCount: Single;
    FMetroVolume: TDAV2SingleArray;
    FFlangeBuffer: TDAVArrayOfSingleDynArray;
    FRobotBuffer: TDAVArrayOfSingleDynArray;
    FRecRevBuffer: TDAVArrayOfSingleDynArray;
    FRobotPos: Integer;
    FDelayBuffer: TDAVArrayOfSingleDynArray;
    FDelayPos: array of Integer;
    FDelayLength: array of Integer;
    FDelayVolume: TDAV2SingleArray;
    FPatPos: Integer;
    FMaxPatSamples: Integer;
    FEventList: TLunchBoxEventList;
    FInputEnvs: TDAV2DoubleArray;
    FInputDCs: TDAV2DoubleArray;
    FInputFilter: Array [0 .. 1] of TInputFilter;
    FASIOData: TAudioDataCollection32;

    FVSTInBuffer: TDAVArrayOfSingleDynArray;
    FVSTOutBuffer: TDAVArrayOfSingleDynArray;
    procedure CalculateSineAngles;
    procedure CreateSample(Index: Integer; Amplitude: Double = 1);
    procedure Requantize;
    procedure AdjustDelayLength;
    procedure RenderOutput(ADC: TAudioDataCollection32; Loop: Boolean);
  public
    property PatternPosition: Integer read FPatPos write FPatPos;
    property EventList: TLunchBoxEventList read FEventList;
  end;

var
  FormLunchBox: TFormLunchBox;
  Samples: Array [0 .. 8] of TSampleRec;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  Math, IniFiles, DAV_Common, DAV_Math, DAV_Approximations, DAV_AudioFileWAV,
  DAV_AudioFileAIFF, DAV_AudioFileAU, WaveIOX, LunchBoxSetup, LunchBoxAbout,
  LunchBoxVST;

procedure TFormLunchBox.FormActivate(Sender: TObject);
begin
  VstHost[0].EditActivate;
end;

procedure TFormLunchBox.FormDeactivate(Sender: TObject);
begin
  VstHost[0].EditDeActivate;
end;

procedure TFormLunchBox.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FEventList);
  FreeAndNil(FASIOData);
end;

procedure TFormLunchBox.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    86:
      FRobotize := True;
    66:
      FRecRev := True;
    78:
      FFlange := True;
    79:
      CheckBoxOverdrive.Checked := not CheckBoxOverdrive.Checked;
    80:
      CheckBoxDelay.Checked := not CheckBoxDelay.Checked;
    82:
      FRealtimeVST := True;
  end;
end;

procedure TFormLunchBox.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    86:
      FRobotize := False;
    66:
      FRecRev := False;
    78:
      FFlange := False;
    82:
      FRealtimeVST := False;
  end;
end;

procedure TFormLunchBox.MenuItemAboutClick(Sender: TObject);
begin
  FormAbout.Show;
end;

procedure TFormLunchBox.MenuItemExportMIDClick(Sender: TObject);
begin
  if SaveMIDIDialog.Execute then
  begin
    ShowMessage('Feature not implemented yet');
  end;
end;

procedure TFormLunchBox.MenuItemExportWAVClick(Sender: TObject);
var
  ADC: TAudioDataCollection32;
  i: Integer;
begin
  if SaveWAVDialog.Execute then
  begin
    ASIOHost.Active := False;
    ADC := TAudioDataCollection32.Create(Self);
    try
      ADC.ChannelCount := 2;
      ADC.SampleFrames := 2 * FMaxPatSamples;
      FSamplesCount := 0;
      FPatPos := 0;
      for i := 0 to Length(FDelayBuffer) - 1 do
        FillChar(FDelayBuffer[i, 0], Length(FDelayBuffer[i]) *
          SizeOf(Single), 0);
      for i := 0 to Length(FFlangeBuffer) - 1 do
        FillChar(FFlangeBuffer[i, 0], Length(FFlangeBuffer[i]) *
          SizeOf(Single), 0);
      for i := 0 to Length(FRobotBuffer) - 1 do
        FillChar(FRobotBuffer[i, 0], Length(FRobotBuffer[i]) *
          SizeOf(Single), 0);
      for i := 0 to Length(FRecRevBuffer) - 1 do
        FillChar(FRecRevBuffer[i, 0], Length(FRecRevBuffer[i]) *
          SizeOf(Single), 0);
      for i := 0 to FEventList.Count - 1 do
        FEventList.Items[i].NoteOff;
      RenderOutput(ADC, False);
      ADC.SaveToFile(SaveWAVDialog.FileName);
    finally
      FreeAndNil(ADC);
    end;
    FSamplesCount := 0;
    FPatPos := 0;
    ASIOHost.Active := True;
  end;
end;

procedure TFormLunchBox.MenuItemLoadBeatClick(Sender: TObject);
begin
  if OpenDialog.Execute then
  begin
    ShowMessage('Feature not implemented yet');
  end;
end;

procedure TFormLunchBox.MenuItemNewBeatClick(Sender: TObject);
begin
  FEventList.Clear;
  ComboBoxKit.ItemIndex := 0;
  ComboBoxKit.OnChange(Sender);
  ComboBoxStyle.ItemIndex := 0;
  SpinEditBar.Value := 1;
  SpinEditTempo.Value := 120;
end;

procedure TFormLunchBox.MenuItemQuitClick(Sender: TObject);
begin
  Close;
end;

procedure TFormLunchBox.MenuItemSaveBeatAsClick(Sender: TObject);
begin
  ShowMessage('Feature not implemented yet');
end;

procedure TFormLunchBox.MenuItemSaveBeatClick(Sender: TObject);
begin
  ShowMessage('Feature not implemented yet');
end;

procedure TFormLunchBox.MenuItemSettingsClick(Sender: TObject);
begin
  FormSetup.ShowModal;
end;

procedure TFormLunchBox.MenuItemShowKeysClick(Sender: TObject);
begin
  ShowMessage('Feature not implemented yet');
end;

procedure TFormLunchBox.MenuItemVSTClick(Sender: TObject);
begin
  FormVST.Show;
end;

procedure TFormLunchBox.SpinEditBarChange(Sender: TObject);
begin
  FMaxPatSamples := Round(FSamplesPerBeat * 4 * SpinEditBar.Value);
end;

procedure TFormLunchBox.SpinEditTempoChange(Sender: TObject);
var
  i: Integer;
  R: Double;
begin
  R := FSamplesPerBeat;
  FSamplesPerBeat := 60 / SpinEditTempo.Value * ASIOHost.SampleRate;
  R := FSamplesPerBeat / R;
  for i := 0 to FEventList.Count - 1 do
    FEventList[i].PatternPosition := Round(FEventList[i].PatternPosition * R);
  FMaxPatSamples := Round(FSamplesPerBeat * 4 * SpinEditBar.Value);
  AdjustDelayLength;
end;

procedure TFormLunchBox.TrackBarVolumeChange(Sender: TObject);
begin
  FMetroVolume[1] := TrackBarVolume.Position * 0.01;
end;

procedure TFormLunchBox.CalculateSineAngles;
begin
  GetSinCos(2 * Pi * 1000 / ASIOHost.SampleRate, FMetAngle.Im, FMetAngle.Re);
  FMetPosition.Re := 1;
  FMetPosition.Im := 0;

  GetSinCos(Pi / ASIOHost.SampleRate, FFlangeAngle.Im, FFlangeAngle.Re);
  FFlangePosition.Re := 1;
  FFlangePosition.Im := 0;
end;

procedure TFormLunchBox.CheckBoxDelayClick(Sender: TObject);
begin
  if CheckBoxDelay.Checked then
    FDelayVolume[0] := 0.3
  else
    FDelayVolume[0] := 0;
end;

procedure TFormLunchBox.ComboBoxKitChange(Sender: TObject);
var
  i: Integer;
  ADC: TAudioDataCollection32;
  str: string;
  Fl: TSearchRec;
  done: Boolean;
begin
  with TStringList.Create do
    try
      str := ExtractFilePath(Application.ExeName) + '.\sounds\' +
        ComboBoxKit.Text + '.kit';
      if not FileExists(str) then
      begin
        done := FindFirst(ExtractFilePath(Application.ExeName) +
          '.\sounds\*.kit', faAnyFile, Fl) <> 0;
        while not done do
        begin
          with TStringList.Create do
            try
              LoadFromFile(ExtractFilePath(Application.ExeName) + '.\sounds\'
                + Fl.Name);
              if ComboBoxKit.Text = Strings[0] then
              begin
                str := ExtractFilePath(Application.ExeName) + '.\sounds\'
                  + Fl.Name;
                Break;
              end;
            finally
              Free;
              done := FindNext(Fl) <> 0;
            end;
        end;
        FindClose(Fl);
        if not FileExists(str) then
          exit;
      end;
      LoadFromFile(str);
      for i := 0 to 8 do
      begin
        str := ExtractFilePath(Application.ExeName) + '.\sounds\' +
          Strings[i + 1];
        if FileExists(str) then
        begin
          ADC := TAudioDataCollection32.Create(Self);
          with ADC do
            try
              LoadFromFile(str);

              SetLength(Samples[i].Data, ADC.SampleFrames);
              Samples[i].SampleRate := ADC.SampleRate;

              System.Move(ADC[0].ChannelDataPointer^[0], Samples[i].Data[0],
                ADC.SampleFrames * SizeOf(Single));
            finally
              FreeAndNil(ADC);
            end;
        end;
      end;
    finally
      Free;
    end;
end;

procedure TFormLunchBox.CheckBoxMetronomeClick(Sender: TObject);
begin
  FMetroVolume[1] := Integer(CheckBoxMetronome.Checked) * TrackBarVolume.Position * 0.01;
  TrackBarVolume.Visible := CheckBoxMetronome.Checked;
end;

procedure TFormLunchBox.FormCreate(Sender: TObject);
var
  Fl: TSearchRec;
  done: Boolean;
begin
  FEventList := TLunchBoxEventList.Create;
  FSamplesPerBeat := 60 / SpinEditTempo.Value * ASIOHost.SampleRate;
  FMaxPatSamples := Round(FSamplesPerBeat * 4 * SpinEditBar.Value);
  FSamplesCount := 0;
  FMetroVolume[0] := 1;
  FMetroVolume[1] := Integer(CheckBoxMetronome.Checked) * TrackBarVolume.Position * 0.01;
  FMetPosition.Re := 1;
  FMetPosition.Im := 0;
  FVolume := 1;
  FRobotPos := 0;
  FDelayVolume[1] := 0;
  FInputDCs[0] := -1E-3;
  FInputDCs[1] := 0.1;
  FInputEnvs[0] := 0.1;
  FInputEnvs[1] := 0.1;
  FInputFilter[0] := TInputFilterLP.Create;
  with FInputFilter[0] do
  begin
    SampleRate := ASIOHost.SampleRate;
    SetFilterValues(150, -1, 0.5);
    Order := 14;
  end;

  FASIOData := TAudioDataCollection32.Create(Self);
  FASIOData.ChannelCount := 2;

  ComboBoxKit.Items.Clear;
  done := FindFirst(ExtractFilePath(Application.ExeName) + '.\sounds\*.kit',
    faAnyFile, Fl) <> 0;
  while not done do
  begin
    with TStringList.Create do
      try
        LoadFromFile(ExtractFilePath(Application.ExeName) + '.\sounds\'
          + Fl.Name);
        ComboBoxKit.Items.Add(Strings[0]);
        // ComboBoxKit.Items.Add(Copy(Fl.Name,1,Pos('.kit',Fl.Name)-1));
      finally
        Free;
        done := FindNext(Fl) <> 0;
      end;
  end;
  FindClose(Fl);
  ComboBoxKit.ItemIndex := 0;
  ComboBoxKit.OnChange(Sender);

  CalculateSineAngles;

  SetLength(FVSTInBuffer, 2);
  SetLength(FVSTOutBuffer, 2);

  with TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'VSTEditor.INI') do
    try
      Top := ReadInteger('Layout', 'Main Top', Top);
      Left := ReadInteger('Layout', 'Main Left', Left);
    finally
      Free;
    end;
end;

procedure TFormLunchBox.CreateSample(Index: Integer; Amplitude: Double = 1);
var
  nn: TLunchBoxSample;
begin
  nn := TLunchBoxSample.Create(Index);
  with nn do
  begin
    PatternPosition := FPatPos;
    SampleRate := sqr(ASIOHost.SampleRate) / FormSetup.SpinEditSampleRate.Value;
    Frequency := Samples[Index].SampleRate / SampleRate;
    NoteOn(Amplitude);
  end;
  FEventList.Add(nn)
end;

procedure TFormLunchBox.DrumPadClick(Sender: TObject);
begin
  with Sender as TButton do
    CreateSample(Tag);
end;

procedure TFormLunchBox.ButtonMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  i: Integer;
begin
  i := 0;
  if Button = mbRight then
    while i < FEventList.Count do
    begin
      if FEventList[i].SampleIndex = TButton(Sender).Tag then
        FEventList.Delete(i)
      else
        inc(i);
    end;
end;

procedure TFormLunchBox.ButtonClearClick(Sender: TObject);
begin
  FEventList.Clear;
end;

procedure TFormLunchBox.BtExitClick(Sender: TObject);
begin
  Close;
end;

procedure TFormLunchBox.ButtonFlangeMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FFlange := True;
end;

procedure TFormLunchBox.ButtonFlangeMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FFlange := False;
end;

procedure TFormLunchBox.ButtonRecRevMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FRecRev := True;
end;

procedure TFormLunchBox.ButtonRecRevMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FRecRev := False;
end;

procedure TFormLunchBox.ButtonRobotizeMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FRobotize := True;
end;

procedure TFormLunchBox.ButtonRobotizeMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FRobotize := False;
end;

procedure TFormLunchBox.Requantize;
var
  i: Integer;
  q: Double;
begin
  case CheckBoxQuantize.ItemIndex of
    0:
      q := 1;
    1:
      q := 1 / FSamplesPerBeat;
    2:
      q := 2 / FSamplesPerBeat;
    3:
      q := 4 / FSamplesPerBeat;
  else
    exit;
  end;
  for i := 0 to FEventList.Count - 1 do
    with FEventList.Items[i] do
    begin
      PatternPosition := Round(Round(PatternPosition * q) / q)
        mod FMaxPatSamples;
    end;
end;

procedure TFormLunchBox.RenderOutput(ADC: TAudioDataCollection32; Loop: Boolean);
var
  i, j: Integer;
  tmp: Single;
begin
  for i := 0 to ADC.SampleFrames - 1 do
  begin
    for j := 0 to FEventList.Count - 1 do
    begin
      if FPatPos = FEventList.Items[j].PatternPosition then
        FEventList.Items[j].NoteOn(1);
      if FEventList.Items[j].IsPlaying then
        ADC[0].ChannelDataPointer^[i] := ADC[0].ChannelDataPointer^[i] +
          FEventList.Items[j].ProcessSample32;
    end;
    inc(FPatPos);
    if (FPatPos >= FMaxPatSamples) and Loop then
    begin
      FPatPos := 0;
      // FSamplesCount := FSamplesPerBeat; FBeatPos := 4;
      Requantize;
    end;
  end;

  Move(ADC[0].ChannelDataPointer^[0], ADC[1].ChannelDataPointer^[0],
    ADC.SampleFrames * SizeOf(Single));

  // Apply Overdrive
  if CheckBoxOverdrive.Checked then
    for j := 0 to ADC.ChannelCount - 1 do
      for i := 0 to ADC.SampleFrames - 1 do
        ADC[j].ChannelDataPointer^[i] := 0.3 * FastTanhOpt4TermFPU
          (12 * ADC[j].ChannelDataPointer^[i]);

  // Apply Flange
  if FFlange then
    for i := 0 to ADC.SampleFrames - 1 do
    begin
      for j := 0 to ADC.ChannelCount - 1 do
      begin
        tmp := ADC[j].ChannelDataPointer^[i];
        if (i mod 2) = 0 then
          ADC[j].ChannelDataPointer^[i] := ADC[j].ChannelDataPointer^[i] -
            FFlangePosition.Re * FFlangeBuffer[j, 1]
        else
          ADC[j].ChannelDataPointer^[i] := ADC[j].ChannelDataPointer^[i] -
            FFlangePosition.Im * FFlangeBuffer[j, 2];
        FFlangeBuffer[j, 2] := FFlangeBuffer[j, 1];
        FFlangeBuffer[j, 1] := FFlangeBuffer[j, 0];
        FFlangeBuffer[j, 0] := tmp;
      end;
      tmp := FFlangePosition.Re * FFlangeAngle.Re - FFlangePosition.Im *
        FFlangeAngle.Im;
      FFlangePosition.Im := FFlangePosition.Im * FFlangeAngle.Re +
        FFlangePosition.Re * FFlangeAngle.Im;
      FFlangePosition.Re := tmp;
    end;

  // Apply Robotize
  if FRobotize then
    for i := 0 to ADC.SampleFrames - 1 do
    begin
      for j := 0 to ADC.ChannelCount - 1 do
      begin
        FRobotBuffer[j, FRobotPos] := 0.7 * FRobotBuffer[j, FRobotPos] + 0.6 *
          ADC[j].ChannelDataPointer^[i];
        ADC[j].ChannelDataPointer^[i] := FRobotBuffer[j, FRobotPos];
      end;
      if FRobotPos < Length(FRobotBuffer[0]) then
        inc(FRobotPos)
      else
        FRobotPos := 0;
    end;

  if FRecRev then
    for j := 0 to ADC.ChannelCount - 1 do
    begin
      SetLength(FRecRevBuffer[j], Length(FRecRevBuffer[j]) + ADC.SampleFrames);
      Move(ADC[j].ChannelDataPointer^[0],
        FRecRevBuffer[j, Length(FRecRevBuffer[j]) - ADC.SampleFrames],
        ADC.SampleFrames * SizeOf(Single));
    end
  else if Length(FRecRevBuffer[0]) > 0 then
    for j := 0 to ADC.ChannelCount - 1 do
    begin
      for i := 0 to ADC.SampleFrames - 1 do
        ADC[j].ChannelDataPointer^[i] := ADC[j].ChannelDataPointer^[i] +
          FRecRevBuffer[j, Length(FRecRevBuffer[j]) - i - 1];
      SetLength(FRecRevBuffer[j], Length(FRecRevBuffer[j]) - ADC.SampleFrames);
    end;

  for i := 0 to ADC.SampleFrames - 1 do
  begin
    for j := 0 to ADC.ChannelCount - 1 do
    begin
      ADC[j].ChannelDataPointer^[i] := ADC[j].ChannelDataPointer^[i] +
        FDelayVolume[1] * FDelayBuffer[j, FDelayPos[j]];
      FDelayBuffer[j, FDelayPos[j]] := ADC[j].ChannelDataPointer^[i];
      inc(FDelayPos[j]);
      if FDelayPos[j] >= FDelayLength[j] then
        FDelayPos[j] := 0;
    end;
    FDelayVolume[1] := 0.9999 * FDelayVolume[1] + 0.0001 * FDelayVolume[0];
  end;

  if VstHost[0].Active and FRealtimeVST then
    VstHost[0].Process32Replacing(ADC.ChannelDataPointerList,
      ADC.ChannelDataPointerList, ADC.SampleFrames);
  if VstHost[1].Active then
    VstHost[1].Process32Replacing(ADC.ChannelDataPointerList,
      ADC.ChannelDataPointerList, ADC.SampleFrames);

  // Apply Metronome
  if Loop then
    for i := 0 to ADC.SampleFrames - 1 do
    begin
      tmp := FMetPosition.Re * FMetAngle.Re - FMetPosition.Im * FMetAngle.Im;
      FMetPosition.Im := FMetPosition.Im * FMetAngle.Re + FMetPosition.Re *
        FMetAngle.Im;
      FMetPosition.Re := tmp;

      if FBeatPos = 0 then
        tmp := 2 * sqr(tmp) - 1;
      tmp := FVolume * tmp * FMetroVolume[0];
      FMetroVolume[0] := 0.995 * FMetroVolume[0];
      FSamplesCount := FSamplesCount + 1;
      if FSamplesCount > FSamplesPerBeat then
      begin
        FMetroVolume[0] := 1;
        FSamplesCount := FSamplesCount - FSamplesPerBeat;
        FMetPosition.Re := 1;
        FMetPosition.Im := 0;
        if FBeatPos < 3 then
          inc(FBeatPos)
        else
        begin
          FBeatPos := 0;
          FRecRev := False;
        end;
      end;
      for j := 0 to ADC.ChannelCount - 1 do
        ADC[j].ChannelDataPointer^[i] := ADC[j].ChannelDataPointer^[i] + tmp *
          FMetroVolume[1];
    end;
end;

procedure TFormLunchBox.ASIOHostBufferSwitch32(Sender: TObject;
  const InBuffer, OutBuffer: TDAVArrayOfSingleFixedArray);
var
  i: Integer;
  d, t: Double;
begin
  // render output data
  FASIOData.SampleFrames := ASIOHost.BufferSize;
  RenderOutput(FASIOData, True);
  Move(FASIOData[0].ChannelDataPointer^[0], OutBuffer[0],
    FASIOData.SampleFrames);
  Move(FASIOData[1].ChannelDataPointer^[0], OutBuffer[1],
    FASIOData.SampleFrames);

  // process input detection
  for i := 0 to ASIOHost.BufferSize - 1 do
  begin
    t := 5E-3 + InBuffer[0, i];
    d := 0.5 * (t + FInputDCs[0]);
    FInputDCs[0] := t;

    d := abs(d);
    FInputEnvs[0] := 0.99995 * FInputEnvs[0];
    if d > FInputEnvs[0] then
      FInputEnvs[0] := d;

    FInputEnvs[1] := 0.99995 * FInputEnvs[1];
    if d > FInputEnvs[1] then
      FInputEnvs[1] := FInputEnvs[1] + 0.5 * (d - FInputEnvs[1]);

    t := FInputEnvs[0] / FInputEnvs[1] - 1;
    d := abs(t - FInputDCs[1]);
    FInputDCs[1] := t;

    if d > 0.15 then
    begin
      CreateSample(Random(9), min(d - 0.15, 1));
      FInputEnvs[1] := FInputEnvs[0];
      FInputDCs[1] := 0;
    end;
  end;

  (*
    for i := 0 to ASIOHost.BufferSize - 1 do
    begin
    OutBuffer[0,i] := FInputFilter[0].ProcessSample(OutBuffer[0,i]+1E-32);
    OutBuffer[1,i] := OutBuffer[0,i];
    //   z := fHPFilterArray[j].ProcessSample(d + 1E-32);
    end;
  *)
end;

procedure TFormLunchBox.ASIOHostReset(Sender: TObject);
var
  i: Integer;
begin
  VstHost.BlockSize := ASIOHost.BufferSize;
  SetLength(FVSTInBuffer[0], VstHost.BlockSize);
  SetLength(FVSTInBuffer[1], VstHost.BlockSize);
  SetLength(FVSTOutBuffer[0], VstHost.BlockSize);
  SetLength(FVSTOutBuffer[1], VstHost.BlockSize);
  SetLength(FRecRevBuffer, ASIOHost.OutputChannelCount);
  SetLength(FFlangeBuffer, ASIOHost.OutputChannelCount);
  for i := 0 to Length(FFlangeBuffer) - 1 do
    SetLength(FFlangeBuffer[i], 4);
  SetLength(FRobotBuffer, ASIOHost.OutputChannelCount);
  for i := 0 to Length(FRobotBuffer) - 1 do
    SetLength(FRobotBuffer[i], 512);
  FRobotPos := 0;

  SetLength(FDelayBuffer, ASIOHost.OutputChannelCount);
  SetLength(FDelayLength, ASIOHost.OutputChannelCount);
  SetLength(FDelayPos, ASIOHost.OutputChannelCount);
  AdjustDelayLength;
end;

procedure TFormLunchBox.AdjustDelayLength;
var
  i: Integer;
begin
  for i := 0 to Length(FDelayBuffer) - 1 do
  begin
    if i mod 2 = 0 then
      FDelayLength[i] := Round(1.5 * FSamplesPerBeat)
    else
      FDelayLength[i] := Round(FSamplesPerBeat);
    SetLength(FDelayBuffer[i], FDelayLength[i]);
    FDelayPos[i] := 0;
  end;
end;

procedure TFormLunchBox.ASIOHostSampleRateChanged(Sender: TObject);
begin
  FSamplesPerBeat := 60 / SpinEditTempo.Value * ASIOHost.SampleRate;
  FMaxPatSamples := Round(FSamplesPerBeat * 4 * SpinEditBar.Value);
  FInputFilter[0].SampleRate := ASIOHost.SampleRate;
  CalculateSineAngles;
end;

procedure TFormLunchBox.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  ASIOHost.Active := False;
  with TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'VSTEditor.INI') do
    try
      WriteInteger('Layout', 'Main Top', Top);
      WriteInteger('Layout', 'Main Left', Left);
    finally
      Free;
    end;
end;

end.
