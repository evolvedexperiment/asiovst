unit EditorForm;

{$I DAV_Compiler.INC}

interface

uses
  {$IFDEF FPC}LCLIntf, LResources, Buttons, {$ELSE}Windows, Messages, XPMan,
  {$ENDIF}SysUtils, Classes, Graphics, Controls, Forms, ComCtrls, ExtCtrls,
  StdCtrls, ToolWin, Dialogs, DAV_Common, DAV_ASIOHost, DAV_VSTHost, Menus;

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
    XPManifest: TXPManifest;
    PUPreset: TPopupMenu;
    MILoadPreset: TMenuItem;
    SavePreset1: TMenuItem;
    OD: TOpenDialog;
    SD: TSaveDialog;
    {$ENDIF}
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormActivate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure ASIOHostBufferSwitch32(Sender: TObject; const InBuffer, OutBuffer: TDAVArrayOfSingleDynArray);
    procedure ASIOHostReset(Sender: TObject);
    procedure BtSetupClick(Sender: TObject);
    procedure BtExitClick(Sender: TObject);
    procedure CBPresetChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MILoadPresetClick(Sender: TObject);
    procedure SavePreset1Click(Sender: TObject);
  private
    FVSTInBuffer  : array of PDAVSingleFixedArray;
    FVSTOutBuffer : array of PDAVSingleFixedArray;
  public
  end;

var
  FmVSTEditor: TFmVSTEditor;

implementation

{$IFNDEF FPC}
{$R *.dfm}
{$ENDIF}

uses
  IniFiles, DAV_VSTEffect, EditorSetup;

procedure TFmVSTEditor.FormCreate(Sender: TObject);
var
  theRect  : TRect;
  i        : Integer;
  s, p     : string;
begin
 with VstHost[0] do
  begin
   if ParamCount > 0
    then DLLFileName := ParamStr(1)
    else DLLFileName := 'SimpleFilter.DLL';
   if not FileExists(DLLFileName) then
    with TOpenDialog.Create(Self) do
     try
      DefaultExt := 'dll';
      Filter := 'VST Plugin (*.dll)|*.dll';
      Options := Options + [ofFileMustExist];
      if Execute then DLLFileName := FileName;
     finally
      Free;
     end;

   if not FileExists(DLLFileName) then
    begin
     Application.Terminate;
     Exit;
    end;
   
   Active := True;
   Idle;
   ShowEdit(VSTPanel);
   Idle;
   EditIdle;
   Caption :=  GetVendorString + ' ' + GetEffectName;
  end;
 CBPreset.Clear;

 for i := 0 to VstHost[0].numPrograms - 1 do
  begin
   VstHost[0].GetProgramNameIndexed(-1, i, p);
   s := IntToStr(i);
   if i < 10 then s := '00' + s else
   if i < 100 then s := '0' + s;
   s := s + ' - ' + p;
   CBPreset.Items.Add(s)
  end;
 CBPreset.ItemIndex := 0;

 s := VstHost[0].GetProgramName;
 s := IntToStr(CBPreset.ItemIndex) + ' - ' + s;
 if CBPreset.ItemIndex < 10 then s := '00' + s else
 if CBPreset.ItemIndex < 100 then s := '0' + s;
 if (CBPreset.Text <> s) then
  begin
   CBPreset.Text := s;
   for i := 0 to VstHost[0].numPrograms - 1 do
    begin
     VstHost[0].ProgramNr := i;
     s := VstHost[0].GetProgramName;
     s := IntToStr(i) + ' - ' + s;
     if i < 10 then s := '00' + s else
     if i < 100 then s := '0' + s;
     CBPreset.Items[i] := s;
    end;
   VstHost[0].ProgramNr := 0;
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

procedure TFmVSTEditor.SavePreset1Click(Sender: TObject);
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
 VstHost[0].ProgramNr := CBPreset.ItemIndex;
end;

procedure TFmVSTEditor.ASIOHostBufferSwitch32(Sender: TObject; const InBuffer,
  OutBuffer: TDAVArrayOfSingleDynArray);
begin
 if VSTHost[0].Active
  then VSTHost[0].ProcessReplacing(@InBuffer[ASIOHost.InputChannelOffset],
                                   @OutBuffer[ASIOHost.OutputChannelOffset],
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
 sleep(10);
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
