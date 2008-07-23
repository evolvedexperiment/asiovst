unit VAMain;

{$I ASIOVST.INC}

interface

uses
  {$IFDEF FPC}LCLIntf, LResources, Buttons, {$ELSE}Windows, Messages, XPMan,
  {$ENDIF}SysUtils, Classes, Graphics, Controls, Forms, ComCtrls, ExtCtrls,
  StdCtrls, DAVDCommon, DVSTHost, Menus, Dialogs;

type
  TFmVSTAnalyser = class(TForm)
    MainMenu: TMainMenu;
    MIFile: TMenuItem;
    MIIR: TMenuItem;
    MILoad: TMenuItem;
    MIOpen: TMenuItem;
    MIPlotIR: TMenuItem;
    MIPrograms: TMenuItem;
    MIQuit: TMenuItem;
    MIRenderIR: TMenuItem;
    MISave: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    OD: TOpenDialog;
    VstHost: TVstHost;
    VSTPanel: TPanel;
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
    procedure VSTPanelClick(Sender: TObject);
  private
    VSTInBuffer: TAVDArrayOfSingleDynArray;
    VSTOutBuffer: TAVDArrayOfSingleDynArray;
    procedure LoadVSTPlugin(DLLName: TFileName);
  public
  end;

var
  FmVSTAnalyser: TFmVSTAnalyser;

implementation

{$IFNDEF FPC}
{$R *.dfm}
{$ENDIF}

uses
  IniFiles, VAPlotIR;

procedure TFmVSTAnalyser.FormActivate(Sender: TObject);
begin
 VstHost[0].EditActivate;
end;

procedure TFmVSTAnalyser.FormDeactivate(Sender: TObject);
begin
 VstHost[0].EditDeActivate;
end;

procedure TFmVSTAnalyser.MIOpenClick(Sender: TObject);
begin
 if OD.Execute then LoadVSTPlugin(OD.FileName);
end;

procedure TFmVSTAnalyser.MIPlotIRClick(Sender: TObject);
begin
 with VstHost[0] do
  if Active then
   begin
    VSTInBuffer[0, 0] := 1;
    FillChar(VSTInBuffer[0, 1], (VstHost.BlockSize - 1) * SizeOf(Single), 0);
    ProcessReplacing(@VSTInBuffer[0], @VSTOutBuffer[0], VstHost.BlockSize);

    FmPlotIR.Waveform.SetWaveForm(VSTOutBuffer, True, True);
   end;
 FmPlotIR.ShowModal;
end;

procedure TFmVSTAnalyser.MIQuitClick(Sender: TObject);
begin
 Close;
end;

procedure TFmVSTAnalyser.MILoadClick(Sender: TObject);
begin
 ShowMessage('ToDo');
end;

procedure TFmVSTAnalyser.MISaveClick(Sender: TObject);
begin
 ShowMessage('ToDo');
end;

procedure TFmVSTAnalyser.VSTPanelClick(Sender: TObject);
begin
 if not VstHost[0].Active
  then MIOpenClick(Sender);
end;

procedure TFmVSTAnalyser.MIPresetClick(Sender: TObject);
begin
 with Sender as TMenuItem
  do VstHost[0].ProgramNr := Tag;
end;

procedure TFmVSTAnalyser.LoadVSTPlugin(DLLName : TFileName);
var
  i        : integer;
  s        : string;
  temp     : pchar;
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
    Caption :=  GetVendorString + ' ' + GetEffectName;
    SetLength(VSTInBuffer,numInputs);
    SetLength(VSTOutBuffer,numOutputs);
    for i := 0 to numInputs-1 do SetLength(VSTInBuffer[i],VSTHost.BlockSize);
    for i := 0 to numOutputs-1 do SetLength(VSTOutBuffer[i],VSTHost.BlockSize);
   end;

 while MIPrograms.Count > 3 do MIPrograms.Delete(3);
 getmem(temp, 25);
 for i := 0 to VstHost[0].numPrograms - 1 do
  begin
   VstHost[0].GetProgramNameIndexed(-1, i, temp);
   s := inttostr(i);
   if i < 10 then s := '00' + s else
   if i < 100 then s := '0' + s;
   s := s + ' - ' + StrPas(temp);
   MenuItem := TMenuItem.Create(MIPrograms);
   with MenuItem do
    begin
     Caption := s;
     Tag := i;
     OnClick := MIPresetClick;
    end;
   MIPrograms.Add(MenuItem);
  end;
 Freemem(temp);

 with VstHost[0].GetRect do
  begin
   ClientWidth := Right - Left;
   ClientHeight := Bottom - Top;
  end;
end;

procedure TFmVSTAnalyser.FormCreate(Sender: TObject);
begin
 if ParamCount > 0 then LoadVSTPlugin(ParamStr(1));
 with TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'VSTEditor.INI') do
  try
   Top := ReadInteger('Layout', 'Main Top', Top);
   Left := ReadInteger('Layout', 'Main Left', Left);
  finally
   Free;
  end;
end;

procedure TFmVSTAnalyser.FormClose(Sender: TObject; var Action: TCloseAction);
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
  {$i VAMain.lrs}
{$ENDIF}

end.
