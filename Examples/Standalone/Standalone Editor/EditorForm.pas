unit EditorForm;

{$I ASIOVST.INC}

interface

uses
  {$IFDEF FPC}LCLIntf, LResources, Buttons, {$ELSE}Windows, Messages, XPMan,
  {$ENDIF}SysUtils, Classes, Graphics, Controls, Forms, ComCtrls, ExtCtrls,
  StdCtrls, DDSPBase, DVSTHost, DASIOHost, ToolWin;

type
  TFmVSTEditor = class(TForm)
    ToolBar: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    LbPreset: TLabel;
    ToolButton3: TToolButton;
    VstHost: TVstHost;
    ToolButton4: TToolButton;
    ASIOHost: TASIOHost;
    {$IFNDEF FPC}
    XPManifest1: TXPManifest;
    {$ENDIF}
    VSTPanel: TPanel;
    CBPreset: TComboBox;
    BtSetup: TButton;
    BtExit: TButton;
    procedure ASIOHostBufferSwitch32(Sender: TObject; const InBuffer, OutBuffer: TArrayOfSingleDynArray);
    procedure CBPresetChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BtSetupClick(Sender: TObject);
    procedure BtExitClick(Sender: TObject);
    procedure ASIOHostReset(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    VSTInBuffer: TArrayOfSingleDynArray;
    VSTOutBuffer: TArrayOfSingleDynArray;
  public
  end;

var
  FmVSTEditor: TFmVSTEditor;

implementation

{$IFNDEF FPC}
{$R *.dfm}
{$ENDIF}

uses inifiles, DVSTEffect, EditorSetup;

procedure TFmVSTEditor.CBPresetChange(Sender: TObject);
begin
 VstHost[0].ProgramNr:=CBPreset.ItemIndex;
end;

procedure TFmVSTEditor.FormActivate(Sender: TObject);
begin
 VstHost[0].EditActivate;
end;

procedure TFmVSTEditor.FormDeactivate(Sender: TObject);
begin
 VstHost[0].EditDeActivate;
end;

procedure TFmVSTEditor.FormCreate(Sender: TObject);
var theRect  : TRect;
    i        : integer;
    s        : String;
    temp     : pchar;
    Settings : TInifile;
begin
 with VstHost[0] do
  begin
   if ParamCount>0
    then DLLFileName:=ParamStr(1)
    else DLLFileName:='NoGuiFilter.DLL';
   Active:=True;
   Idle;
   ShowEdit(TForm(VSTPanel));
   Idle;
   EditIdle;
   Caption :=  GetVendorString + ' ' + GetEffectName;
  end;
 CBPreset.Clear;
 getmem(temp, 25);
 for i:=0 to VstHost[0].numPrograms-1 do
  begin
   VstHost[0].GetProgramNameIndexed(-1, i, temp);
   s := inttostr(i);
   if i < 10 then s := '00' + s else
   if i < 100 then s := '0' + s;
   s := s+' - '+StrPas(temp);
   CBPreset.Items.Add(s)
  end;
 CBPreset.ItemIndex:=0;
 Freemem(temp);

 s := VstHost[0].GetProgramName;
 s := inttostr(CBPreset.ItemIndex)+' - '+s;
 if CBPreset.ItemIndex < 10 then s := '00' + s else
 if CBPreset.ItemIndex < 100 then s := '0' + s;
 if (CBPreset.Text <> s) then
  begin
   CBPreset.Text:=s;
   for i:=0 to VstHost[0].numPrograms-1 do
    begin
     VstHost[0].ProgramNr:=i;
     s := VstHost[0].GetProgramName;
     s := inttostr(i)+' - '+s;
     if i < 10 then s := '00' + s else
     if i < 100 then s := '0' + s;
     CBPreset.Items[i] := s;
    end;
   VstHost[0].ProgramNr:=0;
   CBPreset.ItemIndex:=0;
  end;
 if (effFlagsHasEditor in VstHost[0].EffectOptions) then
  begin
   theRect:=VstHost[0].GetRect;
   ClientWidth:=theRect.Right-theRect.Left;
   ClientHeight:=theRect.Bottom-theRect.Top+ToolBar.Height;
  end;
 SetLength(VSTInBuffer,2);
 SetLength(VSTOutBuffer,2);
 Settings:=TIniFile.Create(ExtractFilePath(ParamStr(0))+'VSTEditor.INI');
 Top:=Settings.ReadInteger('Layout','Main Top',Top);
 Left:=Settings.ReadInteger('Layout','Main Left',Left);
 Settings.Free;
end;

procedure TFmVSTEditor.BtSetupClick(Sender: TObject);
begin
 FmSetup.Visible:=not FmSetup.Visible;
end;

procedure TFmVSTEditor.BtExitClick(Sender: TObject);
begin
 Close;
end;

procedure TFmVSTEditor.ASIOHostBufferSwitch32(Sender: TObject; const InBuffer,
  OutBuffer: TArrayOfSingleDynArray);
begin
 VSTHost[0].ProcessReplacing(@InBuffer[ASIOHost.InputChannelOffset],@OutBuffer[ASIOHost.OutputChannelOffset],ASIOHost.BufferSize);
end;

procedure TFmVSTEditor.ASIOHostReset(Sender: TObject);
begin
 VSTHost.BlockSize:=ASIOHost.BufferSize;
 SetLength(VSTInBuffer[0],VSTHost.BlockSize);
 SetLength(VSTInBuffer[1],VSTHost.BlockSize);
 SetLength(VSTOutBuffer[0],VSTHost.BlockSize);
 SetLength(VSTOutBuffer[1],VSTHost.BlockSize);
end;

procedure TFmVSTEditor.FormClose(Sender: TObject;
  var Action: TCloseAction);
var Settings : TInifile;
begin
 with VSTHost[0] do
  try
   Active:=False; UnLoad;
  except
  end;
 ASIOHOST.Active:=False;
 with TIniFile.Create(ExtractFilePath(ParamStr(0))+'VSTEditor.INI') do
  try
   WriteInteger('Layout','Main Top',Top);
   WriteInteger('Layout','Main Left',Left);
  finally
   Free;
  end;
end;

{$IFDEF FPC}
initialization
  {$i EditorForm.lrs}
{$ENDIF}

end.
