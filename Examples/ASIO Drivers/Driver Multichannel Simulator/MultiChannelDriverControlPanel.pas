unit MultiChannelDriverControlPanel;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, ExtCtrls,
  ToolWin, ComCtrls, StdCtrls, Menus, DAV_ASIODriver;

type
  TFmAsioDriverControlPanel = class(TDavASIODriverCP)
    LbDriver: TLabel;
    CbDriver: TComboBox;
    BtControlPanel: TButton;
    pcAssignments: TPageControl;
    pageInputs: TTabSheet;
    pageOutputs: TTabSheet;
    lbIn01: TLabel;
    lbIn02: TLabel;
    lbIn03: TLabel;
    lbIn04: TLabel;
    lbIn05: TLabel;
    lbIn06: TLabel;
    lbIn07: TLabel;
    lbIn08: TLabel;
    lbIn09: TLabel;
    lbIn10: TLabel;
    lbIn11: TLabel;
    lbIn12: TLabel;
    lbIn13: TLabel;
    lbIn14: TLabel;
    lbIn15: TLabel;
    lbIn16: TLabel;
    cbIn01: TComboBox;
    cbIn02: TComboBox;
    cbIn03: TComboBox;
    cbIn04: TComboBox;
    cbIn05: TComboBox;
    cbIn06: TComboBox;
    cbIn07: TComboBox;
    cbIn08: TComboBox;
    cbIn09: TComboBox;
    cbIn10: TComboBox;
    cbIn11: TComboBox;
    cbIn12: TComboBox;
    cbIn13: TComboBox;
    cbIn14: TComboBox;
    cbIn15: TComboBox;
    cbIn16: TComboBox; 
    lbOut01: TLabel;
    lbOut02: TLabel;
    lbOut03: TLabel;
    lbOut04: TLabel;
    lbOut05: TLabel;
    lbOut06: TLabel;
    lbOut07: TLabel;
    lbOut08: TLabel;
    lbOut09: TLabel;
    lbOut10: TLabel;
    lbOut11: TLabel;
    lbOut12: TLabel;
    lbOut13: TLabel;
    lbOut14: TLabel;
    lbOut15: TLabel;
    lbOut16: TLabel;
    cbOut01: TComboBox;
    cbOut02: TComboBox;
    cbOut03: TComboBox;
    cbOut04: TComboBox;
    cbOut05: TComboBox;
    cbOut06: TComboBox;
    cbOut07: TComboBox;
    cbOut08: TComboBox;
    cbOut09: TComboBox;
    cbOut10: TComboBox;
    cbOut11: TComboBox;
    cbOut12: TComboBox;
    cbOut13: TComboBox;
    cbOut14: TComboBox;
    cbOut15: TComboBox;
    cbOut16: TComboBox;
    btnClose: TButton;
    btnApply: TButton;
    procedure BtControlPanelClick(Sender: TObject);
    procedure CbDriverChange(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnApplyClick(Sender: TObject);
    procedure InputSettingsChanged(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure OutputSettingsChanged(Sender: TObject);
  public
    inputboxes: array[1..16] of TComboBox;
    outputboxes: array[1..16] of TComboBox;
    ModifiedSelections: boolean;
    procedure PanelLoaded; override;
    procedure UpdateSelection;
    procedure CopyToSettings;
  end;

implementation

uses
  Math, Dialogs, Registry, DAV_AsioHost, MultiChannelDriverMain;

{$R *.dfm}

procedure TFmAsioDriverControlPanel.PanelLoaded;
var i: integer;
begin
  if not assigned(Driver) then exit;

  Caption:=Driver.GetDriverName + ' (Version ' + inttostr(Driver.GetDriverVersion) + ')';
  cbDriver.Items:=TAsioHostDriver(Driver).AsioHost.DriverList;

  pcAssignments.Visible := false;  // no redraw for 16 comboboxes

  cbIn01.Clear;
  cbIn01.Items.Add('- none -');
  cbIn01.Items.Add('- zero -');

  cbOut01.Clear;
  cbOut01.Items.Add('- none -');
  cbOut01.Items.Add('- zero -');
  with TAsioHostDriver(Driver).Asiohost do
  begin
   for i := 0 to InputChannelCount-1 do cbIn01.Items.Add(string(OutputChannelInfos[i].Name));
   for i := 0 to OutputChannelCount-1 do cbOut01.Items.Add(string(OutputChannelInfos[i].Name));
  end;

  for i := 2 to 16 do inputboxes[i].Items.Assign(cbIn01.Items);
  for i := 2 to 16 do outputboxes[i].Items.Assign(cbOut01.Items);

  UpdateSelection;
  pcAssignments.Visible := true;
end;

procedure TFmAsioDriverControlPanel.InputSettingsChanged(Sender: TObject);
var i: integer;
begin
  if not (Sender is TComboBox) then exit;

  with TComboBox(Sender) do
    if ItemIndex<1 then
    begin
      for i := tag+1 to 16 do inputboxes[i].Enabled := false;

    end else
      for i := tag+1 to 16 do
       if (inputboxes[i-1].ItemIndex>0) and outputboxes[i-1].enabled then inputboxes[i].Enabled := true;

  ModifiedSelections := true;
end;

procedure TFmAsioDriverControlPanel.OutputSettingsChanged(Sender: TObject);
var i: integer;
begin
  if not (Sender is TComboBox) then exit;

  with TComboBox(Sender) do
    if ItemIndex<1 then
    begin
      for i := tag+1 to 16 do outputboxes[i].Enabled := false;
    end else
      for i := tag+1 to 16 do
       if (outputboxes[i-1].ItemIndex>0) and outputboxes[i-1].enabled then outputboxes[i].Enabled := true;

  ModifiedSelections := true;
end;

procedure TFmAsioDriverControlPanel.UpdateSelection;
var i: integer;
begin
  with TAsioHostDriver(Driver).Settings do
  begin
    CbDriver.ItemIndex := DriverIndex;

    for i := 1 to 16 do
    begin
      inputboxes[i].ItemIndex := InputAssignment[i];
      inputboxes[i].Enabled := (i=1) or ( (InputAssignment[i-1]>0) and inputboxes[i-1].Enabled);

      outputboxes[i].ItemIndex := OutputAssignment[i];
      outputboxes[i].Enabled := (i=1) or ( (OutputAssignment[i-1]>0) and outputboxes[i-1].Enabled);
    end;
  end;
  ModifiedSelections := false;
end;

procedure TFmAsioDriverControlPanel.btnCloseClick(Sender: TObject);
begin
  UpdateSelection;
  close;
end;

procedure TFmAsioDriverControlPanel.CopyToSettings;
var i: integer;
begin
  with TAsioHostDriver(Driver).Settings do
  begin
    DriverIndex := CbDriver.ItemIndex;

    for i := 1 to 16 do
    begin
      InputAssignment[i] := inputboxes[i].ItemIndex;
      OutputAssignment[i] := outputboxes[i].ItemIndex;
    end;
  end;
end;

procedure TFmAsioDriverControlPanel.FormCreate(Sender: TObject);
var i: integer;
begin
  for i := 0 to pageInputs.ControlCount-1 do
   if pageInputs.Controls[i] is TComboBox then
    inputboxes[pageInputs.Controls[i].Tag] := TComboBox(pageInputs.Controls[i]);

  for i := 0 to pageOutputs.ControlCount-1 do
   if pageOutputs.Controls[i] is TComboBox then
    outputboxes[pageOutputs.Controls[i].Tag] := TComboBox(pageOutputs.Controls[i]);
end;

procedure TFmAsioDriverControlPanel.btnApplyClick(Sender: TObject);
begin
  if not assigned(Driver) then exit;

  CopyToSettings;
  TAsioHostDriver(Driver).SaveAndReset(self);
end;

procedure TFmAsioDriverControlPanel.CbDriverChange(Sender: TObject);
begin
 if not assigned(Driver) then exit;

 CopyToSettings;
 TAsioHostDriver(Driver).SaveAndReset(self);
end;

procedure TFmAsioDriverControlPanel.BtControlPanelClick(Sender: TObject);
var r: integer;
begin
  if not assigned(Driver) then exit;

  if ModifiedSelections then
  begin
   r := MessageBox(handle,'Save modified settings?', 'Settings modified', MB_YESNOCANCEL);
   case r of
     IDYES: begin
       CopyToSettings;
       TAsioHostDriver(Driver).SaveDriverSettings;
     end;
     IDCANCEL: exit;
   end;
  end;
  
  TAsioHostDriver(Driver).AsioHost.ControlPanel;
end;

end.
