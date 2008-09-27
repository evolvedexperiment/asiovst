unit SPCmain;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Forms, Dialogs, StdCtrls,
  Menus, Spin, ExtCtrls, DAV_GuiLabel, DAV_GuiBaseControl, DAV_GuiDial,
  DAV_GuiGroup, DAV_ChunkClasses, DAV_ChunkPluginGUI;

type
  TFmSplitPluginCreator = class(TForm)
    EdPluginA: TEdit;
    LbPluginA: TLabel;
    LbPluginB: TLabel;
    EdPluginB: TEdit;
    BtCreate: TButton;
    BtOpenA: TButton;
    BtClearA: TButton;
    BtOpenB: TButton;
    BtClearB: TButton;
    procedure EdPluginClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure EdPluginChange(Sender: TObject);
    procedure BtCreateClick(Sender: TObject);
    procedure BtOpenAClick(Sender: TObject);
    procedure BtOpenBClick(Sender: TObject);
    procedure BtClearAClick(Sender: TObject);
    procedure BtClearBClick(Sender: TObject);
  private
    procedure SavePlugin(FileName: TFileName);
  end;

var
  FmSplitPluginCreator: TFmSplitPluginCreator;

implementation

uses
  Graphics, IniFiles, PNGImage, DAV_DLLResources, ShellAPI;

{$R *.dfm}

procedure TFmSplitPluginCreator.SavePlugin(FileName: TFileName);
var
  RS  : TResourceStream;
  RM  : TPEResourceModule;
  RD  : TResourceDetails;
begin
 RM := TPEResourceModule.Create;
 with RM do
  try
   RS := TResourceStream.Create(HInstance, 'SplitTemplate', 'DLL');
   try
    LoadFromStream(RS);
   finally
    FreeAndNil(RS);
   end;
//       LoadFromFile(FileName);

   // store VST Plugins
   with TMemoryStream.Create do
    try
     LoadFromFile(EdPluginA.Text);
     RD := TResourceDetails.CreateResourceDetails(RM, 0, 'VST1', 'DLL', Size, Memory);
     AddResource(RD);
    finally
     Free;
    end;

   // store VST Plugins
   if EdPluginB.Enabled and FileExists(EdPluginB.Text) then
    with TMemoryStream.Create do
     try
      LoadFromFile(EdPluginB.Text);
      RD := TResourceDetails.CreateResourceDetails(RM, 0, 'VST2', 'DLL', Size, Memory);
      AddResource(RD);
     finally
      Free;
     end;

   SortResources;
   SaveToFile(FileName);
   ShowMessage('Plugin successfully created!');
  finally
   FreeAndNil(RM);
  end;
end;

procedure TFmSplitPluginCreator.BtCreateClick(Sender: TObject);
begin
 if not FileExists(EdPluginA.Text) then
  begin
   BtCreate.Enabled := False;
   exit;
  end;
 with TSaveDialog.Create(Self) do
  try
   DefaultExt := 'dll';
   Filter := 'VST Plugin (*.dll)|*.dll';
   Title := 'Save As VST DLL';
   if Execute then SavePlugin(FileName);
  finally
   Free;
  end;
end;

procedure TFmSplitPluginCreator.BtClearAClick(Sender: TObject);
begin
 EdPluginA.Text := '';
end;

procedure TFmSplitPluginCreator.BtClearBClick(Sender: TObject);
begin
 if EdPluginB.Enabled
  then EdPluginB.Text := '';
end;

procedure TFmSplitPluginCreator.BtOpenAClick(Sender: TObject);
begin
 EdPluginClick(EdPluginA);
end;

procedure TFmSplitPluginCreator.BtOpenBClick(Sender: TObject);
begin
 if EdPluginB.Enabled
  then EdPluginClick(EdPluginB)
  else ShellExecute(Handle, 'open', PChar('http://delphiasiovst.sourceforge.net'), nil, nil, SW_SHOWNORMAL);
end;

procedure TFmSplitPluginCreator.EdPluginChange(Sender: TObject);
begin
 BtCreate.Enabled := FileExists(EdPluginA.Text);
end;

procedure TFmSplitPluginCreator.EdPluginClick(Sender: TObject);
begin
 with TOpenDialog.Create(Self) do
  try
   DefaultExt := 'dll';
   Filter := 'VST Plugin (*.dll)|*.dll';
   Title := 'Choose a plugin that should be used for splitting';
   Options := Options + [ofFileMustExist];
   if Execute then
    with Sender as TEdit
     do Text := FileName;
  finally
   Free;
  end;
end;

procedure TFmSplitPluginCreator.FormCreate(Sender: TObject);
begin
 with TIniFile.Create('SplitPluginCreator.ini') do
  try
   EdPluginA.Text := ReadString('Last State','Plugin A', EdPluginA.Text);
   if EdPluginB.Enabled
    then EdPluginB.Text := ReadString('Last State','Plugin B', EdPluginB.Text)
    else EdPluginB.Text := 'please donate to unlock this feature';
  finally
   Free;
  end;
end;

procedure TFmSplitPluginCreator.FormDestroy(Sender: TObject);
begin
 with TIniFile.Create('SplitPluginCreator.ini') do
  try
   WriteString('Last State','Plugin A', EdPluginA.Text);
   WriteString('Last State','Plugin B', EdPluginB.Text);
  finally
   Free;
  end;
end;

end.
