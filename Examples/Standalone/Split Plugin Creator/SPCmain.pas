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
    procedure EdPluginAClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure EdPluginAChange(Sender: TObject);
    procedure BtCreateClick(Sender: TObject);
  end;

var
  FmSplitPluginCreator: TFmSplitPluginCreator;

implementation

uses
  Graphics, IniFiles, PNGImage, DAV_DLLResources;

{$R *.dfm}

procedure TFmSplitPluginCreator.BtCreateClick(Sender: TObject);
var
  RS  : TResourceStream;
  RM  : TPEResourceModule;
  RD  : TResourceDetails;
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
   if Execute then
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
        finally
         Free;
        end;
       AddResource(RD);
       SortResources;

(*
       // store VST Plugins
       with TMemoryStream.Create do
        try
         LoadFromFile(EdPluginB.Text);
         RD := TResourceDetails.CreateResourceDetails(RM, 0, 'VST2', 'DLL', Size, Memory);
        finally
         Free;
        end;
       AddResource(RD);
*)
       SaveToFile(FileName);
      finally
       FreeAndNil(RM);
      end;
    end;
  finally
   Free;
  end;
 ShowMessage('Plugin successfully created!');
end;

procedure TFmSplitPluginCreator.EdPluginAChange(Sender: TObject);
begin
 BtCreate.Enabled := FileExists(EdPluginA.Text);
end;

procedure TFmSplitPluginCreator.EdPluginAClick(Sender: TObject);
begin
 with TOpenDialog.Create(Self) do
  try
   DefaultExt := 'dll';
   Filter := 'VST Plugin (*.dll)|*.dll';
   Title := 'Choose a plugin that should be used for splitting';
   Options := Options + [ofFileMustExist];
   if Execute
    then EdPluginA.Text := FileName;
  finally
   Free;
  end;
end;

procedure TFmSplitPluginCreator.FormCreate(Sender: TObject);
begin
 with TIniFile.Create('SplitPluginCreator.ini') do
  try
   EdPluginA.Text := ReadString('Last State','Plugin A', EdPluginA.Text);
   EdPluginB.Text := ReadString('Last State','Plugin B', EdPluginB.Text);
  finally
   Free;
  end;
end;

procedure TFmSplitPluginCreator.FormDestroy(Sender: TObject);
begin
 with TIniFile.Create('SplitPluginCreator.ini') do
  try
   WriteString('Last State','Plugin A', EdPluginA.Text);
  finally
   Free;
  end;
end;

end.
