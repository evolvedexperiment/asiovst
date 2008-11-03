unit SEmain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, StdCtrls, DAV_SEModule, DAV_SEHost;

type
  TFmSEModuleExplorer = class(TForm)
    MainMenu: TMainMenu;
    MIFile: TMenuItem;
    MIOpen: TMenuItem;
    MIExit: TMenuItem;
    N1: TMenuItem;
    MISaveAs: TMenuItem;
    LBSEMs: TListBox;
    MINew: TMenuItem;
    procedure MIExitClick(Sender: TObject);
    procedure MIOpenClick(Sender: TObject);
    procedure MINewClick(Sender: TObject);
    procedure MISaveAsClick(Sender: TObject);
  private
    procedure SaveModule(Filename: TFileName);
  end;

var
  FmSEModuleExplorer: TFmSEModuleExplorer;

implementation

uses
  IniFiles, DAV_DLLResources, DAV_SECommon;

{$R *.dfm}

procedure TFmSEModuleExplorer.MIOpenClick(Sender: TObject);
begin
 with TOpenDialog.Create(Self) do
  begin
   DefaultExt := '.sem';
   Filter := 'SE Modules (*.sem)|*.sem';
   Options := [ofHideReadOnly, ofFileMustExist, ofEnableSizing];
   Title := 'Select SE Module';
   if Execute then
    begin
     LBSEMs.Items.Add(FileName);
     MISaveAs.Enabled := True;
    end;
  end;
end;

procedure TFmSEModuleExplorer.MISaveAsClick(Sender: TObject);
begin
 with TSaveDialog.Create(Self) do
  begin
   DefaultExt := '.sem';
   Filter := 'SE Modules (*.sem)|*.sem';
   Options := [ofHideReadOnly, ofEnableSizing];
   Title := 'Save as SE Module';
   if Execute then SaveModule(FileName);
  end;
end;

procedure TFmSEModuleExplorer.SaveModule(Filename: TFileName);
var
  RS : TResourceStream;
  RM : TPEResourceModule;
  RD : TResourceDetails;
  i  : Integer;
begin
 RM := TPEResourceModule.Create;
 with RM do
  try
   // load template
   RS := TResourceStream.Create(HInstance, 'SEMerger', 'SEM');
   try
    LoadFromStream(RS);
   finally
    FreeAndNil(RS);
   end;

   // store SE modules
   for i := 0 to LBSEMs.Count - 1 do
    with TMemoryStream.Create do
     try
      assert(FileExists(LBSEMs.Items[i]));
      LoadFromFile(LBSEMs.Items[i]);
      RD := TResourceDetails.CreateResourceDetails(RM, 0, 'SEM' + IntToStr(i), 'SEM', Size, Memory);
      AddResource(RD);
     finally
      Free;
     end;

   SortResources;
   SaveToFile(Filename);
   ShowMessage('Merged SEM successfully created!');
  finally
   FreeAndNil(RM);
  end;
end;

procedure TFmSEModuleExplorer.MINewClick(Sender: TObject);
begin
 LBSEMs.Clear;
 MISaveAs.Enabled := False;
end;

procedure TFmSEModuleExplorer.MIExitClick(Sender: TObject);
begin
 Close;
end;

end.
