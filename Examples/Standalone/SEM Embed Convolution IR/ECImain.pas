unit ECImain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, StdCtrls, DAV_DLLResources;

type
  TFmSemEmbedConvolutionIR = class(TForm)
    MainMenu: TMainMenu;
    MIFile: TMenuItem;
    MIExit: TMenuItem;
    N1: TMenuItem;
    MIOpenSEM: TMenuItem;
    MIAddIR: TMenuItem;
    MISaveAs: TMenuItem;
    N2: TMenuItem;
    MISave: TMenuItem;
    OpenDialogSEM: TOpenDialog;
    OpenDialogWAV: TOpenDialog;
    ListBox: TListBox;
    SaveDialogSEM: TSaveDialog;
    procedure FormCreate(Sender: TObject);
    procedure MIExitClick(Sender: TObject);
    procedure MIOpenSEMClick(Sender: TObject);
    procedure MISaveAsClick(Sender: TObject);
    procedure MIAddIRClick(Sender: TObject);
    procedure MISaveClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FFileName : TFileName;
  public
    FSEModule : TPEResourceModule;
  end;

var
  FmSemEmbedConvolutionIR: TFmSemEmbedConvolutionIR;

implementation

{$R *.dfm}

procedure TFmSemEmbedConvolutionIR.FormCreate(Sender: TObject);
var
  RS : TResourceStream;
begin
 FSEModule := TPEResourceModule.Create;
 RS := TResourceStream.Create(HInstance, 'ConvolutionModule', 'DLL');
 try
  FSEModule.LoadFromStream(RS);
 finally
  FreeAndNil(RS);
 end;
end;

procedure TFmSemEmbedConvolutionIR.FormDestroy(Sender: TObject);
begin
 FreeAndNil(FSEModule);
end;

procedure TFmSemEmbedConvolutionIR.MIOpenSEMClick(Sender: TObject);
var
  i : Integer;
begin
 with OpenDialogSEM do
  if Execute then
   try
    ListBox.Clear;

    FSEModule.LoadFromFile(FileName);

    for i := 0 to FSEModule.ResourceCount - 1 do
     if FSEModule.ResourceDetails[i].ResourceType = 'IR'
      then ListBox.Items.Add(FSEModule.ResourceDetails[i].ResourceName);

    FFileName := FileName;
   finally
    MISave.Enabled := FileExists(FFileName);
   end;
end;

procedure TFmSemEmbedConvolutionIR.MISaveAsClick(Sender: TObject);
begin
 with SaveDialogSEM do
  if Execute then
   try
    FSEModule.SortResources;
    FSEModule.SaveToFile(FileName);
    FFileName := FileName;
   finally
    MISave.Enabled := FileExists(FFileName);
   end;
end;

procedure TFmSemEmbedConvolutionIR.MISaveClick(Sender: TObject);
begin
 if FFileName <> ''
  then FSEModule.SaveToFile(FFileName)
  else MISave.Enabled := False;
end;

procedure TFmSemEmbedConvolutionIR.MIAddIRClick(Sender: TObject);
var
  i  : Integer;
  RD : TResourceDetails;
begin
 with OpenDialogWAV do
  if Execute then
   for i := 0 to Files.Count - 1 do
    begin
     with TMemoryStream.Create do
      try
       ListBox.Items.Add(ExtractFileName(Files[i]));
       LoadFromFile(Files[i]);
       RD := TResourceDetails.CreateResourceDetails(FSEModule, 0, 'IR' + IntToStr(i), 'IR', Size, Memory);
       FSEModule.AddResource(RD);
      finally
       Free;
      end;
    end;
end;

procedure TFmSemEmbedConvolutionIR.MIExitClick(Sender: TObject);
begin
 Close;
end;

end.
