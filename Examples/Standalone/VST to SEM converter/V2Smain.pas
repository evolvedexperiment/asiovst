unit V2Smain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, ComCtrls, StdCtrls, DAV_VSTHost, DAV_SEHost, XPMan;

type
  TFmVST2SEM = class(TForm)
    LbName: TLabel;
    LbSemAbout: TLabel;
    LbSemId: TLabel;
    LbVSTName: TLabel;
    MainMenu: TMainMenu;
    MemoID: TMemo;
    MemoAbout: TMemo;
    MemoName: TMemo;
    MIExit: TMenuItem;
    MIFile: TMenuItem;
    MIOpen: TMenuItem;
    N1: TMenuItem;
    PC: TPageControl;
    MISaveAs: TMenuItem;
    TSSEMProperties: TTabSheet;
    TSVSTPlugin: TTabSheet;
    VstHost: TVstHost;
    VstName: TEdit;
    MemoInfo: TMemo;
    XPManifest1: TXPManifest;
    MISave: TMenuItem;
    procedure MIExitClick(Sender: TObject);
    procedure MIOpenClick(Sender: TObject);
    procedure MISaveAsClick(Sender: TObject);
    procedure MISaveClick(Sender: TObject);
  private
    FVSTPluginDLL : TFileName;
    procedure SaveModule(Filename: TFileName);
  end;

var
  FmVST2SEM: TFmVST2SEM;

implementation

uses
  DAV_DLLResources;

{$R *.dfm}

procedure TFmVST2SEM.MIExitClick(Sender: TObject);
begin
 Close;
end;

procedure TFmVST2SEM.MIOpenClick(Sender: TObject);
var
  str : string;
begin
 with TOpenDialog.Create(Self) do
  begin
   DefaultExt := '.dll';
   Filter := 'VST Plugin (*.DLL)|*.DLL';
   Options := Options + [ofFileMustExist];
   Title := 'Select a VST Plugin';
   if Execute then
    try
     FVSTPluginDLL := FileName;
     if VstHost[0].Active then
      begin
       VstHost[0].Close;
       VstHost[0].UnLoad;
      end;
     VstHost[0].LoadFromFile(FileName);
     VstHost[0].Open;
     str := Trim(VstHost[0].GetEffectName);
     if str = '' then
      begin
       str := ExtractFileName(FVSTPluginDLL);
       if Pos('.', str) > 1 then SetLength(str, Pos('.', str) - 1); 
      end;
     while Pos(' ', str) > 0 do str[Pos(' ', str)] := '_';
     VstName.Text := Uppercase(str);
     MemoName.Clear;
     MemoName.Lines.Add('DAV VST-Wrapper - ' + VstName.Text);
     MemoID.Clear;
     MemoID.Lines.Add('VST2SEM - ' + VstName.Text);
     MemoInfo.Clear;
     MemoInfo.Lines.Add('Inputs: ' + IntToStr(VstHost[0].numInputs));
     MemoInfo.Lines.Add('Outputs: ' + IntToStr(VstHost[0].numOutputs));
     MemoInfo.Lines.Add('Parameters: ' + IntToStr(VstHost[0].numParams));
     MISaveAs.Enabled := True;
    except
     MISaveAs.Enabled := False;
    end;
  end;
end;

procedure TFmVST2SEM.MISaveAsClick(Sender: TObject);
begin
 with TSaveDialog.Create(Self) do
  begin
   DefaultExt := '.SEM';
   Filter     := 'SE Module (*.SEM)|*.SEM';
   Title      := 'Save as SEM Module';
   FileName   := FVSTPluginDLL + '.sem';
   if Execute then SaveModule(FileName);
  end;
end;

procedure TFmVST2SEM.MISaveClick(Sender: TObject);
begin
 SaveModule(FVSTPluginDLL + '.sem');
end;

procedure TFmVST2SEM.SaveModule(Filename: TFileName);
var
  RS : TResourceStream;
  RM : TPEResourceModule;
  RD : TResourceDetails;
begin
 assert(FileExists(FVSTPluginDLL));

 RM := TPEResourceModule.Create;
 with RM do
  try
   // load template
   RS := TResourceStream.Create(HInstance, 'VST2SEM', 'SEM');
   try
    LoadFromStream(RS);
   finally
    FreeAndNil(RS);
   end;

   while ResourceCount > 0 do DeleteResource(0);

   // store SE modules
   with TMemoryStream.Create do
    try
     LoadFromFile(FVSTPluginDLL);
     RD := TResourceDetails.CreateResourceDetails(RM, 0, VstName.Text, 'VST', Size, Memory);
     AddResource(RD);
    finally
     Free;
    end;

   SortResources;
   SaveToFile(Filename);
//   ShowMessage('Wrapped SEM successfully created!');
  finally
   FreeAndNil(RM);
  end;
end;

end.
