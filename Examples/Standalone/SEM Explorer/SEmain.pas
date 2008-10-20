unit SEmain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, StdCtrls, DAV_SEModule, DAV_SEHost;

type
  TFmSEModuleExplorer = class(TForm)
    MainMenu: TMainMenu;
    MIFile: TMenuItem;
    MIOpen: TMenuItem;
    MIExit: TMenuItem;
    Memo: TMemo;
    N1: TMenuItem;
    procedure MIExitClick(Sender: TObject);
    procedure MIOpenClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    SEHost : TSEHost;
    procedure LoadSEModule(FileName: TFileName);
    procedure SavePatchedModule;
  end;

var
  FmSEModuleExplorer: TFmSEModuleExplorer;

implementation

uses
  DAV_DLLResources, DAV_SECommon;

{$R *.dfm}

procedure TFmSEModuleExplorer.MIOpenClick(Sender: TObject);
begin
 with TOpenDialog.Create(Self) do
  begin
   DefaultExt := '.sem';
   Filter := 'SE Modules (*.sem)|*.sem';
   Options := [ofHideReadOnly, ofFileMustExist, ofEnableSizing];
   Title := 'Select SE Module';
   if Execute then LoadSEModule(FileName);
  end;
end;

procedure TFmSEModuleExplorer.FormCreate(Sender: TObject);
begin
 SEHost := TSEHost.Create(Self);
 SEHost.HostedSEModules.Add;
end;

procedure TFmSEModuleExplorer.FormDestroy(Sender: TObject);
begin
 FreeAndNil(SEHost);
end;

procedure TFmSEModuleExplorer.LoadSEModule(FileName: TFileName);
var
  PartNr, PinNr : Integer;
  Pin           : TSEPinProperties;
begin
 SEHost[0].LoadFromFile(FileName);
 Memo.Clear;
 for PartNr := 0 to SEHost[0].PartCount - 1 do
  with SEHost[0].Part[PartNr] do
   begin
    if PartNr > 0 then Memo.Lines.Add('');
    Memo.Lines.Add('Module '      + IntToStr(PartNr + 1));
    Memo.Lines.Add('Name: '       + Properties.Name);
    Memo.Lines.Add('ID: '         + Properties.ID);
    Memo.Lines.Add('About: '      + Properties.About);
    Memo.Lines.Add('Flags: '      + PropertyFlagsToString(Properties.Flags));
    Memo.Lines.Add('GuiFlags: '   + PropertyGUIFlagsToString(Properties.GuiFlags));
    Memo.Lines.Add('SdkVersion: ' + IntToStr(Properties.SdkVersion));
   end;
 if SEHost[0].PartCount > 0 then
  begin
   SEHost[0].Part[0].Instanciate;

   PinNr := 0;
   FillChar(Pin, SizeOf(TSEPinProperties), 0);
   while SEHost[0].Part[0].GetPinProperties(PinNr, Pin) do
    begin
     Memo.Lines.Add('');
     Memo.Lines.Add('Pin '            + IntToStr(PinNr + 1));
     case Pin.Direction of
      drIn           : Memo.Lines.Add('DataType: Input');
      drOut          : Memo.Lines.Add('DataType: Output');
      drContainer_IO : Memo.Lines.Add('DataType: Container I/O');
      drParameter    : Memo.Lines.Add('DataType: Parameter');
      else Memo.Lines.Add('Unknown');
     end;
     case Pin.Datatype of
      dtNone     : Memo.Lines.Add('DataType: None');
      dtEnum     : Memo.Lines.Add('DataType: Enum');
      dtText     : Memo.Lines.Add('DataType: Text');
      dtMidi2    : Memo.Lines.Add('DataType: Midi2');
      dtDouble   : Memo.Lines.Add('DataType: Double');
      dtBoolean  : Memo.Lines.Add('DataType: Boolean');
      dtFSample  : Memo.Lines.Add('DataType: Float Sample');
      dtSingle   : Memo.Lines.Add('DataType: Single');
      dtVstParam : Memo.Lines.Add('DataType: VST Parameter');
      dtInteger  : Memo.Lines.Add('DataType: Integer');
      dtBlob     : Memo.Lines.Add('DataType: Blob');
     end;
     Memo.Lines.Add('Name: '          + Pin.Name);
     Memo.Lines.Add('DefaultValue: '  + Pin.DefaultValue);
     if assigned(Pin.DatatypeExtra)
      then Memo.Lines.Add('DatatypeExtra: ' + Pin.DatatypeExtra);
     Memo.Lines.Add('Spare: '         + IntToStr(Pin.Spare));
     inc(PinNr);
    end;

   if SEHost[0].Part[0].Magic <> SepMagic then
    if (MessageBox(0, 'Module seems to be corrupted!' + #13#10 +
          'Would you like to create a patched version?', 'Module corrupted!',
          MB_ICONWARNING or MB_YESNO) = idYes) then SavePatchedModule;

(*
   SEHost[0].Part[0].Open;
   try
//    SEHost[0].Part[0].
   finally
    SEHost[0].Part[0].Close;
   end;
*)
  end;
end;

procedure TFmSEModuleExplorer.SavePatchedModule;
var
  FN : TFileName;
  RS : TResourceStream;
  RM : TPEResourceModule;
  RD : TResourceDetails;
begin
 RM := TPEResourceModule.Create;
 with RM do
  try
   // load template
   RS := TResourceStream.Create(HInstance, 'SEMagicHealer', 'SEM');
   try
    LoadFromStream(RS);
   finally
    FreeAndNil(RS);
   end;
   FN := SEHost[0].SEMFileName;

   // store VST Plugins
   with TMemoryStream.Create do
    try
     LoadFromFile(FN);
     RD := TResourceDetails.CreateResourceDetails(RM, 0, 'SEM', 'SEM', Size, Memory);
     AddResource(RD);
    finally
     Free;
    end;

   SortResources;
   SaveToFile(ExtractFilePath(FN) + 'X' + ExtractFileName(FN));
   ShowMessage('Patched SEM successfully created!');
  finally
   FreeAndNil(RM);
  end;
end;

procedure TFmSEModuleExplorer.MIExitClick(Sender: TObject);
begin
 Close;
end;

end.
