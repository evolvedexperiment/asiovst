unit AEVstSetup;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls;

type
  TFmVSTSetup = class(TForm)
    ListView: TListView;
    StatusBar: TStatusBar;
    PnSelect: TPanel;
    BtDirectorySelect: TButton;
    BtScan: TButton;
    EdDirectory: TEdit;
    procedure BtScanClick(Sender: TObject);
    procedure EdDirectoryChange(Sender: TObject);
    procedure BtDirectorySelectClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  FmVSTSetup: TFmVSTSetup;

implementation

{$R *.dfm}

uses
  FileCtrl, DAV_VSTHost, AEmain;

procedure TFmVSTSetup.BtDirectorySelectClick(Sender: TObject);
var
  Dir : string;
begin
 SelectDirectory('Select a directory', '', Dir);
 EdDirectory.Text := Dir;
 if DirectoryExists(EdDirectory.Text)
  then BtScan.SetFocus;
end;

procedure TFmVSTSetup.BtScanClick(Sender: TObject);
var
  SR      : TSearchRec;
begin
 ListView.Clear;
 if not DirectoryExists(EdDirectory.Text)
  then exit;

 if FindFirst(EdDirectory.Text + '\' + '*.dll', faAnyFile, SR) = 0 then
  try
   repeat
    with FmAudioEditor.VSTHost[0], ListView.Items.Add do
     try
      Caption := SR.Name;
      LoadFromFile(EdDirectory.Text + '\' + SR.Name);
      if not Loaded
       then raise Exception.CreateFmt('Could not load %s', [SR.Name]);

      try
       SubItems.Add(UniqueID);
       SubItems.Add(IntToStr(numPrograms));
       SubItems.Add(IntToStr(numParams));
       SubItems.Add(IntToStr(numInputs));
       SubItems.Add(IntToStr(numOutputs));
       SubItems.Add(EffOptions2String(EffectOptions));
       SubItems.Add(IntToStr(InitialDelay));
       SubItems.Add(IntToStr(RealQualities));
       SubItems.Add(IntToStr(OffQualities));
       SubItems.Add(FloatToStr(IORatio));
       SubItems.Add(IntToStr(Version));

       sleep(1); Application.ProcessMessages;
       Open;
       try
        sleep(1); Application.ProcessMessages;
        SubItems.Add(EffectName);
        SubItems.Add(ProductString);
        SubItems.Add(VendorString);
       finally
        Close;
       end;
      finally
       UnLoad;
      end;
     except
      On E: Exception do MessageDlg(E.Message, mtError, [mbOK], 0);
     end;
    sleep(1);
    Application.ProcessMessages;
   until FindNext(SR) <> 0;

  finally
   // Must free up resources used by these successful finds
   FindClose(SR);
  end;
end;

procedure TFmVSTSetup.EdDirectoryChange(Sender: TObject);
begin
 BtScan.Enabled := DirectoryExists(EdDirectory.Text)
end;

procedure TFmVSTSetup.FormShow(Sender: TObject);
begin
 EdDirectory.Text := FmAudioEditor.VSTHost.PlugInDir;
end;

end.
