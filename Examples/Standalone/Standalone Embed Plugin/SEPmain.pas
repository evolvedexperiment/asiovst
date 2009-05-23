unit SEPmain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, StdCtrls, DAV_DLLResources, DAV_GuiBaseControl, DAV_GuiLabel;

type
  TFmStandaloneEmbedPlugin = class(TForm)
    MainMenu: TMainMenu;
    MIFile: TMenuItem;
    MIExit: TMenuItem;
    N1: TMenuItem;
    MISelectVstPlugin: TMenuItem;
    OpenDialogVST: TOpenDialog;
    SaveDialogStandalone: TSaveDialog;
    LbEmbedPlugin: TGuiLabel;
    procedure MIExitClick(Sender: TObject);
    procedure MISelectVstPluginClick(Sender: TObject);
  end;

var
  FmStandaloneEmbedPlugin: TFmStandaloneEmbedPlugin;

implementation

{$R *.dfm}

procedure TFmStandaloneEmbedPlugin.MISelectVstPluginClick(Sender: TObject);
var
  Standalone : TPEResourceModule;
  RS         : TResourceStream;
  RD         : TResourceDetails;
begin
 if OpenDialogVST.Execute then
  with SaveDialogStandalone do
   if Execute then
    begin
     Standalone := TPEResourceModule.Create;

     RS := TResourceStream.Create(HInstance, 'Standalone', 'EXE');
     try
      Standalone.LoadFromStream(RS);
     finally
      FreeAndNil(RS);
     end;

     try
      with TMemoryStream.Create do
       try
        LoadFromFile(OpenDialogVST.FileName);
        RD := TResourceDetails.CreateResourceDetails(Standalone, 0, 'DLL', 'DLL', Size, Memory);
        Standalone.AddResource(RD);
       finally
        Free;
       end;

      Standalone.SortResources;
      Standalone.SaveToFile(FileName);
    finally
     FreeAndNil(Standalone);
    end;
   end;
end;

procedure TFmStandaloneEmbedPlugin.MIExitClick(Sender: TObject);
begin
 Close;
end;

end.
