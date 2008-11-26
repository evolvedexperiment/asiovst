unit ConvolutionGUI;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, Controls, DAV_Common,
  DAV_VSTModule, DAV_GuiLabel, DAV_GuiBaseControl, DAV_GuiDial, StdCtrls;

type
  TFmConvolution = class(TForm)
    EdFileName: TEdit;
    LbFileName: TLabel;
    BtLoad: TButton;
    procedure BtLoadClick(Sender: TObject);
    procedure EdFileNameChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
  public
  end;

implementation

{$R *.DFM}

uses
  Dialogs, ConvolutionDM, DAV_VSTModuleWithPrograms;

procedure TFmConvolution.BtLoadClick(Sender: TObject);
begin
 with TOpenDialog.Create(Self) do
  try
   DefaultExt := 'wav';
   Filter := 'Wave (*.wav)|*.wav';
   Options := [ofHideReadOnly, ofFileMustExist, ofEnableSizing];
   Title := 'Select an Impulse Response';
   if Execute then EdFileName.Text := FileName;
  finally
   Free; 
  end;
end;

procedure TFmConvolution.EdFileNameChange(Sender: TObject);
begin
 if FileExists(EdFileName.Text)
  then TConvolutionDataModule(Owner).LoadIR(EdFileName.Text);
end;

procedure TFmConvolution.FormShow(Sender: TObject);
begin
 EdFileName.Text := 'C:\MFW\Bin\Audio\LS-MI.wav';
end;

end.
