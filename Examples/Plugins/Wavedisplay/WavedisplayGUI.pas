unit WavedisplayGUI;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, DAVDCommon, DVSTModule,
  Controls, DGuiBaseControl, DGuiStaticWaveform, DGuiDynamicWaveform,
  ExtCtrls, StdCtrls;

type
  TWavedisplayGUI = class(TForm)
    Display: TGuiDynamicWaveform;
    ddDrawMode: TComboBox;
    Label1: TLabel;
    ddWaveSize: TComboBox;
    Label2: TLabel;
    Label3: TLabel;
    ddProcessing: TComboBox;
    procedure ddProcessingChange(Sender: TObject);
    procedure ddWaveSizeChange(Sender: TObject);
    procedure ddDrawModeChange(Sender: TObject);
  public
    WavedisplayModule: TVSTModule;
  end;

implementation

{$R *.DFM}

procedure TWavedisplayGUI.ddProcessingChange(Sender: TObject);
begin
  case ddProcessing.ItemIndex of
    0: Display.WaveProcessMode := wpmScroll;
    1: Display.WaveProcessMode := wpmReplace;
    2: Display.WaveProcessMode := wpmStretch;
  end;
end;

procedure TWavedisplayGUI.ddWaveSizeChange(Sender: TObject);
begin
  case ddWaveSize.ItemIndex of
    0: Display.InternalBufferSize := 256;
    1: Display.InternalBufferSize := 512;
    2: Display.InternalBufferSize := 1024;
    3: Display.InternalBufferSize := 2048;
    4: Display.InternalBufferSize := 4096;
    5: Display.InternalBufferSize := 8192;
  end;
end;

procedure TWavedisplayGUI.ddDrawModeChange(Sender: TObject);
begin
  case ddDrawMode.ItemIndex of
    0: Display.WaveDrawMode:=wdmSolid;
    1: Display.WaveDrawMode:=wdmOutline;
    2: Display.WaveDrawMode:=wdmPoints;
  end;
end;

end.
