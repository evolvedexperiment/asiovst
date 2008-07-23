unit WavedisplayGUI;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, DAVDCommon, DVSTModule,
  Controls, DGuiBaseControl, DGuiStaticWaveform, DGuiDynamicWaveform,
  ExtCtrls, StdCtrls, DGuiLevelMeter;

type
  TWavedisplayGUI = class(TForm)
    Display: TGuiDynamicWaveform;
    LbDrawMode: TLabel;
    LbWaveSize: TLabel;
    LbProcessingMode: TLabel;
    ddWaveSize: TComboBox;
    ddDrawMode: TComboBox;
    ddProcessing: TComboBox;
    LevelMeter: TGuiLevelMeter;
    procedure ddProcessingChange(Sender: TObject);
    procedure ddWaveSizeChange(Sender: TObject);
    procedure ddDrawModeChange(Sender: TObject);
  end;

implementation

{$R *.DFM}

procedure TWavedisplayGUI.ddProcessingChange(Sender: TObject);
begin
 with Display do
  case ddProcessing.ItemIndex of
   0: WaveProcessMode := wpmScroll;
   1: WaveProcessMode := wpmReplace;
   2: WaveProcessMode := wpmStretch;
  end;
end;

procedure TWavedisplayGUI.ddWaveSizeChange(Sender: TObject);
begin
 with Display do
  case ddWaveSize.ItemIndex of
   0: InternalBufferSize := 256;
   1: InternalBufferSize := 512;
   2: InternalBufferSize := 1024;
   3: InternalBufferSize := 2048;
   4: InternalBufferSize := 4096;
   5: InternalBufferSize := 8192;
  end;
end;

procedure TWavedisplayGUI.ddDrawModeChange(Sender: TObject);
begin
 with Display do
  case ddDrawMode.ItemIndex of
   0: WaveDrawMode := wdmSolid;
   1: WaveDrawMode := wdmOutline;
   2: WaveDrawMode := wdmPoints;
   3: WaveDrawMode := wdmSimple;
  end;
end;

end.
