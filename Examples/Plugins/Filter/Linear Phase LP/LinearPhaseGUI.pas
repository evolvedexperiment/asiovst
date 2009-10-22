unit LinearPhaseGUI;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, Controls, DAV_Types,
  DAV_VSTModule, DAV_GuiLabel, DAV_GuiBaseControl, DAV_GuiDial;

type
  TFmLinearPhase = class(TForm)
    DialFrequency: TGuiDial;
    LbFrequency: TGuiLabel;
    LbFrequencyValue: TGuiLabel;
    procedure DialFrequencyChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  public
    procedure UpdateFrequency;
  end;

implementation

{$R *.DFM}

uses
  LinearPhaseDM, DAV_VSTModuleWithPrograms;

procedure TFmLinearPhase.FormCreate(Sender: TObject);
var
  RS  : TResourceStream;
begin
 RS := TResourceStream.Create(hInstance, 'YamahaKnob', 'BMP');
 try
  DialFrequency.DialBitmap.LoadFromStream(RS);
 finally
  RS.Free;
 end;
end;

procedure TFmLinearPhase.FormShow(Sender: TObject);
begin
 UpdateFrequency;
end;

procedure TFmLinearPhase.DialFrequencyChange(Sender: TObject);
begin
 with TLinearPhaseDataModule(Owner) do
  begin
   if Parameter[0] <> DialFrequency.Position
    then Parameter[0] := DialFrequency.Position;
  end;
end;

procedure TFmLinearPhase.UpdateFrequency;
var
  Freq : Single;
begin
 with TLinearPhaseDataModule(Owner) do
  begin
   Freq := Parameter[0];
   if DialFrequency.Position <> Freq
    then DialFrequency.Position := Freq;
   if Freq < 1000
    then LbFrequencyValue.Caption := FloatToStrF(Freq, ffGeneral, 3, 3) + ' Hz'
    else LbFrequencyValue.Caption := FloatToStrF(1E-3 * Freq, ffGeneral, 3, 3) + ' kHz';
  end;
end;

end.
