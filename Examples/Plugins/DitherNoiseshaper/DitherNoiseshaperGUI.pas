unit DitherNoiseshaperGUI;

interface

uses
  Windows, Messages, Classes, Forms, Controls, StdCtrls, Spin, DAV_Common,
  DAV_VSTModule;

type
  TFmDitherNoiseshaper = class(TForm)
    LbNoiseshaperType: TLabel;
    CBNoiseshaperType: TComboBox;
    LbFinalBitDepth: TLabel;
    SEBitDepth: TSpinEdit;
    LbBit: TLabel;
    procedure CBNoiseshaperTypeChange(Sender: TObject);
    procedure SEBitDepthChange(Sender: TObject);
  end;

implementation

{$R *.DFM}

uses
  DitherNoiseshaperDM;

procedure TFmDitherNoiseshaper.SEBitDepthChange(Sender: TObject);
begin
 TDitherNoiseshaperModule(Owner).Parameter[0] := SEBitDepth.Value;
end;

procedure TFmDitherNoiseshaper.CBNoiseshaperTypeChange(Sender: TObject);
begin
 TDitherNoiseshaperModule(Owner).Parameter[1] := CBNoiseshaperType.ItemIndex;
end;

end.
