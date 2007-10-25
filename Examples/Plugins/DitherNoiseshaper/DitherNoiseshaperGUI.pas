unit DitherNoiseshaperGUI;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, DAVDCommon, DVSTModule, Controls,
  StdCtrls;

type
  TFmDitherNoiseshaper = class(TForm)
    LbNoiseshaperType: TLabel;
    CBNoiseshaperType: TComboBox;
    procedure CBNoiseshaperTypeChange(Sender: TObject);
  end;

implementation

{$R *.DFM}

uses
  DitherNoiseshaperDM;

procedure TFmDitherNoiseshaper.CBNoiseshaperTypeChange(Sender: TObject);
begin
 TDitherNoiseshaperModule(Owner).Parameter[0] := CBNoiseshaperType.ItemIndex;
end;

end.