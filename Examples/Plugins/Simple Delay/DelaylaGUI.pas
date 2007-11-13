unit DelaylaGUI;

interface

uses
  Windows, SysUtils, Classes, Forms, Controls, StdCtrls, DVSTModule, DAVDCommon;

type
  TVSTGUI = class(TForm)
    SampleBar: TScrollBar;
    LbSamples: TLabel;
    procedure SampleBarChange(Sender: TObject);
  private
  public
  end;

implementation

{$R *.DFM}

uses DelaylaModule;

procedure TVSTGUI.SampleBarChange(Sender: TObject);
begin
 LbSamples.Caption:=IntToStr(SampleBar.Position);
 TSimpleDelayVST(Owner).Parameter[0]:=SampleBar.Position;
end;

end.