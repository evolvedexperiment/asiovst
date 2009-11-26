unit EqGraphTestMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, DAV_GuiEQGraph, DAV_DspFilter, DAV_DspFilterBasics;

type
  TFmEqGraphTest = class(TForm)
    EqGraphA: TGuiEQGraph;
    EqGraphB: TGuiEQGraph;
    EqGraphC: TGuiEQGraph;
    EqGraphD: TGuiEQGraph;
    function EqGraphGetFilterGain(Sender: TObject; const Frequency: Single): Single;
    procedure FormCreate(Sender: TObject);
    function EqGraphAFilterSeries1GetFilterGain(Sender: TObject;
      const Frequency: Single): Single;
  private
    FLowpass : TBasicLowpassFilter;
  public
    { Public-Deklarationen }
  end;

var
  FmEqGraphTest: TFmEqGraphTest;

implementation

{$R *.dfm}

procedure TFmEqGraphTest.FormCreate(Sender: TObject);
begin
 FLowpass := TBasicLowpassFilter.Create;
end;

function TFmEqGraphTest.EqGraphAFilterSeries1GetFilterGain(Sender: TObject;
  const Frequency: Single): Single;
begin
 Result := FLowpass.MagnitudeLog10(0.5 * Frequency);
end;

function TFmEqGraphTest.EqGraphGetFilterGain(Sender: TObject;
  const Frequency: Single): Single;
begin
 Result := FLowpass.MagnitudeLog10(Frequency);
end;

end.
