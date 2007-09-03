unit VSTPropertySheet_Page;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, DVSTHost;

type
  TFmPage = class(TForm)
    VstHost: TVstHost;
    Memo: TMemo;
  private
    FFileName: TFileName;
    procedure SetFileName(const Value: TFileName);
  published
  public
    property FileName : TFileName read FFileName write SetFileName;
  end;

implementation

{$R *.dfm}

{ TFmPage }

procedure TFmPage.SetFileName(const Value: TFileName);
begin
 Memo.Clear;
 FFileName := Value;
 if FileExists(FFileName) then
  with VstHost[0] do
   try
    DLLFileName := FFileName;
    Active := True;
    with Memo.Lines do
     begin
      Add('Effect Name: ' + GetEffectName {+ 'Unique ID: ' + PVstEffect^.UniqueID + ', '});
      Add('Vendor: ' + VendorString + ', Product: ' + ProductString);
      Add('VST Version: ' + IntToStr(GetVstVersion) + ', Category: ' + PlugCategory2String(GetPlugCategory));
      Add('Inputs: ' + IntToStr(numInputs) + ', Outputs: ' + IntToStr(numOutputs));
      Add('Parameters: ' + IntToStr(numParams));
      Add('Programs: ' + IntToStr(numPrograms));
      Add('Initial Delay: ' + IntToStr(InitialDelay) + ', Tail Size: ' + IntToStr(GetTailSize));
     end;
   finally
    if not Active
     then Memo.Lines.Add('Error while loading');
    Active := False;
    UnLoad;
   end;
end;

end.
