unit VSTPropertySheet_Page;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  StdCtrls, DVSTHost;

type
  TFmPage = class(TForm)
    VstHost: TVstHost;
    Memo: TMemo;
    procedure FormPaint(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FFileName  : TFileName;
    FGUIBitmap : TBitmap;
    procedure SetFileName(const Value: TFileName);
  published
  public
    property FileName : TFileName read FFileName write SetFileName;
  end;

implementation

{$R *.dfm}

uses DVSTEffect;

{ TFmPage }

procedure TFmPage.FormCreate(Sender: TObject);
begin
 FGUIBitmap := TBitmap.Create;
end;

procedure TFmPage.FormDestroy(Sender: TObject);
begin
 if Assigned(FGUIBitmap)
  then FGUIBitmap.Free;
end;

procedure TFmPage.FormPaint(Sender: TObject);
begin
 Canvas.Draw(0, 200, FGUIBitmap);
end;

procedure TFmPage.SetFileName(const Value: TFileName);
var r  : ERect;
    fm : TForm;
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

      if (effFlagsHasEditor in PVstEffect.EffectFlags) then
       begin
        fm := TForm.Create(nil);
        with fm do
         try
          EditOpen(Handle);
          EditIdle; Idle;
          r := EditGetRect;
          Width := r.Right - r.Left;
          Height := r.Bottom - r.Top;
          FGUIBitmap.Width := Self.Width;
          FGUIBitmap.Height := (Height * FGUIBitmap.Width) div Width;
          Sleep(100); Idle; EditIdle;
          StretchBlt(FGUIBitmap.Canvas.Handle, 0, 0, FGUIBitmap.Width, FGUIBitmap.Height,
                     Canvas.Handle, r.Left, r.Top, r.Right - r.Left, r.Bottom - r.Top, cmSrcCopy);
         finally
          EditClose;
         end;
        fm.Free;
       end;
     end;
   finally
    if not Active
     then Memo.Lines.Add('Error while loading');
    Active := False;
    UnLoad;
   end;
end;

end.
