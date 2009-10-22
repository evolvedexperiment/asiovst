unit ChebyshevWaveshaperGUI;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, Controls, Graphics, DAV_Types,
  DAV_VSTModule, DAV_GuiBaseControl, DAV_GuiDial, DAV_GuiLabel;

type
  TFmChebyshevWaveshaper = class(TForm)
    procedure GuiDialChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FDials  : array of TGuiDial;
    FLabels : array of TGuiLabel;
  public
    procedure UpdateHarmonic(Index: Integer);  
  end;

implementation

{$R *.DFM}

uses
  Math, ChebyshevWaveshaperDM, DAV_VSTModuleWithPrograms;

procedure TFmChebyshevWaveshaper.FormCreate(Sender: TObject);
var
  i  : Integer;
  RS : TResourceStream;
begin
 ClientWidth := 8 + 6 * 56;
 ClientHeight := 8 + 4 * 72;

 RS := TResourceStream.Create(hInstance, 'ChebyshevWaveshaper', 'BMP');
 try
  with TChebyshevWaveshaperDataModule(Owner) do
   begin
    SetLength(FDials, numParams);
    SetLength(FLabels, numParams);
    for i := 0 to numParams - 2 do
     begin
      FDials[i] := TGuiDial.Create(Self);
      with FDials[i] do
       begin
        Parent   := Self;
        Anchors  := [];
        Width    := 48;
        Height   := 48;
        Left     := 8 + (i mod 6 * (Width + 8));
        Top      := 8 + (i div 6) * 72;
        DialBitmap.LoadFromStream(RS);
        RS.Position       := 0;
        NumGlyphs         := 65;
        Min               := -1;
        Max               := 1;
        ScrollRange_Pixel := 200;
        StitchKind        := skHorizontal;
        Tag               := i;
        Position          := sign(Parameter[i]) * sqr(Parameter[i]);
        OnChange          := GuiDialChange;
       end;
      FLabels[i] := TGuiLabel.Create(Self);
      with FLabels[i] do
       begin
        Parent    := Self;
        Anchors   := [];
        Width     := 48;
        Height    := 16;
        Left      := 8 + (i mod 6 * (Width + 8));
        Top       := 56 + (i div 6) * 72;
        Alignment := taCenter;
        AntiAlias := gaaLinear4x;
        if i = 0
         then Caption := 'Fun.'
         else Caption := 'H' + IntToStr(i + 1);
        Font.Charset := DEFAULT_CHARSET;
        Font.Color   := 13877402;
        Font.Height  := -13;
        Font.Name    := 'Verdana';
        Font.Style   := [fsBold];
        Tag          := i;
       end;
     end;
   end;
 finally
  FreeAndNil(RS);
 end;
end;

procedure TFmChebyshevWaveshaper.FormDestroy(Sender: TObject);
var
  i : Integer;
begin
 with TChebyshevWaveshaperDataModule(Owner) do
  begin
   for i := 0 to numParams - 1 do
    begin
     FreeAndNil(FDials[i]);
     FreeAndNil(FLabels[i]);
    end;
   SetLength(FDials, 0);
   SetLength(FLabels, 0);
  end;
end;

procedure TFmChebyshevWaveshaper.GuiDialChange(Sender: TObject);
var
  NewValue : Single;
begin
 with TChebyshevWaveshaperDataModule(Owner), (Sender as TGuiDial) do
  begin
   NewValue := sign(Position) * sqrt(abs(Position));
   if Parameter[Tag] <> NewValue
    then Parameter[Tag] := NewValue;
  end;
end;

procedure TFmChebyshevWaveshaper.UpdateHarmonic(Index: Integer);
begin
 with TChebyshevWaveshaperDataModule(Owner), FDials[Index] do
  begin
   Position := sign(Parameter[Index]) * sqr(Parameter[Index]);
  end;
end;

end.