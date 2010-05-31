unit UFEmain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Spin;

type
  TFmUnknownFileExplorer = class(TForm)
    EdFileName: TEdit;
    LbFileName: TLabel;
    BtFileSelect: TButton;
    OD: TOpenDialog;
    BtConvert: TButton;
    EdHex: TEdit;
    LbStartHex: TLabel;
    Memo: TMemo;
    SeValueCount: TSpinEdit;
    LbValueCount: TLabel;
    LbDataFormat: TLabel;
    RbSingle: TRadioButton;
    RbDouble: TRadioButton;
    CbSwap: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure BtFileSelectClick(Sender: TObject);
    procedure EdFileNameChange(Sender: TObject);
    procedure BtConvertClick(Sender: TObject);
    procedure LbFileNameClick(Sender: TObject);
  private
    FIniFileName   : TFileName;
    FFileUnderTest : TFileStream;
  public
    { Public-Deklarationen }
  end;

var
  FmUnknownFileExplorer: TFmUnknownFileExplorer;

implementation

uses
  Math, IniFiles, DAV_Common;

{$R *.dfm}

procedure TFmUnknownFileExplorer.BtFileSelectClick(Sender: TObject);
begin
 if OD.Execute then
  begin
   EdFileName.Text := OD.FileName;
  end;
end;

procedure TFmUnknownFileExplorer.EdFileNameChange(Sender: TObject);
begin
 if FileExists(EdFileName.Text) then
  begin
   if Assigned(FFileUnderTest)
    then FreeAndNil(FFileUnderTest);

   FFileUnderTest := TFileStream.Create(EdFileName.Text, fmOpenRead);
  end;
end;

procedure TFmUnknownFileExplorer.FormCreate(Sender: TObject);
begin
 FIniFileName := ExtractFilePath(ParamStr(0)) + 'Unknown File Explorer.ini';
end;

procedure TFmUnknownFileExplorer.FormShow(Sender: TObject);
begin
 with TIniFile.Create(FIniFileName) do
  try
   EdFileName.Text := ReadString('Recent', 'Filename', EdFileName.Text);
   EdHex.Text := ReadString('Recent', 'Position', EdHex.Text);
  finally
   Free;
  end;
end;

procedure TFmUnknownFileExplorer.LbFileNameClick(Sender: TObject);
var
  Value   : Double;
  IntCast : Int64 absolute Value;
  ByteVal : Byte;
  IdIndx  : Byte;
  IdCount : Byte;
  Pos     : Integer;
  Index   : Integer;
  LogIndx : Integer;
  MS      : TMemoryStream;
const
  CIdentifier : array [0..7] of Byte = ($00, $00, $00, $00, $00, $60, $74,
    $C0);   
begin
 if Assigned(FFileUnderTest) then
  begin
   MS := TMemoryStream.Create;
   FFileUnderTest.Position := 0;
   MS.LoadFromStream(FFileUnderTest);

   with MS do
    try
     LogIndx := 0;
     MS.Position := 0;

     while (MS.Position + 216 < MS.Size) do
      begin
       Read(ByteVal, 1);
       if ByteVal = CIdentifier[IdIndx] then
        begin
         Inc(IdIndx);
         if IdIndx >= 8 then
          begin
           IdIndx := 0;
           Inc(IdCount)
          end;
        end
       else
        begin
         IdIndx := 0;
         IdCount := 0;
        end;

       if IdCount >= 26 then
        begin
         IdCount := 0;
         Memo.Clear;
         Memo.Lines.Add('Position: ' + IntToHex(MS.Position, 8));
         Memo.Lines.Add('');

         // intensity
         Memo.Lines.Add('Intensity');

         // offset file
         MS.Seek(16, soFromCurrent);

         for Index := 0 to 26 do
          begin
           MS.Read(Value, SizeOf(Double));

           // offset file
           MS.Seek(-480, soFromCurrent);
           MS.Read(ByteVal, 1);
           MS.Seek(+479, soFromCurrent);

           if IsNan(Value)
            then Memo.Lines.Add('Error') else
           if ByteVal <> 0
            then Memo.Lines.Add('-' + FloatToStr(Value))
            else Memo.Lines.Add('+' + FloatToStr(Value));
          end;

         Memo.Lines.Add('');

         // pressure
         Memo.Lines.Add('Pressure');

         // offset file
         MS.Seek(16, soFromCurrent);

         for Index := 0 to 26 do
          begin
           MS.Read(Value, SizeOf(Double));

           if IsNan(Value)
            then Memo.Lines.Add('Error')
            else Memo.Lines.Add(FloatToStr(Value));
          end;

         Inc(LogIndx);
         Memo.Lines.SaveToFile('DataExtract' + IntToStr(LogIndx) + '.txt');
        end;
      end;

    finally
     Free;
    end;
  end;
 ShowMessage(IntToStr(LogIndx) + ' files extracted');  
end;

procedure TFmUnknownFileExplorer.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
 with TIniFile.Create(FIniFileName) do
  try
   WriteString('Recent', 'Filename', EdFileName.Text);
   WriteString('Recent', 'Position', EdHex.Text);
  finally
   Free;
  end;
end;

procedure TFmUnknownFileExplorer.BtConvertClick(Sender: TObject);
var
  Value32   : Single;
  Int32Cast : Integer absolute Value32;
  Value64   : Double;
  Int64Cast : Int64 absolute Value64;
  Pos       : Integer;
  Index     : Integer;
begin
 if Assigned(FFileUnderTest) then
  begin
   Pos := StrToInt(EdHex.Text);
   FFileUnderTest.Position := Pos;
   Memo.Clear;
   for Index := 0 to SeValueCount.Value - 1 do
    if RbSingle.Checked then
     begin
      FFileUnderTest.Read(Value32, SizeOf(Single));

      // swap bytes
      if CbSwap.Checked
       then Int32Cast := SWAP_32(Int32Cast);

      if IsNan(Value32)
       then Memo.Lines.Add('Error')
       else Memo.Lines.Add(FloatToStr(Value32));
     end else
    if RbDouble.Checked then
     begin
      FFileUnderTest.Read(Value64, SizeOf(Double));

      // swap bytes
      if CbSwap.Checked
       then Int64Cast := SWAP_64(Int64Cast);

      if IsNan(Value64)
       then Memo.Lines.Add('Error')
       else Memo.Lines.Add(FloatToStr(Value64));
     end;
  end;
end;

end.
