unit IRMTmain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, StdCtrls, Contnrs, DAV_Classes, DAV_DspFilter, DAV_AudioData,
  DAV_DifferentialEvolution,  DAV_GuiGroup, DAV_GuiAudioDataDisplay,
  DAV_AudioFile, DAV_AudioFileWAV, DAV_AudioFileAIFF, DAV_AudioFileAU,
  DAV_DspHrtf;

type
  TOptimizer = class(TThread)
  private
    FOwner : TComponent;
  protected
    constructor Create(AOwner: TComponent); reintroduce; virtual; 
    procedure Execute; override;
  end;

  TFmImpulseResponseMatchTool = class(TForm)
    Adc: TAudioDataCollection32;
    AdOriginal: TGuiAudioDataDisplay;
    AdResidual: TGuiAudioDataDisplay;
    GbLog: TGuiGroup;
    GbOriginal: TGuiGroup;
    GbResidual: TGuiGroup;
    MainMenu: TMainMenu;
    MeLog: TMemo;
    MiFile: TMenuItem;
    MiFileExit: TMenuItem;
    MiFileOpen: TMenuItem;
    MiMatch: TMenuItem;
    MiMatchStart: TMenuItem;
    MiNew: TMenuItem;
    MiS1: TMenuItem;
    MiS2: TMenuItem;
    OdImpulseResponse: TOpenDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure MiNewClick(Sender: TObject);
    procedure MiFileOpenClick(Sender: TObject);
    procedure MiFileClick(Sender: TObject);
    procedure MiFileExitClick(Sender: TObject);
    procedure MiMatchStartClick(Sender: TObject);
  private
    FIniFileName   : TFileName;
    FFilterCascade : TFilterCascade;
    FDiffEvol      : TDifferentialEvolution;
    FOptimizer     : TOptimizer;
    FTrialFilter   : TBiquadIIRFilter;
    FCurrentGain   : Single;
    FUpdateCount   : Integer;
    procedure GenerateImpulseResponse;
  public
    function CalculateCosts(Sender: TObject; const Population: TDifferentialEvolutionPopulation): Double;
    procedure LoadFromFile(AudioFileName: TFileName);
    procedure UpdateRecentFileHistory;
    procedure ThreadedStep;
    procedure ThreadedUpdate;
  end;

var
  FmImpulseResponseMatchTool: TFmImpulseResponseMatchTool;

implementation

{$R *.dfm}

uses
  Math, IniFiles, DAV_Common, DAV_Approximations, DAV_DspFilterBasics,
  DAV_DspPinkNoiseGenerator;

{ TOptimizer }

constructor TOptimizer.Create(AOwner: TComponent);
begin
 inherited Create(True);
 FOwner := AOwner;
end;

procedure TOptimizer.Execute;
begin
 inherited;
 while not Terminated
  do TFmImpulseResponseMatchTool(FOwner).ThreadedStep;
end;


{ TFmImpulseResponseMatchTool }

procedure TFmImpulseResponseMatchTool.FormCreate(Sender: TObject);
begin
 FIniFileName := ExtractFilePath(ParamStr(0)) + 'IRMT.ini';
 FFilterCascade := TFilterCascade.Create;

 UpdateRecentFileHistory;
 GenerateImpulseResponse;
end;

procedure TFmImpulseResponseMatchTool.FormDestroy(Sender: TObject);
begin
 if Assigned(FOptimizer) then
  with FOptimizer do
   begin
    Resume;
    Terminate;
    WaitFor;
    Free;
   end;
 FOptimizer := nil;  

 FreeAndNil(FFilterCascade);
 FreeAndNil(FDiffEvol);
end;

procedure TFmImpulseResponseMatchTool.FormShow(Sender: TObject);
begin
 with TIniFile.Create(FIniFileName) do
  try
   Left := ReadInteger('Layout', 'Left', Left);
   Top := ReadInteger('Layout', 'Top', Top);
  finally
   Free;
  end;
end;

procedure TFmImpulseResponseMatchTool.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
 with TIniFile.Create(FIniFileName) do
  try
   WriteInteger('Layout', 'Left', Left);
   WriteInteger('Layout', 'Top', Top);
  finally
   Free;
  end;
end;

procedure TFmImpulseResponseMatchTool.MiNewClick(Sender: TObject);
begin
 GenerateImpulseResponse;
end;

procedure TFmImpulseResponseMatchTool.MiFileOpenClick(Sender: TObject);
begin
 with OdImpulseResponse do
  begin

   if Execute
    then LoadFromFile(FileName);

  end;
end;

procedure TFmImpulseResponseMatchTool.MiFileExitClick(Sender: TObject);
begin
 Close;
end;

procedure TFmImpulseResponseMatchTool.MiMatchStartClick(Sender: TObject);
begin
 if Assigned(FDiffEvol)
  then FreeAndNil(FDiffEvol);

 FDiffEvol := TDifferentialEvolution.Create(Self);
 with FDiffEvol do
  begin
   VariableCount := 3;
   MinArr[0] := -15.00;
   MaxArr[0] :=  15.00;
   MinArr[1] :=   0.00;
   MaxArr[1] :=   1.00;
   MinArr[2] :=   0.01;
   MaxArr[2] :=  10.00;
   Initialize;
   OnCalcCosts := CalculateCosts;
  end;

 FUpdateCount := 0;
 FTrialFilter := TBasicLowpassFilter.Create;

 // start optimizer thread;
 FOptimizer := TOptimizer.Create(Self);
 FOptimizer.Resume;
end;

procedure TFmImpulseResponseMatchTool.ThreadedStep;
begin
 if Assigned(FDiffEvol) then
  begin
   FCurrentGain := FDiffEvol.Evolve;

   Inc(FUpdateCount);
   if FUpdateCount > 5 then
    begin
     FUpdateCount := 0;
     FOptimizer.Synchronize(ThreadedUpdate);
    end;
  end;
end;

procedure TFmImpulseResponseMatchTool.ThreadedUpdate;
var
  SampleIndex: Integer;
begin
 MeLog.Lines.Add(FloatToStrF(FCurrentGain, ffGeneral, 4, 4));

 with TBasicLowpassFilter.Create do
  try
   Gain := FDiffEvol.BestArr[0];
   Frequency := FreqLinearToLog(FDiffEvol.BestArr[1]);
   Bandwidth := FDiffEvol.BestArr[2];

   Adc.ChannelDataPointer[1]^[0] := Adc.ChannelDataPointer[0]^[0] - ProcessSample64(1);
   for SampleIndex := 1 to Adc.SampleFrames - 1
    do Adc.ChannelDataPointer[1]^[SampleIndex] := Adc.ChannelDataPointer[0]^[SampleIndex] - ProcessSample64(0);
  finally
   Free;  
  end;
end;

procedure TFmImpulseResponseMatchTool.UpdateRecentFileHistory;
var
  ItemPos, j  : Integer;
  MenuItem    : TMenuItem;
  FileSection : TStringList;
begin
 with TIniFile.Create(FIniFileName) do
  try
   FileSection := TStringList.Create;
   try
    ReadSectionValues('Recent Files', FileSection);

    // remove duplicates
    ItemPos := 0;
    while ItemPos < FileSection.Count do
     begin
      j := ItemPos + 1;
      while j < FileSection.Count do
       if FileSection.ValueFromIndex[ItemPos] = FileSection.ValueFromIndex[j]
        then FileSection.Delete(j)
        else Inc(j);

      Inc(ItemPos);
     end;

    // sort file section and erase
    FileSection.Sort;
    EraseSection('Recent Files');

    // write file section back on disc
    ItemPos := 1;
    while FileSection.Count > 0 do
     begin
      WriteString('Recent Files', 'File' + IntToStr(ItemPos),
        FileSection.ValueFromIndex[0]);
      FileSection.Delete(0);
      Inc(ItemPos);
     end;

    // reload file section 
    ReadSectionValues('Recent Files', FileSection);

    // clear all old menu items
    while MiS1.MenuIndex + 1 < MiS2.MenuIndex do
     begin
      MenuItem := MainMenu.Items[0].Items[MiS1.MenuIndex + 1];
      MainMenu.Items[0].Delete(MiS1.MenuIndex + 1);
      FreeAndNil(MenuItem);
     end;


    for ItemPos := 0 to FileSection.Count - 1 do
     begin
      if MiS1.MenuIndex + ItemPos + 1 >= MiS2.MenuIndex then
       begin
        MenuItem := TMenuItem.Create(MainMenu);
        MainMenu.Items[0].Insert(MiS1.MenuIndex + ItemPos + 1, MenuItem);
       end
      else MenuItem := MainMenu.Items[0].Items[MiS1.MenuIndex + ItemPos + 1];
      MenuItem.Tag := ItemPos;
      MenuItem.Name := FileSection.Names[ItemPos];
      MenuItem.Caption := FileSection.Values[FileSection.Names[ItemPos]];
      MenuItem.Visible := True;
      MenuItem.OnClick := MiFileClick;
      MenuItem.OnAdvancedDrawItem := MainMenu.Items[0].OnAdvancedDrawItem;
      MenuItem.OnDrawItem := MainMenu.Items[0].OnDrawItem;
     end;

    MiS2.Visible := FileSection.Count > 0;
   finally
    FileSection.Free;
   end;
  finally
   Free;
  end;
end;

procedure TFmImpulseResponseMatchTool.MiFileClick(Sender: TObject);
var
  theFile : TFilename;
begin
 Assert(Sender is TMenuItem);

 with TIniFile.Create(FIniFileName) do
  try
   theFile := ReadString('Recent Files', TMenuItem(Sender).Name,
                                         TMenuItem(Sender).Caption);
   DeleteKey('Recent Files', TMenuItem(Sender).Name);
  finally
   Free;
  end;

 if FileExists(theFile)
  then LoadFromFile(theFile)
  else MessageDlg('Recent file does not exists', mtError, [mbOK], 0);
end;

procedure TFmImpulseResponseMatchTool.LoadFromFile(AudioFileName: TFileName);
begin
 Adc.LoadFromFile(AudioFileName);
 AdOriginal.Invalidate;
 Adc.ChannelCount := 1;
 Adc.ChannelCount := 2;

 with TIniFile.Create(FIniFileName) do
  try
   WriteString('Program', 'Open File Path', ExtractFilePath(AudioFileName));
   WriteString('Recent Files', 'File0', AudioFileName);
  finally
   Free;
  end;

 UpdateRecentFileHistory;
end;

procedure TFmImpulseResponseMatchTool.GenerateImpulseResponse;
var
  SampleIndex    : Integer;
  Envelope       : Double;
  DecayPerSample : Double;
begin
 // generate impulse response
 DecayPerSample := Power(dB_to_Amp(-96), 1 / Adc.SampleFrames);
 Envelope := 1;
 Adc.BeginUpdate;
 with TPinkNoiseGenerator.Create do
  try
   // get some randomness
   for SampleIndex := 0 to 10 + Random(10)
    do ProcessSample32;
//   RandSeed := $DEADBEEF;

   for SampleIndex := 0 to Adc.SampleFrames - 1 do
    begin
     Adc.ChannelDataPointer[0]^[SampleIndex] := ProcessSample32 * Envelope;
     Envelope := Envelope * DecayPerSample;
    end;
  finally
   Free;
  end;
 Adc.EndUpdate;
 AdOriginal.Invalidate;
end;

function TFmImpulseResponseMatchTool.CalculateCosts(Sender: TObject;
  const Population: TDifferentialEvolutionPopulation): Double;
var
  SampleIndex: Integer;
begin
 with FTrialFilter do
  begin
   if (Population[1] < 0) or (Population[1] > 1) or (Population[2] < 0.01) then
    begin
     Result := 1000;
     Exit;
    end;

   Gain := Population[0];
   Frequency := FreqLinearToLog(Population[1]);
   Bandwidth := Population[2];
   ResetStates;

   Result := Sqr(Adc.ChannelDataPointer[0]^[0] - ProcessSample64(1));
   for SampleIndex := 1 to Adc.SampleFrames - 1
    do Result := Result + Sqr(Adc.ChannelDataPointer[0]^[SampleIndex] - ProcessSample64(0));

   Result := 10 * FastLog10MinError3(Sqrt(Result / Adc.SampleFrames));
  end;
end;

end.
