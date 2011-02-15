unit LoudnessMeterDSP;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, SyncObjs,
  DAV_Types, DAV_VSTModule, DAV_DspFilter, DAV_DspFilterBasics,
  DAV_DspDelayLines, DAV_DspR128;

type
  PPLinkedLoudnessRecord = ^PLinkedLoudnessRecord;
  PLinkedLoudnessRecord = ^TLinkedLoudnessRecord;
  TLinkedLoudnessRecord = record
    Loudness : Single;
    Value    : Single;
    Next     : PLinkedLoudnessRecord;
  end;

  TLoudnessMeterModule = class(TVSTModule)
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleDestroy(Sender: TObject);
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleProcessMono(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcessStereo(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure ParameterScaleChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterScaleDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterStateChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterStateDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterTimeDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterTimeChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterLoudnessDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterPeakMomDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterPeakMomChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterLoudnessChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    FR128Stereo          : TStereoR128;

    FTotalSamples        : Integer;
    FUpdateSampleCount   : Integer;
    FUpdateSamples       : Integer;
    FIsRunning           : Boolean;
    FUnitOffset          : Single;
    procedure PeakLoudnessChanged(Sender: TObject);
    procedure LoudnessChanged(Sender: TObject);
  protected
    FCriticalSection  : TCriticalSection;
    function GetLoudness: Single;
    function GetPeakHoldLoudness: Single;
    function GetLoudnessShort: Single;
    function GetLoudnessMomentary: Single;
    function GetLoudnessIntegration: Single;
  public
    procedure ChooseProcess;
    procedure ResetPeak;

    property LoudnessShort: Single read GetLoudnessShort;
    property LoudnessMomentary: Single read GetLoudnessMomentary;
    property LoudnessIntegration: Single read GetLoudnessIntegration;
    property Loudness: Single read GetLoudness;
    property UnitOffset: Single read FUnitOffset;
    property PeakHold: Single read GetPeakHoldLoudness;
    property TotalSamples: Integer read FTotalSamples;
  end;

implementation

{$R *.DFM}

uses
  Math, DAV_Common, DAV_Approximations, LoudnessMeterGUI;

const
  CMeanSquareBias : Single = 1E-10;


{ TLoudnessMeterModule }

procedure TLoudnessMeterModule.VSTModuleCreate(Sender: TObject);
begin
 FCriticalSection := TCriticalSection.Create;
end;

procedure TLoudnessMeterModule.VSTModuleDestroy(Sender: TObject);
begin
 FreeAndNil(FCriticalSection);
end;

procedure TLoudnessMeterModule.VSTModuleOpen(Sender: TObject);
begin
 FR128Stereo := TStereoR128.Create;
 FR128Stereo.OnPeakLoudnessChanged := PeakLoudnessChanged;
 FR128Stereo.OnLoudnessChanged := LoudnessChanged;

 ChooseProcess;

 // initialize parameters
 Parameter[0] := 0;
 Parameter[1] := 0;
 Parameter[2] := 0;

 // set editor form class
 EditorFormClass := TFmLoudnessMeter;
end;

procedure TLoudnessMeterModule.VSTModuleClose(Sender: TObject);
begin
 FreeAndNil(FR128Stereo);
end;

{ Parameters }

procedure TLoudnessMeterModule.ParameterScaleChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Round(Value) = 0
  then FUnitOffset := 23.056476593
  else FUnitOffset := 0.056476593;

 FUpdateSamples := 0;

 // update GUI
 if EditorForm is TFmLoudnessMeter
  then TFmLoudnessMeter(EditorForm).UpdateScale;
end;

procedure TLoudnessMeterModule.ParameterTimeChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 case Round(Value) of
  0 : FUpdateSampleCount := Round(0.1 * SampleRate);
  1 : FUpdateSampleCount := Round(SampleRate);
  2 : FUpdateSampleCount := Round(SampleRate);
 end;

 FUpdateSamples := 0;

 // update GUI
 if EditorForm is TFmLoudnessMeter
  then TFmLoudnessMeter(EditorForm).UpdateTime;
end;

procedure TLoudnessMeterModule.ParameterStateChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FIsRunning := Round(Value) = 1;

 // update GUI
 if EditorForm is TFmLoudnessMeter
  then TFmLoudnessMeter(EditorForm).UpdateState;
end;

procedure TLoudnessMeterModule.ParameterStateDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 case Round(Parameter[Index]) of
  0 : PreDefined := 'Stopped';
  1 : PreDefined := 'Running';
 end;
end;

procedure TLoudnessMeterModule.ParameterTimeDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 case Round(Parameter[Index]) of
  0 : PreDefined := 'Mom';
  1 : PreDefined := 'Short';
  2 : PreDefined := 'Int';
 end;
end;

procedure TLoudnessMeterModule.LoudnessChanged(Sender: TObject);
begin
 // update parameter
 Parameter[3] := Limit(23 + FR128Stereo.Loudness, -18, 9);

 // update GUI
 if EditorForm is TFmLoudnessMeter
  then TFmLoudnessMeter(EditorForm).UpdateLoudness;
end;

procedure TLoudnessMeterModule.PeakLoudnessChanged(Sender: TObject);
begin
 // update parameter
 Parameter[4] := Limit(23 + FR128Stereo.PeakHold, -18, 9);

 // update GUI
 if EditorForm is TFmLoudnessMeter
  then TFmLoudnessMeter(EditorForm).UpdatePeak;
end;

procedure TLoudnessMeterModule.ParameterScaleDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 if Round(Parameter[Index]) = 0
  then PreDefined := 'LU'
  else PreDefined := 'LUFS';
end;

function TLoudnessMeterModule.GetLoudness: Single;
begin
 Result := FR128Stereo.Loudness;
end;

function TLoudnessMeterModule.GetLoudnessIntegration: Single;
begin
 Result := FR128Stereo.LoudnessIntegration;
end;

function TLoudnessMeterModule.GetLoudnessMomentary: Single;
begin
 Result := FR128Stereo.LoudnessMomentary;
end;

function TLoudnessMeterModule.GetLoudnessShort: Single;
begin
 Result := FR128Stereo.LoudnessShort;
end;

function TLoudnessMeterModule.GetPeakHoldLoudness: Single;
begin
 Result := FR128Stereo.Loudness;
end;

procedure TLoudnessMeterModule.ResetPeak;
begin
 FR128Stereo.ResetPeak;
end;

procedure TLoudnessMeterModule.ParameterLoudnessDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
var
  Value : Single;
begin
 Value := RoundTo(23 + Loudness, -1);

 // correct FS scale
 if Round(Parameter[0]) = 1
  then PreDefined := FloatToAnsiString(Value - 23, 4)
  else PreDefined := FloatToAnsiString(Value, 4);

 if Value < -18.1 then PreDefined := 'LOW' else
 if Value > 9 then PreDefined := 'OVER';
end;

procedure TLoudnessMeterModule.ParameterPeakMomDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
var
  Value : Single;
begin
 Value := RoundTo(23 + PeakHold, -1);

 // correct FS scale
 if Round(Parameter[0]) = 1
  then PreDefined := FloatToAnsiString(Value - 23, 4)
  else PreDefined := FloatToAnsiString(Value, 4);

 if Value < -18.1 then PreDefined := 'LOW' else
 if Value > 9 then PreDefined := 'OVER';
end;

procedure TLoudnessMeterModule.ParameterLoudnessChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 Value := Limit(23 + FR128Stereo.Loudness, -18, 9);
end;

procedure TLoudnessMeterModule.ParameterPeakMomChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 Value := Limit(23 + FR128Stereo.PeakHold, -18, 9);
end;

procedure TLoudnessMeterModule.ChooseProcess;
begin
 case numInputs of
   1 : OnProcess := VSTModuleProcessMono;
   2 : OnProcess := VSTModuleProcessStereo;
  else raise Exception.Create('Number of channels not supported');
 end;

 OnProcess32Replacing := OnProcess;
end;

procedure TLoudnessMeterModule.VSTModuleSampleRateChange(
  Sender: TObject; const SampleRate: Single);
begin
 if Abs(Self.SampleRate) > 0
  then FR128Stereo.SampleRate := Abs(SampleRate);
end;

procedure TLoudnessMeterModule.VSTModuleProcessMono(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  SampleIndex  : Integer;
begin
 for SampleIndex := 0 to SampleFrames - 1 do
  begin
   FR128Stereo.ProcessMono(Inputs[0, SampleIndex]);
   Outputs[0, SampleIndex] := Inputs[0, SampleIndex];
  end;
end;

procedure TLoudnessMeterModule.VSTModuleProcessStereo(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  SampleIndex  : Integer;
begin
 for SampleIndex := 0 to SampleFrames - 1 do
  begin
   FR128Stereo.ProcessStereo(Inputs[0, SampleIndex], Inputs[1, SampleIndex]);
   Outputs[0, SampleIndex] := Inputs[0, SampleIndex];
   Outputs[1, SampleIndex] := Inputs[1, SampleIndex];
  end;
end;

end.
