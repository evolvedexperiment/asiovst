unit TunerDM;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, DAV_Common, DAV_VSTModule,
  DAV_DspTuner;

type
  TTunerDataModule = class(TVSTModule)
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure ParameterNoteDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleSampleRateChange(Sender: TObject;
      const SampleRate: Single);
    procedure ParameterGuitarStringChange(
      Sender: TObject; const Index: Integer; var Value: Single);
  private
    FTuner : TAdvancedTuner;
  public
    property Tuner : TAdvancedTuner read FTuner;
  end;

implementation

{$R *.DFM}

uses
  TunerGUI;

procedure TTunerDataModule.VSTModuleOpen(Sender: TObject);
begin
 FTuner := TAdvancedTuner.Create;
 FTuner.OneCrossingOnly := True;
 FTuner.SampleRate := SampleRate;
 FTuner.Threshold := 0.1;
 FTuner.Attack := 0.1;
 FTuner.Release := 1;
 FTuner.SmoothFactor := 0.99;

 Parameter[0] := 2;
end;

procedure TTunerDataModule.ParameterGuitarStringChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  CenterFrequency : Single;
begin
 case round(Parameter[Index]) of
  1 : CenterFrequency := 329.62755691286992973584176104656;
  2 : CenterFrequency := 440;
  3 : CenterFrequency := 587.32953583481512052556602772116;
  4 : CenterFrequency := 783.99087196349858817139906091965;
  5 : CenterFrequency := 987.76660251224822366150908371768;
  6 : CenterFrequency := 1318.5102276514797189433670441862;
  else raise Exception.Create('Current Frequency doesn''t exist');
 end;

 FTuner.MinimumFrequency := 0.5 * CenterFrequency;
 FTuner.MinimumFrequency := 2 * CenterFrequency;
end;

procedure TTunerDataModule.VSTModuleClose(Sender: TObject);
begin
 FreeAndNil(FTuner);
end;

procedure TTunerDataModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
begin
  GUI := TFmTuner.Create(Self);
end;

procedure TTunerDataModule.ParameterNoteDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 case round(Parameter[Index]) of
  1 : PreDefined := 'E';
  2 : PreDefined := 'A';
  3 : PreDefined := 'D';
  4 : PreDefined := 'G';
  5 : PreDefined := 'H';
  6 : PreDefined := 'E''';
 end;
end;

procedure TTunerDataModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
begin
 FTuner.SampleRate := SampleRate;
end;

end.