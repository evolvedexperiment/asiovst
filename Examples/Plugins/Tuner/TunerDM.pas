unit TunerDM;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, DAV_Common, DAV_VSTModule,
  DAV_DspTuner;

type
  TTunerDataModule = class(TVSTModule)
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure ParameterNoteDisplay(
      Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleSampleRateChange(Sender: TObject;
      const SampleRate: Single);
  private
    FTuner : TTuner;
  public
  end;

implementation

{$R *.DFM}

uses
  TunerGUI;

procedure TTunerDataModule.VSTModuleOpen(Sender: TObject);
begin
 FTuner := TTuner.Create;

 Parameter[0] := 2;
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