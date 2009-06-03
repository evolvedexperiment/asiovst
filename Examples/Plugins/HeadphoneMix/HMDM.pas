unit HMDM;

interface

{$I DAV_Compiler.inc}

uses
  DAV_Common, DAV_VSTModule, DAV_DspCrosstalkSimulator;

type
  THMModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure HMMEffectChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure HMMModelDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure HMMModelChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure HMMPolarityDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure HMMPolarityChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    FCTSimulator : TCustomIIRCrosstalkSimulator;
  public
  end;

implementation

{$IFDEF UseDelphi}
{$R *.DFM}
{$ENDIF}

uses
  Math, SysUtils;

procedure THMModule.VSTModuleOpen(Sender: TObject);
begin
 FCTSimulator := TCustomIIRCrosstalkSimulator.Create;

 Parameter[0] := 50;
 Parameter[1] := 0;
end;

procedure THMModule.VSTModuleClose(Sender: TObject);
begin
 FreeAndNil(FCTSimulator);
end;

procedure THMModule.HMMEffectChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
 if assigned(FCTSimulator)
  then FCTSimulator.Mix := Value;
end;

procedure THMModule.HMMModelDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 case round(Parameter[index]) of
  0 : PreDefined := 'handcrafted';
  1 : PreDefined := 'matched IRCAM';
  2 : PreDefined := 'matched HDPHX';
 end;
end;

procedure THMModule.HMMModelChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
 if assigned(FCTSimulator) then
  case round(Parameter[index]) of
   0 : begin
        FCTSimulator.Model := csmHandcrafted;
        FCTSimulator.Diameter := 0.12;
       end;
   1 : begin
        FCTSimulator.Model := csmIRCAM;
        FCTSimulator.Diameter := 0.11;
       end;
   2 : begin
        FCTSimulator.Model := csmHDPHX;
        FCTSimulator.Diameter := 0.11;
       end;
  end;
end;

procedure THMModule.HMMPolarityDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 if Parameter[index] > 0.5
  then PreDefined := '-'
  else PreDefined := '+';
end;

procedure THMModule.HMMPolarityChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
 if assigned(FCTSimulator)
  then FCTSimulator.Polarity := (Value < 0.5);
end;

procedure THMModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  i  : Integer;
begin
 for i := 0 to SampleFrames - 1 do
  begin
   Outputs[0, i] := Inputs[1, i];
   Outputs[1, i] := Inputs[1, i];

   FCTSimulator.Process(outputs[0, i], outputs[1, i]);
  end;
end;

procedure THMModule.VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
begin
 if assigned(FCTSimulator)
  then FCTSimulator.SampleRate := SampleRate;
end;

end.