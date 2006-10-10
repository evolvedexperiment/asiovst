unit FilterModule;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses {$IFDEF FPC} LResources, LCLClasses, {$ENDIF}DDSPBase, DVSTModule;

type
  TVSTFilter = class(TVSTModule)
    procedure VSTModuleProcess(const inputs, outputs: TArrayOfSingleArray; sampleframes: Integer);
    procedure VSTModuleProcessDoubleReplacing(const inputs, outputs: TArrayOfDoubleArray; sampleframes: Integer);
    procedure VSTFilterParameterProperties0ParameterChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTModuleInitialize(Sender: TObject);
  private
    fCutOffFrequency   : Single;
    fOld               : array [0..1] of array[0..1] of Double;
  public
    { Public declarations }
  end;

implementation

{$IFNDEF FPC}
{$R *.dfm}
{$ENDIF}

const kDenorm = 1E-20;

////////////////////////////////////////////////////////////////////////////////
// OnInitialize
////////////////////////////////////////////////////////////////////////////////

procedure TVSTFilter.VSTModuleInitialize(Sender: TObject);
begin
 fCutOffFrequency:=0.5;
 Parameter[0]:=1000;
 Parameter[1]:=1;
end;


////////////////////////////////////////////////////////////////////////////////
// Parameter 0 Changed (Cutoff Frequency)
////////////////////////////////////////////////////////////////////////////////

procedure TVSTFilter.VSTFilterParameterProperties0ParameterChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 fCutOffFrequency:=FreqLogToLinear(Parameter[0]);
end;


////////////////////////////////////////////////////////////////////////////////
// 32 Bit Processing
////////////////////////////////////////////////////////////////////////////////

procedure TVSTFilter.VSTModuleProcess(const inputs, outputs: TArrayOfSingleArray; sampleframes: Integer);
var i         : integer;
    cut, res  : single;
    fb        : single;
begin
 cut := fCutOffFrequency;
 res := 0.1*Parameter[1];
 for i := 0 to sampleFrames - 1 do
  begin
   fb := res + res / (1 - cut * 0.9);
   fOld[0,0] := fOld[0,0] + cut * (i - fOld[0,0] + fb * (fOld[0,0] - fOld[1,0])) + kDenorm;
   fOld[0,1] := fOld[0,1] + cut * (i - fOld[0,1] + fb * (fOld[0,1] - fOld[1,1])) + kDenorm;
   fOld[1,0] := fOld[1,0] + cut * (fOld[0,0] - fOld[1,0]);
   fOld[1,1] := fOld[1,1] + cut * (fOld[0,1] - fOld[1,1]);
   outputs[0, i] := f_limit(fOld[1,0]);
   outputs[1, i] := f_limit(fOld[1,1]);
  end;
end;


////////////////////////////////////////////////////////////////////////////////
// 64 Bit Processing
////////////////////////////////////////////////////////////////////////////////

procedure TVSTFilter.VSTModuleProcessDoubleReplacing(const inputs,
  outputs: TArrayOfDoubleArray; sampleframes: Integer);
var i         : integer;
    cut, res  : Double;
    fb        : Double;
begin
 cut := Parameter[0] * 0.8;
 res := Parameter[1];
 for i := 0 to sampleFrames - 1 do
  begin
   fb := res + res / (1 - cut * 0.9);
   fOld[0,0] := fOld[0,0] + cut * (i - fOld[0,0] + fb * (fOld[0,0] - fOld[1,0])) + kDenorm;
   fOld[0,1] := fOld[0,1] + cut * (i - fOld[0,1] + fb * (fOld[0,1] - fOld[1,1])) + kDenorm;
   fOld[1,0] := fOld[1,0] + cut * (fOld[0,0] - fOld[1,0]);
   fOld[1,1] := fOld[1,1] + cut * (fOld[0,1] - fOld[1,1]);
   outputs[0, i] := f_limit(fOld[1,0]);
   outputs[1, i] := f_limit(fOld[1,1]);
  end;
end;

{$IFDEF FPC}
initialization
  {$i FilterModule.lrs}
  {$i FilterModule.lrs}
{$ENDIF}

end.
