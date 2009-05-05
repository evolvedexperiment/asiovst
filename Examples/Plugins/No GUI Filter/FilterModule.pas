unit FilterModule;

{$I DAV_COmpiler.inc}

interface

uses
  DAV_Common, DAV_VSTModule;

type
  TVSTFilter = class(TVSTModule)
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcessDoubleReplacing(const Inputs, Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
    procedure VSTFilterParameterChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTModuleOpen(Sender: TObject);
  private
    FCutOffFrequency : Single;
    FOld             : array [0..1] of array[0..1] of Double;
  public
  end;

implementation

{$IFNDEF FPC}
{$R *.dfm}
{$ENDIF}

////////////////////////////////////////////////////////////////////////////////
// OnOpen
////////////////////////////////////////////////////////////////////////////////

procedure TVSTFilter.VSTModuleOpen(Sender: TObject);
begin
 FCutOffFrequency := 0.5;
 Parameter[0] := 1000;
 Parameter[1] := 1;
end;

////////////////////////////////////////////////////////////////////////////////
// Parameter 0 Changed (Cutoff Frequency)
////////////////////////////////////////////////////////////////////////////////

procedure TVSTFilter.VSTFilterParameterChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FCutOffFrequency := 0.01 + Value * 0.00005;
end;


////////////////////////////////////////////////////////////////////////////////
// 32 Bit Processing
////////////////////////////////////////////////////////////////////////////////

procedure TVSTFilter.VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  i         : Integer;
  cut, res  : Single;
  fb        : Single;
begin
 cut := FCutOffFrequency;
 res := 0.1 * Parameter[1];
 fb := res + res / (1 - cut * 0.9);
 for i := 0 to SampleFrames - 1 do
  begin
   FOld[0, 0] := FOld[0, 0] + cut * (Inputs[0, i] - FOld[0, 0] + fb * (FOld[0, 0] - FOld[1, 0])) + CDenorm32;
   FOld[0, 1] := FOld[0, 1] + cut * (Inputs[1, i] - FOld[0, 1] + fb * (FOld[0, 1] - FOld[1, 1])) + CDenorm32;
   FOld[1, 0] := FOld[1, 0] + cut * (FOld[0, 0] - FOld[1, 0]);
   FOld[1, 1] := FOld[1, 1] + cut * (FOld[0, 1] - FOld[1, 1]);
   Outputs[0, i] := Limit(FOld[1, 0]);
   Outputs[1, i] := Limit(FOld[1, 1]);
  end;
end;


////////////////////////////////////////////////////////////////////////////////
// 64 Bit Processing
////////////////////////////////////////////////////////////////////////////////

procedure TVSTFilter.VSTModuleProcessDoubleReplacing(const Inputs,
  Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
var
  i         : Integer;
  cut, res  : Double;
  fb        : Double;
begin
 cut := Parameter[0] * 0.8;
 res := Parameter[1];
 fb := res + res / (1 - Cut * 0.9);
 for i := 0 to SampleFrames - 1 do
  begin
   FOld[0, 0] := FOld[0, 0] + cut * (Inputs[0, i] - FOld[0, 0] + fb * (FOld[0, 0] - FOld[1, 0])) + CDenorm32;
   FOld[0, 1] := FOld[0, 1] + cut * (Inputs[1, i] - FOld[0, 1] + fb * (FOld[0, 1] - FOld[1, 1])) + CDenorm32;
   FOld[1, 0] := FOld[1, 0] + cut * (FOld[0, 0] - FOld[1, 0]);
   FOld[1, 1] := FOld[1, 1] + cut * (FOld[0, 1] - FOld[1, 1]);
   Outputs[0, i] := Limit(FOld[1,0]);
   Outputs[1, i] := Limit(FOld[1,1]);
  end;
end;

end.
