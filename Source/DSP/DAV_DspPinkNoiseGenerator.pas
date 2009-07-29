unit DAV_DspPinkNoiseGenerator;

interface

{$I ..\DAV_Compiler.inc}

uses
  Classes, DAV_Common, DAV_Complex, DAV_DspCommon;

type
  TPinkNoiseGenerator = class(TDspObject)
  private
    procedure SetSampleRate(const Value: Double);
    procedure SampleRateChanged;
  protected
    FSampleRate   : Double;
    FContribution : Array [0..4] of Double;
  public
    constructor Create; virtual;
    function ProcessSample: Double; virtual;

    property SampleRate: Double read FSampleRate write SetSampleRate;
  end;

implementation

{ TPinkNoiseGenerator }

constructor TPinkNoiseGenerator.Create;
begin
 inherited;
 FSampleRate := 44100;
 FillChar(FContribution[0], Length(FContribution) * SizeOf(Double), 0);
end;

procedure TPinkNoiseGenerator.SetSampleRate(const Value: Double);
begin
 if FSampleRate <> Value then
  begin
   FSampleRate := Value;
   SampleRateChanged;
  end;
end;

procedure TPinkNoiseGenerator.SampleRateChanged;
begin
 // yet unused!
end;

function TPinkNoiseGenerator.ProcessSample: Double;
var
  ur1      : Double;
const
  pA   : Array [0..4] of Double = (0.23980, 0.18727, 0.1638, 0.194685, 0.214463);
  pSUM : Array [0..4] of Double = (0.00198, 0.01478, 0.06378, 0.23378, 0.91578);
begin
 ur1 := random;
 if (ur1 <= pSUM[0]) then FContribution[0] := (2 * random - 1) * pA[0] else
 if (ur1 <= pSUM[1]) then FContribution[1] := (2 * random - 1) * pA[1] else
 if (ur1 <= pSUM[2]) then FContribution[2] := (2 * random - 1) * pA[2] else
 if (ur1 <= pSUM[3]) then FContribution[3] := (2 * random - 1) * pA[3] else
 if (ur1 <= pSUM[4]) then FContribution[4] := (2 * random - 1) * pA[4];
 result := (FContribution[0] + FContribution[1] +
            FContribution[2] + FContribution[3] +
            FContribution[4]);
end;

end.
