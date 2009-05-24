unit DAV_DspDownsampleScheduler;

interface

{$I ..\DAV_Compiler.inc}

uses
  Classes, DAV_Common, DAV_DspCommon;

type
  TDownsampleProcess = function(Sender: TObject; Stage: Integer; Input: Single): Single of object;

  TCustomDownsampleScheduler = class(TDspObject)
  private
    FMaxDSStages          : Integer;
    FDownSampleCount      : Integer;
    FDownSampleMax        : Integer;
    FOnProcessDownsampled : TDownsampleProcess;
  public
    constructor Create; virtual;
    procedure Process(Input: Single); virtual;

    property MaximumDownsampleStages: Integer read FMaxDSStages write FMaxDSStages;
    property OnProcessDownsampled: TDownsampleProcess read FOnProcessDownsampled write FOnProcessDownsampled;
  end;

  TDownsampleScheduler = class(TCustomDownsampleScheduler)
  published
    property MaximumDownsampleStages;
    property OnProcessDownsampled;
  end;


implementation

{ TCustomDownsampleScheduler }

constructor TCustomDownsampleScheduler.Create;
begin
 FMaxDSStages := 8;
 FDownSampleMax := 1 shl FMaxDSStages;
 FDownSampleCount := 0;
end;

procedure TCustomDownsampleScheduler.Process(Input: Single);
var
  Stage: Integer;
begin
 for Stage := 0 to FMaxDSStages - 1 do
  begin
   if (FDownSampleCount mod (1 shl Stage)) <> 0
    then Break;
   Input := FOnProcessDownsampled(Self, Stage, Input);
  end;

 Inc(FDownSampleCount);
 if FDownSampleCount >= FDownSampleMax
  then FDownSampleCount := 0;
end;

end.
