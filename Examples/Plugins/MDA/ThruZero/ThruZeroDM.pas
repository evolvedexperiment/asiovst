unit ThruZeroDM;

interface

uses
  Windows, Messages, SysUtils, Classes, DAV_Common, DAV_VSTModule;

const
  cBUFMAX = $7FF;

type
  TThruZeroDataModule = class(TVSTModule)
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleDestroy(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleResume(Sender: TObject);
    procedure VSTModuleSuspend(Sender: TObject);
    procedure VSTModuleSampleRateChange(Sender: TObject;const SampleRate: Single);
    procedure ParameterMixChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterDepthModChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterRateChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterRateDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterDepthDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterDepthChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterFeedbackChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    FBuffer    : array [0..1] of PDAVSingleFixedArray;
    FRate      : Single;
    FPhi, FDem : Single;
    FDeps      : Single;
    FDepth     : Single;
    FWet, FDry : Single;
    FBufPos    : Integer;
    FFeedback  : array [0..2] of Single;
    procedure RateChanged;
    procedure DepthChanged;
  public
  end;

implementation

{$R *.DFM}

uses
  Math;

procedure TThruZeroDataModule.VSTModuleCreate(Sender: TObject);
begin
  Parameter[0] := 0.30;  // Rate
  Parameter[1] := 0.43;  // Depth
  Parameter[2] :=  47;   // Mix
  Parameter[3] := -40;   // Feedback
  Parameter[4] := 100;   // Minimum delay to stop LF buildup with feedback

  ///differences from default program...
  with Programs[1] do
   begin
    Parameter[0] := 0.50;
    Parameter[1] := 0.20;
    Parameter[2] :=   47;
    Parameter[3] :=  -40;
    Parameter[4] :=  100;
   end;

  with Programs[2] do
   begin
    Parameter[0] := 0.60;
    Parameter[1] := 0.60;
    Parameter[2] :=   35;
    Parameter[3] :=  -40;
    Parameter[4] :=   70;
   end;

  with Programs[3] do
   begin
    Parameter[0] := 0.75;
    Parameter[1] := 1.00;
    Parameter[2] :=   50;
    Parameter[3] :=   50;
    Parameter[4] :=  100;
   end;

 ///initialise...
 FBufPos := 0;
 GetMem(FBuffer[0], cBUFMAX * SizeOf(Single));
 GetMem(FBuffer[1], cBUFMAX * SizeOf(Single));
 FPhi         := 0;
 FFeedback[0] := 0;
 FFeedback[1] := 0;
 FFeedback[2] := 0;
 FDeps        := 0;

 VSTModuleSuspend(Sender);
end;

procedure TThruZeroDataModule.VSTModuleDestroy(Sender: TObject);
begin
 if assigned(FBuffer[0]) then Dispose(FBuffer[0]);
 if assigned(FBuffer[1]) then Dispose(FBuffer[1]);
end;

procedure TThruZeroDataModule.VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample                  : Integer;
  a, b, f, f1, f2, ph     : Single;
  ra, de, we, dr, ds, dm  : Single;
  tmp, tmpi, bp           : Integer;
  tmpf, dpt               : Single;
begin
 f  := FFeedback[0];
 f1 := FFeedback[1];
 f2 := FFeedback[2];
 ph := FPhi;
 ra := FRate;
 de := FDepth;
 we := FWet;
 dr := FDry;
 ds := FDeps;
 dm := FDem;
 bp := FBufPos;

 for Sample := 0 to SampleFrames - 1 do
  begin
    a := Inputs[0, Sample];
    b := Inputs[1, Sample];

    ph := ph + ra;
    if ph > 1
     then ph := ph - 2;

    dec(bp);
    bp := bp and cBUFMAX;
    FBuffer[0, bp] := a + f * f1;
    FBuffer[1, bp] := b + f * f2;

    // ds := 0.995 * (ds - de) + de;           // smoothed depth change ...try inc not mult
    dpt  := dm + de * (1 - sqr(ph));           // delay mod shape
    tmpf := dpt;
    tmp  := round(tmpf);
    tmpf := tmpf - tmp;
    tmp  := (tmp + bp) and $7FF;
    tmpi := (tmp + 1) and $7FF;

    f1 := FBuffer[0, tmp];                     // try adding a constant to reduce denormalling
    f2 := FBuffer[1, tmp];
    f1 := tmpf * (FBuffer[0, tmpi] - f1) + f1; // linear interpolation
    f2 := tmpf * (FBuffer[1, tmpi] - f2) + f2;

    a := a * dr - f1 * we;
    b := b * dr - f2 * we;

    Outputs[0, Sample] := a;
    Outputs[1, Sample] := b;
  end;

 if abs(f1) > 1E-10 then
  begin
   FFeedback[1] := f1;
   FFeedback[2] := f2;
  end
 else
  begin
   FFeedback[1] := 0;
   FFeedback[2] := 0; //catch denormals
  end;
 FPhi    := ph;
 FDeps   := ds;
 FBufPos := bp;
end;

procedure TThruZeroDataModule.VSTModuleResume(Sender: TObject);
begin
 DepthChanged;
end;

procedure TThruZeroDataModule.DepthChanged;
begin
 FDepth := 2000 * sqr(Parameter[1]);
 FDem := sqr(FDepth) * 0.01 * Parameter[4];
 FDepth := FDepth - FDem;
end;

procedure TThruZeroDataModule.ParameterMixChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
 FWet := 0.01 * Value;
 FDry := 1 - FWet;
end;

procedure TThruZeroDataModule.ParameterDepthModChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
 FFeedback[0] := 0.0095 * Value;
 FPhi         := 0.0;             //reset cycle
end;

procedure TThruZeroDataModule.ParameterRateChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
 RateChanged;
 if Value < 0.01 then
  begin
   FRate := 0;
   FPhi  := 0;
  end;
end;

procedure TThruZeroDataModule.RateChanged;
begin
 FRate := Power(10, 3 * Parameter[0] - 2) * 2 / SampleRate;
end;

procedure TThruZeroDataModule.ParameterFeedbackChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
 DepthChanged;
end;

procedure TThruZeroDataModule.ParameterDepthChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 DepthChanged;
end;

procedure TThruZeroDataModule.ParameterRateDisplay(Sender: TObject;
  const Index: Integer; var PreDefined: string);
begin
 if Parameter[0] < 0.01
  then PreDefined := '-'
  else PreDefined := FloatToStrF(Power(10, 2 - 3 * Parameter[index]), ffGeneral, 4, 4);
end;

procedure TThruZeroDataModule.ParameterDepthDisplay(Sender: TObject;
  const Index: Integer; var PreDefined: string);
begin
 PreDefined := FloatToStrF(1000 * FDepth / SampleRate, ffGeneral, 4, 4);
end;

procedure TThruZeroDataModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
begin
 RateChanged;
end;

procedure TThruZeroDataModule.VSTModuleSuspend(Sender: TObject);
begin
 if assigned(FBuffer[0]) then FillChar(FBuffer[0, 0], cBUFMAX * SizeOf(Single), 0);
 if assigned(FBuffer[1]) then FillChar(FBuffer[1, 0], cBUFMAX * SizeOf(Single), 0);
end;

end.
