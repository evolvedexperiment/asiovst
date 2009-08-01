unit DAV_DifferentialEvolution;

// Based on code by Laurent de Soras, which was based on an implementation by 
// Olli Niemitalo and Magnus Jonsson itself
// see http://ldesoras.free.fr/prod.html for more details       

interface

{$I DAV_Compiler.inc}

uses
  DAV_Common, SysUtils, Classes;

type
  TDiffEvolPopulation = TDAVDoubleDynArray;
  TDiffEvolEvent = function(Sender: TObject; const Population: TDiffEvolPopulation): Double of object;

  TEvaluatedPopulation = class(TObject)
  public
    FPopulation    : TDiffEvolPopulation;
    FCost          : Double;
    FValidCostFlag : Boolean;
    constructor Create; overload;
    constructor Create(const DiffEvolPopulation: TDiffEvolPopulation; Cost: Double); overload;
  end;

  TDiffEvol = class(TComponent)
  private
    FPopulationCount    : Integer; // Number of populations
    FVariableCount      : Integer; // Number of variables in population
    FBestPopulation     : Integer; // Negative if not yet determinated
    FGainBest           : Double;
    FGainBase           : Double;
    FGainR1             : Double;
    FGainR2             : Double;
    FGainR3             : Double;
    FCrossOver          : Double;
    FMinArr, FMaxArr    : TDiffEvolPopulation;
    FBestArr            : TDiffEvolPopulation;
    FCurrentGeneration  : array of TEvaluatedPopulation;
    FNextGeneration     : array of TEvaluatedPopulation;
    procedure RandomizePopulation;
    function FindBest: Double;
    procedure SetVariableCount(const Value: Integer);
    procedure SetPopulationCount(const Value: Integer);
    procedure SetCrossOver(const Value: Double);
    procedure SetGainBest(const Value: Double);
    procedure SetGainR1(const Value: Double);
    procedure SetGainR2(const Value: Double);
    procedure SetGainR3(const Value: Double);
    procedure CalculateGainR0;
    function GetBestArr(Index: Integer): Double;
    procedure SetBestArr(Index: Integer; const Value: Double);
    procedure SetMinArr(Index: Integer; const Value: Double);
    function GetMinArr(Index: Integer): Double;
    procedure SetMaxArr(Index: Integer; const Value: Double);
    function GetMaxArr(Index: Integer): Double;
    procedure RecreatePopulation;
  protected
    FOnCalcCosts : TDiffEvolEvent;
    FOnInitPopulation : TDiffEvolEvent;
    FAutoInitialize : Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Initialize;
    function Evolve: Double;
    function GetBestPopulation: TDiffEvolPopulation;
    function GetBestCost: Double;
    property MinArr[Index: Integer] : Double read GetMinArr write SetMinArr;
    property MaxArr[Index: Integer] : Double read GetMaxArr write SetMaxArr;
    property BestArr[Index: Integer] : Double read GetBestArr write SetBestArr;
  published
    property OnCalcCosts: TDiffEvolEvent read FOnCalcCosts write FOnCalcCosts;
    property OnInitPopulation: TDiffEvolEvent read FOnInitPopulation write FOnInitPopulation;
    property PopulationCount :Integer read FPopulationCount write SetPopulationCount;
    property VariableCount :Integer read FVariableCount write SetVariableCount;
    property GainBest :Double read FGainBest write SetGainBest;
    property GainR1 :Double read FGainR1 write SetGainR1;
    property GainR2 :Double read FGainR2 write SetGainR2;
    property GainR3 :Double read FGainR3 write SetGainR3;
    property CrossOver :Double read FCrossOver write SetCrossOver;
    property AutoInitialize : Boolean read FAutoInitialize write FAutoInitialize;
  end;

implementation

uses
  Math;

resourcestring
  RCStrCrossOverBoundError = 'CrossOver must be 0<=x<=1';
  RCStrIndexOutOfBounds = 'Index out of bounds (%d)';

{ TEvaluatedPopulation }

constructor TEvaluatedPopulation.Create;
begin
 inherited;
 FCost := 0;
 FValidCostFlag := False;
end;

constructor TEvaluatedPopulation.Create(const DiffEvolPopulation: TDiffEvolPopulation; Cost: Double);
begin
 inherited Create;
 FCost := Cost;
 FPopulation := DiffEvolPopulation;
end;


{ TDiffEvol }

constructor TDiffEvol.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
 Randomize;
 FGainR1 := -0.7;
 FGainR2 := 0.7;
 FGainR3 := 1;
 FGainBest := 0;
 FCrossOver := 1;
 CalculateGainR0;

 FBestPopulation := -1;
 PopulationCount := 1000;
 VariableCount := 1;
end;

destructor TDiffEvol.Destroy;
var
  Generation : Integer;
begin
 for Generation := 0 to Length(FCurrentGeneration) - 1 do
  if assigned(FCurrentGeneration[Generation])
   then FCurrentGeneration[Generation].Free;

 for Generation := 0 to Length(FNextGeneration) - 1 do
  if assigned(FNextGeneration[Generation])
   then FNextGeneration[Generation].Free;

 inherited;
end;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  Name: Evolve                                                              //
//  ------------                                                              //
//                                                                            //
//  Description:                                                              //
//    Compute the next generation, trying to find a population which cost is  //
//    lower than the previous one. Note: coefficient for the original         //
//    population is 1.0 minus sum of gain_*. The coefficients can be          //
//    negative as well as positive, and be higher than 1.                     //
//                                                                            //
//  Input/output parameters:                                                  //
//    - func: Object implementing the cost function. It shall accept          //
//      populations of the size given at the DiffEvol creation.               //
//                                                                            //
//  Returns: The new best cost.                                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

function TDiffEvol.Evolve: Double;
var
  NewBestPopulation : Integer;
  NewBestCost       : Double;
  RandomPopulation  : array [0..2] of Integer;
  Population        : Integer;
  CurVar, VarCnt, i : Integer;
begin
 if (FBestPopulation < 0) then FindBest;

 NewBestPopulation := FBestPopulation;
 NewBestCost := FCurrentGeneration[FBestPopulation].FCost;

 for Population := 0 to FPopulationCount - 1 do
  begin
   // Find 3 different populations randomly
   repeat
    RandomPopulation[0] := Random(FPopulationCount);
   until (RandomPopulation[0] <> Pop) and
         (RandomPopulation[0] <> FBestPopulation);

   repeat
    RandomPopulation[1] := Random(FPopulationCount);
   until (RandomPopulation[1] <> Pop) and
         (RandomPopulation[1] <> FBestPopulation) and
         (RandomPopulation[1] <> RandomPopulation[0]);

   repeat
    RandomPopulation[2] := Random(FPopulationCount);
   until (RandomPopulation[2] <> Pop) and
         (RandomPopulation[2] <> FBestPopulation) and
         (RandomPopulation[2] <> RandomPopulation[1]) and
         (RandomPopulation[2] <> RandomPopulation[0]);

   // Generate trial vector with crossing-over
   CurVar := Random(FVariableCount);
   VarCnt := 0;

   if Pop = NewBestPopulation
    then VarCnt := 0;

   repeat
    FNextGeneration[Population].FPopulation[CurVar] :=
      FCurrentGeneration[Population].FPopulation[CurVar] * FGainBase +
      FCurrentGeneration[RandomPopulation[0]].FPopulation[CurVar] * FGainR1 +
      FCurrentGeneration[RandomPopulation[1]].FPopulation[CurVar] * FGainR2 +
      FCurrentGeneration[RandomPopulation[2]].FPopulation[CurVar] * FGainR3 +
      FCurrentGeneration[FBestPopulation].FPopulation[CurVar] * FGainBest;
    Inc(CurVar);
    if CurVar >= FVariableCount then CurVar := 0;
    Inc(VarCnt);
   until (VarCnt >= FVariableCount) or (Random >= FCrossOver);

   while (VarCnt < FVariableCount) do
    begin
     FNextGeneration[Pop].FPopulation[CurVar] := FCurrentGeneration[Pop].FPopulation[CurVar];
     Inc(CurVar);
     if CurVar >= FVariableCount then CurVar := 0;
     Inc(VarCnt);
    end;

   // Evaluate the new population
   FNextGeneration[Pop].FCost := FOnCalcCosts(self, FNextGeneration[Pop].FPopulation);
   FNextGeneration[Pop].FValidCostFlag := True;

//   if IsNan(FNextGeneration[Pop].FCost)
//    then FNextGeneration[Pop].FCost := FOnCalcCosts(self, FNextGeneration[Pop].FPopulation);

   if (FNextGeneration[Pop].FCost < FCurrentGeneration[Pop].FCost)
    then
     begin
      if (FNextGeneration[Pop].FCost < NewBestCost) then
       begin // New best
        NewBestPopulation := Pop;
        NewBestCost := FNextGeneration[Pop].FCost;
       end;
      Move(FNextGeneration[Pop].FPopulation[0], FCurrentGeneration[Pop].FPopulation[0], FVariableCount * SizeOf(Double));
      FCurrentGeneration[Pop].FCost := FNextGeneration[Pop].FCost;
      FCurrentGeneration[Pop].FValidCostFlag := FNextGeneration[Pop].FValidCostFlag;
     end;
  end;

 FBestPopulation := NewBestPopulation;
 for i := 0 to FVariableCount - 1
  do FBestArr[i] := FCurrentGeneration[FBestPopulation].FPopulation[i];
 Result := NewBestCost;
end;


////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  Name: GetBestPopulation                                                   //
//  -----------------------                                                   //
//                                                                            //
//  Description:                                                              //
//    Return the current best population. This function must not be called    //
//    before the first call to evolve ().                                     //
//                                                                            //
//  Returns: A reference on the population. It remains valid until            //
//    subsequent call to evolve() function.                                   //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

function TDiffEvol.GetBestPopulation: TDiffEvolPopulation;
begin
 assert (FBestPopulation >= 0);
 Result := (FCurrentGeneration[FBestPopulation].FPopulation);
end;


////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  Name: GetBestCost                                                         //
//  -----------------                                                         //
//                                                                            //
//  Description:                                                              //
//    Return the cost evaluation for the current best population. This        //
//    function must not be called before the first call to evolve ().         //
//                                                                            //
//  Returns: The cost.                                                        //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

function TDiffEvol.GetBestCost: Double;
begin
 assert (FBestPopulation >= 0);
 assert (FCurrentGeneration[FBestPopulation].FValidCostFlag);
 Result := FCurrentGeneration[FBestPopulation].FCost;
end;

procedure TDiffEvol.Initialize;
var
  Population : Integer;
begin
 // Initialize populations with random values
 FBestPopulation := -1;

 for Population := 0 to FPopulationCount - 1 do
  begin
   FCurrentGeneration[Population].FValidCostFlag := False;
   FCurrentGeneration[Population].FCost := 0;
   FNextGeneration[Population].FValidCostFlag := False;
   FNextGeneration[Population].FCost := 0;
  end;

 RandomizePopulation;

 (*
 // Introduce the "best" population if it is provided
 if assigned(FBestArr) then
  begin
   for i := 0 to FVariableCount - 1
    do FCurrentGeneration[0].FPopulation[i] := FBestArr[i];
//   FCurrentGeneration[0].FValidCostFlag := False;
//   FBestPopulation := 0;
  end;
 *)
end;

procedure TDiffEvol.RandomizePopulation;
var
  Offset  : Double;
  Mul     : Double;
  i, Pop  : Integer;
begin
 if assigned(FOnInitPopulation) then
  for Pop := 0 to FPopulationCount - 1
   do FOnInitPopulation(Self, FCurrentGeneration[Pop].FPopulation)
  else
 for i := 0 to FVariableCount - 1 do
  begin
   assert(FMinArr[i] <= FMaxArr[i]);
   Offset := FMinArr[i];
   Mul    := FMaxArr[i] - FMinArr[i];
   for Pop := 0 to FPopulationCount - 1
    do FCurrentGeneration[Pop].FPopulation[i] := Offset + random * Mul;
  end;
 FBestPopulation := -1;
end;

function TDiffEvol.FindBest;
var
  CurCost  : Double;
  Pop, i   : Integer;
begin
 if (FBestPopulation < 0)
  then FBestPopulation :=  0;

 with FCurrentGeneration[FBestPopulation] do
  if (not FValidCostFlag) then
   begin
    FCost := FOnCalcCosts(Self, FPopulation);
    FValidCostFlag :=  True;
   end;

 Result := FCurrentGeneration[FBestPopulation].FCost;
 for Pop := 0 to FPopulationCount - 1 do
  begin
   if (Pop <> FBestPopulation) then
    begin
     CurCost := FOnCalcCosts(Self, FCurrentGeneration[Pop].FPopulation);
     FCurrentGeneration[Pop].FCost := CurCost;
     FCurrentGeneration[Pop].FValidCostFlag := True;
     if (CurCost < Result) then
      begin
       FBestPopulation := Pop;
       Result := CurCost;
      end;
    end;
  end;

 for i := 0 to FVariableCount - 1
  do FBestArr[i] := FCurrentGeneration[FBestPopulation].FPopulation[i];
end;

procedure TDiffEvol.SetVariableCount(const Value: Integer);
var
  i : Integer;
begin
 assert(Value > 0);
 if Value <> FVariableCount then
  begin
   FVariableCount := Value;
   SetLength(FMinArr, FVariableCount);
   SetLength(FMaxArr, FVariableCount);
   SetLength(FBestArr, FVariableCount);
   for i := 0 to FVariableCount - 1 do
    begin
     FMinArr[i]  := -100;
     FMaxArr[i]  :=  100;
     FBestArr[i] :=    0;
    end;
   RecreatePopulation;
  end;
end;

procedure TDiffEvol.SetPopulationCount(const Value: Integer);
var
  i : Integer;
begin
 assert(Value >= 5);
 for i := 0 to Length(FCurrentGeneration) - 1 do
  if assigned(FCurrentGeneration[i])
   then FreeAndNil(FCurrentGeneration[i]);
 for i := 0 to Length(FNextGeneration) - 1 do
  if assigned(FNextGeneration[i])
   then FreeAndNil(FNextGeneration[i]);
 FPopulationCount := Value;
 // Size all the arrays
 SetLength(FCurrentGeneration,FPopulationCount);
 SetLength(FNextGeneration,FPopulationCount);
 RecreatePopulation;
end;

function TDiffEvol.GetMaxArr(Index: Integer): Double;
begin
 if (Index < 0) or (Index >= Length(FMaxArr))
  then raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);

 Result := FMaxArr[Index];
end;

function TDiffEvol.GetMinArr(Index: Integer): Double;
begin
 if (Index < 0) or (Index >= Length(FMinArr))
  then raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);

 Result := FMinArr[Index];
end;

function TDiffEvol.GetBestArr(Index: Integer): Double;
begin
 if (Index < 0) or (Index >= Length(FBestArr))
  then raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);
 
 Result := FBestArr[Index];
end;

procedure TDiffEvol.SetBestArr(Index: Integer; const Value: Double);
begin
 if (Index < 0) or (Index >= Length(FBestArr))
  then raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);
 
 FBestArr[Index] := Value;
// raise Exception.Create('Not Supported!');
end;

procedure TDiffEvol.SetMinArr(Index: Integer; const Value: Double);
begin
 if (Index < 0) or (Index >= Length(FMinArr))
  then raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);

 if Value > FMaxArr[Index]
  then raise Exception.Create('Min < Max, please!');

 if FMinArr[Index] <> Value then
  begin
   FMinArr[Index] := Value;
   if FAutoInitialize
    then Initialize;
  end;
end;

procedure TDiffEvol.SetMaxArr(Index: Integer; const Value: Double);
begin
 if (Index < 0) or (Index >= Length(FMaxArr))
  then raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);

 if Value < FMinArr[Index]
  then raise Exception.Create('Min < Max, please!');

 if FMaxArr[Index] <> Value then
  begin
   FMaxArr[Index] := Value;
   if FAutoInitialize
    then Initialize;
  end;
end;

procedure TDiffEvol.SetCrossOver(const Value: Double);
begin
 if not ((Value >= 0) and (Value <= 1))
  then raise Exception.Create(RCStrCrossOverBoundError);
 FCrossOver := Value;
end;

procedure TDiffEvol.SetGainBest(const Value: Double);
begin
 if FGainBest <> Value then
  begin
   FGainBest := Value;
   CalculateGainR0;
  end;
end;

procedure TDiffEvol.SetGainR1(const Value: Double);
begin
 if FGainR1 <> Value then
  begin
   FGainR1 := Value;
   CalculateGainR0;
  end;
end;

procedure TDiffEvol.SetGainR2(const Value: Double);
begin
 if FGainR2 <> Value then
  begin
   FGainR2 := Value;
   CalculateGainR0;
  end;
end;

procedure TDiffEvol.SetGainR3(const Value: Double);
begin
 if FGainR3 <> Value then
  begin
   FGainR3 := Value;
   CalculateGainR0;
  end;
end;

procedure TDiffEvol.CalculateGainR0;
begin
 FGainBase := 1 - FGainBest - FGainR1 - FGainR2 - FGainR3;
end;

procedure TDiffEvol.RecreatePopulation;
var
  I : Integer;
begin
 for i := 0 to FPopulationCount - 1 do
  begin
   if not Assigned(FCurrentGeneration[i])
    then FCurrentGeneration[i] := TEvaluatedPopulation.Create
    else FCurrentGeneration[i].FValidCostFlag := False;

   if not Assigned(FNextGeneration[i])
    then FNextGeneration[i] := TEvaluatedPopulation.Create
    else FNextGeneration[i].FValidCostFlag := False;

   SetLength(FCurrentGeneration[i].FPopulation,FVariableCount);
   SetLength(FNextGeneration[i].FPopulation,FVariableCount);
  end;

 if AutoInitialize
  then Initialize;
end;

end.
