unit DAV_DspPreisach;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  Version: MPL 1.1 or LGPL 2.1 with linking exception                       //
//                                                                            //
//  The contents of this file are subject to the Mozilla Public License       //
//  Version 1.1 (the "License"); you may not use this file except in          //
//  compliance with the License. You may obtain a copy of the License at      //
//  http://www.mozilla.org/MPL/                                               //
//                                                                            //
//  Software distributed under the License is distributed on an "AS IS"       //
//  basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the   //
//  License for the specific language governing rights and limitations under  //
//  the License.                                                              //
//                                                                            //
//  Alternatively, the contents of this file may be used under the terms of   //
//  the Free Pascal modified version of the GNU Lesser General Public         //
//  License Version 2.1 (the "FPC modified LGPL License"), in which case the  //
//  provisions of this license are applicable instead of those above.         //
//  Please see the file LICENSE.txt for additional information concerning     //
//  this license.                                                             //
//                                                                            //
//  The code is part of the Delphi ASIO & VST Project                         //
//                                                                            //
//  The initial developer of this code is Christian-W. Budde                  //
//                                                                            //
//  Portions created by Christian-W. Budde are Copyright (C) 2011             //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I DAV_Compiler.inc}

uses
  Classes, SysUtils, DAV_Types, DAV_Classes, DAV_DspRelay;

type
  TCustomDspPreisach = class(TDspPersistent)
  private
    FHysterons          : array of TDspIntegerRelay;
    FHysteronResolution : Cardinal;
    FHysteronScale      : Single;
    FStates             : PByteArray;
    procedure SetHysteronResolution(const Value: Cardinal);
    function GetTotalHysteronCount: Cardinal;
    function GetDistance: Single;
    function GetHysterons(Index: Integer): TCustomDspRelay;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure HysteronResolutionChanged; virtual;

    property TotalHysteronCount: Cardinal read GetTotalHysteronCount;
    property Distance: Single read GetDistance;
    property Hysterons[Index: Integer]: TCustomDspRelay read GetHysterons;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    property HysteronResolution: Cardinal read FHysteronResolution write SetHysteronResolution;
  end;

  TDspPreisach32 = class(TCustomDspPreisach, IDspProcessor32)
  public
    procedure ProcessBlock32(const Data: PDAVSingleFixedArray; SampleCount: Integer);
    function ProcessSample32(Input: Single): Single;
  published
    property HysteronResolution;
  end;

  TDspPreisach64 = class(TCustomDspPreisach, IDspProcessor64)
  public
    procedure ProcessBlock64(const Data: PDAVDoubleFixedArray; SampleCount: Integer);
    function ProcessSample64(Input: Double): Double;
  published
    property HysteronResolution;
  end;

implementation

{ TCustomDspPreisach }

constructor TCustomDspPreisach.Create;
begin
 inherited;
 FStates := nil;
 HysteronResolution := 3;
end;

destructor TCustomDspPreisach.Destroy;
var
  HysteronIndex : Integer;
begin
 FreeMem(FStates);

 for HysteronIndex := 0 to Length(FHysterons) - 1
  do FreeAndNil(FHysterons[HysteronIndex]);
 inherited;
end;

function TCustomDspPreisach.GetDistance: Single;
begin
 Result := 1 / (FHysteronResolution);
end;

function TCustomDspPreisach.GetHysterons(Index: Integer): TCustomDspRelay;
begin
 if (Index > 0) and (Index < Length(FHysterons))
  then Result := FHysterons[Index]
  else raise Exception.CreateFmt('Index out of bounds', [Index]);
end;

function TCustomDspPreisach.GetTotalHysteronCount: Cardinal;
begin
 Result := ((FHysteronResolution - 1) * FHysteronResolution) div 2;
end;

procedure TCustomDspPreisach.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomDspPreisach then
  with TCustomDspPreisach(Dest) do
   begin
    inherited;
    HysteronResolution := Self.HysteronResolution;
   end
 else inherited;
end;

procedure TCustomDspPreisach.HysteronResolutionChanged;
var
  NewTotalHysteronCount : Integer;
  HysteronIndex         : Integer;
  X, Y                  : Integer;
  HalfDistance          : Single;
begin
 NewTotalHysteronCount := TotalHysteronCount;
 for HysteronIndex := NewTotalHysteronCount to Length(FHysterons) - 1
  do FreeAndNil(FHysterons[HysteronIndex]);

 SetLength(FHysterons, TotalHysteronCount);

 // allocate states
 ReallocMem(FStates, TotalHysteronCount);

 // reset states (todo!)
 for HysteronIndex := 0 to TotalHysteronCount - 1
  do FStates[HysteronIndex] := 1;


 FHysteronScale := 1 / Length(FHysterons);
 HalfDistance := Distance;
 X := 0;
 Y := 0;
 for HysteronIndex := 0 to Length(FHysterons) - 1 do
  begin
   // eventually create hysteron
   if not Assigned(FHysterons[HysteronIndex])
    then FHysterons[HysteronIndex] := TDspIntegerRelay.Create;

   FHysterons[HysteronIndex].Lower := (2 * X - FHysteronResolution) + 1;
   FHysterons[HysteronIndex].Upper := (FHysteronResolution - 2 * Y) - 1;

   Inc(X);
   if X >= FHysteronResolution - Y - 1 then
    begin
     X := 0;
     Inc(Y);
    end;
  end;

 Assert(X = 0);
 Assert(Y + 1 = FHysteronResolution);
end;

procedure TCustomDspPreisach.SetHysteronResolution(const Value: Cardinal);
begin
 if Value <> FHysteronResolution then
  begin
   FHysteronResolution := Value;
   HysteronResolutionChanged;
  end;
end;


{ TDspPreisach32 }

procedure TDspPreisach32.ProcessBlock32(const Data: PDAVSingleFixedArray;
  SampleCount: Integer);
var
  Sample: Integer;
begin
 for Sample := 0 to SampleCount - 1
  do Data[Sample] := ProcessSample32(Data[Sample]);
end;

function TDspPreisach32.ProcessSample32(Input: Single): Single;
var
  IntegerInput  : Integer;
  IntegerResult : Integer;
  HysteronIndex : Integer;
  X, Y, Offset  : Integer;
begin
(*
 IntegerInput := Round(FHysteronResolution * Input);

 X := ((FHysteronResolution + IntegerInput) div 2);
 if X < 0 then X := 0;
 Offset := FHysteronResolution - 1;
 HysteronIndex := X;
 for y := 0 to FHysteronResolution - X - 2 do
  begin
{
   Assert((FHysteronResolution - 1) - X - Y > 0);
   if HysteronIndex + FHysteronResolution - 1 - X - Y >= Length(FHysterons)
    then
     begin
      IntegerInput := HysteronIndex;
      Break;
     end;
}
   FillChar(FStates^[HysteronIndex], FHysteronResolution - 1 - X - Y, 0);
   Inc(HysteronIndex, Offset);
   Dec(Offset);
  end;

 Y := (FHysteronResolution - IntegerInput) div 2;
 if Y < 0 then Y := 0;
{
  begin
   FillChar(FStates^[0], Length(FHysterons), 1);
   IntegerResult := Length(FHysterons);
  end
 else
}
  begin
   Offset := Y * FHysteronResolution - (Y * (Y + 1)) div 2;
   FillChar(FStates^[Offset], Length(FHysterons) - Offset, 1);

   IntegerResult := 0;
   for HysteronIndex := 0 to Offset - 1
    do Inc(IntegerResult, FStates[HysteronIndex]);
   Inc(IntegerResult, Length(FHysterons) - Offset);
  end;

 Result := 2 * FHysteronScale * IntegerResult - 1;
*)


 IntegerInput := Round(FHysteronResolution * Input);

 X := ((FHysteronResolution + IntegerInput) div 2);
 if X <= 0 then
  begin
   FillChar(FStates^[0], Length(FHysterons), 0);
   Result := -1;
   Exit;
  end
 else
  if X < FHysteronResolution - 1 then
   begin
    Offset := FHysteronResolution - 1;
    HysteronIndex := X;
    for y := 0 to FHysteronResolution - X - 2 do
     begin
      FillChar(FStates^[HysteronIndex], FHysteronResolution - 1 - X - Y, 0);
      Inc(HysteronIndex, Offset);
      Dec(Offset);
     end;
   end;


(*
 X := ((FHysteronResolution + IntegerInput) div 2);
 if X < 0 then X := 0;
 Y := 0;
 Offset := FHysteronResolution - 1;
 HysteronIndex := X;
 while (X >= 0) and (X < FHysteronResolution - 1) do
  begin
   FStates[HysteronIndex] := 0;
   Inc(HysteronIndex, Offset);
   Dec(Offset);
   Inc(Y);
   if Y >= FHysteronResolution - X - 1 then
    begin
     Y := 0;
     Inc(X);
     Offset := FHysteronResolution - 1;
     HysteronIndex := X;
    end;
  end;
*)


 Y := (FHysteronResolution - IntegerInput) div 2;
 if Y <= 0 then
  begin
   FillChar(FStates^[0], Length(FHysterons), 1);
   Result := 1;
   Exit;
  end
 else
  begin
   if Y < FHysteronResolution - 1 then
    begin
     Offset := Y * FHysteronResolution - (Y * (Y + 1)) div 2;
     FillChar(FStates^[Offset], Length(FHysterons) - Offset, 1);
    end else Offset := Length(FHysterons);

    IntegerResult := 0;
    for HysteronIndex := 0 to Offset - 1
     do Inc(IntegerResult, FStates[HysteronIndex]);
    Inc(IntegerResult, Length(FHysterons) - Offset);
  end;
 Result := 2 * FHysteronScale * IntegerResult - 1;

(*
 IntegerInput := Round(FHysteronResolution * Input);
 IntegerResult := FHysterons[0].ProcessSample(IntegerInput);
 for HysteronIndex := 1 to Length(FHysterons) - 1 do
  with FHysterons[HysteronIndex]
   do Inc(IntegerResult, ProcessSample(IntegerInput));
 Result := FHysteronScale * IntegerResult;
*)
end;


{ TDspPreisach64 }

procedure TDspPreisach64.ProcessBlock64(const Data: PDAVDoubleFixedArray;
  SampleCount: Integer);
var
  Sample: Integer;
begin
 for Sample := 0 to SampleCount - 1
  do Data[Sample] := ProcessSample64(Data[Sample]);
end;

function TDspPreisach64.ProcessSample64(Input: Double): Double;
var
  IntegerInput  : Integer;
  IntegerResult : Integer;
  HysteronIndex : Integer;
begin
 IntegerInput := Round(FHysteronResolution * Input);
 IntegerResult := FHysterons[0].ProcessSample(IntegerInput);
 for HysteronIndex := 1 to Length(FHysterons) - 1 do
  with FHysterons[HysteronIndex]
   do Inc(IntegerResult, ProcessSample(IntegerInput));
 Result := FHysteronScale * IntegerResult;
end;

end.
