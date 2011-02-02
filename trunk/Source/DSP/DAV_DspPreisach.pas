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
    FHysterons          : array of TCustomDspFloatingPointRelay;
    FHysteronResolution : Cardinal;
    FHysteronScale      : Single;
    procedure SetHysteronResolution(const Value: Cardinal);
    function GetTotalHysteronCount: Cardinal;
    function GetDistance: Single;
    function GetHysterons(Index: Integer): TCustomDspRelay;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    class function HysteronClass: TCustomDspFloatingPointRelayClass; virtual; abstract;
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
  protected
    class function HysteronClass: TCustomDspFloatingPointRelayClass; override;
  public
    procedure ProcessBlock32(const Data: PDAVSingleFixedArray; SampleCount: Integer);
    function ProcessSample32(Input: Single): Single;
  published
    property HysteronResolution;
  end;

  TDspPreisach64 = class(TCustomDspPreisach, IDspProcessor64)
  protected
    class function HysteronClass: TCustomDspFloatingPointRelayClass; override;
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
 HysteronResolution := 3;
end;

destructor TCustomDspPreisach.Destroy;
var
  HysteronIndex : Integer;
begin
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
  InvHysteronResolution : Single;
  X, Y                  : Integer;
  HalfDistance          : Single;
begin
 NewTotalHysteronCount := TotalHysteronCount;
 for HysteronIndex := NewTotalHysteronCount to Length(FHysterons) - 1
  do FreeAndNil(FHysterons[HysteronIndex]);

 SetLength(FHysterons, TotalHysteronCount);
 FHysteronScale := 1 / Length(FHysterons);
 HalfDistance := Distance;
 X := 0;
 Y := 0;
 InvHysteronResolution := 1 / FHysteronResolution;
 for HysteronIndex := 0 to Length(FHysterons) - 1 do
  begin
   // eventually create hysteron
   if not Assigned(FHysterons[HysteronIndex])
    then FHysterons[HysteronIndex] := HysteronClass.Create;

   FHysterons[HysteronIndex].Lower := (2 * X * InvHysteronResolution - 1) + HalfDistance;
   FHysterons[HysteronIndex].Upper := (1 - 2 * Y * InvHysteronResolution) - HalfDistance;

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

class function TDspPreisach32.HysteronClass: TCustomDspFloatingPointRelayClass;
begin
 Result := TDspRelay32;
end;

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
  HysteronIndex : Integer;
begin
 Result := TDspRelay32(FHysterons[0]).ProcessSample32(Input);
 for HysteronIndex := 1 to Length(FHysterons) - 1 do
  with TDspRelay32(FHysterons[HysteronIndex])
   do Result := Result + ProcessSample32(Input);
 Result := FHysteronScale * Result;
end;


{ TDspPreisach64 }

class function TDspPreisach64.HysteronClass: TCustomDspFloatingPointRelayClass;
begin
 Result := TDspRelay64;
end;

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
  HysteronIndex : Integer;
begin
 Result := TDspRelay64(FHysterons[0]).ProcessSample64(Input);
 for HysteronIndex := 1 to Length(FHysterons) - 1 do
  with TDspRelay64(FHysterons[HysteronIndex])
   do Result := Result + ProcessSample64(Input);
 Result := FHysteronScale * Result;
end;

end.
