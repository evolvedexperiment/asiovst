unit DAV_Bindings;

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
//  The initial developer of this Project is Christian-W. Budde               //
//                                                                            //
//  The unit may contain code snippets found in several other open source     //
//  projects.                                                                 //
//                                                                            //
//  Portions created by Christian-W. Budde are Copyright (C) 2008-2009        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I DAV_Compiler.inc}

uses
  Classes, SysUtils, DAV_ProcessorInfo;

type
  TProcessorFeature = (pfMMX, pfEMMX, pf3DNow, pf3DNowExt, pfCMOV,
    pfSSE, pfSSE2, pfSSE3, pfSSE3x, pfSSE4A, pfSSE4B, pfSSE5, pfAVX);
  TProcessorFeatures = set of TProcessorFeature;

  PFunctionInfo = ^TFunctionInfo;
  TFunctionInfo = record
    Proc        : Pointer;
    CPUFeatures : TProcessorFeatures;
//    OpcodeCount : Integer;
  end;

  TFunctionBinding = class(TPersistent)
  private
    FFunctions   : TList;
    FDefaultProc : Pointer;
    FPrototype   : Pointer;
    function GetFunctions(Index: Integer): PFunctionInfo;
    procedure SetFunctions(Index: Integer; const Value: PFunctionInfo);
  public
    constructor Create(Prototype: Pointer; DefaultProc: Pointer = nil); virtual;
    destructor Destroy; override;
    procedure Clear;

    procedure Add(Proc: Pointer; RequiredFeatures: TProcessorFeatures = []);

    procedure Rebind(AvailableFeatures: TProcessorFeatures = []);

    //function FindFunction(FunctionID: Integer; PriorityCallback: TFunctionPriority = nil): Pointer;
    property Functions[Index: Integer]: PFunctionInfo read GetFunctions write SetFunctions;
  end;

implementation

uses
  Math;

var
  GBindings: TList;

{ TFunctionBinding }

procedure TFunctionBinding.Add(Proc: Pointer;
  RequiredFeatures: TProcessorFeatures = []);
var
  Info: PFunctionInfo;
begin
 New(Info);
 Info.Proc := Proc;
 Info.CPUFeatures := RequiredFeatures;

 // add function
 FFunctions.Add(Info);
end;

procedure TFunctionBinding.Clear;
var
  I: Integer;
begin
 for I := 0 to FFunctions.Count - 1
  do Dispose(PFunctionInfo(FFunctions[I]));

 // clear functions
 FFunctions.Clear;
end;

constructor TFunctionBinding.Create(Prototype: Pointer;
  DefaultProc: Pointer = nil);
begin
 FPrototype := Prototype;
 Pointer(FPrototype^) := DefaultProc;
 FFunctions := TList.Create;
 GBindings.Add(Self);
end;

destructor TFunctionBinding.Destroy;
var
  BindingIndex : Integer;
begin
 Clear;
 FreeAndNil(FFunctions);

 // remove current binding
 BindingIndex := GBindings.IndexOf(Self);
 if BindingIndex >= 0
  then GBindings.Delete(BindingIndex);

 inherited;
end;

procedure TFunctionBinding.Rebind(AvailableFeatures: TProcessorFeatures);
var
  i : Integer;
begin
 with FFunctions do
  for i := 0 to Count - 1 do
   if AvailableFeatures + PFunctionInfo(Items[i])^.CPUFeatures = AvailableFeatures
    then Pointer(FPrototype^) := PFunctionInfo(Items[i])^.Proc;
end;

function TFunctionBinding.GetFunctions(Index: Integer): PFunctionInfo;
begin
 Result := FFunctions[Index];
end;

procedure TFunctionBinding.SetFunctions(Index: Integer;
  const Value: PFunctionInfo);
begin
 FFunctions[Index] := Value;
end;

initialization
  GBindings := TList.Create;

finalization
  GBindings.Free;

end.
