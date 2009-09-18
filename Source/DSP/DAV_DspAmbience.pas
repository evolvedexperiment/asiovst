unit DAV_DspAmbience;

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
//  The code is based on the mda VST plug-ins by Paul Kellett, which is       //
//  located at http://sourceforge.net/projects/mda-vst/                       //
//  It was reviewed and rewritten from scratch by Christian-W. Budde          //
//                                                                            //
//  Portions created by Christian-W. Budde are Copyright (C) 2008-2009        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I ..\DAV_Compiler.inc}

uses
  Classes, DAV_Common, DAV_DspCommon, DAV_DspFilter, DAV_DSPFilterButterworth,
  DAV_DspFilterBasics;

type
  TCustomAmbience = class(TDspSampleRatePersistent, IDspProcessor32)
  private
    FDamping    : Single;
    FDry, FWet  : Single;
    FRoomsize   : Single;
    FOutputGain : Single;
    function GetMix: Single;
    procedure SetDamping(const Value: Single);
    procedure SetDry(const Value: Single);
    procedure SetMix(const Value: Single);
    procedure SetOutputGain(const Value: Single);
    procedure SetRoomSize(const Value: Single);
    procedure SetWet(const Value: Single);
    procedure CalculateDryFactor;
    procedure CalculateWetFactor;
    procedure CalculateDampingFactor;
    procedure CalculateRoomsizeFactor;
    procedure CalculateOutputFactor;
  protected
    FBuffers        : Array [0..3] of PDAVSingleFixedArray;
    FPos            : Integer;
    FHfDampState    : Single;
    FDampFactor     : Single;
    FOutputFactor   : Single;
    FRoomsizeFactor : Double;
    FDryFactor      : Single;
    FWetFactor      : Single;
    FFlushedBuffers : Boolean;
    FHighShelf      : TBasicHighShelfFilter;
    procedure AllocateBuffers; virtual;
    procedure DampingChanged; virtual;
    procedure DryChanged; virtual;
    procedure FlushBuffers; virtual;
    procedure OutputGainChanged; virtual;
    procedure RoomsizeChanged; virtual;
    procedure SampleRateChanged; override;
    procedure WetChanged; virtual;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; override;
    destructor Destroy; override;

    function ProcessSample32(Input: Single): Single; overload;
    procedure ProcessSample(var Left, Right: Single); overload;

    property Damping: Single read FDamping write SetDamping;
    property Dry: Single read FDry write SetDry;
    property Mix: Single read GetMix write SetMix;
    property OutputGain: Single read FOutputGain write SetOutputGain;
    property RoomSize: Single read FRoomsize write SetRoomSize;
    property Wet: Single read FWet write SetWet;
  end;

  TAmbience = class(TCustomAmbience)
  published
    property Damping;
    property Dry;
    property Mix;
    property OutputGain;
    property RoomSize;
    property SampleRate;
    property Wet;
  end;

implementation

uses
  SysUtils;

const
  CBufferSize = 1024;
  CFeedback = 0.8;

{ TCustomAmbience }

constructor TCustomAmbience.Create;
begin
 inherited;

 AllocateBuffers;

 FHfDampState := 0.0;
 FFlushedBuffers := FPos = 0;

 // initialize
 FRoomsize   := 0.7;
 FDamping    := 0.7;
 FWet        := 0.9;
 FDry        := 0.1;
 FOutputGain := 0.5;
 FHighShelf  := TBasicHighShelfFilter.Create;
 with FHighShelf do
  begin
   Frequency := 1900;
   SampleRate := Self.SampleRate;
   Bandwidth := 2.8;
   Gain := 12;
  end;

 CalculateOutputFactor;
 CalculateDryFactor;
 CalculateWetFactor;
 CalculateDampingFactor;
 CalculateRoomsizeFactor;
end;

destructor TCustomAmbience.Destroy;
begin
 FreeAndNil(FHighShelf);
 Dispose(FBuffers[0]);
 Dispose(FBuffers[1]);
 Dispose(FBuffers[2]);
 Dispose(FBuffers[3]);
 inherited;
end;

procedure TCustomAmbience.AllocateBuffers;
begin
 GetMem(FBuffers[0], CBufferSize * SizeOf(Single));
 GetMem(FBuffers[1], CBufferSize * SizeOf(Single));
 GetMem(FBuffers[2], CBufferSize * SizeOf(Single));
 GetMem(FBuffers[3], CBufferSize * SizeOf(Single));
end;

function TCustomAmbience.GetMix: Single;
begin
 if FWet + FDry <= 0
  then result := 0.5
  else result := FWet / (FWet + FDry);
end;

procedure TCustomAmbience.SetDamping(const Value: Single);
begin
 if FDamping <> Value then
  begin
   FDamping := Value;
   DampingChanged;
  end;
end;

procedure TCustomAmbience.SetDry(const Value: Single);
begin
 if FDry <> Value then
  begin
   FDry := Value;
   DryChanged;
  end;
end;

procedure TCustomAmbience.SetMix(const Value: Single);
var
  Sum : Single;
begin
 if Mix <> Value then
  begin
   Sum := (FWet + FDry);

   FWet := Value;
   FDry := 1 - Value;

   // todo: verify this!!!
   FWet := FWet * Sum;
   FDry := FDry * Sum;
  end;
end;

procedure TCustomAmbience.SetOutputGain(const Value: Single);
begin
 if FOutputGain <> Value then
  begin
   FOutputGain := Value;
   OutputGainChanged;
  end;
end;

procedure TCustomAmbience.OutputGainChanged;
begin
 CalculateOutputFactor;
 CalculateDryFactor;
 CalculateWetFactor;
end;

procedure TCustomAmbience.CalculateOutputFactor;
begin
 FOutputFactor := dB_to_Amp(FOutputGain);
end;

procedure TCustomAmbience.SetRoomSize(const Value: Single);
begin
 if FRoomsize <> Value then
  begin
   FRoomSize := Value;
   RoomSizeChanged;
  end;
end;

procedure TCustomAmbience.SampleRateChanged;
begin
 inherited;
 FHighShelf.SampleRate := SampleRate;
end;

procedure TCustomAmbience.RoomsizeChanged;
begin
 CalculateRoomsizeFactor;
 FlushBuffers;
end;

procedure TCustomAmbience.CalculateRoomsizeFactor;
begin
 FRoomsizeFactor := 0.025 + 0.2665 * FRoomSize;
end;

procedure TCustomAmbience.FlushBuffers;
begin
 FillChar(FBuffers[0]^[0], CBufferSize * SizeOf(Single), 0);
 FillChar(FBuffers[1]^[0], CBufferSize * SizeOf(Single), 0);
 FillChar(FBuffers[2]^[0], CBufferSize * SizeOf(Single), 0);
 FillChar(FBuffers[3]^[0], CBufferSize * SizeOf(Single), 0);
end;

procedure TCustomAmbience.SetWet(const Value: Single);
begin
 if FWet <> Value then
  begin
   FWet := Value;
   WetChanged;
  end;
end;

procedure TCustomAmbience.DryChanged;
begin
 CalculateDryFactor;
end;

procedure TCustomAmbience.CalculateDryFactor;
begin
 FDryFactor := FOutputFactor - sqr(FDry) * FOutputFactor;
end;

procedure TCustomAmbience.WetChanged;
begin
 CalculateWetFactor;
end;

procedure TCustomAmbience.CalculateWetFactor;
begin
 FWetFactor := 0.8 * FWet * FOutputFactor;
end;

procedure TCustomAmbience.DampingChanged;
begin
 CalculateDampingFactor; 
end;

procedure TCustomAmbience.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomAmbience then
  with TCustomAmbience(Dest) do
   begin
    inherited;
    FBuffers        := Self.FBuffers;
    FPos            := Self.FPos;
    FHfDampState    := Self.FHfDampState;
    FDampFactor     := Self.FDampFactor;
    FOutputFactor   := Self.FOutputFactor;
    FRoomsizeFactor := Self.FRoomsizeFactor;
    FDryFactor      := Self.FDryFactor;
    FWetFactor      := Self.FWetFactor;
    FFlushedBuffers := Self.FFlushedBuffers;
    FHighShelf      := Self.FHighShelf;
   end
 else inherited;
end;

procedure TCustomAmbience.CalculateDampingFactor;
begin
 FDampFactor := 0.05 + 0.01 * FDamping;
end;

function TCustomAmbience.ProcessSample32(Input: Single): Single;
var
  r : Double;
  t : Double;
begin
 Input := FHighShelf.ProcessSample32(Input);

 // apply HF damping
 FHfDampState := FHfDampState + FDampFactor * (FWet * Input - FHfDampState);  // HF damping
 r := FHfDampState;

 if (abs(FHfDampState) > 1E-10) then
  begin
   // Catch Denormals
   FFlushedBuffers := False;
  end
 else
  begin
   FHfDampState := 0;
   if FFlushedBuffers = False then
    begin
     FFlushedBuffers := True;
     FlushBuffers;
    end;
  end;

 // decorrelation allpass delay filters
 t := FBuffers[0]^[FPos];
 r := r - CFeedback * t;
 FBuffers[0]^[(FPos + round(107 * FRoomsize)) and 1023] := r; // Allpass
 r := r + t;

 t := FBuffers[1]^[FPos];
 r := r - CFeedback * t;
 FBuffers[1]^[(FPos + round(142 * FRoomsize)) and 1023] := r; // Allpass
 r := r + t;

 t := FBuffers[2]^[FPos];
 r := r - CFeedback * t;
 FBuffers[2]^[(FPos + round(277 * FRoomsize)) and 1023] := r; // Allpass
 r := r + t;
 Result := FDry * Input + r - FHfDampState; // Left Output

 t := FBuffers[3]^[FPos];
 r := r - CFeedback * t;
 FBuffers[3]^[(FPos + round(379 * FRoomsize)) and 1023] := r; // Allpass
 r := r + t;
 result := CHalf32 * (result + (FDry * Input + r - FHfDampState)); // Right Output

 // advance position
 FPos := (FPos + 1) and 1023;
end;

procedure TCustomAmbience.ProcessSample(var Left, Right: Single);
var
  r : Double;
  t : Double;
begin
 // apply HF damping
 FHfDampState := FHfDampState + FDampFactor * (FWet * FHighShelf.ProcessSample64(Left + Right) - FHfDampState);  // HF damping
 r := FHfDampState;

 if (abs(FHfDampState) > 1E-10) then
  begin
   // Catch Denormals
   FFlushedBuffers := False;
  end
 else
  begin
   FHfDampState := 0;
   if FFlushedBuffers = False then
    begin
     FFlushedBuffers := True;
     FlushBuffers;
    end;
  end;

 // decorrelation allpass delay filters
 t := FBuffers[0]^[FPos];
 r := r - CFeedback * t;
 FBuffers[0]^[(FPos + round(107 * FRoomsize)) and 1023] := r; // Allpass
 r := r + t;

 t := FBuffers[1]^[FPos];
 r := r - CFeedback * t;
 FBuffers[1]^[(FPos + round(142 * FRoomsize)) and 1023] := r; // Allpass
 r := r + t;

 t := FBuffers[2]^[FPos];
 r := r - CFeedback * t;
 FBuffers[2]^[(FPos + round(277 * FRoomsize)) and 1023] := r; // Allpass
 r := r + t;
 Left := FDry * Left + r - FHfDampState; // Left Output

 t := FBuffers[3]^[FPos];
 r := r - CFeedback * t;
 FBuffers[3]^[(FPos + round(379 * FRoomsize)) and 1023] := r; // Allpass
 r := r + t;
 Right := FDry * Right + r - FHfDampState; // Right Output

 // advance position
 FPos := (FPos + 1) and 1023;
end;

end.
