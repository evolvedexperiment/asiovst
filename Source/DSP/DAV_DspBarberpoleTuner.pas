unit DAV_DspBarberpoleTuner;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2009             //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I ..\DAV_Compiler.inc}

uses
  Classes, DAV_Common, DAV_DspCommon, DAV_DspLfo, DAV_DspTuner,
  DAV_DspFilterButterworth;

type
  TCustomBarberpoleFilter = class(TDspSampleRatePersistent, IDspProcessor32)
  private
    FLFO     : TLFOSine32;
    FLowpass : TButterworthLowPassFilter;
    function GetFrequency: Single;
    function GetOrder: Integer;
    procedure SetFrequency(const Value: Single);
    procedure SetOrder(const Value: Integer);
  protected
    procedure SampleRateChanged; override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function ProcessSample32(Input: Single): Single; virtual;

    property Frequency: Single read GetFrequency write SetFrequency;
    property Order: Integer read GetOrder write SetOrder;
  end;

  TBarberpoleFilter = class(TCustomBarberpoleFilter)
  published
    property Frequency;
    property Order;
    property SampleRate;
  end;

  TCustomBarberpoleTuner = class(TCustomTuner)
  private
    FBarberpoleFilter : TBarberpoleFilter;
    FZCTuner          : TZeroCrossingTuner;
    function GetFrequency: Single;
    function GetFrequencyDifference: Single;
    function GetOrder: Integer;
    procedure SetFrequency(const Value: Single);
    procedure SetOrder(const Value: Integer);
  protected
    procedure SampleRateChanged; override;
    function GetCurrentFrequency: Single; override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure ProcessSample32(Input: Single); override;

    property Frequency: Single read GetFrequency write SetFrequency;
    property FrequencyDifference: Single read GetFrequencyDifference;
    property Order: Integer read GetOrder write SetOrder;
  end;

  TBarberpoleTuner = class(TCustomBarberpoleTuner)
  published
    property Frequency;
    property Order;
  end;

implementation

uses
  SysUtils, DAV_Approximations;

resourcestring
  RCStrOrderMustBeLarger0 = 'Order must be larger than 0!';

{ TCustomBarberpoleFilter }

procedure TCustomBarberpoleFilter.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomBarberpoleFilter then
  with TCustomBarberpoleFilter(Dest) do
   begin
    inherited;
    FLFO.Assign(Self.FLFO);
    FLowpass.Assign(Self.FLowpass);
   end
 else inherited;
end;

constructor TCustomBarberpoleFilter.Create;
begin
 inherited;

 FLFO := TLFOSine32.Create;
 with FLFO do
  begin
   Frequency := 440;
   SampleRate := Self.SampleRate;
  end;

 FLowpass := TButterworthLowPassFilter.Create(4);
 with FLowpass do
  begin
   SampleRate := Self.SampleRate;
   Frequency := Self.Frequency * FastPower2MinError4(2 * COneTwelfth32) - Self.Frequency;
  end;
end;

destructor TCustomBarberpoleFilter.Destroy;
begin
 FreeAndNil(FLFO);
 FreeAndNil(FLowpass);
 inherited;
end;

function TCustomBarberpoleFilter.GetFrequency: Single;
begin
 result := FLFO.Frequency;
end;

function TCustomBarberpoleFilter.GetOrder: Integer;
begin
 result := FLowpass.Order;
end;

function TCustomBarberpoleFilter.ProcessSample32(Input: Single): Single;
begin
 inherited;
 FLFO.CalculateNextSample;
 result := FLowpass.ProcessSample64(FLFO.Sine * Input);
end;

procedure TCustomBarberpoleFilter.SampleRateChanged;
begin
 inherited;
 if assigned(FLFO) then FLFO.SampleRate := SampleRate;
 if assigned(FLowpass) then FLowpass.SampleRate := SampleRate;
end;

procedure TCustomBarberpoleFilter.SetFrequency(const Value: Single);
begin
 if FLFO.Frequency <> Value then
  begin
   FLFO.Frequency := Value;
   FLowpass.Frequency := Value * FastPower2MinError4(2 * COneTwelfth32) - Value;
  end;
end;

procedure TCustomBarberpoleFilter.SetOrder(const Value: Integer);
begin
 if Value > 0
  then FLowpass.Order := Value
  else raise Exception.Create(RCStrOrderMustBeLarger0);
end;


{ TCustomBarberpoleTuner }

procedure TCustomBarberpoleTuner.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomBarberpoleTuner then
  with TCustomBarberpoleTuner(Dest) do
   begin
    inherited;
    FBarberpoleFilter.Assign(Self.FBarberpoleFilter);
    FZCTuner.Assign(Self.FZCTuner);
   end
 else inherited;
end;

constructor TCustomBarberpoleTuner.Create;
begin
 inherited;
 FBarberpoleFilter := TBarberpoleFilter.Create;
 with FBarberpoleFilter do
  begin
   Frequency := 440;
   SampleRate := Self.SampleRate;
  end;

 FZCTuner := TZeroCrossingTuner.Create;
 with FZCTuner do
  begin
   MinimumFrequency := 0;
   MaximumFrequency := 2 * Self.Frequency * FastPower2MinError4(2 * COneTwelfth32) - Self.Frequency;
   OneCrossingOnly := True;
   SampleRate := Self.SampleRate;
  end;
end;

destructor TCustomBarberpoleTuner.Destroy;
begin
 FreeAndNil(FBarberpoleFilter);
 FreeAndNil(FZCTuner);
 inherited;
end;

procedure TCustomBarberpoleTuner.SampleRateChanged;
begin
 inherited;
 if assigned(FBarberpoleFilter) then FBarberpoleFilter.SampleRate := SampleRate;
 if assigned(FZCTuner) then FZCTuner.SampleRate := SampleRate;
end;

procedure TCustomBarberpoleTuner.SetFrequency(const Value: Single);
begin
 FBarberpoleFilter.Frequency := Value;
 FZCTuner.MaximumFrequency := 2 * Frequency * FastPower2MinError4(2 * COneTwelfth32) - Frequency;
end;

procedure TCustomBarberpoleTuner.SetOrder(const Value: Integer);
begin
 FBarberpoleFilter.Order := Value;
end;

function TCustomBarberpoleTuner.GetCurrentFrequency: Single;
begin
 result := Frequency + FrequencyDifference;
end;

function TCustomBarberpoleTuner.GetFrequency: Single;
begin
 result := FBarberpoleFilter.Frequency;
end;

function TCustomBarberpoleTuner.GetFrequencyDifference: Single;
begin
 result := FZCTuner.CurrentFrequency;
end;

function TCustomBarberpoleTuner.GetOrder: Integer;
begin
 result := FBarberpoleFilter.Order;
end;

procedure TCustomBarberpoleTuner.ProcessSample32(Input: Single);
begin
 inherited;
 FZCTuner.ProcessSample32(FBarberpoleFilter.ProcessSample32(Input));
end;

end.
