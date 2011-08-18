unit FloatVST;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, DAV_Types, DAV_VSTModule,
  DAV_HalfFloat, DAV_MiniFloat;

type
  TFloatModule = class(TVSTModule)
    procedure ParameterFloatBitsDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterFloatBitsChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTModuleProcess32_8(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcess32_16(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcess32_32(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcess64_8(const Inputs, Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcess64_16(const Inputs, Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcess64_32(const Inputs, Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcess64_64(const Inputs, Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
  end;

implementation

{$R *.DFM}

procedure TFloatModule.ParameterFloatBitsChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 case Round(Parameter[Index] - 0.499) of
   0 :
     begin
       OnProcess32Replacing := VSTModuleProcess32_8;
       OnProcess64Replacing := VSTModuleProcess64_8;
     end;
   1 :
     begin
       OnProcess32Replacing := VSTModuleProcess32_16;
       OnProcess64Replacing := VSTModuleProcess64_16;
     end;
   2 :
     begin
       OnProcess32Replacing := VSTModuleProcess32_32;
       OnProcess64Replacing := VSTModuleProcess64_32;
     end
  else
     begin
       OnProcess32Replacing := VSTModuleProcess32_32;
       OnProcess64Replacing := VSTModuleProcess64_64;
     end;
 end;
 OnProcess := OnProcess32Replacing;
end;

procedure TFloatModule.ParameterFloatBitsDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 case Round(Parameter[Index] - 0.499) of
   0 : PreDefined := '8-Bit';
   1 : PreDefined := '16-Bit';
   2 : PreDefined := '32-Bit';
  else PreDefined := '64-Bit';
 end;
end;

procedure TFloatModule.VSTModuleProcess32_8(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  SampleIndex : Integer;
  Value       : TMiniFloat;
begin
  for SampleIndex := 0 to SampleFrames - 1 do
  begin
    Value := SingleToMiniFloat(Inputs[0, SampleIndex]);
    Outputs[0, SampleIndex] := MiniFloatToSingle(Value);
    Value := SingleToMiniFloat(Inputs[1, SampleIndex]);
    Outputs[1, SampleIndex] := MiniFloatToSingle(Value);
  end;
end;

procedure TFloatModule.VSTModuleProcess64_8(const Inputs,
  Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
var
  SampleIndex : Integer;
  Value       : TMiniFloat;
begin
  for SampleIndex := 0 to SampleFrames - 1 do
  begin
    Value := SingleToMiniFloat(Inputs[0, SampleIndex]);
    Outputs[0, SampleIndex] := MiniFloatToSingle(Value);
    Value := SingleToMiniFloat(Inputs[1, SampleIndex]);
    Outputs[1, SampleIndex] := MiniFloatToSingle(Value);
  end;
end;

procedure TFloatModule.VSTModuleProcess32_16(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  SampleIndex : Integer;
  Value       : THalfFloat;
begin
  for SampleIndex := 0 to SampleFrames - 1 do
  begin
    Value := SingleToHalfFloat(Inputs[0, SampleIndex]);
    Outputs[0, SampleIndex] := FastHalfFloatToSingle(Value);
    Value := SingleToHalfFloat(Inputs[1, SampleIndex]);
    Outputs[1, SampleIndex] := FastHalfFloatToSingle(Value);
  end;
end;

procedure TFloatModule.VSTModuleProcess64_16(const Inputs,
  Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
var
  SampleIndex : Integer;
begin
  for SampleIndex := 0 to SampleFrames - 1 do
  begin
    Outputs[0, SampleIndex] := FastHalfFloatToSingle(SingleToHalfFloat(
      Inputs[0, SampleIndex]));
    Outputs[1, SampleIndex] := FastHalfFloatToSingle(SingleToHalfFloat(
      Inputs[1, SampleIndex]));
  end;
end;

procedure TFloatModule.VSTModuleProcess32_32(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  SampleIndex : Integer;
begin
  for SampleIndex := 0 to SampleFrames - 1 do
  begin
    Outputs[0, SampleIndex] := FastHalfFloatToSingle(SingleToHalfFloat(
      Inputs[0, SampleIndex]));
    Outputs[1, SampleIndex] := FastHalfFloatToSingle(SingleToHalfFloat(
      Inputs[1, SampleIndex]));
  end;
end;

procedure TFloatModule.VSTModuleProcess64_32(const Inputs,
  Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
var
  SampleIndex : Integer;
  Value       : Single;
begin
  for SampleIndex := 0 to SampleFrames - 1 do
  begin
    Value := Inputs[0, SampleIndex];
    Outputs[0, SampleIndex] := Value;
    Value := Inputs[1, SampleIndex];
    Outputs[1, SampleIndex] := Value;
  end;
end;

procedure TFloatModule.VSTModuleProcess64_64(const Inputs,
  Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
var
  SampleIndex : Integer;
  Value       : Single;
begin
  Move(Inputs[0, 0], Outputs[0, 0], SampleFrames * SizeOf(Single));
  Move(Inputs[1, 0], Outputs[1, 0], SampleFrames * SizeOf(Single));
end;

end.
