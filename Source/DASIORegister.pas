unit DASIORegister;

interface

procedure Register;

implementation

uses Classes, DASIOHOST, DASIOGenerator;

procedure Register;
begin
 RegisterComponents('ASIO/VST Basics', [TASIOHost, TASIOHostBasic,TASIOGeneratorNoise,TASIOGeneratorTone]);
 {$IFDEF DELPHI5}
 {$IFDEF D5CP}
 RegisterComponentEditor(TASIOHost, TASIOControlPanel);
 {$ENDIF}
 {$ENDIF}
end;

end.
