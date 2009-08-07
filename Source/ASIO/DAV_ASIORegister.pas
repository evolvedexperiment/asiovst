unit DAV_ASIORegister;

interface

{$I ..\DAV_Compiler.inc}

procedure Register;

implementation

{$IFNDEF FPC}{$R ..\..\Resources\DAV_AsioHostRegister.res}{$ENDIF}

uses
  {$IFDEF FPC} LResources, {$ENDIF} Classes,
  {$IFDEF DELPHI7_UP} PropertyCategories, DesignIntf,{$ENDIF}
  DAV_ASIOHOST, DAV_ASIOGenerator;

procedure Register;
begin
 RegisterComponents('ASIO/VST Basics', [TASIOHost, TASIOHostBasic,
   TASIOHostAudioData, TASIOGeneratorNoise, TASIOGeneratorTone]);
 {$IFDEF COMPILER5}
 RegisterComponentEditor(TASIOHost, TASIOControlPanel);
 {$ENDIF}

 {$IFDEF DELPHI7_UP}
 RegisterPropertiesInCategory('Buffer Information', TCustomASIOHostBasic,
    ['BufferGranularity', 'BufferMaximum', 'BufferMinimum',
     'BufferPreferredSize', 'BufferSize']);
 RegisterPropertiesInCategory('Input', TCustomASIOHostBasic,
    ['InputChannelCount', 'InputChannelInfo', 'InputLatency', 'InputMeter']);

 {$ENDIF}
end;

{$IFDEF FPC}
initialization
  {$i ..\..\Resources\DAV_AsioHostRegister.lrs}
{$ENDIF}

end.
