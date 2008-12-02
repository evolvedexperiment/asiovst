unit DAV_GuiRegister;

interface

{$I ..\ASIOVST.INC}

uses
  Classes, TypInfo,
  {$IFDEF FPC}
  LCLIntf, LazIDEIntf, PropEdits, ComponentEditors
  {$ELSE}
  {$IFDEF COMPILER6_UP}
  DesignIntf
  {$ELSE}
  DsgnIntf
  {$ENDIF}
  {$ENDIF};

procedure Register;

implementation

{$IFNDEF FPC}{$R ..\..\Resources\DAV_GuiRegister.res}{$ENDIF}

uses
  {$IFDEF FPC} LResources, {$ELSE} DAV_GuiGroup, DAV_GuiLevelMeter, {$ENDIF}
  DAV_GuiADSRGraph, DAV_GuiStaticWaveform, DAV_GuiDynamicWaveform,
  DAV_GuiAudioDataDisplay, DAV_GuiDial, DAV_GuiLED, DAV_GuiPanel, DAV_GuiLabel,
  DAV_GuiButton, DAV_GuiMidiKeys, DAV_GuiSelectBox, DAV_CorrelationMeter,
  DAV_GuiVUMeter, DAV_GuiGraphXY, DAV_GuiGraphXYDesign;

procedure Register;
begin
  RegisterComponents('ASIO/VST GUI', [TGuiDynamicWaveform, TGuiStaticWaveform,
    TGuiADSRGraph, TGuiAudioDataDisplay, TGuiLabel, TGuiPanel, TGuiLED,
    TGuiVUMeter, TGuiDial, TGuiDialMetal, TGuiCorrelationMeter, TGuiSelectBox,
    TGuiMidiKeys, TGuiButton, {$IFNDEF FPC} TGuiGroup, TGuiLevelMeter, {$ENDIF}
    TGuiGraphXY, TGuiDialEx, TGuiDialImageList, TGuiDialImageRenderer]);
  RegisterPropertyEditor(TypeInfo(string), TGuiGraphXYSeriesCollectionItem, 'SeriesClassName', nil);
  RegisterPropertyEditor(TypeInfo(TCustomGuiGraphXYSeries), TGuiGraphXYSeriesCollectionItem, 'Series', TSeriesClassProperty);
end;

{$IFDEF FPC}
initialization
  {$i ..\..\Resources\DAV_GuiRegister.lrs}
{$ENDIF}

end.
