unit SEIntToListGUI;

interface

uses
  Windows, DAV_SECommon, DAV_SEModule, DAV_SEGUI, SEIntToListModule;

const
  pinIn   = 0;
  pinOut  = 1;
  pinMode = 2;

type
  TSEIntToListGui = class(TSEGUIBase)
  protected
    procedure GuiPinValueChange(CurrentPin: TSeGuiPin); override;
  public
    constructor Create(SEGuiCallback: TSEGuiCallback; AHostPtr: Pointer); override;
  end;

implementation

uses
  SysUtils;

constructor TSEIntToListGui.Create(SEGuiCallback: TSEGuiCallback; AHostPtr: Pointer);
begin
 inherited;
end;

procedure TSEIntToListGui.GuiPinValueChange(CurrentPin: TSeGuiPin);
var
  Mode      : Integer;
  InValue   : Integer;
  OutValue  : Integer;
  it        : TItEnumList;
  ExtraData : TSeSdkString2;
begin
 inherited;
 Mode := Pin[pinMode].getValueInt;

 case CurrentPin.GetIndex of
  pinIn,
  pinMode:
   begin
    InValue := Pin[pinIn].getValueInt;
    if (Mode = 0) then // calc what value this index maps to
     begin
      ExtraData := Pin[pinOut].getExtraData;
      it := TItEnumList.Create(ExtraData);
      it.First;
      while (not it.IsDone) and (it.CurrentItem^.Index <> InValue) do it.Next;
      if not it.IsDone
       then Pin[pinOut].setValueInt(it.CurrentItem^.value);
     end else Pin[pinOut].setValueInt(InValue);
   end;
  pinOut:
   begin
    OutValue := Pin[pinOut].getValueInt;
    if Mode = 0 then // calc what index this value maps to
     begin
      ExtraData := Pin[pinOut].getExtraData;
      it := TItEnumList.Create(ExtraData);
      it.First;
      while (not it.IsDone) and (it.CurrentItem^.Index <> OutValue) do it.Next;
      if not it.IsDone
       then Pin[pinIn].setValueInt(it.CurrentItem^.value);
     end else Pin[pinIn].setValueInt(OutValue);
   end;
 end;
end;

end.
