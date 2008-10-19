unit SEIntToListGUI;

interface

uses
  Windows, SECommon, SEDSP, SEGUI, SEIntToListModule;

const
  pinIn   = 0;
  pinOut  = 1;
  pinMode = 2;

type
  TSEIntToListGui = class(TSEGUIBase)
  public
    constructor Create(SEGuiCallback: TSEGuiCallback; AHostPtr: Pointer); override;
    procedure GuiPinValueChange(Pin: TSeGuiPin); override;
  end;

implementation

uses
  SysUtils;

constructor TSEIntToListGui.Create(SEGuiCallback: TSEGuiCallback; AHostPtr: Pointer);
begin
 inherited;
end;

procedure TSEIntToListGui.GuiPinValueChange(Pin: TSeGuiPin);
var
  Mode      : Integer;
  InValue   : Integer;
  OutValue  : Integer;
  it        : TItEnumList;
  ExtraData : TSeSdkString2;
begin
 inherited;
 Mode := getPin(pinMode).getValueInt;

 case Pin.GetIndex of
  pinIn,
  pinMode:
   begin
    InValue := getPin(pinIn).getValueInt;
    if (Mode = 0) then // calc what value this index maps to
     begin
      ExtraData := getPin(pinOut).getExtraData;
      it := TItEnumList.Create(ExtraData);
      it.First;
      while (not it.IsDone) and (it.CurrentItem^.Index <> InValue) do it.Next;
      if not it.IsDone
       then getPin(pinOut).setValueInt(it.CurrentItem^.value);
     end else getPin(pinOut).setValueInt(InValue);
   end;
  pinOut:
   begin
    OutValue := getPin(pinOut).getValueInt;
    if Mode = 0 then // calc what index this value maps to
     begin
      ExtraData := getPin(pinOut).getExtraData;
      it := TItEnumList.Create(ExtraData);
      it.First;
      while (not it.IsDone) and (it.CurrentItem^.Index <> OutValue) do it.Next;
      if not it.IsDone
       then getPin(pinIn).setValueInt(it.CurrentItem^.value);
     end else getPin(pinIn).setValueInt(OutValue);
   end;
 end;
end;

end.
