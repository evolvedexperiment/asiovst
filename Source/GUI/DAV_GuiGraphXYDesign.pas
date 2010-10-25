unit DAV_GuiGraphXYDesign;

interface

{$I ..\DAV_Compiler.inc}

uses
  {$IFDEF FPC} LCLIntf, LazIDEIntf, PropEdits,{$ELSE} {$IFDEF DELPHI6_UP}
  DesignIntf, DesignEditors, {$ELSE} DsgnIntf, {$ENDIF} {$ENDIF}
  Classes, TypInfo, DAV_Classes, DAV_GuiDesign, DAV_GuiGraphXY;

type
  TSeriesClassProperty = class(TCustomClassProperty)
  protected
    class function GetClassList: TClassList; override;
    function GetObject: TObject; override;
    procedure SetClassName(const CustomClass: string); override;
  end;

implementation

{ TSeriesClassProperty }

class function TSeriesClassProperty.GetClassList: TClassList;
begin
  Result := SeriesClassList;
end;

function TSeriesClassProperty.GetObject: TObject;
begin
  Result := TGuiGraphXYSeriesCollectionItem(GetComponent(0)).Series;
end;

procedure TSeriesClassProperty.SetClassName(const CustomClass: string);
begin
  TGuiGraphXYSeriesCollectionItem(GetComponent(0)).SeriesClassName := CustomClass;
end;

end.
