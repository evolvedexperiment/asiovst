unit SBmain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, DAV_GuiBaseControl, DAV_GuiSelectBox;

type
  TFmSelectBoxTest = class(TForm)
    LbAntiAlias: TLabel;
    LbArrowWidth: TLabel;
    LbLineWidth: TLabel;
    LbRadius: TLabel;
    SbAntiAlias: TGuiSelectBox;
    SbArrowWidth: TGuiSelectBox;
    SbCornerRadius: TGuiSelectBox;
    SbLineWidth: TGuiSelectBox;
    procedure FormShow(Sender: TObject);
    procedure SbAntiAliasChange(Sender: TObject);
    procedure SbLineWidthChange(Sender: TObject);
    procedure SbCornerRadiusChange(Sender: TObject);
    procedure SbArrowWidthChange(Sender: TObject);
  end;

var
  FmSelectBoxTest: TFmSelectBoxTest;

implementation

{$R *.dfm}

procedure TFmSelectBoxTest.FormShow(Sender: TObject);
begin
 SbAntiAlias.ItemIndex := Integer(SbAntiAlias.AntiAlias);
 SBCornerRadius.ItemIndex := SbAntiAlias.Radius - 1;
 SBLineWidth.ItemIndex := SbAntiAlias.LineWidth - 1;
 SbArrowWidth.ItemIndex := SbAntiAlias.ArrowWidth - 1;
end;

procedure TFmSelectBoxTest.SbAntiAliasChange(Sender: TObject);
begin
 SbAntiAlias.AntiAlias := TGuiAntiAlias(SbAntiAlias.ItemIndex);
 SBCornerRadius.AntiAlias := SbAntiAlias.AntiAlias;
 SBLineWidth.AntiAlias := SbAntiAlias.AntiAlias;
 SbArrowWidth.AntiAlias := SbAntiAlias.AntiAlias;
end;

procedure TFmSelectBoxTest.SbArrowWidthChange(Sender: TObject);
begin
 SbAntiAlias.ArrowWidth := SbArrowWidth.ItemIndex + 1;
 SBCornerRadius.ArrowWidth := SbAntiAlias.ArrowWidth;
 SBLineWidth.ArrowWidth := SbAntiAlias.ArrowWidth;
 SbArrowWidth.ArrowWidth := SbAntiAlias.ArrowWidth;
end;

procedure TFmSelectBoxTest.SbCornerRadiusChange(Sender: TObject);
begin
 SbAntiAlias.Radius := SBCornerRadius.ItemIndex + 1;
 SBCornerRadius.Radius := SbAntiAlias.Radius;
 SBLineWidth.Radius := SbAntiAlias.Radius;
 SbArrowWidth.Radius := SbAntiAlias.Radius;
end;

procedure TFmSelectBoxTest.SbLineWidthChange(Sender: TObject);
begin
 SbAntiAlias.LineWidth := SBLineWidth.ItemIndex + 1;
 SBCornerRadius.LineWidth := SbAntiAlias.LineWidth;
 SBLineWidth.LineWidth := SbAntiAlias.LineWidth;
 SbArrowWidth.LineWidth := SbAntiAlias.LineWidth;
end;

end.
