object FmESTP: TFmESTP
  Left = 299
  Top = 51
  Caption = 'Equal Spaced Thick Polyline'
  ClientHeight = 269
  ClientWidth = 411
  Color = clBtnFace
  Constraints.MinHeight = 128
  Constraints.MinWidth = 128
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  DesignSize = (
    411
    269)
  PixelsPerInch = 96
  TextHeight = 13
  object PaintBox: TPaintBox
    Left = 8
    Top = 32
    Width = 395
    Height = 229
    Anchors = [akLeft, akTop, akRight, akBottom]
    PopupMenu = PuScenario
    OnClick = PaintBoxClick
    OnPaint = PaintBoxPaint
  end
  object Label1: TLabel
    Left = 128
    Top = 258
    Width = 31
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Label1'
    Visible = False
  end
  object Label2: TLabel
    Left = 184
    Top = 258
    Width = 31
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Label2'
    Visible = False
  end
  object SlLineWidth: TGuiSlider
    Left = 8
    Top = 8
    Width = 395
    Height = 18
    Anchors = [akLeft, akTop, akRight]
    BorderRadius = 4.000000000000000000
    BorderWidth = 1.500000000000000000
    Caption = 'Linewidth'
    Color = clBtnFace
    CurveMapping = 0.500000000000000000
    DefaultValue = 2.000000000000000000
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Tahoma'
    Font.Style = []
    FontOversampling = fo4x
    FontShadow.Blur = 2.000000000000000000
    FontShadow.Color = clBtnFace
    FontShadow.OffsetX = 0
    FontShadow.OffsetY = 0
    FontShadow.Saturation = 3.000000000000000000
    FontShadow.Visible = True
    Max = 20.000000000000000000
    Min = 1.000000000000000000
    ParentColor = False
    PopupMenu = PuLinePreset
    Value = 2.000000000000000000
    ShowText = True
    SlideColor = 6316128
    OnChange = SlLineWidthChange
    OnDblClick = SlLineWidthDblClick
  end
  object PuLinePreset: TPopupMenu
    Left = 40
    Top = 32
    object MiPositionA: TMenuItem
      Caption = 'Position A (7.076)'
      OnClick = MiPositionAClick
    end
    object MiPositionB: TMenuItem
      Caption = 'Position B (2.0)'
      OnClick = MiPositionBClick
    end
    object MiPositionC: TMenuItem
      Caption = 'Position C (2.99999)'
      OnClick = MiPositionCClick
    end
    object MiPositionD: TMenuItem
      Caption = 'Position D (7.0)'
      OnClick = MiPositionDClick
    end
    object MiPositionE: TMenuItem
      Caption = 'Position E (9.0)'
      OnClick = MiPositionEClick
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object MiAddTinyValue: TMenuItem
      Caption = 'Add Tiny Value'
      OnClick = MiAddTinyValueClick
    end
    object MiSubtractTinyValue: TMenuItem
      Caption = 'Subtract Tiny Value'
      OnClick = MiSubtractTinyValueClick
    end
  end
  object PuScenario: TPopupMenu
    Left = 40
    Top = 80
    object MiScenarioStandard: TMenuItem
      Caption = '&Standard'
      Checked = True
      RadioItem = True
      OnClick = MiScenarioStandardClick
    end
    object MiScenarioPeakLineI: TMenuItem
      Caption = '&Peak Line I'
      RadioItem = True
      OnClick = MiScenarioPeakLineIClick
    end
    object MiScenarioRandom: TMenuItem
      Caption = 'Random'
      RadioItem = True
      OnClick = MiScenarioRandomClick
    end
    object MiScenarioSmallIncrease: TMenuItem
      Caption = 'Small Increase'
      RadioItem = True
      OnClick = MiScenarioSmallIncreaseClick
    end
    object MiScenarioExceedBorders: TMenuItem
      Caption = 'Exceed Borders'
      RadioItem = True
      OnClick = MiScenarioExceedBordersClick
    end
  end
end
