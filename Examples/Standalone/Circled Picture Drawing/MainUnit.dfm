object FmCircledPictureDialog: TFmCircledPictureDialog
  Left = 299
  Top = 55
  Caption = 'Circled Picture Dialog'
  ClientHeight = 217
  ClientWidth = 424
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  DesignSize = (
    424
    217)
  PixelsPerInch = 96
  TextHeight = 13
  object PaintBoxRef: TPaintBox
    Left = 8
    Top = 8
    Width = 201
    Height = 201
    OnPaint = PaintBoxRefPaint
  end
  object PaintBoxDraw: TPaintBox
    Left = 215
    Top = 8
    Width = 201
    Height = 201
    OnPaint = PaintBoxDrawPaint
  end
  object LbCosts: TLabel
    Left = 352
    Top = 8
    Width = 64
    Height = 13
    Alignment = taRightJustify
    Anchors = [akTop, akRight]
    AutoSize = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    Transparent = True
  end
  object LbTrialCount: TLabel
    Left = 352
    Top = 196
    Width = 64
    Height = 13
    Alignment = taRightJustify
    Anchors = [akRight, akBottom]
    AutoSize = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    Transparent = True
  end
  object MainMenu: TMainMenu
    Left = 24
    Top = 16
    object MiFile: TMenuItem
      Caption = '&File'
      object MiOpenReference: TMenuItem
        Caption = 'Open Reference...'
        OnClick = MiOpenReferenceClick
      end
      object MiSaveResult: TMenuItem
        Caption = 'Save Result...'
        OnClick = MiSaveResultClick
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object MiOpenDrawing: TMenuItem
        Caption = 'Open Drawing...'
        OnClick = MiOpenDrawingClick
      end
      object MiSaveDrawing: TMenuItem
        Caption = 'Save Drawing...'
        Enabled = False
        OnClick = MiSaveDrawingClick
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object MiExit: TMenuItem
        Caption = 'E&xit'
        OnClick = MiExitClick
      end
    end
    object MiEvolve: TMenuItem
      Caption = '&Evolve'
      object MiStart: TMenuItem
        Caption = '&Start'
        OnClick = MiStartClick
      end
      object MiStopContinue: TMenuItem
        Caption = 'St&op'
        Enabled = False
        OnClick = MiStopContinueClick
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object MiNext: TMenuItem
        Caption = '&Next'
        OnClick = MiNextClick
      end
    end
  end
  object OpenDialog: TOpenDialog
    DefaultExt = '.bmp'
    Filter = 'Bitmap (*.bmp)|*.bmp|Portable Network Graphics (*.png)|*.png'
    Left = 112
    Top = 16
  end
  object SaveDialog: TSaveDialog
    DefaultExt = '.bmp'
    Filter = 'Bitmap (*.bmp)|*.bmp|Portable Network Graphics (*.png)|*.png'
    Left = 200
    Top = 16
  end
  object OpenDialogCircles: TOpenDialog
    DefaultExt = '.bmp'
    Filter = 'Circles (*.circles)|*.circles'
    Left = 112
    Top = 72
  end
  object SaveDialogCircles: TSaveDialog
    DefaultExt = '.bmp'
    Filter = 'Circles (*.circles)|*.circles'
    Left = 200
    Top = 72
  end
end
