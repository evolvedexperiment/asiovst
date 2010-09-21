object FmPixelMapDialog: TFmPixelMapDialog
  Left = 299
  Top = 55
  Caption = 'Manage Pixelmap'
  ClientHeight = 270
  ClientWidth = 275
  Color = clBtnFace
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
    275
    270)
  PixelsPerInch = 96
  TextHeight = 13
  object PaintBox: TPaintBox
    Left = 8
    Top = 47
    Width = 259
    Height = 215
    Anchors = [akLeft, akTop, akRight, akBottom]
    OnPaint = PaintBoxPaint
  end
  object PnToolbar: TPanel
    Left = 0
    Top = 0
    Width = 275
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object BtOpen: TButton
      Left = 8
      Top = 9
      Width = 75
      Height = 25
      Caption = 'Open...'
      TabOrder = 0
      OnClick = BtOpenClick
    end
    object BtSave: TButton
      Left = 89
      Top = 9
      Width = 75
      Height = 25
      Caption = 'Save...'
      TabOrder = 1
      OnClick = BtSaveClick
    end
  end
  object OpenDialog: TOpenDialog
    DefaultExt = '.bmp'
    Filter = 'Bitmap (*.bmp)|*.bmp|Portable Network Graphics (*.png)|*.png'
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Title = 'Open Pixelmap'
    Left = 24
    Top = 56
  end
  object SaveDialog: TSaveDialog
    DefaultExt = '.bmp'
    Filter = 'Bitmap (*.bmp)|*.bmp|Portable Network Graphics (*.png)|*.png'
    Title = 'Save Pixelmap'
    Left = 88
    Top = 56
  end
end
