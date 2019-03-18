object VSTPluginWizardForm: TVSTPluginWizardForm
  Left = 311
  Top = 246
  HorzScrollBar.Visible = False
  VertScrollBar.Visible = False
  BorderStyle = bsDialog
  Caption = 'VST Plugin Project Wizard'
  ClientHeight = 345
  ClientWidth = 452
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object LbEffectName: TLabel
    Left = 190
    Top = 264
    Width = 82
    Height = 13
    Caption = 'Plugin unit name:'
  end
  object BlSeparator: TBevel
    Left = 0
    Top = 42
    Width = 452
    Height = 2
    Align = alTop
    Shape = bsBottomLine
  end
  object PnHeader: TPanel
    Left = 0
    Top = 0
    Width = 452
    Height = 42
    Align = alTop
    BevelOuter = bvNone
    Color = clWhite
    TabOrder = 0
    object ImageVST: TImage
      Left = 5
      Top = 5
      Width = 32
      Height = 32
      AutoSize = True
    end
    object LbHeading: TLabel
      Left = 45
      Top = 5
      Width = 170
      Height = 13
      Caption = 'New VST Plugin Project Wizard'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object LbSubHeading: TLabel
      Left = 45
      Top = 21
      Width = 235
      Height = 13
      Caption = 'Create a new project for developing a VST plugin'
    end
  end
  object PnControl: TPanel
    Left = 0
    Top = 306
    Width = 452
    Height = 39
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    DesignSize = (
      452
      39)
    object Bevel2: TBevel
      Left = 0
      Top = 0
      Width = 452
      Height = 2
      Align = alTop
      Shape = bsBottomLine
    end
    object btnFinish: TButton
      Left = 372
      Top = 9
      Width = 75
      Height = 25
      Hint = 'This is Finish, but not the End'
      Anchors = [akTop, akRight]
      Caption = '&Finish'
      Default = True
      ModalResult = 1
      TabOrder = 0
      Visible = False
    end
    object btnCancel: TButton
      Left = 5
      Top = 9
      Width = 75
      Height = 25
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 2
    end
    object btnNext: TButton
      Left = 372
      Top = 9
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '&Next'
      Default = True
      TabOrder = 1
      OnClick = btnNextClick
    end
    object btnPrev: TButton
      Left = 292
      Top = 9
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '&Back'
      Enabled = False
      TabOrder = 3
      OnClick = btnPrevClick
    end
  end
  object PageControl: TPageControl
    Left = 0
    Top = 44
    Width = 452
    Height = 262
    ActivePage = TSDestination
    Align = alClient
    Style = tsFlatButtons
    TabOrder = 1
    object TSWelcome: TTabSheet
      Caption = 'Welcome'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object LbWelcomeTitle: TLabel
        Left = 24
        Top = 13
        Width = 236
        Height = 13
        Caption = 'Welcome to the VST Plugin Project Wizard'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object LbWelcomeInstructions4: TLabel
        Left = 24
        Top = 136
        Width = 217
        Height = 13
        Caption = 'Click "Next" to start creating your VST plugin.'
      end
      object LbWelcomeInstructions3: TLabel
        Left = 24
        Top = 104
        Width = 387
        Height = 26
        Caption = 
          'Once compiled, your VST plugin can be loaded by a suitable VST H' +
          'ost application such as Steinberg Cubase.'
        WordWrap = True
      end
      object LbWelcomeInstructions2: TLabel
        Left = 24
        Top = 72
        Width = 386
        Height = 26
        Caption = 
          'A VST plugin is a .DLL project that contains a TVSTModule descen' +
          'dant class (the plugin code istelf) and, optionally, a GUI edito' +
          'r form.'
        WordWrap = True
      end
      object LbWelcomeInstructions1: TLabel
        Left = 24
        Top = 40
        Width = 362
        Height = 26
        Caption = 
          'This wizard will guide you through the process of creating a new' +
          ' VST plugin project.'
        WordWrap = True
      end
    end
    object TSDestination: TTabSheet
      Caption = 'Dest'
      ImageIndex = 4
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object LbDestinationTitle: TLabel
        Left = 24
        Top = 13
        Width = 254
        Height = 13
        Caption = 'Select a Destination Folder and Project Name'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object LBDesinationSelect: TLabel
        Left = 24
        Top = 40
        Width = 310
        Height = 13
        Caption = 
          'Select the folder where you would like the project to be created' +
          ':'
      end
      object LBProjectName: TLabel
        Left = 24
        Top = 93
        Width = 141
        Height = 13
        Caption = 'Enter a name for the project:'
      end
      object LbDpr: TLabel
        Left = 229
        Top = 115
        Width = 20
        Height = 13
        Caption = '.dpr'
      end
      object edtProjectPath: TEdit
        Left = 24
        Top = 59
        Width = 321
        Height = 21
        TabOrder = 0
      end
      object edtProjectName: TEdit
        Left = 24
        Top = 112
        Width = 201
        Height = 21
        TabOrder = 2
      end
      object btnBrowse: TButton
        Left = 351
        Top = 59
        Width = 65
        Height = 21
        Caption = 'Browse'
        TabOrder = 1
        OnClick = btnBrowseClick
      end
      object chkSaveWhenFinished: TCheckBox
        Left = 24
        Top = 152
        Width = 113
        Height = 17
        Caption = 'Save when finished'
        Checked = True
        State = cbChecked
        TabOrder = 3
        Visible = False
      end
    end
    object TSPluginType: TTabSheet
      Caption = 'Plugin Type'
      ImageIndex = 2
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object LbPluginType: TLabel
        Left = 24
        Top = 13
        Width = 65
        Height = 13
        Caption = 'Plugin Type'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object LbSelectVSTTypeInstruction: TLabel
        Left = 24
        Top = 40
        Width = 259
        Height = 13
        Caption = 'Select the type of VST plugin you would like to create.'
        WordWrap = True
      end
      object optPluginTypeSynth: TRadioButton
        Left = 24
        Top = 96
        Width = 216
        Height = 17
        Caption = 'Synth (a VST Instrument, or "VSTi")'
        TabOrder = 1
      end
      object optPluginTypeEffect: TRadioButton
        Left = 24
        Top = 73
        Width = 217
        Height = 17
        Caption = 'Audio effect (such as a delay or reverb)'
        Checked = True
        TabOrder = 0
        TabStop = True
      end
    end
    object TSModule: TTabSheet
      Caption = 'Module'
      ImageIndex = 5
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object LbModuleTitle: TLabel
        Left = 24
        Top = 13
        Width = 167
        Height = 13
        Caption = 'Add a VSTModule Descendant'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object LbModuleInstructions: TLabel
        Left = 24
        Top = 40
        Width = 362
        Height = 26
        Caption = 
          'Each VST plugin needs to contain a VSTModule descendant class. T' +
          'his class provides the audio/MIDI processing code for your VST p' +
          'lugin.'
        WordWrap = True
      end
      object LbModuleName: TLabel
        Left = 24
        Top = 85
        Width = 284
        Height = 13
        Caption = 'Please enter a name for your VSTModule descendant class:'
      end
      object LbModuleUnit: TLabel
        Left = 24
        Top = 139
        Width = 257
        Height = 13
        Caption = 'Please enter a name for the unit containing this class:'
      end
      object LbPas: TLabel
        Left = 303
        Top = 161
        Width = 21
        Height = 13
        Caption = '.pas'
      end
      object edtPluginFormName: TEdit
        Left = 24
        Top = 104
        Width = 321
        Height = 21
        TabOrder = 0
      end
      object edtPluginUnitName: TEdit
        Left = 24
        Top = 158
        Width = 273
        Height = 21
        TabOrder = 1
      end
    end
    object TSEditor: TTabSheet
      Caption = 'Editor'
      ImageIndex = 3
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object LbGUIFormTitle: TLabel
        Left = 24
        Top = 13
        Width = 124
        Height = 13
        Caption = 'Add a GUI Editor Form'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object LbGUIFormInstructions: TLabel
        Left = 24
        Top = 40
        Width = 382
        Height = 26
        Caption = 
          'A VST plugin may optionally include a GUI editor form to enable ' +
          'the end-user to manipulate the parameters of the plugin.'
        WordWrap = True
      end
      object pnlEditorDetails: TPanel
        Left = 0
        Top = 106
        Width = 433
        Height = 105
        BevelOuter = bvNone
        TabOrder = 1
        object LbPasDfm: TLabel
          Left = 302
          Top = 81
          Width = 47
          Height = 13
          Caption = '.pas/.dfm'
        end
        object LbGUIFormUnit: TLabel
          Left = 24
          Top = 58
          Width = 255
          Height = 13
          Caption = 'Please enter a name for the unit containing the form:'
        end
        object lblEditorFormName: TLabel
          Left = 24
          Top = 5
          Width = 194
          Height = 13
          Caption = 'Please enter a name for the editor form:'
        end
        object edtEditorUnitName: TEdit
          Left = 24
          Top = 77
          Width = 273
          Height = 21
          TabOrder = 1
        end
        object edtEditorFormName: TEdit
          Left = 24
          Top = 24
          Width = 321
          Height = 21
          TabOrder = 0
        end
      end
      object chkUseEditor: TCheckBox
        Left = 24
        Top = 83
        Width = 209
        Height = 17
        Caption = 'Include a GUI editor form in the project'
        Checked = True
        State = cbChecked
        TabOrder = 0
        OnClick = chkUseEditorClick
      end
    end
    object TSNames: TTabSheet
      Caption = 'Names'
      ImageIndex = 6
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object LbNameTitle: TLabel
        Left = 24
        Top = 13
        Width = 108
        Height = 13
        Caption = 'Naming Your Plugin'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object LbNameInstructions: TLabel
        Left = 24
        Top = 40
        Width = 393
        Height = 26
        Caption = 
          'Choose a name for your VST Plugin. Most host applications will g' +
          'enerally make this name visible to the end-user.'
        WordWrap = True
      end
      object LbVSTPluginName: TLabel
        Left = 24
        Top = 79
        Width = 190
        Height = 13
        Caption = 'Please enter a name for the VST plugin:'
      end
      object LbProductName: TLabel
        Left = 24
        Top = 127
        Width = 315
        Height = 13
        Caption = 
          'Please enter a name for the overall product your plugin is part ' +
          'of:'
      end
      object LBCompanyName: TLabel
        Left = 24
        Top = 175
        Width = 164
        Height = 13
        Caption = 'Please enter your company name:'
      end
      object edtEffectName: TEdit
        Left = 24
        Top = 98
        Width = 321
        Height = 21
        TabOrder = 0
      end
      object edtProductName: TEdit
        Left = 24
        Top = 146
        Width = 321
        Height = 21
        TabOrder = 1
      end
      object edtVendorName: TEdit
        Left = 24
        Top = 194
        Width = 321
        Height = 21
        TabOrder = 2
      end
    end
    object TSVersionID: TTabSheet
      Caption = 'VersionAndID'
      ImageIndex = 7
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object LbVersionID: TLabel
        Left = 24
        Top = 13
        Width = 119
        Height = 13
        Caption = 'Plugin Version and ID'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object LbVersionIDInstructions: TLabel
        Left = 24
        Top = 40
        Width = 350
        Height = 26
        Caption = 
          'You need to provide a version number for your plugin, and also a' +
          ' unique 4-character ID.'
        WordWrap = True
      end
      object LbMajorVersion: TLabel
        Left = 24
        Top = 81
        Width = 69
        Height = 13
        Caption = 'Major version:'
      end
      object LbUniqueID: TLabel
        Left = 24
        Top = 137
        Width = 51
        Height = 13
        Caption = 'Unique ID:'
      end
      object LbMinorVersion: TLabel
        Left = 104
        Top = 81
        Width = 68
        Height = 13
        Caption = 'Minor version:'
      end
      object LbRelease: TLabel
        Left = 186
        Top = 81
        Width = 42
        Height = 13
        Caption = 'Release:'
      end
      object edtVersionMajor: TEdit
        Left = 24
        Top = 100
        Width = 65
        Height = 21
        MaxLength = 1
        TabOrder = 0
        OnKeyPress = edtVersionMajorKeyPress
      end
      object edtUniqueID: TEdit
        Left = 24
        Top = 156
        Width = 65
        Height = 21
        MaxLength = 4
        TabOrder = 3
      end
      object edtVersionMinor: TEdit
        Left = 104
        Top = 100
        Width = 65
        Height = 21
        MaxLength = 1
        TabOrder = 1
        OnKeyPress = edtVersionMajorKeyPress
      end
      object edtVersionRelease: TEdit
        Left = 186
        Top = 100
        Width = 65
        Height = 21
        MaxLength = 1
        TabOrder = 2
        OnKeyPress = edtVersionMajorKeyPress
      end
    end
    object TSFinish: TTabSheet
      Caption = 'Finish'
      ImageIndex = 2
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object LbDone: TLabel
        Left = 24
        Top = 13
        Width = 32
        Height = 13
        Caption = 'Done!'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object LbDoneInstruction: TLabel
        Left = 24
        Top = 40
        Width = 383
        Height = 26
        Caption = 
          'The Wizard is now ready to create your VST plugin project with t' +
          'he options you have selected.'
        WordWrap = True
      end
      object LbClickFinish: TLabel
        Left = 24
        Top = 95
        Width = 278
        Height = 13
        Caption = 'Click the "Finish" button to create your VST plugin project.'
      end
    end
  end
end
