object F_ReliefOptions: TF_ReliefOptions
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsToolWindow
  Caption = 'Nastaven'#237
  ClientHeight = 265
  ClientWidth = 313
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poScreenCenter
  TextHeight = 13
  object Label6: TLabel
    Left = 8
    Top = 167
    Width = 74
    Height = 13
    Caption = 'Soubor s bloky:'
  end
  object GB_Colors: TGroupBox
    Left = 8
    Top = 0
    Width = 297
    Height = 161
    Caption = ' Barvy '
    TabOrder = 0
    object Label1: TLabel
      Left = 8
      Top = 16
      Width = 37
      Height = 13
      Caption = 'M'#345#237#382'ka :'
    end
    object Label2: TLabel
      Left = 8
      Top = 44
      Width = 38
      Height = 13
      Caption = 'Pozad'#237' :'
    end
    object Label3: TLabel
      Left = 8
      Top = 72
      Width = 38
      Height = 13
      Caption = 'Kurzor :'
    end
    object Label4: TLabel
      Left = 8
      Top = 100
      Width = 99
      Height = 13
      Caption = 'Kurzor - na objektu :'
    end
    object Label5: TLabel
      Left = 8
      Top = 128
      Width = 126
      Height = 13
      Caption = 'Kurzor - prob'#237'h'#225' operace :'
    end
    object CB_Mrizka: TColorBox
      Left = 144
      Top = 16
      Width = 145
      Height = 22
      Style = [cbStandardColors]
      TabOrder = 0
    end
    object CB_Kurzor: TColorBox
      Left = 144
      Top = 72
      Width = 145
      Height = 22
      Style = [cbStandardColors]
      TabOrder = 1
    end
    object CB_KurzorOperation: TColorBox
      Left = 144
      Top = 128
      Width = 145
      Height = 22
      Style = [cbStandardColors]
      TabOrder = 2
    end
    object CB_Pozadi: TColorBox
      Left = 144
      Top = 44
      Width = 145
      Height = 22
      Style = [cbStandardColors]
      TabOrder = 3
    end
    object CB_KurzorOnObject: TColorBox
      Left = 144
      Top = 100
      Width = 145
      Height = 22
      Style = [cbStandardColors]
      TabOrder = 4
    end
  end
  object B_Apply: TButton
    Left = 230
    Top = 232
    Width = 75
    Height = 25
    Caption = 'Pou'#382#237't'
    Default = True
    TabOrder = 1
    OnClick = B_ApplyClick
  end
  object B_Storno: TButton
    Left = 149
    Top = 232
    Width = 75
    Height = 25
    Caption = 'Storno'
    TabOrder = 2
    OnClick = B_StornoClick
  end
  object E_BlocksFileName: TEdit
    Left = 8
    Top = 186
    Width = 216
    Height = 21
    TabOrder = 3
  end
  object B_Prochazet: TButton
    Left = 230
    Top = 184
    Width = 75
    Height = 25
    Caption = 'Proch'#225'zet'
    TabOrder = 4
    OnClick = B_ProchazetClick
  end
  object CHB_RelativePath: TCheckBox
    Left = 8
    Top = 213
    Width = 97
    Height = 17
    Caption = 'Relativn'#237' cesty'
    Checked = True
    State = cbChecked
    TabOrder = 5
    OnClick = CHB_RelativePathClick
  end
  object OD_Open: TOpenDialog
    Filter = 'Soubory bloku (*.ini)|*.ini'
    Title = 'Vyberte soubor s bloky'
    Left = 16
    Top = 216
  end
end
