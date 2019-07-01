object F_Popisek: TF_Popisek
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  Caption = '[operace]'
  ClientHeight = 336
  ClientWidth = 241
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 29
    Height = 13
    Caption = 'Text :'
  end
  object B_Apply: TButton
    Left = 158
    Top = 303
    Width = 75
    Height = 25
    Caption = 'Pou'#382#237't'
    TabOrder = 3
    OnClick = B_ApplyClick
  end
  object B_Storno: TButton
    Left = 77
    Top = 303
    Width = 75
    Height = 25
    Caption = 'Storno'
    TabOrder = 4
    OnClick = B_StornoClick
  end
  object E_Text: TEdit
    Left = 80
    Top = 8
    Width = 153
    Height = 21
    MaxLength = 128
    TabOrder = 0
    OnKeyPress = E_TextKeyPress
  end
  object GB_Color: TGroupBox
    Left = 8
    Top = 35
    Width = 225
    Height = 206
    Caption = ' Barva '
    TabOrder = 1
    object S_Col0: TShape
      Left = 43
      Top = 17
      Width = 174
      Height = 17
      Brush.Color = clFuchsia
    end
    object S_Col1: TShape
      Left = 43
      Top = 40
      Width = 174
      Height = 17
      Brush.Color = 10526880
    end
    object S_Col2: TShape
      Left = 43
      Top = 63
      Width = 174
      Height = 17
      Brush.Color = clRed
    end
    object S_Col3: TShape
      Left = 43
      Top = 86
      Width = 174
      Height = 17
      Brush.Color = clLime
    end
    object S_Col4: TShape
      Left = 43
      Top = 109
      Width = 174
      Height = 17
    end
    object S_Col5: TShape
      Left = 43
      Top = 132
      Width = 174
      Height = 17
      Brush.Color = clAqua
    end
    object S_Col6: TShape
      Left = 43
      Top = 155
      Width = 174
      Height = 17
      Brush.Color = clBlue
    end
    object S_Col7: TShape
      Left = 43
      Top = 178
      Width = 174
      Height = 17
      Brush.Color = clYellow
    end
    object RB_Col0: TRadioButton
      Left = 16
      Top = 17
      Width = 13
      Height = 17
      Color = clFuchsia
      ParentColor = False
      TabOrder = 0
      OnClick = RB_Col0Click
    end
    object RB_Col1: TRadioButton
      Tag = 1
      Left = 16
      Top = 40
      Width = 13
      Height = 17
      TabOrder = 1
      OnClick = RB_Col0Click
    end
    object RB_Col2: TRadioButton
      Tag = 2
      Left = 16
      Top = 63
      Width = 13
      Height = 17
      TabOrder = 2
      OnClick = RB_Col0Click
    end
    object RB_Col3: TRadioButton
      Tag = 3
      Left = 16
      Top = 86
      Width = 13
      Height = 17
      TabOrder = 3
      OnClick = RB_Col0Click
    end
    object RB_Col4: TRadioButton
      Tag = 4
      Left = 16
      Top = 109
      Width = 13
      Height = 17
      TabOrder = 4
      OnClick = RB_Col0Click
    end
    object RB_Col5: TRadioButton
      Tag = 5
      Left = 16
      Top = 132
      Width = 13
      Height = 17
      TabOrder = 5
      OnClick = RB_Col0Click
    end
    object RB_Col6: TRadioButton
      Tag = 6
      Left = 16
      Top = 155
      Width = 13
      Height = 17
      TabOrder = 6
      OnClick = RB_Col0Click
    end
    object RB_Col7: TRadioButton
      Tag = 7
      Left = 16
      Top = 178
      Width = 13
      Height = 17
      TabOrder = 7
      OnClick = RB_Col0Click
    end
  end
  object CHB_Blok_Popisek: TCheckBox
    Left = 8
    Top = 248
    Width = 81
    Height = 17
    Caption = 'Popisek bloku'
    TabOrder = 2
  end
  object StaticText1: TStaticText
    AlignWithMargins = True
    Left = 8
    Top = 271
    Width = 225
    Height = 26
    AutoSize = False
    Caption = 
      'Popisky bloku se zobrazuj'#237' podtr'#382'en'#283', v panelu pak nebudou podtr' +
      #382'en'#233'.'
    TabOrder = 5
  end
end
