object F_Offset: TF_Offset
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  Caption = 'Posun'
  ClientHeight = 130
  ClientWidth = 219
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poOwnerFormCenter
  TextHeight = 15
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 205
    Height = 15
    Caption = 'Relativn'#237' posun importovan'#233'ho reli'#233'fu:'
  end
  object Label2: TLabel
    Left = 8
    Top = 29
    Width = 10
    Height = 15
    Caption = 'X:'
  end
  object Label3: TLabel
    Left = 8
    Top = 59
    Width = 10
    Height = 15
    Caption = 'Y:'
  end
  object SE_X: TSpinEdit
    Left = 92
    Top = 29
    Width = 121
    Height = 24
    MaxValue = 0
    MinValue = 0
    TabOrder = 0
    Value = 0
  end
  object SE_Y: TSpinEdit
    Left = 92
    Top = 59
    Width = 121
    Height = 24
    MaxValue = 0
    MinValue = 0
    TabOrder = 1
    Value = 0
  end
  object B_OK: TButton
    Left = 138
    Top = 96
    Width = 75
    Height = 25
    Caption = 'Pokra'#269'ovat'
    Default = True
    TabOrder = 2
    OnClick = B_OKClick
  end
  object B_Cancel: TButton
    Left = 57
    Top = 96
    Width = 75
    Height = 25
    Caption = 'Storno'
    TabOrder = 3
    OnClick = B_CancelClick
  end
end
