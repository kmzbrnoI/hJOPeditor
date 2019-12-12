object F_ReliefProperties: TF_ReliefProperties
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsToolWindow
  Caption = 'Zm'#283'nit vlastnosti reli'#233'fu'
  ClientHeight = 113
  ClientWidth = 249
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
  object Label2: TLabel
    Left = 8
    Top = 8
    Width = 63
    Height = 13
    Caption = #352#237#345'ka reli'#233'fu :'
  end
  object Label1: TLabel
    Left = 8
    Top = 40
    Width = 68
    Height = 13
    Caption = 'V'#253#353'ka reli'#233'fu :'
  end
  object B_Storno: TButton
    Left = 85
    Top = 80
    Width = 75
    Height = 25
    Caption = 'Storno'
    TabOrder = 0
    OnClick = B_StornoClick
  end
  object B_Apply: TButton
    Left = 166
    Top = 80
    Width = 75
    Height = 25
    Caption = 'Pou'#382#237't'
    Default = True
    TabOrder = 1
    OnClick = B_ApplyClick
  end
  object SE_Width: TSpinEdit
    Left = 120
    Top = 8
    Width = 121
    Height = 22
    MaxValue = 255
    MinValue = 5
    TabOrder = 2
    Value = 0
  end
  object SE_Height: TSpinEdit
    Left = 120
    Top = 40
    Width = 121
    Height = 22
    MaxValue = 255
    MinValue = 5
    TabOrder = 3
    Value = 0
  end
end
