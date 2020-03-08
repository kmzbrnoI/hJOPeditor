object F_ImportLog: TF_ImportLog
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  Caption = 'Informace o importu'
  ClientHeight = 641
  ClientWidth = 645
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 93
    Height = 13
    Caption = 'Log importu reli'#233'fu:'
  end
  object M_Log: TMemo
    Left = 8
    Top = 27
    Width = 629
    Height = 574
    Lines.Strings = (
      'M_Log')
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object B_OK: TButton
    Left = 562
    Top = 607
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 1
    OnClick = B_OKClick
  end
end
