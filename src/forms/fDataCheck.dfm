object F_DataCheck: TF_DataCheck
  Left = 0
  Top = 0
  Caption = 'Validnost dat'
  ClientHeight = 300
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object LV_Errors: TListView
    Left = 0
    Top = 57
    Width = 635
    Height = 243
    Align = alClient
    Columns = <
      item
        Caption = 'ID'
      end
      item
        Caption = 'Stav'
        Width = 500
      end>
    DoubleBuffered = False
    GridLines = True
    ReadOnly = True
    RowSelect = True
    ParentDoubleBuffered = False
    TabOrder = 0
    ViewStyle = vsReport
    OnCustomDrawItem = LV_ErrorsCustomDrawItem
  end
  object GB_Prop: TGroupBox
    Left = 0
    Top = 0
    Width = 635
    Height = 57
    Align = alTop
    TabOrder = 1
    object B_Update: TButton
      Left = 16
      Top = 16
      Width = 105
      Height = 25
      Caption = 'Aktualizovat'
      Default = True
      TabOrder = 0
      OnClick = B_UpdateClick
    end
  end
end
