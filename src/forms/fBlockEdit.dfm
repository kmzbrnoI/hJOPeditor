object F_BlockEdit: TF_BlockEdit
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsToolWindow
  Caption = 'Editace bloku [blok]'
  ClientHeight = 633
  ClientWidth = 193
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyPress = FormKeyPress
  OnShow = FormShow
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 84
    Height = 13
    Caption = 'Blok technologie :'
  end
  object Label3: TLabel
    Left = 8
    Top = 458
    Width = 63
    Height = 13
    Caption = 'Oblast '#345#237'zen'#237':'
  end
  object GB_Usek: TGroupBox
    Left = 8
    Top = 504
    Width = 185
    Height = 102
    TabOrder = 5
    object Label2: TLabel
      Left = 13
      Top = 12
      Width = 68
      Height = 13
      Caption = 'N'#225'zev koleje :'
    end
    object E_KPopisek: TEdit
      Left = 87
      Top = 12
      Width = 86
      Height = 21
      MaxLength = 4
      TabOrder = 0
      OnChange = E_KPopisekChange
      OnKeyPress = E_KPopisekKeyPress
    end
  end
  object GB_Vyhybka: TGroupBox
    Left = 8
    Top = 504
    Width = 185
    Height = 103
    TabOrder = 6
    object Label4: TLabel
      Left = 15
      Top = 12
      Width = 58
      Height = 13
      Caption = 'Poloha plus:'
    end
    object CB_VyhPlus: TComboBox
      Left = 12
      Top = 31
      Width = 157
      Height = 21
      Style = csDropDownList
      TabOrder = 0
      OnChange = CB_VyhPlusChange
      Items.Strings = (
        'vodorovn'#283
        #353'ikmo')
    end
  end
  object GB_Uvazka: TGroupBox
    Left = 8
    Top = 504
    Width = 185
    Height = 102
    TabOrder = 7
    object Label5: TLabel
      Left = 12
      Top = 12
      Width = 69
      Height = 13
      Caption = 'Z'#225'kladn'#237' sm'#283'r:'
    end
    object Image1: TImage
      Left = 54
      Top = 31
      Width = 26
      Height = 18
      Picture.Data = {
        07544269746D617076020000424D760200000000000036000000280000001000
        00000C000000010018000000000040020000C21E0000C21E0000000000000000
        0000C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3
        C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3
        C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3
        C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3
        000000000000000000C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3
        C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3000000000000000000C3C3C3C3
        C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3
        C3C3C3C3C3C3000000000000000000C3C3C30000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        000000000000000000000000000000000000C3C3C3C3C3C3C3C3C3C3C3C3C3C3
        C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3000000000000000000C3
        C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3
        C3C3C3000000000000000000C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3
        C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3000000000000000000C3C3C3C3C3C3C3
        C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3
        C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3
        C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3
        C3C3}
    end
    object Image2: TImage
      Left = 95
      Top = 31
      Width = 25
      Height = 18
      Picture.Data = {
        07544269746D617076020000424D760200000000000036000000280000001000
        00000C000000010018000000000040020000C21E0000C21E0000000000000000
        0000C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3
        C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3
        C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3
        C3C3C3C3C3C3C3C3C3C3C3000000000000000000C3C3C3C3C3C3C3C3C3C3C3C3
        C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C30000000000000000
        00C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3
        C3C3C3C3C3000000000000000000C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3
        C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C30000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        000000000000000000000000000000000000C3C3C3000000000000000000C3C3
        C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3
        C3C3C3C3C3C3C3C3000000000000000000C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3
        C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C30000000000
        00000000C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3
        C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3
        C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3
        C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3
        C3C3}
    end
    object RB_UvazkaSmer1: TRadioButton
      Left = 55
      Top = 47
      Width = 17
      Height = 17
      TabOrder = 0
      OnClick = RB_UvazkaSmer2Click
    end
    object RB_UvazkaSmer2: TRadioButton
      Tag = 1
      Left = 95
      Top = 47
      Width = 17
      Height = 17
      TabOrder = 1
      OnClick = RB_UvazkaSmer2Click
    end
  end
  object GB_UvazkaSpr: TGroupBox
    Left = 8
    Top = 501
    Width = 185
    Height = 105
    TabOrder = 8
    object Label6: TLabel
      Left = 16
      Top = 16
      Width = 101
      Height = 13
      Caption = 'Sm'#283'r pln'#283'n'#237' souprav:'
    end
    object Label7: TLabel
      Left = 16
      Top = 72
      Width = 73
      Height = 13
      Caption = 'Po'#269'et souprav:'
    end
    object CB_UvazkaSpr_VertDir: TComboBox
      Left = 16
      Top = 35
      Width = 145
      Height = 21
      Style = csDropDownList
      TabOrder = 0
      OnChange = CB_UvazkaSpr_VertDirChange
      Items.Strings = (
        'odspoda nahoru'
        'odshora dol'#367)
    end
    object SE_UvazkaSpr_Cnt: TSpinEdit
      Left = 95
      Top = 73
      Width = 66
      Height = 22
      MaxValue = 5
      MinValue = 1
      TabOrder = 1
      Value = 1
      OnChange = CB_UvazkaSpr_VertDirChange
    end
  end
  object B_Apply: TButton
    Left = 118
    Top = 613
    Width = 75
    Height = 25
    Caption = 'Pou'#382#237't'
    TabOrder = 2
    OnClick = B_ApplyClick
  end
  object B_Storno: TButton
    Left = 37
    Top = 613
    Width = 75
    Height = 25
    Caption = 'Storno'
    TabOrder = 3
    OnClick = B_StornoClick
  end
  object LB_Blocks: TListBox
    Left = 8
    Top = 54
    Width = 185
    Height = 398
    ItemHeight = 13
    TabOrder = 0
    OnClick = LB_BlocksClick
    OnDblClick = LB_BlocksDblClick
    OnKeyPress = LB_BlocksKeyPress
  end
  object CB_OR: TComboBox
    Left = 8
    Top = 477
    Width = 185
    Height = 21
    Style = csDropDownList
    TabOrder = 1
    OnChange = CB_ORChange
  end
  object E_Blk: TEdit
    Left = 8
    Top = 27
    Width = 185
    Height = 21
    TabOrder = 4
    OnChange = E_BlkChange
    OnKeyDown = E_BlkKeyDown
    OnKeyPress = E_BlkKeyPress
  end
end
