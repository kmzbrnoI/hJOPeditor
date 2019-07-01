unit fBlockEdit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, IniFiles, ReliefSettings, ReliefObjects, ExtCtrls, RPConst,
  StrUtils, Spin, ObjBlok;

type
  TBlok=record
    Nazev:string;
    id:integer;
    typ:Integer;
  end;

  EFileNotFound = class(Exception);

  TBloky=class
   private const
     _MAX_BLK = 1023;
   private
     Bloky:array [0.._MAX_BLK] of TBlok;
     Count:Integer;
     iniFile:TMemIniFile;
     fTriedLoad:Boolean;
   public
    constructor Create();

    procedure LoadData(const FileName:string);

    property triedLoad: boolean read fTriedLoad;
  end;

  TF_BlockEdit = class(TForm)
    B_Apply: TButton;
    B_Storno: TButton;
    Label1: TLabel;
    LB_Blocks: TListBox;
    Label3: TLabel;
    CB_OR: TComboBox;
    E_Blk: TEdit;
    GB_Usek: TGroupBox;
    GB_Vyhybka: TGroupBox;
    CB_VyhPlus: TComboBox;
    Label4: TLabel;
    Label2: TLabel;
    E_KPopisek: TEdit;
    GB_Uvazka: TGroupBox;
    Label5: TLabel;
    GB_UvazkaSpr: TGroupBox;
    Image1: TImage;
    Image2: TImage;
    RB_UvazkaSmer1: TRadioButton;
    RB_UvazkaSmer2: TRadioButton;
    Label6: TLabel;
    CB_UvazkaSpr_VertDir: TComboBox;
    Label7: TLabel;
    SE_UvazkaSpr_Cnt: TSpinEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure B_StornoClick(Sender: TObject);
    procedure B_ApplyClick(Sender: TObject);
    procedure E_KPopisekKeyPress(Sender: TObject; var Key: Char);
    procedure FormShow(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure LB_BlocksKeyPress(Sender: TObject; var Key: Char);
    procedure LB_BlocksDblClick(Sender: TObject);
    procedure LB_BlocksClick(Sender: TObject);
    procedure CB_ORChange(Sender: TObject);
    procedure E_KPopisekChange(Sender: TObject);
    procedure CB_VyhPlusChange(Sender: TObject);
    procedure E_BlkChange(Sender: TObject);
    procedure E_BlkKeyPress(Sender: TObject; var Key: Char);
    procedure E_BlkKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure CB_UvazkaSpr_VertDirChange(Sender: TObject);
    procedure RB_UvazkaSmer2Click(Sender: TObject);
  private
    OpenBlok:TGraphBlok;

    LB_BlocksIndexes:array [0.._MAX_BLK+1] of Integer;     // indexaci pole pro ListBox, zde jsou ulozeny inedxy bloku (nikoliv id!!)

    procedure LoadOR();
    procedure SaveData();

    procedure UpdateLB(text:string);

    procedure OpenFormGlobal();

    function GetTechBlokType(panel_type:TBlkType):Integer;

  public
   Bloky:TBloky;

    procedure OpenForm(Blok:TGraphBlok);
  end;

var
  F_BlockEdit: TF_BlockEdit;

implementation

uses fMain, OblastRizeni, ObjBLokUvazkaSpr, ObjBlokVyhybka, ObjBlokUsek,
     ObjBlokUvazka;

{$R *.dfm}

constructor TBloky.Create();
begin
 inherited;
 Self.fTriedLoad := false;
end;

procedure TBloky.LoadData(const FileName:string);
var i:Integer;
    str:TStrings;
begin
 Self.fTriedLoad := true;

 if (not FileExists(FileName)) then
   raise EFileNotFound.Create('Soubor '+FileName+' neexistuje!');

 str := TStringList.Create();
 Self.iniFile := TMemIniFile.Create(FileName, TEncoding.UTF8);
 try
   Self.iniFile.ReadSections(str);
   Self.Count := str.Count;

   for i := 0 to str.Count-1 do
    begin
     Self.Bloky[i].id    := StrToInt(str[i]);
     Self.Bloky[i].Nazev := Self.iniFile.ReadString(str[i], 'nazev','Blok '+IntToStr(i));
     Self.Bloky[i].typ   := Self.iniFile.ReadInteger(str[i], 'typ', -1);
    end;//for i
 finally
   Self.iniFile.Free;
   str.Free();
 end;
end;//procedure

procedure TF_BlockEdit.B_ApplyClick(Sender: TObject);
begin
 Self.Close;
end;//procedure

procedure TF_BlockEdit.B_StornoClick(Sender: TObject);
begin
 Self.Close;
end;

procedure TF_BlockEdit.CB_ORChange(Sender: TObject);
begin
 Self.OpenBlok.OblRizeni := (Sender as TComboBox).ItemIndex;
 Self.SaveData();
end;

procedure TF_BlockEdit.CB_UvazkaSpr_VertDirChange(Sender: TObject);
begin
 if (Self.OpenBlok.typ = TBlkType.uvazka_spr) then
  begin
   (Self.OpenBlok as TUvazkaSpr).spr_cnt      := Self.SE_UvazkaSpr_Cnt.Value;
   (Self.OpenBlok as TUvazkaSpr).vertical_dir := TUvazkaSprVertDir(Self.CB_UvazkaSpr_VertDir.ItemIndex);
   Self.SaveData();
  end;
end;

procedure TF_BlockEdit.CB_VyhPlusChange(Sender: TObject);
begin
 if (Self.OpenBlok.typ = TBlkType.vyhybka) then
  begin
   (Self.OpenBlok as TVyhybka).PolohaPlus  := (Sender as TComboBox).ItemIndex;
   Self.SaveData();
  end;
end;//procedure

procedure TF_BlockEdit.E_BlkChange(Sender: TObject);
begin
 Self.UpdateLB(Self.E_Blk.Text);
 Self.LB_BlocksClick(Self.LB_Blocks);
end;

procedure TF_BlockEdit.E_BlkKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
 case (Key) of
  VK_UP:begin
    if (Self.LB_Blocks.ItemIndex > 0) then
     begin
      Self.LB_Blocks.ItemIndex := Self.LB_Blocks.ItemIndex - 1;
      Self.LB_BlocksClick(Self.LB_Blocks);
      Key := 0;
     end;
  end;

  VK_DOWN:begin
    if (Self.LB_Blocks.ItemIndex < Self.LB_Blocks.Items.Count) then
     begin
      Self.LB_Blocks.ItemIndex := Self.LB_Blocks.ItemIndex + 1;
      Self.LB_BlocksClick(Self.LB_Blocks);
      Key := 0;
     end;
  end;

 end;
end;

procedure TF_BlockEdit.E_BlkKeyPress(Sender: TObject; var Key: Char);
begin
 case (Key) of
  #13:Self.B_ApplyClick(Self.B_Apply);
  #$1B: begin
    if (Self.E_Blk.Text = '') then
     begin
      Self.Close();
     end else begin
      Self.E_Blk.Text := '';
      Key := #0;
      Self.E_BlkChange(Self.E_KPopisek);
     end;
  end;
 end;//case
end;

procedure TF_BlockEdit.E_KPopisekChange(Sender: TObject);
begin
 if (Self.OpenBlok.typ = TBlkType.usek) then
  begin
   (Self.OpenBlok as TUsek).KpopisekStr := (Sender as TEdit).Text;
   Self.SaveData();
  end;
end;

procedure TF_BlockEdit.E_KPopisekKeyPress(Sender: TObject; var Key: Char);
begin
 if (key = #13) then Self.B_ApplyClick(Self);
 if (key = #27) then Self.Close;

 case Key of
  '0'..'9',#9,#8,'a'..'z','A'..'Z':begin
                 end else begin
                  Key := #0;
                 end;
  end;//case
end;//procedure

procedure TF_BlockEdit.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 Self.SaveData();
end;

procedure TF_BlockEdit.FormCreate(Sender: TObject);
begin
 Self.Bloky := TBloky.Create;
 Self.OpenBlok := nil;
end;//procedure

procedure TF_BlockEdit.FormDestroy(Sender: TObject);
begin
 Self.Bloky.Free;
end;//procedure

procedure TF_BlockEdit.FormKeyPress(Sender: TObject; var Key: Char);
begin
 if (key = #27) then Self.Close;
end;

procedure TF_BlockEdit.FormShow(Sender: TObject);
begin
 Self.E_Blk.Text := '';
 Self.E_BlkChange(Self.E_Blk);
 Self.E_Blk.SetFocus;

 if (Self.OpenBlok = nil) then
  begin
   Self.LB_Blocks.ItemIndex := 0;
   Self.LB_BlocksClick(Self.LB_Blocks);
  end;
end;//procedure

procedure TF_BlockEdit.OpenForm(Blok:TGraphBlok);
begin
 Self.OpenFormGlobal();
 Self.OpenBlok := Blok;
 Self.CB_OR.ItemIndex := Blok.OblRizeni;

 case (Blok.typ) of
  TBlkType.usek:begin
     Self.GB_Usek.Visible := true;
     if ((Blok as TUsek).KPopisek.Count > 0) then
      begin
       Self.Label2.Visible     := true;
       Self.E_KPopisek.Visible := true;
      end else begin
       Self.Label2.Visible     := false;
       Self.E_KPopisek.Visible := false;
      end;//else KPopisek.Count > 0
     Self.E_KPopisek.Text := (Blok as TUsek).KpopisekStr;
     Self.Caption := 'Editace úseku';
  end;

  TBlkType.navestidlo:begin
     Self.Caption := 'Editace návìstidla';
  end;

  TBlkType.vyhybka:begin
     Self.GB_Vyhybka.Visible := true;
     Self.CB_VyhPlus.ItemIndex := (Blok as TVyhybka).PolohaPlus;
     Self.CB_OR.ItemIndex      := (Blok as TVyhybka).OblRizeni;
     Self.Caption := 'Editace výhybky';
  end;

  TBlkType.prejezd:begin
     Self.Caption := 'Editace pøejezdu';
  end;

  TBlkType.text:begin
     Self.Caption := 'Editace popisku';
  end;

  TBlkType.uvazka:begin
     Self.GB_Uvazka.Visible := true;
     case ((Blok as TUvazka).defalt_dir) of
      0: Self.RB_UvazkaSmer1.Checked := true;
      1: Self.RB_UvazkaSmer2.Checked := true;
     end;
     Self.Caption    := 'Editace úvazky';
  end;

  TBlkType.uvazka_spr:begin
     Self.GB_UvazkaSpr.Visible := true;
     Self.CB_UvazkaSpr_VertDir.ItemIndex := Integer((Blok as TUvazkaSpr).vertical_dir);
     Self.SE_UvazkaSpr_Cnt.Value         := (Blok as TUvazkaSpr).spr_cnt;
  end;

  TBlkType.zamek:begin
     Self.Caption := 'Editace zámku';
  end;

  TBlkType.vykol:begin
     Self.Caption := 'Editace výkolejky';
  end;

  TBlkType.rozp:begin
     Self.Caption := 'Editace rozpojovaèe';
  end;

 end;//case Blok.typ

 if (Self.Showing) then
   Self.OnShow(Self)
 else
   Self.Show;
end;//procedur

////////////////////////////////////////////////////////////////////////////////

procedure TF_BlockEdit.LB_BlocksClick(Sender: TObject);
var id:Integer;
begin
 if (((Sender as TListBox).ItemIndex < 0) or (Self.LB_BlocksIndexes[(Sender as TListBox).ItemIndex] < 0)) then
  id := Self.LB_BlocksIndexes[(Sender as TListBox).ItemIndex]
 else
  id := Self.Bloky.Bloky[Self.LB_BlocksIndexes[(Sender as TListBox).ItemIndex]].id;

 if (Self.OpenBlok <> nil) then
  begin
   Self.OpenBlok.Blok := id;
   Self.SaveData();
  end;
end;//procedure

procedure TF_BlockEdit.LB_BlocksDblClick(Sender: TObject);
begin
 Self.B_ApplyClick(Self);
end;//procedure

procedure TF_BlockEdit.LB_BlocksKeyPress(Sender: TObject; var Key: Char);
begin
 if (key = #27) then Self.Close;
 if (key = #13) then Self.B_ApplyClick(Self);
end;

procedure TF_BlockEdit.LoadOR();
var ORs:TORList;
    i:Integer;
begin
 //nacteni OR
 Self.CB_OR.Clear();
 ORs := F_Hlavni.Relief.GetORList();
 for i := 0 to ORs.Cnt-1 do Self.CB_OR.Items.Add(ORs.Data[i]);
end;

procedure TF_BlockEdit.SaveData();
begin
 if (Assigned(F_Hlavni.Relief)) then
   F_Hlavni.Relief.Show(Point(-1,-1));
end;//procedure

procedure TF_BlockEdit.UpdateLB(text:string);
var i, index:Integer;
begin
 Self.LB_Blocks.Clear();

 if (Self.OpenBlok = nil) then Exit();

 Self.LB_Blocks.Items.Add('--- Žádný blok ---');
 Self.LB_Blocks.Items.Add('--- Žádný blok zámìrnì ---');
 Self.LB_BlocksIndexes[0] := -1;
 Self.LB_BlocksIndexes[1] := -2;

 if (Self.OpenBlok.Blok = -2) then
   index := 1
 else if (Self.OpenBlok.Blok = -1) then
   index := 0
 else
   index := -1;

 for i := 0 to Self.Bloky.Count-1 do
  begin
   if ((LeftStr(Self.Bloky.Bloky[i].Nazev, Length(text)) = text) and
      ((Self.Bloky.Bloky[i].typ = Self.GetTechBlokType(Self.OpenBlok.typ)) or
       ((Self.GetTechBlokType(Self.OpenBlok.typ) = 1) and (Self.Bloky.Bloky[i].typ = 9)) or
        (Self.GetTechBlokType(Self.OpenBlok.typ) = -1))) then
    begin
     Self.LB_BlocksIndexes[Self.LB_Blocks.Items.Count] := i;
     Self.LB_Blocks.Items.Add(Self.Bloky.Bloky[i].Nazev);
     if (Self.Bloky.Bloky[i].id = Self.OpenBlok.Blok) then
      index := Self.LB_Blocks.Items.Count-1;
    end;
  end;//for i

 if ((Self.E_Blk.Text <> '') and (index <= 0)) then
   index := 2; // vybrat prvni filtrovany blok

 Self.LB_Blocks.ItemIndex := index;
end;//procedure

procedure TF_BlockEdit.OpenFormGlobal();
begin
 Self.GB_Usek.Visible      := false;
 Self.GB_Vyhybka.Visible   := false;
 Self.GB_Uvazka.Visible    := false;
 Self.GB_UvazkaSpr.Visible := false;

 Self.LoadOR();
end;

procedure TF_BlockEdit.RB_UvazkaSmer2Click(Sender: TObject);
begin
 if (Self.OpenBlok.typ = TBlkType.uvazka) then
  begin
   (Self.OpenBlok as TUvazka).defalt_dir := (Sender as TRadioButton).Tag;
   Self.SaveData();
  end;
end;//procedure

function TF_BlockEdit.GetTechBlokType(panel_type:TBlkType):Integer;
begin
 case (panel_type) of
  TBlkType.vyhybka,
  TBlkType.vykol      : Result := 0;
  TBlkType.usek       : Result := 1;
  TBlkType.navestidlo : Result := 3;
  TBlkType.prejezd    : Result := 4;
  TBlkType.uvazka,
  TBlkType.uvazka_spr : Result := 6;
  TBlkType.zamek      : Result := 7;
  TBlkType.rozp       : Result := 8;
 else
  Result := -1;
 end;
end;//function

end.//unit
