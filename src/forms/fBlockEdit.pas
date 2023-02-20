unit fBlockEdit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, IniFiles, ReliefSettings, ReliefObjects, ExtCtrls, Global,
  StrUtils, Spin, ObjBlok, Generics.Collections, Types;

type
  TBlok = record
    Nazev: string;
    id: integer;
    typ: integer;
  end;

  EFileNotFound = class(Exception);

  TBloky = class
  private
    bloky: TList<TBlok>;
    iniFile: TMemIniFile;
    fTriedLoad: Boolean;

  public
    constructor Create();
    destructor Destroy(); override;

    procedure LoadData(const FileName: string);

    property triedLoad: Boolean read fTriedLoad;
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
    OpenBlok: TGraphBlok;
    LB_BlocksIndexes: TList<Integer>; // indexaci pole pro ListBox, zde jsou ulozeny inedxy bloku (nikoliv id!!)

    procedure LoadOR();
    procedure SaveData();

    procedure UpdateLB(text: string);

    procedure OpenFormGlobal();

    function GetTechBlokType(panel_type: TBlkType): integer;

  public
    Bloky: TBloky;

    procedure OpenForm(Blok: TGraphBlok);
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
  Self.bloky := TList<TBlok>.Create();
  Self.fTriedLoad := false;
end;

destructor TBloky.Destroy();
begin
  Self.bloky.Free();
end;

procedure TBloky.LoadData(const FileName: string);
begin
  Self.fTriedLoad := true;
  Self.bloky.Clear();

  if (not FileExists(FileName)) then
    raise EFileNotFound.Create('Soubor ' + FileName + ' neexistuje!');

  var str: TStrings := TStringList.Create();
  Self.iniFile := TMemIniFile.Create(FileName, TEncoding.UTF8);
  try
    Self.iniFile.ReadSections(str);

    for var i := 0 to str.Count - 1 do
    begin
      var blk: TBlok;
      blk.id := StrToInt(str[i]);
      blk.Nazev := Self.iniFile.ReadString(str[i], 'nazev', 'Blok ' + IntToStr(i));
      blk.typ := Self.iniFile.ReadInteger(str[i], 'typ', -1);
      Self.bloky.Add(blk);
    end; // for i
  finally
    Self.iniFile.Free();
    str.Free();
  end;
end;

procedure TF_BlockEdit.B_ApplyClick(Sender: TObject);
begin
  Self.LB_BlocksClick(Self.LB_Blocks);
  Self.Close;
end;

procedure TF_BlockEdit.B_StornoClick(Sender: TObject);
begin
  Self.Close;
end;

procedure TF_BlockEdit.CB_ORChange(Sender: TObject);
begin
  Self.OpenBlok.area := (Sender as TComboBox).ItemIndex;
  Self.SaveData();
end;

procedure TF_BlockEdit.CB_UvazkaSpr_VertDirChange(Sender: TObject);
begin
  if (Self.OpenBlok.typ = TBlkType.linker_train) then
  begin
    (Self.OpenBlok as TLinkerTrain).spr_cnt := Self.SE_UvazkaSpr_Cnt.Value;
    (Self.OpenBlok as TLinkerTrain).vertical_dir := TUvazkaSprVertDir(Self.CB_UvazkaSpr_VertDir.ItemIndex);
    Self.SaveData();
  end;
end;

procedure TF_BlockEdit.CB_VyhPlusChange(Sender: TObject);
begin
  if (Self.OpenBlok.typ = TBlkType.turnout) then
  begin
    (Self.OpenBlok as TTurnout).PolohaPlus := (Sender as TComboBox).ItemIndex;
    Self.SaveData();
  end;
end;

procedure TF_BlockEdit.E_BlkChange(Sender: TObject);
begin
  Self.UpdateLB(Self.E_Blk.text);
  Self.LB_BlocksClick(Self.LB_Blocks);

  if ((Self.E_Blk.text <> '') and (Self.LB_Blocks.ItemIndex <= 0) and (Self.OpenBlok.block = -1)) then
    Self.LB_Blocks.ItemIndex := 2; // vybrat prvni filtrovany blok
end;

procedure TF_BlockEdit.E_BlkKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case (Key) of
    VK_UP:
      begin
        if (Self.LB_Blocks.ItemIndex > 0) then
        begin
          Self.LB_Blocks.ItemIndex := Self.LB_Blocks.ItemIndex - 1;
          Self.LB_BlocksClick(Self.LB_Blocks);
          Key := 0;
        end;
      end;

    VK_DOWN:
      begin
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
    #13:
      Self.B_ApplyClick(Self.B_Apply);
    #$1B:
      begin
        if (Self.E_Blk.text = '') then
        begin
          Self.Close();
        end else begin
          Self.E_Blk.text := '';
          Key := #0;
          Self.E_BlkChange(Self.E_KPopisek);
        end;
      end;
  end; // case
end;

procedure TF_BlockEdit.E_KPopisekChange(Sender: TObject);
begin
  if (Self.OpenBlok.typ = TBlkType.track) then
  begin
    (Self.OpenBlok as TTrack).caption := (Sender as TEdit).text;
    Self.SaveData();
  end;
end;

procedure TF_BlockEdit.E_KPopisekKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = #13) then
    Self.B_ApplyClick(Self);
  if (Key = #27) then
    Self.Close;

  case Key of
    '0' .. '9', #9, #8, 'a' .. 'z', 'A' .. 'Z':
      begin
      end
  else
    begin
      Key := #0;
    end;
  end; // case
end;

procedure TF_BlockEdit.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Self.SaveData();
end;

procedure TF_BlockEdit.FormCreate(Sender: TObject);
begin
  Self.Bloky := TBloky.Create;
  Self.LB_BlocksIndexes := TList<Integer>.Create();
  Self.OpenBlok := nil;
end;

procedure TF_BlockEdit.FormDestroy(Sender: TObject);
begin
  Self.LB_BlocksIndexes.Free();
  Self.Bloky.Free();
end;

procedure TF_BlockEdit.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = #27) then
    Self.Close;
end;

procedure TF_BlockEdit.FormShow(Sender: TObject);
begin
  Self.UpdateLB(Self.E_Blk.text);
  Self.E_Blk.SetFocus();

  if (Self.OpenBlok = nil) then
  begin
    Self.LB_Blocks.ItemIndex := 0;
    Self.LB_BlocksClick(Self.LB_Blocks);
  end;
end;

procedure TF_BlockEdit.OpenForm(Blok: TGraphBlok);
begin
  Self.OpenFormGlobal();
  Self.OpenBlok := Blok;
  Self.CB_OR.ItemIndex := Blok.area;

  case (Blok.typ) of
    TBlkType.track:
      begin
        Self.GB_Usek.Visible := true;
        if ((Blok as TTrack).labels.Count > 0) then
        begin
          Self.Label2.Visible := true;
          Self.E_KPopisek.Visible := true;
        end else begin
          Self.Label2.Visible := false;
          Self.E_KPopisek.Visible := false;
        end; // else KPopisek.Count > 0
        Self.E_KPopisek.text := (Blok as TTrack).caption;
        Self.Caption := 'Editace úseku';
      end;

    TBlkType.signal:
      begin
        Self.Caption := 'Editace návěstidla';
      end;

    TBlkType.turnout:
      begin
        Self.GB_Vyhybka.Visible := true;
        Self.CB_VyhPlus.ItemIndex := (Blok as TTurnout).PolohaPlus;
        Self.CB_OR.ItemIndex := (Blok as TTurnout).area;
        Self.Caption := 'Editace výhybky';
      end;

    TBlkType.crossing:
      begin
        Self.Caption := 'Editace přejezdu';
      end;

    TBlkType.text:
      begin
        Self.Caption := 'Editace popisku';
      end;

    TBlkType.linker:
      begin
        Self.GB_Uvazka.Visible := true;
        case ((Blok as TLinker).defalt_dir) of
          0:
            Self.RB_UvazkaSmer1.Checked := true;
          1:
            Self.RB_UvazkaSmer2.Checked := true;
        end;
        Self.Caption := 'Editace úvazky';
      end;

    TBlkType.linker_train:
      begin
        Self.GB_UvazkaSpr.Visible := true;
        Self.CB_UvazkaSpr_VertDir.ItemIndex := integer((Blok as TLinkerTrain).vertical_dir);
        Self.SE_UvazkaSpr_Cnt.Value := (Blok as TLinkerTrain).spr_cnt;
      end;

    TBlkType.lock:
      begin
        Self.Caption := 'Editace zámku';
      end;

    TBlkType.derail:
      begin
        Self.Caption := 'Editace výkolejky';
      end;

    TBlkType.disconnector:
      begin
        Self.Caption := 'Editace rozpojovače';
      end;

    TBlkType.pst:
      begin
        Self.Caption := 'Editace pomocného stavědla';
      end;

  end; // case Blok.typ

  if (Self.Showing) then
    Self.OnShow(Self)
  else
    Self.Show;
end; // procedur

/// /////////////////////////////////////////////////////////////////////////////

procedure TF_BlockEdit.LB_BlocksClick(Sender: TObject);
var id: integer;
begin
  if ((Sender as TListBox).ItemIndex < 0) then
    Exit();

  if (Self.LB_BlocksIndexes[(Sender as TListBox).ItemIndex] < 0) then
    id := Self.LB_BlocksIndexes[(Sender as TListBox).ItemIndex]
  else
    id := Self.Bloky.Bloky[Self.LB_BlocksIndexes[(Sender as TListBox).ItemIndex]].id;

  if (Self.OpenBlok <> nil) then
  begin
    Self.OpenBlok.block := id;
    Self.SaveData();
  end;
end;

procedure TF_BlockEdit.LB_BlocksDblClick(Sender: TObject);
begin
  Self.B_ApplyClick(Self);
end;

procedure TF_BlockEdit.LB_BlocksKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = #27) then
    Self.Close;
  if (Key = #13) then
    Self.B_ApplyClick(Self);
end;

procedure TF_BlockEdit.LoadOR();
var oblr: TOR;
begin
  Self.CB_OR.Clear();
  for oblr in F_Hlavni.Relief.ORs do
    Self.CB_OR.Items.Add(oblr.Name);
end;

procedure TF_BlockEdit.SaveData();
begin
  if (Assigned(F_Hlavni.Relief)) then
    F_Hlavni.Relief.Show(Point(-1, -1));
end;

procedure TF_BlockEdit.UpdateLB(text: string);
var index: integer;
begin
  Self.LB_Blocks.Clear();
  Self.LB_BlocksIndexes.Clear();

  if (Self.OpenBlok = nil) then
    Exit();

  Self.LB_Blocks.Items.Add('--- Žádný blok ---');
  Self.LB_Blocks.Items.Add('--- Žádný blok záměrně ---');
  Self.LB_BlocksIndexes.Add(-1);
  Self.LB_BlocksIndexes.Add(-2);

  if (Self.OpenBlok.block = -2) then
    index := 1
  else if (Self.OpenBlok.block = -1) then
    index := 0
  else
    index := -1;

  for var i := 0 to Self.Bloky.bloky.Count - 1 do
  begin
    if ((LeftStr(Self.Bloky.Bloky[i].Nazev, Length(text)) = text) and
      ((Self.Bloky.Bloky[i].typ = Self.GetTechBlokType(Self.OpenBlok.typ)) or
      ((Self.GetTechBlokType(Self.OpenBlok.typ) = 1) and (Self.Bloky.Bloky[i].typ = 9)) or
      (Self.GetTechBlokType(Self.OpenBlok.typ) = -1))) then
    begin
      Self.LB_BlocksIndexes.Add(i);
      Self.LB_Blocks.Items.Add(Self.Bloky.Bloky[i].Nazev);
      if (Self.Bloky.Bloky[i].id = Self.OpenBlok.block) then
        index := Self.LB_Blocks.Items.Count - 1;
    end;
  end; // for i

  Self.LB_Blocks.ItemIndex := index;
end;

procedure TF_BlockEdit.OpenFormGlobal();
begin
  Self.GB_Usek.Visible := false;
  Self.GB_Vyhybka.Visible := false;
  Self.GB_Uvazka.Visible := false;
  Self.GB_UvazkaSpr.Visible := false;

  Self.LoadOR();
end;

procedure TF_BlockEdit.RB_UvazkaSmer2Click(Sender: TObject);
begin
  if (Self.OpenBlok.typ = TBlkType.linker) then
  begin
    (Self.OpenBlok as TLinker).defalt_dir := (Sender as TRadioButton).Tag;
    Self.SaveData();
  end;
end;

function TF_BlockEdit.GetTechBlokType(panel_type: TBlkType): integer;
begin
  case (panel_type) of
    TBlkType.turnout, TBlkType.derail:
      Result := 0;
    TBlkType.track:
      Result := 1;
    TBlkType.signal:
      Result := 3;
    TBlkType.crossing:
      Result := 4;
    TBlkType.linker, TBlkType.linker_train:
      Result := 6;
    TBlkType.lock:
      Result := 7;
    TBlkType.disconnector:
      Result := 8;
    TBlkType.pst:
      Result := 14;
  else
    Result := -1;
  end;
end;

end.// unit
