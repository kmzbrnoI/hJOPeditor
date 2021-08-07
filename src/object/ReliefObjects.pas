unit ReliefObjects;
// tato unita pracuje s objektovymi daty reliefu

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, IniFiles,
  StrUtils, Global, Menus, Forms, DXDraws, OblastRizeni, PGraphics, Types,
  Generics.Collections, symbolHelper, vetev, ObjBlok;

const
  _Def_Color_Selected: SymbolColor = scRed;
  _Def_Color_Normal: SymbolColor = scGray;
  _Def_Color_Alert: SymbolColor = scAqua;
  _Def_Color_IntUnassigned: SymbolColor = scWhite;

  _FILEVERSION_10 = $0100;
  _FILEVERSION_11 = $0101;
  _FILEVERSION_12 = $0102;
  _FILEVERSION_13 = $0103;
  _FILEVERSION_20 = $0200;

  _FileVersion = '2.0';
  _FileVersion_accept: array [0 .. 3] of string = ('1.1', '1.2', '1.3', '2.0');

type

  EFileLoad = class(Exception);

  TBlokAskEvent = procedure(Sender: TObject; Blok: TGraphBlok) of object;
  TMsgEvent = procedure(Sender: TObject; msg: string) of object;

  TPanelObjects = class
  private

    PM_Properties: TPopUpMenu;

    FSoubor: string;
    FStav: ShortInt;
    FMode: TMode;
    Graphics: TPanelGraphics;
    fShowBlokPopisky: boolean;

    PopUpPos: TPoint;
    DrawObject: TDrawObject;
    Colors: TObjColors;

    Selected: TGraphBlok;

    FOnBlokEdit: TBlokAskEvent;
    FOnShow: TNEvent;
    FOnMsg: TMsgEvent;
    FFormBlkClose: TGlobalEvent;

    procedure CreatePM(var PM: TPopUpMenu; Parent: TDXDraw);
    procedure PMPropertiesClick(Sender: TObject);

    procedure SetMode(mode: TMode);
    procedure ComputeVyhybkaFlag(); // volano pri prechodu z Bloky do Koreny

    class function FileSupportedVersionsStr(): string;

  public
    Bloky: TList<TGraphBlok>;

    constructor Create(SymbolIL, TextIL: TImageList; DrawCanvas: TCanvas; Width, Height: Integer; Parent: TDXDraw;
      Graphics: TPanelGraphics);
    destructor Destroy; override;

    procedure Import(Data: TObject); // import dat zatim z PanelBitmap

    procedure FLoad(aFile: string; var ORs: string);
    procedure FSave(aFile: string; const ORs: string);

    procedure Paint(); // obecny paint
    procedure PaintBloky(); // paint modu bloky
    function PaintCursor(CursorPos: TPoint): TCursorDraw;

    procedure ResetPanel;
    procedure Escape;

    procedure MouseUp(Position: TPoint; Button: TMouseButton);
    procedure MouseMove(Position: TPoint);
    procedure BlokyMouseUp(Position: TPoint; Button: TMouseButton);
    procedure KorenyMouseUp(Position: TPoint; Button: TMouseButton);
    procedure DblClick(Position: TPoint);

    function GetObject(Pos: TPoint): Integer;

    function SetOR(OblR: Integer): Byte; // nastavi oblast rizeni vybranemu bloku

    function CheckValid(var error_cnt: Byte): TStrings; // overi validitu naeditovanych dat a vrati chybove hlasky

    procedure ComputePrjPanelUsek();

    property PanelWidth: Integer read DrawObject.Width;
    property PanelHeight: Integer read DrawObject.Height;
    property Soubor: string read FSoubor;
    property FileStav: ShortInt read FStav;
    property mode: TMode read FMode write SetMode;

    property ColorSelected: SymbolColor read Colors.Selected write Colors.Selected;
    property ColorNormal: SymbolColor read Colors.Normal write Colors.Normal;
    property ColorAlert: SymbolColor read Colors.Alert write Colors.Alert;
    property ColorIntUnassigned: SymbolColor read Colors.IntUnassigned write Colors.IntUnassigned;
    property selected_obj: TGraphBlok read Selected;

    property OnBlokEdit: TBlokAskEvent read FOnBlokEdit write FOnBlokEdit;

    property OnShow: TNEvent read FOnShow write FOnShow;
    property OnMsg: TMsgEvent read FOnMsg write FOnMsg;
    property OnFormBlkClose: TGlobalEvent read FFormBlkClose write FFormBlkClose;
    property ShowBlokPopisky: boolean read fShowBlokPopisky write fShowBlokPopisky;
  end; // TPanelObjects

  // FileSystemStav:
  // 0 - soubor zavren
  // 1 - soubor neulozen
  // 2 - soubor ulozen

implementation

uses ReliefBitmap, BitmapToObj, VetveComputer, ObjBlokUsek, ObjBlokVyhybka,
  ObjBlokPomocny, ObjBlokNavestidlo, ObjBlokText, ObjBlokPrejezd, ObjBlokUvazka,
  ObjBlokUvazkaSpr, ObjBlokZamek, ObjBlokVykol, ObjBlokRozp, ownStrUtils,
  ObjBlokPSt;

// vytvoreni objektu
constructor TPanelObjects.Create(SymbolIL, TextIL: TImageList; DrawCanvas: TCanvas; Width, Height: Integer;
  Parent: TDXDraw; Graphics: TPanelGraphics);
begin
  Self.DrawObject.Canvas := DrawCanvas;

  Self.DrawObject.SymbolIL := SymbolIL;
  Self.DrawObject.TextIL := TextIL;

  Self.DrawObject.Width := Width;
  Self.DrawObject.Height := Height;

  Self.Graphics := Graphics;
  Self.fShowBlokPopisky := true;

  // vychozi barvy
  Self.Colors.Selected := _Def_Color_Selected;
  Self.Colors.Normal := _Def_Color_Normal;
  Self.Colors.Alert := _Def_Color_Alert;
  Self.Colors.IntUnassigned := _Def_Color_IntUnassigned;

  Self.Selected := nil;

  Self.FMode := dmBloky;

  Self.Bloky := TList<TGraphBlok>.Create();

  Self.CreatePM(Self.PM_Properties, Parent);
end; // constructor

destructor TPanelObjects.Destroy;
begin
  Self.ResetPanel();
  Self.Bloky.Free();

  if (Assigned(Self.PM_Properties) = true) then
  begin
    Self.PM_Properties.Free;
    Self.PM_Properties := nil;
  end;
end; // destructor

// reset dat
procedure TPanelObjects.ResetPanel();
var Blok: TGraphBlok;
begin
  for Blok in Self.Bloky do
    Blok.Free();
  Self.Bloky.Clear();
  Self.Selected := nil;
end;

// nacitani souboru
procedure TPanelObjects.FLoad(aFile: string; var ORs: string);
var i: Integer;
  inifile: TMemIniFile;
  ver: string;
  verWord: Word;
  sect_str: TStrings;
  Blok: TGraphBlok;
  count: Integer;
  blkTyp: TBlkType;
  versionOk: boolean;
  strs: TStrings;
begin
  Self.FStav := 2;
  Self.FSoubor := aFile;

  Self.ResetPanel;

  // samotne nacitani dat
  inifile := TMemIniFile.Create(aFile, TEncoding.UTF8);
  try
    // kontrola verze
    ver := inifile.ReadString('G', 'ver', _FileVersion);

    versionOk := false;
    for i := 0 to Length(_FileVersion_accept) - 1 do
    begin
      if (ver = _FileVersion_accept[i]) then
      begin
        versionOk := true;
        break;
      end;
    end;

    if (not versionOk) then
    begin
      if (Application.MessageBox(PChar('Načítáte soubor s verzí ' + ver + #13#10 +
        'Aplikace momentálně podporuje verze ' + Self.FileSupportedVersionsStr() + #13#10 + 'Chcete pokračovat?'),
        'Varování', MB_YESNO OR MB_ICONQUESTION) = mrNo) then
        raise EFileLoad.Create('Uživatel zrušil načítání souboru!');
    end;

    strs := TStringList.Create();
    try
      ExtractStringsEx(['.'], [], ver, strs);
      verWord := (StrToInt(strs[0]) shl 8) + StrToInt(strs[1]);
    finally
      strs.Free();
    end;

    Self.DrawObject.Height := inifile.ReadInteger('P', 'H', 0);
    Self.DrawObject.Width := inifile.ReadInteger('P', 'W', 0);

    // oblati rizeni
    // tato silena metoda vytvoreni binarniho souboru opravdu zjednodusuje cely program
    sect_str := TStringList.Create();
    inifile.ReadSection('OR', sect_str);
    ORs := '';
    for i := 0 to sect_str.count - 1 do
      ORs := ORs + inifile.ReadString('OR', sect_str[i], '') + #13;
    ORs := ORs + #13;
    sect_str.Free;

    for blkTyp := Low(TBlkType) to High(TBlkType) do
    begin
      count := inifile.ReadInteger('P', TGraphBlok.TypeToFileStr(blkTyp), 0);
      for i := 0 to count - 1 do
      begin
        try
          case (blkTyp) of
            usek:
              Blok := ObjBlokUsek.TUsek.Create(i);
            navestidlo:
              Blok := ObjBlokNavestidlo.TNavestidlo.Create(i);
            vyhybka:
              Blok := ObjBlokVyhybka.TVyhybka.Create(i);
            prejezd:
              Blok := ObjBlokPrejezd.TPrejezd.Create(i);
            text:
              Blok := ObjBlokText.TText.Create(i);
            blok_popisek:
              begin
                Blok := ObjBlokText.TText.Create(i);
                Blok.typ := TBlkType.blok_popisek; // override type
              end;
            pomocny_obj:
              Blok := ObjBlokPomocny.TPomocnyObj.Create(i);
            uvazka:
              Blok := ObjBlokUvazka.TUvazka.Create(i);
            uvazka_spr:
              Blok := ObjBlokUvazkaSpr.TUvazkaSpr.Create(i);
            zamek:
              Blok := ObjBlokZamek.TZamek.Create(i);
            vykol:
              Blok := ObjBlokVykol.TVykol.Create(i);
            rozp:
              Blok := ObjBlokRozp.TRozp.Create(i);
            pst:
              Blok := ObjBlokPst.TPSt.Create(i);
          else
            Blok := nil;
          end;

          Blok.Load(inifile, TGraphBlok.TypeToFileStr(blkTyp) + IntToStr(i), verWord);
          Self.Bloky.Add(Blok);
        except

        end;
      end;
    end; // for blkTyp

    Self.ComputeVyhybkaFlag();
    Self.Escape();
  finally
    inifile.Free;
  end;
end;

procedure TPanelObjects.FSave(aFile: string; const ORs: string);
var i: Integer;
  inifile: TMemIniFile;
  str_list: TStrings;
  Blok: TGraphBlok;
  counts: TDictionary<TBlkType, Integer>;
  blkTyp: TBlkType;
begin
  Self.FStav := 2;

  Self.ComputeVyhybkaFlag();
  ComputeVetve(Self);

  DeleteFile(aFile);

  inifile := TMemIniFile.Create(aFile, TEncoding.UTF8);
  counts := TDictionary<TBlkType, Integer>.Create();
  try
    inifile.WriteString('G', 'ver', _FileVersion);

    inifile.WriteInteger('P', 'H', Self.DrawObject.Height);
    inifile.WriteInteger('P', 'W', Self.DrawObject.Width);

    Self.ComputePrjPanelUsek();

    // oblasti rizeni
    str_list := TStringList.Create();
    ExtractStringsEx([#13], [], LeftStr(ORs, Length(ORs) - 1), str_list);
    for i := 0 to str_list.count - 1 do
      inifile.WriteString('OR', IntToStr(i), str_list[i]);
    str_list.Free;

    // ulozit bloky v kanonickem poradi
    for blkTyp := Low(TBlkType) to High(TBlkType) do
    begin
      for Blok in Self.Bloky do
      begin
        if (Blok.typ = blkTyp) then
        begin
          Blok.Save(inifile, TGraphBlok.TypeToFileStr(Blok.typ) + IntToStr(Blok.index));
          if (not counts.ContainsKey(Blok.typ)) then
            counts.Add(Blok.typ, 0);
          counts[Blok.typ] := counts[Blok.typ] + 1;
        end;
      end;
    end;

    for blkTyp := Low(TBlkType) to High(TBlkType) do
      if (counts.ContainsKey(blkTyp)) then
        inifile.WriteInteger('P', TGraphBlok.TypeToFileStr(blkTyp), counts[blkTyp]);

  finally
    inifile.UpdateFile();
    inifile.Free();
    counts.Free();
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TPanelObjects.Paint();
begin
  case (Self.FMode) of
    dmBloky, dmRoots:
      Self.PaintBloky();
  end;
end;

procedure TPanelObjects.PaintBloky();
var Blok: TGraphBlok;
begin
  Self.DrawObject.Canvas.Pen.mode := pmMerge; // pruhlednost
  for Blok in Self.Bloky do
    if ((Self.ShowBlokPopisky) or (Blok.typ <> TBlkType.blok_popisek)) then
      Blok.Paint(Self.DrawObject, Self.Graphics, Self.Colors, Self.Selected = Blok, Self.mode);
end;

/// /////////////////////////////////////////////////////////////////////////////

// Escape
procedure TPanelObjects.Escape();
begin
  Self.Selected := nil;
end;

// vykresleni kurzoru - vraci data v PIXELECH!
function TPanelObjects.PaintCursor(CursorPos: TPoint): TCursorDraw;
begin
  // vykreslit kurzor
  Result.Color := 0;
  Result.Pos1.X := CursorPos.X * _Symbol_Sirka;
  Result.Pos1.Y := CursorPos.Y * _Symbol_Vyska;
  Result.Pos2.X := CursorPos.X * _Symbol_Sirka;
  Result.Pos2.Y := CursorPos.Y * _Symbol_Vyska;

  // vykreslit koren pod kurzorem
  if (Self.FMode = dmRoots) then
    SymbolDraw(Self.DrawObject.SymbolIL, Self.DrawObject.Canvas, CursorPos, _Kolecko, scAqua);
end;

/// /////////////////////////////////////////////////////////////////////////////

function TPanelObjects.GetObject(Pos: TPoint): Integer;
var tmp: Integer;
begin
  tmp := -1;
  for var i := 0 to Self.Bloky.count - 1 do
  begin
    var ObjBlok := Self.Bloky[i];

    case (ObjBlok.typ) of
      TBlkType.usek:
        begin
          for var j := 0 to (ObjBlok as TUsek).Symbols.count - 1 do
            if ((Pos.X = (ObjBlok as TUsek).Symbols[j].Position.X) and
              (Pos.Y = (ObjBlok as TUsek).Symbols[j].Position.Y)) then
              tmp := i; // usek nema prioritu (napriklad nad rozpojovacem)
        end;

      TBlkType.navestidlo:
        if ((Pos.X = (ObjBlok as TNavestidlo).Position.X) and (Pos.Y = (ObjBlok as TNavestidlo).Position.Y)) then
          Exit(i);

      TBlkType.vyhybka:
        if ((Pos.X = (ObjBlok as TVyhybka).Position.X) and (Pos.Y = (ObjBlok as TVyhybka).Position.Y)) then
          Exit(i);

      TBlkType.prejezd:
        begin
          for var bpos in (ObjBlok as TPrejezd).StaticPositions do
            if (bpos.X = Pos.X) and (bpos.Y = Pos.Y) then
              Exit(i);
          for var j := 0 to (ObjBlok as TPrejezd).BlikPositions.count - 1 do
            if ((ObjBlok as TPrejezd).BlikPositions[j].Pos.X = Pos.X) and
              ((ObjBlok as TPrejezd).BlikPositions[j].Pos.Y = Pos.Y) then
              Exit(i);
        end;

      TBlkType.text, TBlkType.blok_popisek:
        begin
          if ((Pos.Y = (ObjBlok as TText).Position.Y) and (Pos.X >= (ObjBlok as TText).Position.X) and
            (Pos.X < (ObjBlok as TText).Position.X + Length((ObjBlok as TText).text)) and
            (Length((ObjBlok as TText).text) = 1)) then
            Exit(i);
        end;

      TBlkType.pomocny_obj:
        begin
          // klik na pomocny objekt nas nezajima
          if ((ObjBlok as TPomocnyObj).Symbol in ObjBlokPomocny.BLK_ASSIGN_SYMBOLS) then
          begin
            for var bpos in (ObjBlok as TPomocnyObj).Positions do
              if ((bpos.X = Pos.X) and (bpos.Y = Pos.Y)) then
                Exit(i);
          end;

        end;

      TBlkType.uvazka:
        if (((ObjBlok as TUvazka).Pos.Y = Pos.Y) and (Pos.X >= (ObjBlok as TUvazka).Pos.X) and
          (Pos.X <= (ObjBlok as TUvazka).Pos.X + 1)) then
          Exit(i);

      TBlkType.uvazka_spr:
        if (((ObjBlok as TUvazkaSpr).Pos.Y = Pos.Y) and (Pos.X = (ObjBlok as TUvazkaSpr).Pos.X)) then
          Exit(i);

      TBlkType.zamek:
        if (((ObjBlok as TZamek).Pos.Y = Pos.Y) and (Pos.X = (ObjBlok as TZamek).Pos.X)) then
          Exit(i);

      TBlkType.vykol:
        if (((ObjBlok as TVykol).Pos.Y = Pos.Y) and (Pos.X = (ObjBlok as TVykol).Pos.X)) then
          Exit(i);

      TBlkType.rozp:
        if (((ObjBlok as TRozp).Pos.Y = Pos.Y) and (Pos.X = (ObjBlok as TRozp).Pos.X)) then
          Exit(i);

      TBlkType.pst:
        if ((Pos.X = (ObjBlok as TPSt).Pos.X) and (((ObjBlok as TPSt).Pos.Y = Pos.Y) or ((ObjBlok as TPSt).Pos.Y+1 = Pos.Y))) then
          Exit(i);

    end; // case
  end;

  Result := tmp;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TPanelObjects.MouseMove(Position: TPoint);
var tmp: Integer;
begin
  if (Self.FMode = dmRoots) then
  begin
    tmp := Self.GetObject(Position);
    if ((tmp = -1) or (Self.Bloky[tmp].typ <> TBlkType.usek)) then
    begin
      Self.Selected := nil;
      Exit;
    end;
    if ((Self.Bloky[tmp] as TUsek).IsVyhybka) then
      Self.Selected := Self.Bloky[tmp];
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TPanelObjects.MouseUp(Position: TPoint; Button: TMouseButton);
begin
  case (Self.FMode) of
    dmBloky:
      Self.BlokyMouseUp(Position, Button);
    dmRoots:
      Self.KorenyMouseUp(Position, Button);
  end;
end;

procedure TPanelObjects.BlokyMouseUp(Position: TPoint; Button: TMouseButton);
var blk: Integer;
begin
  blk := Self.GetObject(Position);
  if (blk < 0) then
  begin
    Self.Selected := nil;
    Exit();
  end
  else
    Self.Selected := Self.Bloky[blk];

  if (Assigned(Self.FOnMsg)) then
  begin
    case (Self.Selected.typ) of
      TBlkType.usek:
        Self.FOnMsg(Self, 'Blok ' + IntToStr(blk) + ' (úsek ' + IntToStr(Self.Selected.index) + ')');
      TBlkType.navestidlo:
        Self.FOnMsg(Self, 'Blok ' + IntToStr(blk) + ' (návěstidlo ' + IntToStr(Self.Selected.index) + ')');
      TBlkType.vyhybka:
        Self.FOnMsg(Self, 'Blok ' + IntToStr(blk) + ' (výhybka ' + IntToStr(Self.Selected.index) + '), přiřazen úseku '
          + IntToStr((Self.Selected as TVyhybka).obj));
      TBlkType.prejezd:
        Self.FOnMsg(Self, 'Blok ' + IntToStr(blk) + ' (přejezd ' + IntToStr(Self.Selected.index) + ')');
      TBlkType.text:
        Self.FOnMsg(Self, 'Blok ' + IntToStr(blk) + ' (popisek ' + IntToStr(Self.Selected.index) + ')');
      TBlkType.blok_popisek:
        Self.FOnMsg(Self, 'Blok ' + IntToStr(blk) + ' (popisek bloku ' + IntToStr(Self.Selected.index) + ')');
      TBlkType.pomocny_obj:
        Self.FOnMsg(Self, 'Blok ' + IntToStr(blk) + ' (pomocný objekt ' + IntToStr(Self.Selected.index) + ')');
      TBlkType.uvazka:
        Self.FOnMsg(Self, 'Blok ' + IntToStr(blk) + ' (úvazka ' + IntToStr(Self.Selected.index) + ')');
      TBlkType.uvazka_spr:
        Self.FOnMsg(Self, 'Blok ' + IntToStr(blk) + ' (úvazka spr. ' + IntToStr(Self.Selected.index) + ')');
      TBlkType.zamek:
        Self.FOnMsg(Self, 'Blok ' + IntToStr(blk) + ' (zámek ' + IntToStr(Self.Selected.index) + ')');
      TBlkType.vykol:
        Self.FOnMsg(Self, 'Blok ' + IntToStr(blk) + ' (výkolejka ' + IntToStr(Self.Selected.index) +
          '), přiřazena úseku ' + IntToStr((Self.Selected as TVykol).obj));
      TBlkType.rozp:
        Self.FOnMsg(Self, 'Blok ' + IntToStr(blk) + ' (rozpojovač ' + IntToStr(Self.Selected.index) + ')');
      TBlkType.pst:
        Self.FOnMsg(Self, 'Blok ' + IntToStr(blk) + ' (PSt ' + IntToStr(Self.Selected.index) + ')');
    end; // case
  end; // if Assigned(FOnMsg)

  // prave tlacitko mysi
  if (Button = mbRight) then
  begin
    if (Self.Selected.typ <> TBlkType.pomocny_obj) then
    begin
      Self.PopUpPos := Position;
      Self.PM_Properties.Popup(Mouse.CursorPos.X, Mouse.CursorPos.Y);
    end;
  end; // mbRight

  /// /////////////////////////////////////

  // leve tlacitko mysi
  if (Button = mbLeft) then
    if ((Self.Selected.typ <> TBlkType.pomocny_obj) or ((Self.Selected as TPomocnyObj).Symbol
      in ObjBlokPomocny.BLK_ASSIGN_SYMBOLS)) then
      Self.PMPropertiesClick(Self);
end;

procedure TPanelObjects.KorenyMouseUp(Position: TPoint; Button: TMouseButton);
var blk, i: Integer;
begin
  // leve tlacitko mysi
  if (Button <> mbLeft) then
    Exit;

  blk := Self.GetObject(Position);
  if (blk < 0) then
    Self.Selected := nil
  else if (Self.Bloky[blk].typ = TBlkType.usek) then
    Self.Selected := Self.Bloky[blk]
  else if (Self.Bloky[blk].typ = TBlkType.vyhybka) then
  begin
    for i := 0 to Self.Bloky.count - 1 do
      if ((Self.Bloky[i].typ = TBlkType.usek) and (Self.Bloky[i].index = (Self.Bloky[blk] as TVyhybka).obj)) then
      begin
        Self.Selected := Self.Bloky[i];
        break;
      end;
  end
  else
    Self.Selected := nil;

  if (Assigned(Self.FOnMsg)) then
    Self.FOnMsg(Self, 'Blok ' + IntToStr(blk) + ' (úsek)');
  if ((not Assigned(Self.Selected)) or (not(Self.Selected as TUsek).IsVyhybka)) then
    Exit;
  (Self.Selected as TUsek).Root := Position;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TPanelObjects.DblClick(Position: TPoint);
begin
  Self.PopUpPos := Position;
  Self.PMPropertiesClick(Self);
end;

procedure TPanelObjects.CreatePM(var PM: TPopUpMenu; Parent: TDXDraw);
var MI: TMenuItem;
begin
  PM := TPopUpMenu.Create(Parent);

  MI := TMenuItem.Create(PM);
  MI.Caption := 'Vlastnosti';
  MI.OnClick := Self.PMPropertiesClick;

  PM.Items.Add(MI);
end;

procedure TPanelObjects.PMPropertiesClick(Sender: TObject);
begin
  if (Self.Selected <> nil) then
    if (Assigned(Self.FOnBlokEdit)) then
      Self.FOnBlokEdit(Self, Self.Selected);
end;

/// /////////////////////////////////////////////////////////////////////////////

// tato funkce prirazuje aktivnimu bloku obalst rizeni v paramtru
// to je uzitecne k tomu, ze pri vybrani bloku a nesdlednemu kliku na baracek dojde k prirazeni teto oblasti rizeni
function TPanelObjects.SetOR(OblR: Integer): Byte;
begin
  if (Self.Selected = nil) then
    Exit(1);
  Self.Selected.OblRizeni := OblR;
  Result := 0;
end;

// checking data valid
function TPanelObjects.CheckValid(var error_cnt: Byte): TStrings;
var str: string;
  i, j: Integer;
  vetev: TVetev;
begin
  Result := TStringList.Create();

  DateTimeToString(str, 'yyyy-mm-dd hh:nn:ss', Now);
  Result.Add('Validace objektů: ' + str);

  Result.Add('Ověřuji návaznost úseku na technologické bloky, návaznost na oblasti řízeni...');
  Result.Add('Ověřuji návaznost výhybek na úseky...');
  Result.Add('Ověřuji pojmenování kolejí...');
  Result.Add('Ověřuji kořeny...');

  for i := 0 to Self.Bloky.count - 1 do
  begin
    if ((Self.Bloky[i].Blok = -1) and (Self.Bloky[i].typ <> TBlkType.pomocny_obj) and
      ((Self.Bloky[i].typ <> TBlkType.text) or (Length((Self.Bloky[i] as TText).text) = 1))) then
    begin
      Result.Add('ERR: blok ' + IntToStr(i) + ': není návaznost na technologický blok');
      error_cnt := error_cnt + 1;
    end;
    if ((Self.Bloky[i].OblRizeni < 0) and ((Self.Bloky[i].typ <> TBlkType.pomocny_obj)
      xor ((Self.Bloky[i].typ = TBlkType.text) and (Length((Self.Bloky[i] as TText).text) > 1)))) then
    begin
      Result.Add('ERR: blok ' + IntToStr(i) + ': není návaznost na oblast řízení');
      error_cnt := error_cnt + 1;
    end;

    case (Self.Bloky[i].typ) of
      TBlkType.usek:
        begin
          if (((Self.Bloky[i] as TUsek).KPopisek.count > 0) and ((Self.Bloky[i] as TUsek).KpopisekStr = '')) then
          begin
            Result.Add('ERR: blok ' + IntToStr(i) + ' (úsek): kolej není pojmenována');
            error_cnt := error_cnt + 1;
          end;

          if (((Self.Bloky[i] as TUsek).IsVyhybka) and ((Self.Bloky[i] as TUsek).Root.X < 0)) then
          begin
            Result.Add('ERR: blok ' + IntToStr(i) + ' (úsek) : obsahuje výhybky a přesto nemá kořen');
            error_cnt := error_cnt + 1;
          end;

          for j := 0 to (Self.Bloky[i] as TUsek).Vetve.count - 1 do
          begin
            vetev := (Self.Bloky[i] as TUsek).Vetve[j];
            if (((Self.Bloky[i] as TUsek).DKStype <> TDKSType.dksNone) and (j < 3)) then
              continue;
            if (((vetev.node1.vyh >= 0) and ((vetev.node1.ref_plus = -1) or (vetev.node1.ref_minus = -1))) or
              ((vetev.node2.vyh >= 0) and ((vetev.node2.ref_plus = -1) or (vetev.node2.ref_minus = -1)))) then
              Result.Add('WARN: blok ' + IntToStr(i) +
                ' (úsek) : větev ma návaznost na výhybku, přesto za výhybkou nikam nepokračuje');
          end;
        end;

      /// ///////////////////////////////

      TBlkType.vyhybka:
        begin
          if ((Self.Bloky[i] as TVyhybka).obj < 0) then
          begin
            Result.Add('ERR: blok ' + IntToStr(i) + ' (výhybka) : není návaznost na úsek');
            error_cnt := error_cnt + 1;
          end;
        end;

      /// ///////////////////////////////

    end; // case
  end; // for i

  Result.Add('--- Validace hotova. ---');
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TPanelObjects.SetMode(mode: TMode);
begin
  Self.FMode := mode;
  Self.Selected := nil;
  if (mode = TMode.dmRoots) then
    Self.ComputeVyhybkaFlag();
end;

/// /////////////////////////////////////////////////////////////////////////////

// prirazuje blokum flag, ktery rika, jestli jsou an nem vyhybky
procedure TPanelObjects.ComputeVyhybkaFlag();
var i, j: Integer;
begin
  // reset flagu
  for i := 0 to Self.Bloky.count - 1 do
    if (Self.Bloky[i].typ = TBlkType.usek) then
      (Self.Bloky[i] as TUsek).IsVyhybka := false;

  // zjistime, jestli na danych blocich jsou vyhybky
  for i := 0 to Self.Bloky.count - 1 do
    if (Self.Bloky[i].typ = TBlkType.vyhybka) then
    begin
      // .obj referuje na index v seznamu useku -> musime vypocitat index v poli vsech bloku
      for j := 0 to Self.Bloky.count - 1 do
        if ((Self.Bloky[j].typ = TBlkType.usek) and (Self.Bloky[j].index = (Self.Bloky[i] as TVyhybka).obj)) then
        begin
          (Self.Bloky[j] as TUsek).IsVyhybka := true;
          break;
        end;
    end;
end;

/// /////////////////////////////////////////////////////////////////////////////

// import dat z jineho modu
// zatim podporuje jen mod bitmap
procedure TPanelObjects.Import(Data: TObject);
var BitmapToObj: TBitmapToObj;
begin
  if (Data.ClassType = TPanelBitmap) then
  begin
    BitmapToObj := TBitmapToObj.Create;
    BitmapToObj.BitmapToObjects((Data as TPanelBitmap), Self);
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

// pri ukladani dat je vypocteno, jakemu useku patri kazdy prejezd
// resp. jaky usek prejezd obsluhuje
// jedna se o prostredni usek prejezdu
// tento usek se priradi na zaklade leveho useku prejezdu
procedure TPanelObjects.ComputePrjPanelUsek();
var i, j, usek: Integer;
  blik_point: TBlikPoint;
begin
  for i := 0 to Self.Bloky.count - 1 do
  begin
    if (Self.Bloky[i].typ <> TBlkType.prejezd) then
      continue;

    for j := 0 to (Self.Bloky[i] as TPrejezd).BlikPositions.count - 1 do
    begin
      usek := Self.GetObject(Point((Self.Bloky[i] as TPrejezd).BlikPositions[j].Pos.X - 1,
        (Self.Bloky[i] as TPrejezd).BlikPositions[j].Pos.Y));
      blik_point := (Self.Bloky[i] as TPrejezd).BlikPositions[j];
      blik_point.PanelUsek := usek;
      (Self.Bloky[i] as TPrejezd).BlikPositions[j] := blik_point;
    end; // for j
  end; // for i
end;

/// /////////////////////////////////////////////////////////////////////////////

class function TPanelObjects.FileSupportedVersionsStr(): string;
var i: Integer;
begin
  Result := '';
  for i := 0 to Length(_FileVersion_accept) - 1 do
    Result := Result + _FileVersion_accept[i] + ', ';
  Result := LeftStr(Result, Length(Result) - 2);
end;

/// /////////////////////////////////////////////////////////////////////////////

end.// unit
