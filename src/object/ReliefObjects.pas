unit ReliefObjects;
// tato unita pracuje s objektovymi daty reliefu

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, IniFiles,
  StrUtils, Global, Menus, Forms, DXDraws, OblastRizeni, PGraphics, Types,
  Generics.Collections, symbolHelper, vetev, ObjBlok, ReliefCommon;

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

    mFileName: string;
    mFileState: TReliefFileState;
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

    procedure OpnlLoad(aFile: string; var ORs: string);
    procedure OpnlSave(aFile: string; const ORs: string);

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
    property fileName: string read mFileName;
    property FileStav: TReliefFileState read mFileState;
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
begin
  for var blok: TGraphBlok in Self.Bloky do
    Blok.Free();
  Self.Bloky.Clear();
  Self.Selected := nil;
end;

// nacitani souboru
procedure TPanelObjects.OpnlLoad(aFile: string; var ORs: string);
var inifile: TMemIniFile;
  ver: string;
  verWord: Word;
begin
  Self.mFileState := fsSaved;
  Self.mFileName := aFile;

  Self.ResetPanel();

  // samotne nacitani dat
  inifile := TMemIniFile.Create(aFile, TEncoding.UTF8);
  try
    // kontrola verze
    ver := inifile.ReadString('G', 'ver', _FileVersion);

    var versionOk: Boolean := false;
    for var i := 0 to Length(_FileVersion_accept) - 1 do
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

    begin
      var strs: TStrings := TStringList.Create();
      try
        ExtractStringsEx(['.'], [], ver, strs);
        verWord := (StrToInt(strs[0]) shl 8) + StrToInt(strs[1]);
      finally
        strs.Free();
      end;
    end;

    Self.DrawObject.Height := inifile.ReadInteger('P', 'H', 0);
    Self.DrawObject.Width := inifile.ReadInteger('P', 'W', 0);

    // oblati rizeni
    // tato silena metoda vytvoreni binarniho souboru opravdu zjednodusuje cely program
    begin
      var sect_str: TStrings := TStringList.Create();
      try
        inifile.ReadSection('OR', sect_str);
        ORs := '';
        for var i: Integer := 0 to sect_str.count - 1 do
          ORs := ORs + inifile.ReadString('OR', sect_str[i], '') + #13;
        ORs := ORs + #13;
      finally
        sect_str.Free();
      end;
    end;

    for var blkTyp: TBlkType := Low(TBlkType) to High(TBlkType) do
    begin
      var count: Integer := inifile.ReadInteger('P', TGraphBlok.TypeToFileStr(blkTyp), 0);
      for var i: Integer := 0 to count - 1 do
      begin
        try
          var blok: TGraphBlok;
          case (blkTyp) of
            track:
              blok := ObjBlokUsek.TTrack.Create(i);
            signal:
              blok := ObjBlokNavestidlo.TSignal.Create(i);
            turnout:
              blok := ObjBlokVyhybka.TTurnout.Create(i);
            crossing:
              blok := ObjBlokPrejezd.TCrossing.Create(i);
            text:
              blok := ObjBlokText.TText.Create(i);
            description:
              begin
                blok := ObjBlokText.TText.Create(i);
                blok.typ := TBlkType.description; // override type
              end;
            other:
              blok := ObjBlokPomocny.TObjOther.Create(i);
            linker:
              blok := ObjBlokUvazka.TLinker.Create(i);
            linker_train:
              blok := ObjBlokUvazkaSpr.TLinkerTrain.Create(i);
            lock:
              blok := ObjBlokZamek.TLock.Create(i);
            derail:
              blok := ObjBlokVykol.TDerail.Create(i);
            disconnector:
              blok := ObjBlokRozp.TDisconnector.Create(i);
            pst:
              blok := ObjBlokPst.TPSt.Create(i);
          else
            blok := nil;
          end;

          blok.Load(inifile, TGraphBlok.TypeToFileStr(blkTyp) + IntToStr(i), verWord);
          Self.Bloky.Add(blok);
        except

        end;
      end;
    end; // for blkTyp

    Self.ComputeVyhybkaFlag();
    Self.Escape();
  finally
    inifile.Free();
  end;
end;

procedure TPanelObjects.OpnlSave(aFile: string; const ORs: string);
var inifile: TMemIniFile;
  counts: TDictionary<TBlkType, Integer>;
begin
  Self.mFileState := fsSaved;

  Self.ComputeVyhybkaFlag();
  ComputeBranches(Self);

  DeleteFile(aFile);

  inifile := TMemIniFile.Create(aFile, TEncoding.UTF8);
  counts := TDictionary<TBlkType, Integer>.Create();
  try
    inifile.WriteString('G', 'ver', _FileVersion);

    inifile.WriteInteger('P', 'H', Self.DrawObject.Height);
    inifile.WriteInteger('P', 'W', Self.DrawObject.Width);

    Self.ComputePrjPanelUsek();

    // oblasti rizeni
    var str_list: TStrings := TStringList.Create();
    try
      ExtractStringsEx([#13], [], LeftStr(ORs, Length(ORs) - 1), str_list);
      for var i := 0 to str_list.count - 1 do
        inifile.WriteString('OR', IntToStr(i), str_list[i]);
    finally
      str_list.Free();
    end;

    // ulozit bloky v kanonickem poradi
    for var blkTyp: TBlkType := Low(TBlkType) to High(TBlkType) do
    begin
      for var blok: TGraphBlok in Self.Bloky do
      begin
        if (blok.typ = blkTyp) then
        begin
          Blok.Save(inifile, TGraphBlok.TypeToFileStr(Blok.typ) + IntToStr(Blok.index));
          if (not counts.ContainsKey(Blok.typ)) then
            counts.Add(Blok.typ, 0);
          counts[Blok.typ] := counts[Blok.typ] + 1;
        end;
      end;
    end;

    for var blkTyp: TBlkType := Low(TBlkType) to High(TBlkType) do
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
    if ((Self.ShowBlokPopisky) or (Blok.typ <> TBlkType.description)) then
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
  Result.Color := TCursorColor.ccDefault;
  Result.Pos1.X := CursorPos.X * _SYMBOL_WIDTH;
  Result.Pos1.Y := CursorPos.Y * _SYMBOL_HEIGHT;
  Result.Pos2.X := CursorPos.X * _SYMBOL_WIDTH;
  Result.Pos2.Y := CursorPos.Y * _SYMBOL_HEIGHT;

  // vykreslit koren pod kurzorem
  if (Self.FMode = dmRoots) then
    SymbolDraw(Self.DrawObject.SymbolIL, Self.DrawObject.Canvas, CursorPos, _S_CIRCLE, scAqua);
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
      TBlkType.track:
        begin
          for var j := 0 to (ObjBlok as TTrack).Symbols.count - 1 do
            if ((Pos.X = (ObjBlok as TTrack).Symbols[j].Position.X) and
              (Pos.Y = (ObjBlok as TTrack).Symbols[j].Position.Y)) then
              tmp := i; // usek nema prioritu (napriklad nad rozpojovacem)
        end;

      TBlkType.signal:
        if ((Pos.X = (ObjBlok as TSignal).Position.X) and (Pos.Y = (ObjBlok as TSignal).Position.Y)) then
          Exit(i);

      TBlkType.turnout:
        if ((Pos.X = (ObjBlok as TTurnout).Position.X) and (Pos.Y = (ObjBlok as TTurnout).Position.Y)) then
          Exit(i);

      TBlkType.crossing:
        begin
          for var bpos in (ObjBlok as TCrossing).StaticPositions do
            if (bpos.X = Pos.X) and (bpos.Y = Pos.Y) then
              Exit(i);
          for var j := 0 to (ObjBlok as TCrossing).BlikPositions.count - 1 do
            if ((ObjBlok as TCrossing).BlikPositions[j].Pos.X = Pos.X) and
              ((ObjBlok as TCrossing).BlikPositions[j].Pos.Y = Pos.Y) then
              Exit(i);
        end;

      TBlkType.text, TBlkType.description:
        begin
          if ((Pos.Y = (ObjBlok as TText).Position.Y) and (Pos.X >= (ObjBlok as TText).Position.X) and
            (Pos.X < (ObjBlok as TText).Position.X + Length((ObjBlok as TText).text)) and
            (Length((ObjBlok as TText).text) = 1)) then
            Exit(i);
        end;

      TBlkType.other:
        begin
          // klik na pomocny objekt nas nezajima
          if ((ObjBlok as TObjOther).Symbol in ObjBlokPomocny.BLK_ASSIGN_SYMBOLS) then
          begin
            for var bpos in (ObjBlok as TObjOther).Positions do
              if ((bpos.X = Pos.X) and (bpos.Y = Pos.Y)) then
                Exit(i);
          end;

        end;

      TBlkType.linker:
        if (((ObjBlok as TLinker).Pos.Y = Pos.Y) and (Pos.X >= (ObjBlok as TLinker).Pos.X) and
          (Pos.X <= (ObjBlok as TLinker).Pos.X + 1)) then
          Exit(i);

      TBlkType.linker_train:
        if (((ObjBlok as TLinkerTrain).Pos.Y = Pos.Y) and (Pos.X = (ObjBlok as TLinkerTrain).Pos.X)) then
          Exit(i);

      TBlkType.lock:
        if (((ObjBlok as TLock).Pos.Y = Pos.Y) and (Pos.X = (ObjBlok as TLock).Pos.X)) then
          Exit(i);

      TBlkType.derail:
        if (((ObjBlok as TDerail).Pos.Y = Pos.Y) and (Pos.X = (ObjBlok as TDerail).Pos.X)) then
          Exit(i);

      TBlkType.disconnector:
        if (((ObjBlok as TDisconnector).Pos.Y = Pos.Y) and (Pos.X = (ObjBlok as TDisconnector).Pos.X)) then
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
begin
  if (Self.FMode = dmRoots) then
  begin
    var tmp := Self.GetObject(Position);
    if ((tmp = -1) or (Self.Bloky[tmp].typ <> TBlkType.track)) then
    begin
      Self.Selected := nil;
      Exit;
    end;
    if ((Self.Bloky[tmp] as TTrack).IsTurnout) then
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
      TBlkType.track:
        Self.FOnMsg(Self, 'Blok ' + IntToStr(blk) + ' (úsek ' + IntToStr(Self.Selected.index) + ')');
      TBlkType.signal:
        Self.FOnMsg(Self, 'Blok ' + IntToStr(blk) + ' (návěstidlo ' + IntToStr(Self.Selected.index) + ')');
      TBlkType.turnout:
        Self.FOnMsg(Self, 'Blok ' + IntToStr(blk) + ' (výhybka ' + IntToStr(Self.Selected.index) + '), přiřazen úseku '
          + IntToStr((Self.Selected as TTurnout).obj));
      TBlkType.crossing:
        Self.FOnMsg(Self, 'Blok ' + IntToStr(blk) + ' (přejezd ' + IntToStr(Self.Selected.index) + ')');
      TBlkType.text:
        Self.FOnMsg(Self, 'Blok ' + IntToStr(blk) + ' (popisek ' + IntToStr(Self.Selected.index) + ')');
      TBlkType.description:
        Self.FOnMsg(Self, 'Blok ' + IntToStr(blk) + ' (popisek bloku ' + IntToStr(Self.Selected.index) + ')');
      TBlkType.other:
        Self.FOnMsg(Self, 'Blok ' + IntToStr(blk) + ' (pomocný objekt ' + IntToStr(Self.Selected.index) + ')');
      TBlkType.linker:
        Self.FOnMsg(Self, 'Blok ' + IntToStr(blk) + ' (úvazka ' + IntToStr(Self.Selected.index) + ')');
      TBlkType.linker_train:
        Self.FOnMsg(Self, 'Blok ' + IntToStr(blk) + ' (úvazka spr. ' + IntToStr(Self.Selected.index) + ')');
      TBlkType.lock:
        Self.FOnMsg(Self, 'Blok ' + IntToStr(blk) + ' (zámek ' + IntToStr(Self.Selected.index) + ')');
      TBlkType.derail:
        Self.FOnMsg(Self, 'Blok ' + IntToStr(blk) + ' (výkolejka ' + IntToStr(Self.Selected.index) +
          '), přiřazena úseku ' + IntToStr((Self.Selected as TDerail).obj));
      TBlkType.disconnector:
        Self.FOnMsg(Self, 'Blok ' + IntToStr(blk) + ' (rozpojovač ' + IntToStr(Self.Selected.index) + ')');
      TBlkType.pst:
        Self.FOnMsg(Self, 'Blok ' + IntToStr(blk) + ' (PSt ' + IntToStr(Self.Selected.index) + ')');
    end; // case
  end; // if Assigned(FOnMsg)

  // prave tlacitko mysi
  if (Button = mbRight) then
  begin
    if (Self.Selected.typ <> TBlkType.other) then
    begin
      Self.PopUpPos := Position;
      Self.PM_Properties.Popup(Mouse.CursorPos.X, Mouse.CursorPos.Y);
    end;
  end; // mbRight

  /// /////////////////////////////////////

  // leve tlacitko mysi
  if (Button = mbLeft) then
    if ((Self.Selected.typ <> TBlkType.other) or ((Self.Selected as TObjOther).Symbol
      in ObjBlokPomocny.BLK_ASSIGN_SYMBOLS)) then
      Self.PMPropertiesClick(Self);
end;

procedure TPanelObjects.KorenyMouseUp(Position: TPoint; Button: TMouseButton);
begin
  // leve tlacitko mysi
  if (Button <> mbLeft) then
    Exit;

  var blk := Self.GetObject(Position);
  if (blk < 0) then
    Self.Selected := nil
  else if (Self.Bloky[blk].typ = TBlkType.track) then
    Self.Selected := Self.Bloky[blk]
  else if (Self.Bloky[blk].typ = TBlkType.turnout) then
  begin
    for var i := 0 to Self.Bloky.count - 1 do
      if ((Self.Bloky[i].typ = TBlkType.track) and (Self.Bloky[i].index = (Self.Bloky[blk] as TTurnout).obj)) then
      begin
        Self.Selected := Self.Bloky[i];
        break;
      end;
  end
  else
    Self.Selected := nil;

  if (Assigned(Self.FOnMsg)) then
    Self.FOnMsg(Self, 'Blok ' + IntToStr(blk) + ' (úsek)');
  if ((not Assigned(Self.Selected)) or (not(Self.Selected as TTrack).IsTurnout)) then
    Exit;
  (Self.Selected as TTrack).Root := Position;
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
  Self.Selected.area := OblR;
  Result := 0;
end;

// checking data valid
function TPanelObjects.CheckValid(var error_cnt: Byte): TStrings;
begin
  Result := TStringList.Create();

  begin
    var str: string;
    DateTimeToString(str, 'yyyy-mm-dd hh:nn:ss', Now);
    Result.Add('Validace objektů: ' + str);
  end;

  Result.Add('Ověřuji návaznost úseku na technologické bloky, návaznost na oblasti řízeni...');
  Result.Add('Ověřuji návaznost výhybek na úseky...');
  Result.Add('Ověřuji pojmenování kolejí...');
  Result.Add('Ověřuji kořeny...');

  for var i := 0 to Self.Bloky.count - 1 do
  begin
    if ((Self.Bloky[i].block = -1) and (Self.Bloky[i].typ <> TBlkType.other) and
      ((Self.Bloky[i].typ <> TBlkType.text) or (Length((Self.Bloky[i] as TText).text) = 1))) then
    begin
      Result.Add('ERR: blok ' + IntToStr(i) + ': není návaznost na technologický blok');
      error_cnt := error_cnt + 1;
    end;
    if ((Self.Bloky[i].area < 0) and ((Self.Bloky[i].typ <> TBlkType.other)
      xor ((Self.Bloky[i].typ = TBlkType.text) and (Length((Self.Bloky[i] as TText).text) > 1)))) then
    begin
      Result.Add('ERR: blok ' + IntToStr(i) + ': není návaznost na oblast řízení');
      error_cnt := error_cnt + 1;
    end;

    case (Self.Bloky[i].typ) of
      TBlkType.track:
        begin
          if (((Self.Bloky[i] as TTrack).labels.count > 0) and ((Self.Bloky[i] as TTrack).caption = '')) then
          begin
            Result.Add('ERR: blok ' + IntToStr(i) + ' (úsek): kolej není pojmenována');
            error_cnt := error_cnt + 1;
          end;

          if (((Self.Bloky[i] as TTrack).IsTurnout) and ((Self.Bloky[i] as TTrack).Root.X < 0)) then
          begin
            Result.Add('ERR: blok ' + IntToStr(i) + ' (úsek) : obsahuje výhybky a přesto nemá kořen');
            error_cnt := error_cnt + 1;
          end;

          for var j := 0 to (Self.Bloky[i] as TTrack).branches.count - 1 do
          begin
            var vetev := (Self.Bloky[i] as TTrack).branches[j];
            if (((Self.Bloky[i] as TTrack).DKStype <> TDKSType.dksNone) and (j < 3)) then
              continue;
            if (((vetev.node1.vyh >= 0) and ((vetev.node1.ref_plus = -1) or (vetev.node1.ref_minus = -1))) or
              ((vetev.node2.vyh >= 0) and ((vetev.node2.ref_plus = -1) or (vetev.node2.ref_minus = -1)))) then
              Result.Add('WARN: blok ' + IntToStr(i) +
                ' (úsek) : větev ma návaznost na výhybku, přesto za výhybkou nikam nepokračuje');
          end;
        end;

      /// ///////////////////////////////

      TBlkType.turnout:
        begin
          if ((Self.Bloky[i] as TTurnout).obj < 0) then
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
begin
  // reset flagu
  for var i := 0 to Self.Bloky.count - 1 do
    if (Self.Bloky[i].typ = TBlkType.track) then
      (Self.Bloky[i] as TTrack).IsTurnout := false;

  // zjistime, jestli na danych blocich jsou vyhybky
  for var i := 0 to Self.Bloky.count - 1 do
    if (Self.Bloky[i].typ = TBlkType.turnout) then
    begin
      // .obj referuje na index v seznamu useku -> musime vypocitat index v poli vsech bloku
      for var j := 0 to Self.Bloky.count - 1 do
        if ((Self.Bloky[j].typ = TBlkType.track) and (Self.Bloky[j].index = (Self.Bloky[i] as TTurnout).obj)) then
        begin
          (Self.Bloky[j] as TTrack).IsTurnout := true;
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
begin
  for var i := 0 to Self.Bloky.count - 1 do
  begin
    if (Self.Bloky[i].typ <> TBlkType.crossing) then
      continue;

    for var j := 0 to (Self.Bloky[i] as TCrossing).BlikPositions.count - 1 do
    begin
      var usek := Self.GetObject(Point((Self.Bloky[i] as TCrossing).BlikPositions[j].Pos.X - 1,
        (Self.Bloky[i] as TCrossing).BlikPositions[j].Pos.Y));
      var blik_point := (Self.Bloky[i] as TCrossing).BlikPositions[j];
      blik_point.PanelUsek := Self.Bloky[usek].index;
      (Self.Bloky[i] as TCrossing).BlikPositions[j] := blik_point;
    end; // for j
  end; // for i
end;

/// /////////////////////////////////////////////////////////////////////////////

class function TPanelObjects.FileSupportedVersionsStr(): string;
begin
  Result := '';
  for var i := 0 to Length(_FileVersion_accept) - 1 do
    Result := Result + _FileVersion_accept[i] + ', ';
  Result := LeftStr(Result, Length(Result) - 2);
end;

/// /////////////////////////////////////////////////////////////////////////////

end.// unit
