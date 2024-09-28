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
  ENoArea = class(Exception);

  TBlokAskEvent = procedure(Sender: TObject; Blok: TGraphBlok) of object;
  TAreaAskEvent = function(lefttop: TPoint; rightbot: TPoint): Integer of object;
  TMsgEvent = procedure(Sender: TObject; msg: string) of object;

  TPanelObjects = class
  private
    PM_Properties: TPopUpMenu;

    mFileName: string;
    mFileState: TReliefFileState;
    mMode: TMode;
    Graphics: TPanelGraphics;
    fShowBlokPopisky: Boolean;

    PopUpPos: TPoint;
    DrawObject: TDrawObject;
    Colors: TObjColors;

    Selected: TGraphBlok;
    AreaOpStep: TGOpStep;
    AreaOpStart: TPoint;

    mOnBlokEdit: TBlokAskEvent;
    mOnShow: TNEvent;
    mOnMsg: TMsgEvent;
    mFormBlkClose: TGlobalEvent;
    mOnAreaAsk: TAreaAskEvent;

    procedure CreatePM(var PM: TPopUpMenu; Parent: TDXDraw);
    procedure PMPropertiesClick(Sender: TObject);

    procedure SetMode(mode: TMode);
    procedure ComputeVyhybkaFlag(); // volano pri prechodu z Bloky do Koreny
    function FindAreaI(lefttop: TPoint; rightbot: TPoint): Integer;

    class function FileSupportedVersionsStr(): string;
    class function BlockCreate(typ: TBlkType; i: Integer): TGraphBlok;
    class function IsOpnlVersionSupported(version: string): Boolean;
    class function OpnlVersionToWord(version: string): Word;
    class procedure OpnlLoadBlocks(var ini: TMemIniFile; verWord: Word; var blocks: TObjectList<TGraphBlok>);
    class function OpnlLoadAreas(var ini: TMemIniFile): string;

  public
    blocks: TObjectList<TGraphBlok>;

    constructor Create(SymbolIL, TextIL: TImageList; DrawCanvas: TCanvas; Width, Height: Integer; Parent: TDXDraw;
      Graphics: TPanelGraphics);
    destructor Destroy(); override;

    procedure Import(Data: TObject); // import dat zatim z PanelBitmap
    procedure ImportOldOpnl(aFile: string; offset: TPoint);

    procedure OpnlLoad(aFile: string; var ORs: string);
    procedure OpnlSave(aFile: string; const ORs: string);

    procedure Paint(); // obecny paint
    procedure PaintBlocks(); // paint modu bloky
    function PaintCursor(CursorPos: TPoint): TCursorDraw;
    function PaintCursorAreas(CursorPos: TPoint): TCursorDraw;

    procedure ResetPanel();
    procedure Escape();

    procedure MouseUp(Position: TPoint; Button: TMouseButton);
    procedure MouseMove(Position: TPoint);
    procedure BlocksMouseUp(Position: TPoint; Button: TMouseButton);
    procedure RootsMouseUp(Position: TPoint; Button: TMouseButton);
    procedure AreasMouseUp(Position: TPoint; Button: TMouseButton);
    procedure DblClick(Position: TPoint);

    function GetObject(Pos: TPoint): Integer;

    function SetArea(areai: Integer): Byte; // nastavi oblast rizeni vybranemu bloku

    function CheckValid(var error_cnt: Byte): TStrings; // overi validitu naeditovanych dat a vrati chybove hlasky

    procedure ComputePrjPanelUsek();

    property PanelWidth: Integer read DrawObject.Width;
    property PanelHeight: Integer read DrawObject.Height;
    property fileName: string read mFileName;
    property FileStav: TReliefFileState read mFileState;
    property mode: TMode read mMode write SetMode;

    property ColorSelected: SymbolColor read Colors.Selected write Colors.Selected;
    property ColorNormal: SymbolColor read Colors.Normal write Colors.Normal;
    property ColorAlert: SymbolColor read Colors.Alert write Colors.Alert;
    property ColorIntUnassigned: SymbolColor read Colors.IntUnassigned write Colors.IntUnassigned;
    property selected_obj: TGraphBlok read Selected;

    property OnBlokEdit: TBlokAskEvent read mOnBlokEdit write mOnBlokEdit;

    property OnShow: TNEvent read mOnShow write mOnShow;
    property OnMsg: TMsgEvent read mOnMsg write mOnMsg;
    property OnFormBlkClose: TGlobalEvent read mFormBlkClose write mFormBlkClose;
    property ShowBlokPopisky: boolean read fShowBlokPopisky write fShowBlokPopisky;
    property OnAreaAsk: TAreaAskEvent read mOnAreaAsk write mOnAreaAsk;
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
  Self.mMode := dmBlocks;
  Self.AreaOpStep := gosNone;

  Self.blocks := TObjectList<TGraphBlok>.Create();

  Self.CreatePM(Self.PM_Properties, Parent);
end; // constructor

destructor TPanelObjects.Destroy();
begin
  Self.ResetPanel();
  Self.blocks.Free();

  if (Assigned(Self.PM_Properties) = true) then
  begin
    Self.PM_Properties.Free();
    Self.PM_Properties := nil;
  end;
end; // destructor

// reset dat
procedure TPanelObjects.ResetPanel();
begin
  Self.blocks.Clear();
  Self.Selected := nil;
end;

// nacitani souboru
procedure TPanelObjects.OpnlLoad(aFile: string; var ORs: string);
var inifile: TMemIniFile;
begin
  Self.mFileState := fsSaved;
  Self.mFileName := aFile;

  Self.ResetPanel();

  // samotne nacitani dat
  inifile := TMemIniFile.Create(aFile, TEncoding.UTF8);
  try
    // kontrola verze
    var ver: string := inifile.ReadString('G', 'ver', _FileVersion);
    if (not IsOpnlVersionSupported(ver)) then
    begin
      if (Application.MessageBox(PChar('Načítáte soubor s verzí ' + ver + #13#10 +
        'Aplikace momentálně podporuje verze ' + Self.FileSupportedVersionsStr() + #13#10 + 'Chcete pokračovat?'),
        'Varování', MB_YESNO OR MB_ICONQUESTION) = mrNo) then
        raise EFileLoad.Create('Uživatel zrušil načítání souboru!');
    end;

    var verWord: Word := Self.OpnlVersionToWord(ver);

    Self.DrawObject.Height := inifile.ReadInteger('P', 'H', 0);
    Self.DrawObject.Width := inifile.ReadInteger('P', 'W', 0);

    ORs := Self.OpnlLoadAreas(inifile);
    Self.OpnlLoadBlocks(inifile, verWord, Self.blocks);

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
      for var blok: TGraphBlok in Self.blocks do
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
  Self.PaintBlocks();
end;

procedure TPanelObjects.PaintBlocks();
begin
  Self.DrawObject.Canvas.Pen.mode := pmMerge; // pruhlednost
  for var block: TGraphBlok in Self.blocks do
    if ((Self.ShowBlokPopisky) or (block.typ <> TBlkType.description)) then
      block.Paint(Self.DrawObject, Self.Graphics, Self.Colors, Self.Selected = block, Self.mode);
end;

/// /////////////////////////////////////////////////////////////////////////////

// Escape
procedure TPanelObjects.Escape();
begin
  Self.Selected := nil;
  Self.AreaOpStep := gosNone;
end;

// vykresleni kurzoru - vraci data v PIXELECH!
function TPanelObjects.PaintCursor(CursorPos: TPoint): TCursorDraw;
begin
  if (Self.mode = dmAreas) then
    Exit(Self.PaintCursorAreas(CursorPos));

  // vykreslit kurzor
  Result.color := TCursorColor.ccDefault;
  Result.pos1.X := CursorPos.X * _SYMBOL_WIDTH;
  Result.pos1.Y := CursorPos.Y * _SYMBOL_HEIGHT;
  Result.pos2.X := CursorPos.X * _SYMBOL_WIDTH;
  Result.pos2.Y := CursorPos.Y * _SYMBOL_HEIGHT;

  // vykreslit koren pod kurzorem
  if (Self.mMode = dmRoots) then
    SymbolDraw(Self.DrawObject.SymbolIL, Self.DrawObject.Canvas, CursorPos, _S_CIRCLE, scAqua);
end;

function TPanelObjects.PaintCursorAreas(CursorPos: TPoint): TCursorDraw;
begin
  if (Self.AreaOpStep = gosSelecting) then
  begin
    Result.color := TCursorColor.ccOnObject;
    Result.pos1.X := Self.AreaOpStart.X * _SYMBOL_WIDTH;
    Result.pos1.Y := Self.AreaOpStart.Y * _SYMBOL_HEIGHT;
    Result.pos2.X := CursorPos.X * _SYMBOL_WIDTH;
    Result.pos2.Y := CursorPos.Y * _SYMBOL_HEIGHT;
  end else begin
    Result.color := TCursorColor.ccActiveOperation;
    Result.pos1.X := CursorPos.X * _SYMBOL_WIDTH;
    Result.pos1.Y := CursorPos.Y * _SYMBOL_HEIGHT;
    Result.pos2.X := CursorPos.X * _SYMBOL_WIDTH;
    Result.pos2.Y := CursorPos.Y * _SYMBOL_HEIGHT;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TPanelObjects.GetObject(Pos: TPoint): Integer;
var tmp: Integer;
begin
  tmp := -1;
  for var i := 0 to Self.blocks.count - 1 do
  begin
    var ObjBlok := Self.blocks[i];

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
  if (Self.mMode = dmRoots) then
  begin
    var tmp := Self.GetObject(Position);
    if ((tmp = -1) or (Self.blocks[tmp].typ <> TBlkType.track)) then
    begin
      Self.Selected := nil;
      Exit;
    end;
    if ((Self.blocks[tmp] as TTrack).IsTurnout) then
      Self.Selected := Self.blocks[tmp];
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TPanelObjects.MouseUp(Position: TPoint; Button: TMouseButton);
begin
  case (Self.mMode) of
    dmBlocks:
      Self.BlocksMouseUp(Position, Button);
    dmRoots:
      Self.RootsMouseUp(Position, Button);
    dmAreas:
      Self.AreasMouseUp(Position, Button);
  end;
end;

procedure TPanelObjects.BlocksMouseUp(Position: TPoint; Button: TMouseButton);
var blk: Integer;
begin
  blk := Self.GetObject(Position);
  if (blk < 0) then
  begin
    Self.Selected := nil;
    Exit();
  end
  else
    Self.Selected := Self.blocks[blk];

  if (Assigned(Self.OnMsg)) then
  begin
    case (Self.Selected.typ) of
      TBlkType.track:
        Self.OnMsg(Self, 'Blok ' + IntToStr(blk) + ' (úsek ' + IntToStr(Self.Selected.index) + ')');
      TBlkType.signal:
        Self.OnMsg(Self, 'Blok ' + IntToStr(blk) + ' (návěstidlo ' + IntToStr(Self.Selected.index) + ')');
      TBlkType.turnout:
        Self.OnMsg(Self, 'Blok ' + IntToStr(blk) + ' (výhybka ' + IntToStr(Self.Selected.index) + '), přiřazen úseku '
          + IntToStr((Self.Selected as TTurnout).obj));
      TBlkType.crossing:
        Self.OnMsg(Self, 'Blok ' + IntToStr(blk) + ' (přejezd ' + IntToStr(Self.Selected.index) + ')');
      TBlkType.text:
        Self.OnMsg(Self, 'Blok ' + IntToStr(blk) + ' (popisek ' + IntToStr(Self.Selected.index) + ')');
      TBlkType.description:
        Self.OnMsg(Self, 'Blok ' + IntToStr(blk) + ' (popisek bloku ' + IntToStr(Self.Selected.index) + ')');
      TBlkType.other:
        Self.OnMsg(Self, 'Blok ' + IntToStr(blk) + ' (pomocný objekt ' + IntToStr(Self.Selected.index) + ')');
      TBlkType.linker:
        Self.OnMsg(Self, 'Blok ' + IntToStr(blk) + ' (úvazka ' + IntToStr(Self.Selected.index) + ')');
      TBlkType.linker_train:
        Self.OnMsg(Self, 'Blok ' + IntToStr(blk) + ' (úvazka spr. ' + IntToStr(Self.Selected.index) + ')');
      TBlkType.lock:
        Self.OnMsg(Self, 'Blok ' + IntToStr(blk) + ' (zámek ' + IntToStr(Self.Selected.index) + ')');
      TBlkType.derail:
        Self.OnMsg(Self, 'Blok ' + IntToStr(blk) + ' (výkolejka ' + IntToStr(Self.Selected.index) +
          '), přiřazena úseku ' + IntToStr((Self.Selected as TDerail).obj));
      TBlkType.disconnector:
        Self.OnMsg(Self, 'Blok ' + IntToStr(blk) + ' (rozpojovač ' + IntToStr(Self.Selected.index) + ')');
      TBlkType.pst:
        Self.OnMsg(Self, 'Blok ' + IntToStr(blk) + ' (PSt ' + IntToStr(Self.Selected.index) + ')');
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

procedure TPanelObjects.RootsMouseUp(Position: TPoint; Button: TMouseButton);
begin
  // leve tlacitko mysi
  if (Button <> mbLeft) then
    Exit;

  var blk := Self.GetObject(Position);
  if (blk < 0) then
    Self.Selected := nil
  else if (Self.blocks[blk].typ = TBlkType.track) then
    Self.Selected := Self.blocks[blk]
  else if (Self.blocks[blk].typ = TBlkType.turnout) then
  begin
    for var i := 0 to Self.blocks.count - 1 do
      if ((Self.blocks[i].typ = TBlkType.track) and (Self.blocks[i].index = (Self.blocks[blk] as TTurnout).obj)) then
      begin
        Self.Selected := Self.blocks[i];
        break;
      end;
  end
  else
    Self.Selected := nil;

  if (Assigned(Self.OnMsg)) then
    Self.OnMsg(Self, 'Blok ' + IntToStr(blk) + ' (úsek)');
  if ((not Assigned(Self.Selected)) or (not(Self.Selected as TTrack).IsTurnout)) then
    Exit;
  (Self.Selected as TTrack).Root := Position;
end;

procedure TPanelObjects.AreasMouseUp(Position: TPoint; Button: TMouseButton);
begin
  if (Self.AreaOpStep = gosNone) then
  begin
    Self.AreaOpStart := Position;
    Self.AreaOpStep := gosSelecting;

  end else if (Self.AreaOpStep = gosSelecting) then
  begin
    // Assign selected area to selected blocks
    Self.AreaOpStep := gosNone;

    var areai := Self.FindAreaI(Self.AreaOpStart, Position);
    if (areai < 0) then
    begin
      raise ENoArea.Create('Ve výběru nenalezena žádná oblast řízení!');
      //Application.MessageBox('Ve výběru nenalezena žádná oblast řízení!', 'Chyba', MB_OK OR MB_ICONWARNING);
      //Exit();
    end;

    for var x: Integer := Self.AreaOpStart.X to Position.X do
    begin
      for var y: Integer := Self.AreaOpStart.Y to Position.Y do
      begin
        var blki := Self.GetObject(Point(x, y));
        if (blki > -1) then
          Self.blocks[blki].area := areai;
      end;
    end;
  end;
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
    if (Assigned(Self.mOnBlokEdit)) then
      Self.mOnBlokEdit(Self, Self.Selected);
end;

/// /////////////////////////////////////////////////////////////////////////////

// tato funkce prirazuje aktivnimu bloku obalst rizeni v paramtru
// to je uzitecne k tomu, ze pri vybrani bloku a nesdlednemu kliku na baracek dojde k prirazeni teto oblasti rizeni
function TPanelObjects.SetArea(areai: Integer): Byte;
begin
  if (Self.Selected = nil) then
    Exit(1);
  Self.Selected.area := areai;
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

  for var i := 0 to Self.blocks.count - 1 do
  begin
    if ((Self.blocks[i].block = -1) and (Self.blocks[i].typ <> TBlkType.other) and
      ((Self.blocks[i].typ <> TBlkType.text) or (Length((Self.blocks[i] as TText).text) = 1))) then
    begin
      Result.Add('ERR: blok ' + IntToStr(i) + ': není návaznost na technologický blok');
      error_cnt := error_cnt + 1;
    end;
    if ((Self.blocks[i].area < 0) and ((Self.blocks[i].typ <> TBlkType.other)
      xor ((Self.blocks[i].typ = TBlkType.text) and (Length((Self.blocks[i] as TText).text) > 1)))) then
    begin
      Result.Add('ERR: blok ' + IntToStr(i) + ': není návaznost na oblast řízení');
      error_cnt := error_cnt + 1;
    end;

    case (Self.blocks[i].typ) of
      TBlkType.track:
        begin
          if (((Self.blocks[i] as TTrack).labels.count > 0) and ((Self.blocks[i] as TTrack).caption = '')) then
          begin
            Result.Add('ERR: blok ' + IntToStr(i) + ' (úsek): kolej není pojmenována');
            error_cnt := error_cnt + 1;
          end;

          if (((Self.blocks[i] as TTrack).IsTurnout) and ((Self.blocks[i] as TTrack).Root.X < 0)) then
          begin
            Result.Add('ERR: blok ' + IntToStr(i) + ' (úsek) : obsahuje výhybky a přesto nemá kořen');
            error_cnt := error_cnt + 1;
          end;

          for var j := 0 to (Self.blocks[i] as TTrack).branches.count - 1 do
          begin
            var vetev := (Self.blocks[i] as TTrack).branches[j];
            if (((Self.blocks[i] as TTrack).DKStype <> TDKSType.dksNone) and (j < 3)) then
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
          if ((Self.blocks[i] as TTurnout).obj < 0) then
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
  Self.mMode := mode;
  Self.Selected := nil;
  if (mode = dmRoots) then
    Self.ComputeVyhybkaFlag();
end;

/// /////////////////////////////////////////////////////////////////////////////

// prirazuje blokum flag, ktery rika, jestli jsou an nem vyhybky
procedure TPanelObjects.ComputeVyhybkaFlag();
begin
  // reset flagu
  for var i := 0 to Self.blocks.count - 1 do
    if (Self.blocks[i].typ = TBlkType.track) then
      (Self.blocks[i] as TTrack).IsTurnout := false;

  // zjistime, jestli na danych blocich jsou vyhybky
  for var i := 0 to Self.blocks.count - 1 do
    if (Self.blocks[i].typ = TBlkType.turnout) then
    begin
      // .obj referuje na index v seznamu useku -> musime vypocitat index v poli vsech bloku
      for var j := 0 to Self.blocks.count - 1 do
        if ((Self.blocks[j].typ = TBlkType.track) and (Self.blocks[j].index = (Self.blocks[i] as TTurnout).obj)) then
        begin
          (Self.blocks[j] as TTrack).IsTurnout := true;
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
  for var i := 0 to Self.blocks.count - 1 do
  begin
    if (Self.blocks[i].typ <> TBlkType.crossing) then
      continue;

    for var j := 0 to (Self.blocks[i] as TCrossing).BlikPositions.count - 1 do
    begin
      var usek := Self.GetObject(Point((Self.blocks[i] as TCrossing).BlikPositions[j].Pos.X - 1,
        (Self.blocks[i] as TCrossing).BlikPositions[j].Pos.Y));
      var blik_point := (Self.blocks[i] as TCrossing).BlikPositions[j];
      blik_point.PanelUsek := Self.blocks[usek].index;
      (Self.blocks[i] as TCrossing).BlikPositions[j] := blik_point;
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

class function TPanelObjects.BlockCreate(typ: TBlkType; i: Integer): TGraphBlok;
begin
  case (typ) of
    track:
      Result := ObjBlokUsek.TTrack.Create(i);
    signal:
      Result := ObjBlokNavestidlo.TSignal.Create(i);
    turnout:
      Result := ObjBlokVyhybka.TTurnout.Create(i);
    crossing:
      Result := ObjBlokPrejezd.TCrossing.Create(i);
    text:
      Result := ObjBlokText.TText.Create(i);
    description:
      begin
        Result := ObjBlokText.TText.Create(i);
        Result.typ := TBlkType.description; // override type
      end;
    other:
      Result := ObjBlokPomocny.TObjOther.Create(i);
    linker:
      Result := ObjBlokUvazka.TLinker.Create(i);
    linker_train:
      Result := ObjBlokUvazkaSpr.TLinkerTrain.Create(i);
    lock:
      Result := ObjBlokZamek.TLock.Create(i);
    derail:
      Result := ObjBlokVykol.TDerail.Create(i);
    disconnector:
      Result := ObjBlokRozp.TDisconnector.Create(i);
    pst:
      Result := ObjBlokPst.TPSt.Create(i);
  else
    Result := nil;
  end;
end;

class function TPanelObjects.IsOpnlVersionSupported(version: string): Boolean;
begin
  Result := False;
  for var i := 0 to Length(_FileVersion_accept) - 1 do
    if (version = _FileVersion_accept[i]) then
      Exit(True);
end;

class function TPanelObjects.OpnlVersionToWord(version: string): Word;
begin
  var strs: TStrings := TStringList.Create();
  try
    ExtractStringsEx(['.'], [], version, strs);
    Result := (StrToInt(strs[0]) shl 8) + StrToInt(strs[1]);
  finally
    strs.Free();
  end;
end;

class procedure TPanelObjects.OpnlLoadBlocks(var ini: TMemIniFile; verWord: Word; var blocks: TObjectList<TGraphBlok>);
begin
  for var blkTyp: TBlkType := Low(TBlkType) to High(TBlkType) do
  begin
    var count: Integer := ini.ReadInteger('P', TGraphBlok.TypeToFileStr(blkTyp), 0);
    for var i: Integer := 0 to count - 1 do
    begin
      try
        var block: TGraphBlok := Self.BlockCreate(blkTyp, i);
        block.Load(ini, TGraphBlok.TypeToFileStr(blkTyp) + IntToStr(i), verWord);
        blocks.Add(block);
      except

      end;
    end;
  end;
end;

class function TPanelObjects.OpnlLoadAreas(var ini: TMemIniFile): string;
begin
  var sect_str: TStrings := TStringList.Create();
  try
    ini.ReadSection('OR', sect_str);
    Result := '';
    for var i: Integer := 0 to sect_str.count - 1 do
      Result := Result + ini.ReadString('OR', sect_str[i], '') + #13;
    Result := Result + #13;
  finally
    sect_str.Free();
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TPanelObjects.ImportOldOpnl(aFile: string; offset: TPoint);
var inifile: TMemIniFile;
    oldBlocks: TObjectList<TGraphBlok>;
begin
  // samotne nacitani dat
  oldBlocks := TObjectList<TGraphBlok>.Create();
  try
    inifile := TMemIniFile.Create(aFile, TEncoding.UTF8);
    try
      // kontrola verze
      var ver: string := inifile.ReadString('G', 'ver', _FileVersion);
      if (not IsOpnlVersionSupported(ver)) then
      begin
        if (Application.MessageBox(PChar('Importujete soubor s verzí ' + ver + #13#10 +
          'Aplikace momentálně podporuje verze ' + Self.FileSupportedVersionsStr() + #13#10 + 'Chcete pokračovat?'),
          'Varování', MB_YESNO OR MB_ICONQUESTION) = mrNo) then
          raise EFileLoad.Create('Uživatel zrušil načítání souboru!');
      end;

      var verWord: Word := OpnlVersionToWord(ver);
      Self.OpnlLoadBlocks(inifile, verWord, oldBlocks);
      for var block: TGraphBlok in oldBlocks do
        block.Move(offset);
    finally
      inifile.Free();
    end;

    for var block: TGraphBlok in oldBlocks do
      if (block.typ = TBlkType.track) then
        TTrack(block).FillSymbolsSorted();

    for var block: TGraphBlok in Self.blocks do
    begin
      case (block.typ) of
        TBlkType.track:
        begin
          var current: TTrack := TTrack(block);
          current.FillSymbolsSorted();
          var old: TTrack := current.GetEqTrack(oldBlocks);
          if (old <> nil) then
          begin
            current.area := old.area;
            if (current.Root = Point(-1, -1)) then
              current.Root := old.Root;
            if (current.block = -1) then
              current.block := old.block;
            if (current.caption = '') then
              current.caption := old.caption;
          end;
        end;
        TBlkType.signal:
        begin
          var current: TSignal := TSignal(block);
          var old := current.GetEqSignal(oldBlocks);
          if (old <> nil) then
          begin
            current.area := old.area;
            if (current.block = -1) then
              current.block := old.block;
          end;
        end;
        TBlkType.turnout:
        begin
          var current: TTurnout := TTurnout(block);
          var old := current.GetEqTurnout(oldBlocks);
          if (old <> nil) then
          begin
            current.area := old.area;
            if (current.block = -1) then
              current.block := old.block;
            current.PolohaPlus := old.PolohaPlus;
          end;
        end;
        TBlkType.crossing:
        begin
          var current: TCrossing := TCrossing(block);
          var old := current.GetEqCrossing(oldBlocks);
          if (old <> nil) then
          begin
            current.area := old.area;
            if (current.block = -1) then
              current.block := old.block;
          end;
        end;
        TBlkType.linker:
        begin
          var current: TLinker := TLinker(block);
          var old := current.GetEqLinker(oldBlocks);
          if (old <> nil) then
          begin
            current.area := old.area;
            if (current.block = -1) then
              current.block := old.block;
            current.defalt_dir := old.defalt_dir;
          end;
        end;
        TBlkType.linker_train:
        begin
          var current: TLinkerTrain := TLinkerTrain(block);
          var old := current.GetEqLinkerTrain(oldBlocks);
          if (old <> nil) then
          begin
            current.area := old.area;
            if (current.block = -1) then
              current.block := old.block;
            current.vertical_dir := old.vertical_dir;
            current.spr_cnt := old.spr_cnt;
          end;
        end;
        TBlkType.lock:
        begin
          var current: TLock := TLock(block);
          var old := current.GetEqLock(oldBlocks);
          if (old <> nil) then
          begin
            current.area := old.area;
            if (current.block = -1) then
              current.block := old.block;
          end;
        end;
        TBlkType.derail:
        begin
          var current: TDerail := TDerail(block);
          var old := current.GetEqDerail(oldBlocks);
          if (old <> nil) then
          begin
            current.area := old.area;
            if (current.block = -1) then
              current.block := old.block;
          end;
        end;
        TBlkType.disconnector:
        begin
          var current: TDisconnector := TDisconnector(block);
          var old := current.GetEqDisconnector(oldBlocks);
          if (old <> nil) then
          begin
            current.area := old.area;
            if (current.block = -1) then
              current.block := old.block;
          end;
        end;
        TBlkType.pst:
        begin
          var current: TPst := TPst(block);
          var old := current.GetEqPst(oldBlocks);
          if (old <> nil) then
          begin
            current.area := old.area;
            if (current.block = -1) then
              current.block := old.block;
          end;
        end;
      end;
    end;

  finally
    oldBlocks.Free();
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TPanelObjects.FindAreaI(lefttop: TPoint; rightbot: TPoint): Integer;
begin
  if (not Assigned(Self.OnAreaAsk)) then
    raise Exception.Create('OnAreaAsk není přiřazeno!');
  Result := Self.OnAreaAsk(lefttop, rightbot);
end;

/// /////////////////////////////////////////////////////////////////////////////

end.// unit
