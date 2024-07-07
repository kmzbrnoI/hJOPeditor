unit ReliefBitmap;
// tato unita pracuje s bitmapovymi daty reliefu

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, IniFiles,
  StrUtils, ReliefText, VektorBasedObject, ReliefBitmapSymbols, Global, Forms,
  OblastRizeni, PGraphics, symbolHelper, Generics.Collections, Types, ReliefCommon;

const
  _IMPORT_MYJOP_SUFFIX = '.pnj';

type
  TORAskEvent = function(Pos: TPoint): Boolean of object;

  TBitmapOpType = (botNone = 0, botMove = 1, botDelete = 2);

  TPanelBitmap = class
  private
    mPanelWidth, mPanelHeight: Integer;
    mMode: TMode;
    mFileName: string;

    operations: record
      disable: Boolean;
      opType: TBitmapOpType;
    end;

    mFileState: TReliefFileState;
    graphics: TPanelGraphics;
    fShowBlokPopisky: Boolean;

    mORAskEvent: TORAskEvent;
    mOnShow: TNEvent;
    mOnTextEdit: TChangeTextEvent;

    procedure SetGroup(State: Boolean);
    function GetGroup(): Boolean;

    function IsOperation(): Boolean;

    procedure CheckOperations();

    procedure ShowEvent();
    function IsSymbolSymbolEvent(Pos: TPoint): Boolean;
    function IsSymbolTextEvent(Pos: TPoint): Boolean;
    function IsSymbolSeparVertEvent(Pos: TPoint): Boolean;
    function IsSymbolSeparHorEvent(Pos: TPoint): Boolean;
    function IsSymbolKPopiskyJCClickSoupravyEvent(Pos: TPoint): Boolean;
    procedure NullOperationsEvent();
    procedure MoveActivateEvent();
    procedure DeleteActivateEvent();
    function IsOperationEvent(): Boolean;
    procedure ChangeTextEvent(Sender: TObject; var popisek: TPopisek);
    class procedure BpnlReadAndValidateSeparator(var f: File; where: string);

  public

    symbols: TBitmapSymbols;
    separatorsVert: TVBO;
    separatorsHor: TVBO;
    trackNames: TVBO;
    JCClick: TVBO;
    texts: TText;
    trainPoss: TVBO;

    bitmap: array [0 .. _MAX_WIDTH - 1, 0 .. _MAX_HEIGHT - 1] of ShortInt;

    constructor Create(SymbolIL, TextIL: TImageList; DrawCanvas: TCanvas; Width, Height: Integer; Mode: TMode;
      Parent: TForm; Graphics: TPanelGraphics);
    destructor Destroy(); override;

    procedure BpnlLoad(filename: string; var ORs: string);
    procedure BpnlSave(filename: string; const ORs: string);

    procedure Paint();
    function PaintCursor(CursorPos: TPoint): TCursorDraw;
    procedure PaintMove(CursorPos: TPoint);

    procedure SetSize(aWidth, aHeight: Byte);

    procedure Escape(Group: Boolean);

    procedure Move();
    procedure Delete();

    procedure MouseUp(Position: TPoint; Button: TMouseButton);
    procedure DblClick(Position: TPoint);

    function ImportMyJOP(fn: string; ORs: TList<TOR>): string;

    property fileName: string read mFileName;
    property fileState: TReliefFileState read mFileState;
    property PanelWidth: Integer read mPanelWidth;
    property PanelHeight: Integer read mPanelHeight;
    property Group: Boolean read GetGroup write SetGroup;
    property Mode: TMode read mMode write mMode;

    property OnShow: TNEvent read mOnShow write mOnShow;
    property OnTextEdit: TChangeTextEvent read mOnTextEdit write mOnTextEdit;
    property OnORAsk: TORAskEvent read mORAskEvent write mORAskEvent;
    property ShowBlokPopisky: Boolean read fShowBlokPopisky write fShowBlokPopisky;
  end; // class

implementation

uses ReliefObjects, ownStrUtils;

// nacitani souboru s bitmapovymi daty
procedure TPanelBitmap.BpnlLoad(filename: string; var ORs: string);
var buffer: array [0 .. 5] of Byte;
  aCount: Integer;
begin
  Self.mFileState := fsSaved;
  Self.mFileName := filename;

  Self.Symbols.Clear();
  Self.SeparatorsVert.Clear();
  Self.SeparatorsHor.Clear();
  Self.texts.Clear();

  var f: File;
  AssignFile(f, filename);
  Reset(f, 1);

  try
    BlockRead(f, buffer, 5, aCount);
    if (aCount < 5) then
      raise EFileLoad.Create('Nesprávná délka hlavičky!');

    // --- hlavicka zacatek ---
    // kontrola identifikace
    if ((buffer[0] <> ord('b')) or (buffer[1] <> ord('r'))) then
      raise EFileLoad.Create('Nesprávná identifikace v hlavičce!');

    // kontrola verze
    var version: Byte := buffer[2];
    if ((version <> $21) and (version <> $30) and (version <> $31) and (version <> $32) and (version <> $40) and (version <> $41)) then
      Application.MessageBox(PChar('Otevíráte soubor s verzí ' + IntToHex(version,
        2) + ', která není aplikací plně podporována!'), 'Varování', MB_OK OR MB_ICONWARNING);

    Self.mPanelWidth := buffer[3];
    Self.mPanelHeight := buffer[4];

    Self.BpnlReadAndValidateSeparator(f, 'mezi hlavičkou a bitmapovými daty');

    // --- hlavicka konec ---
    // -------------------------------------------
    // nacitani bitmapovych dat

    Self.Symbols.LoadBpnl(f, version, Self.PanelWidth, Self.PanelHeight);
    Self.BpnlReadAndValidateSeparator(f, 'mezi bitmapovými daty a popisky');

    // -------------------------------------------

    if (version >= $32) then
    begin
      // nacteni poctu popisku
      BlockRead(f, buffer, 2, aCount);
      var len := (buffer[0] shl 8) + buffer[1];

      // nacitani popisku
      var bytesBuf: TBytes;
      SetLength(bytesBuf, len);
      BlockRead(f, bytesBuf[0], len, aCount);
      Self.texts.SetLoadedDataV32(bytesBuf);
    end else begin
      // nacteni poctu popisku
      BlockRead(f, buffer, 1, aCount);
      var len := buffer[0] * ReliefText._Block_Length;

      // nacitani popisku
      var bytesBuf: TBytes;
      SetLength(bytesBuf, len);
      BlockRead(f, bytesBuf[0], len, aCount);
      Self.texts.SetLoadedData(bytesBuf);
    end;

    Self.BpnlReadAndValidateSeparator(f, 'mezi popisky a separátory');

    // -------------------------------------------

    Self.SeparatorsVert.LoadBpnl(f, version);
    Self.BpnlReadAndValidateSeparator(f, 'za separátory');

    // -------------------------------------------

    if (version >= $30) then
    begin
      Self.SeparatorsHor.LoadBpnl(f, version);
      Self.BpnlReadAndValidateSeparator(f, 'mezi hor. separátory a kpopisky');
    end else begin
      Self.SeparatorsHor.Clear();
    end;

    // -------------------------------------------

    Self.trackNames.LoadBpnl(f, version);
    Self.BpnlReadAndValidateSeparator(f, 'mezi kolejovými popisky a JCClick');

    // -------------------------------------------

    Self.JCClick.LoadBpnl(f, version);
    Self.BpnlReadAndValidateSeparator(f, 'mezi JCClick a pozicemi pro soupravy');

    // -------------------------------------------

    if (version >= $31) then
    begin
      Self.trainPoss.LoadBpnl(f, version);
      Self.BpnlReadAndValidateSeparator(f, 'mezi pozicemi pro soupravy a oblastmi řízení');
    end;

    // -------------------------------------------

    // nacitani oblasti rizeni
    ORs := '';

    // precteme delku
    BlockRead(f, buffer, 2, aCount);

    // pokud je delka 0, je neco spatne
    if (aCount = 0) then
      raise EFileLoad.Create('Prázdné oblasti řízení!');

    var len := (buffer[0] shl 8) + buffer[1];

    var bytesBuf: TBytes;
    SetLength(bytesBuf, len);
    BlockRead(f, bytesBuf[0], len, aCount);
    ORs := TEncoding.UTF8.GetString(bytesBuf, 0, aCount);
  finally
    CloseFile(f);
  end;
end;

class procedure TPanelBitmap.BpnlReadAndValidateSeparator(var f: File; where: string);
var buf: array [0..1] of Byte;
    count: Integer;
begin
  BlockRead(f, buf, 2, count);
  if ((count < 2) or (buf[0] <> $FF) or (buf[1] <> $FF)) then
    raise EFileLoad.Create('Chybí oddělovací sekvence '+where+'!');
end;

procedure TPanelBitmap.BpnlSave(filename: string; const ORs: string);
var buffer: array [0 .. 4] of Byte;
  separator: array [0 .. 1] of Byte;
begin
  Self.mFileState := fsSaved;
  Self.mFileName := filename;

  separator[0] := $FF;
  separator[1] := $FF;

  var f: File;
  AssignFile(f, filename);
  Rewrite(f, 1);

  try
    // --- hlavicka ---
    // identifikace
    buffer[0] := ord('b');
    buffer[1] := ord('r');
    // verze
    buffer[2] := $41;
    // sirka a vyska
    buffer[3] := Self.PanelWidth;
    buffer[4] := Self.PanelHeight;

    BlockWrite(f, buffer, 5);
    BlockWrite(f, separator, 2);

    // -------------------------------------------
    // bitmapova data
    Self.Symbols.WriteBpnl(f);
    BlockWrite(f, separator, 2);

    // -------------------------------------------
    // popisky
    var bytesBuf := Self.texts.GetSaveData;
    BlockWrite(f, bytesBuf[0], Length(bytesBuf));
    BlockWrite(f, separator, 2);

    // -------------------------------------------
    // vertikalni oddelovace
    Self.SeparatorsVert.WriteBpnl(f);
    BlockWrite(f, separator, 2);

    // -------------------------------------------
    // horizontalni oddelovace
    Self.SeparatorsHor.WriteBpnl(f);
    BlockWrite(f, separator, 2);

    // -------------------------------------------
    // KPopisky
    Self.trackNames.WriteBpnl(f);
    BlockWrite(f, separator, 2);

    // -------------------------------------------
    // JCClick
    Self.JCClick.WriteBpnl(f);
    BlockWrite(f, separator, 2);

    // -------------------------------------------
    // soupravy
    Self.trainPoss.WriteBpnl(f);
    BlockWrite(f, separator, 2);

    // -------------------------------------------
    var len := TEncoding.UTF8.GetByteCount(ORs);
    SetLength(bytesBuf, len);

    // delka zpravy
    buffer[0] := hi(len);
    buffer[1] := lo(len);
    BlockWrite(f, buffer, 2);

    bytesBuf := TEncoding.UTF8.GetBytes(ORs);
    BlockWrite(f, bytesBuf[0], len);
    BlockWrite(f, separator, 2);

    // -------------------------------------------
  finally
    CloseFile(f);
  end;
end;

procedure TPanelBitmap.SetSize(aWidth, aHeight: Byte);
begin
  if (Self.IsOperation) then
    raise Exception.Create('Právě probíhá jiná operace!');

  Self.mPanelWidth := aWidth;
  Self.mPanelHeight := aHeight;

  Self.Symbols.SetRozmery(aWidth, aHeight);
end;

constructor TPanelBitmap.Create(SymbolIL, TextIL: TImageList; DrawCanvas: TCanvas; Width, Height: Integer; Mode: TMode;
  Parent: TForm; Graphics: TPanelGraphics);
begin
  inherited Create;

  Self.fShowBlokPopisky := true;
  Self.Mode := Mode;
  Self.Graphics := Graphics;

  Self.Symbols := TBitmapSymbols.Create(SymbolIL, DrawCanvas, Width, Height);
  Self.Symbols.FOnShow := Self.ShowEvent;
  Self.Symbols.FIsSymbol := Self.IsSymbolSymbolEvent;
  Self.Symbols.FNullOperations := Self.NullOperationsEvent;
  Self.Symbols.FMoveActivate := Self.MoveActivateEvent;
  Self.Symbols.FDeleteActivate := Self.DeleteActivateEvent;
  Self.Symbols.FOPAsk := Self.IsOperationEvent;

  Self.SeparatorsVert := TVBO.Create(DrawCanvas, SymbolIL, _S_SEPAR_VERT, scRed, stVert);
  Self.SeparatorsVert.OnShow := Self.ShowEvent;
  Self.SeparatorsVert.IsSymbol := Self.IsSymbolSeparVertEvent;
  Self.SeparatorsVert.OnNullOperations := Self.NullOperationsEvent;
  Self.SeparatorsVert.OnMoveActivate := Self.MoveActivateEvent;
  Self.SeparatorsVert.OnDeleteActivate := Self.DeleteActivateEvent;
  Self.SeparatorsVert.IsOp := Self.IsOperationEvent;

  Self.SeparatorsHor := TVBO.Create(DrawCanvas, SymbolIL, _S_SEPAR_HOR, scRed, stHor);
  Self.SeparatorsHor.OnShow := Self.ShowEvent;
  Self.SeparatorsHor.IsSymbol := Self.IsSymbolSeparHorEvent;
  Self.SeparatorsHor.OnNullOperations := Self.NullOperationsEvent;
  Self.SeparatorsHor.OnMoveActivate := Self.MoveActivateEvent;
  Self.SeparatorsHor.OnDeleteActivate := Self.DeleteActivateEvent;
  Self.SeparatorsHor.IsOp := Self.IsOperationEvent;

  Self.trackNames := TVBO.Create(DrawCanvas, SymbolIL, _S_FULL, scYellow);
  Self.trackNames.OnShow := Self.ShowEvent;
  Self.trackNames.IsSymbol := Self.IsSymbolKPopiskyJCClickSoupravyEvent;
  Self.trackNames.OnNullOperations := Self.NullOperationsEvent;
  Self.trackNames.OnMoveActivate := Self.MoveActivateEvent;
  Self.trackNames.OnDeleteActivate := Self.DeleteActivateEvent;
  Self.trackNames.IsOp := Self.IsOperationEvent;

  Self.JCClick := TVBO.Create(DrawCanvas, SymbolIL, _S_KC, scLime);
  Self.JCClick.OnShow := Self.ShowEvent;
  Self.JCClick.IsSymbol := Self.IsSymbolKPopiskyJCClickSoupravyEvent;
  Self.JCClick.OnNullOperations := Self.NullOperationsEvent;
  Self.JCClick.OnMoveActivate := Self.MoveActivateEvent;
  Self.JCClick.OnDeleteActivate := Self.DeleteActivateEvent;
  Self.JCClick.IsOp := Self.IsOperationEvent;

  Self.trainPoss := TVBO.Create(DrawCanvas, SymbolIL, _S_FULL, scBlue);
  Self.trainPoss.OnShow := Self.ShowEvent;
  Self.trainPoss.IsSymbol := Self.IsSymbolKPopiskyJCClickSoupravyEvent;
  Self.trainPoss.OnNullOperations := Self.NullOperationsEvent;
  Self.trainPoss.OnMoveActivate := Self.MoveActivateEvent;
  Self.trainPoss.OnDeleteActivate := Self.DeleteActivateEvent;
  Self.trainPoss.IsOp := Self.IsOperationEvent;

  Self.texts := TText.Create(DrawCanvas, TextIL, Parent, Graphics);
  Self.texts.FOnShow := Self.ShowEvent;
  Self.texts.FIsSymbol := Self.IsSymbolTextEvent;
  Self.texts.FNullOperations := Self.NullOperationsEvent;
  Self.texts.FMoveActivate := Self.MoveActivateEvent;
  Self.texts.FDeleteActivate := Self.DeleteActivateEvent;
  Self.texts.FOPAsk := Self.IsOperationEvent;
  Self.texts.FOnChangeText := Self.ChangeTextEvent;

  Self.SetSize(Width, Height);
end;

destructor TPanelBitmap.Destroy;
begin
  FreeAndNil(Self.Symbols);
  FreeAndNil(Self.SeparatorsVert);
  FreeAndNil(Self.SeparatorsHor);
  FreeAndNil(Self.trackNames);
  FreeAndNil(Self.JCClick);
  FreeAndNil(Self.trainPoss);
  FreeAndNil(Self.texts);

  inherited;
end;

// vykresleni vseho
procedure TPanelBitmap.Paint();
begin
  Self.JCClick.Paint();
  Self.trainPoss.Paint();
  Self.trackNames.Paint();
  Self.Symbols.Paint();
  Self.SeparatorsVert.Paint();
  Self.SeparatorsHor.Paint();
  Self.texts.Paint(Self.fShowBlokPopisky);
end;

procedure TPanelBitmap.MouseUp(Position: TPoint; Button: TMouseButton);
begin
  Self.operations.disable := true;

  case (Self.Mode) of
    dmBitmap:
      begin
        Self.texts.MouseUp(Position, Button);
        Self.trackNames.MouseUp(Position, Button);
        Self.JCClick.MouseUp(Position, Button);
        Self.trainPoss.MouseUp(Position, Button);
        Self.Symbols.MouseUp(Position, Button);
      end;

    dmSepHor:
      Self.SeparatorsHor.MouseUp(Position, Button);
    dmSepVert:
      Self.SeparatorsVert.MouseUp(Position, Button);
  end; // case

  Self.operations.disable := false;
  Self.CheckOperations();
end;

procedure TPanelBitmap.DblClick(Position: TPoint);
begin

end;

procedure TPanelBitmap.ShowEvent();
begin
  if (Assigned(Self.OnShow)) then
    Self.OnShow();
end;

function TPanelBitmap.IsSymbolSymbolEvent(Pos: TPoint): Boolean;
begin
  Result := false;

  if (Self.Symbols.GetSymbol(Pos) <> -1) then
    Exit(true);
  // Symbol (platform) could be placed over text
  if (Assigned(Self.OnORAsk)) then
    Exit(Self.OnORAsk(Pos));
end;

function TPanelBitmap.IsSymbolTextEvent(Pos: TPoint): Boolean;
begin
  Result := false;

  // Text could be placed over platform
  var symbol := Self.Symbols.GetSymbol(Pos);
  if ((symbol <> -1) and ((symbol < _S_PLATFORM_B) or (symbol > _S_PLATFORM_E))) then
    Exit(true);
  if (Self.texts.GetPopisek(Pos) <> -1) then
    Exit(true);
  if (Assigned(Self.OnORAsk)) then
    Exit(Self.OnORAsk(Pos));
end;

function TPanelBitmap.IsSymbolSeparVertEvent(Pos: TPoint): Boolean;
begin
  Result := (Self.SeparatorsVert.GetObject(Pos) <> -1);
end;

function TPanelBitmap.IsSymbolSeparHorEvent(Pos: TPoint): Boolean;
begin
  Result := (Self.SeparatorsHor.GetObject(Pos) <> -1);
end;

function TPanelBitmap.IsSymbolKPopiskyJCClickSoupravyEvent(Pos: TPoint): Boolean;
begin
  Result := (Self.trackNames.GetObject(Pos) <> -1) or (Self.JCClick.GetObject(Pos) <> -1) or (Self.trainPoss.GetObject(Pos) <> -1);
end;

procedure TPanelBitmap.NullOperationsEvent();
begin
  Self.Escape(false);
end;

procedure TPanelBitmap.MoveActivateEvent();
begin
  if (Self.operations.disable) then
    Self.operations.opType := botMove
  else
    Self.Move();
end;

procedure TPanelBitmap.DeleteActivateEvent();
begin
  if (Self.operations.disable) then
    Self.operations.opType := botDelete
  else
    Self.Delete();
end;

function TPanelBitmap.IsOperationEvent(): Boolean;
begin
  Result := Self.IsOperation();
end;

procedure TPanelBitmap.SetGroup(State: Boolean);
begin
  if (Assigned(Self.Symbols)) then
    Self.Symbols.Group := State;
end;

function TPanelBitmap.GetGroup(): Boolean;
begin
  Result := false;
  if (Assigned(Self.Symbols)) then
    Result := Self.Symbols.Group;
end;

procedure TPanelBitmap.Escape(Group: Boolean);
begin
  if (Assigned(Self.Symbols)) then
    Self.Symbols.Escape(Group);
  if (Assigned(Self.SeparatorsVert)) then
    Self.SeparatorsVert.Escape();
  if (Assigned(Self.SeparatorsHor)) then
    Self.SeparatorsHor.Escape();
  if (Assigned(Self.trackNames)) then
    Self.trackNames.Escape();
  if (Assigned(Self.JCClick)) then
    Self.JCClick.Escape();
  if (Assigned(Self.trainPoss)) then
    Self.trainPoss.Escape();
  if (Assigned(Self.texts)) then
    Self.texts.Escape();
end;

function TPanelBitmap.PaintCursor(CursorPos: TPoint): TCursorDraw;
var Return: array [0 .. 6] of TCursorDraw;
begin
  Return[0] := Self.Symbols.PaintCursor(CursorPos);
  Return[1] := Self.SeparatorsVert.PaintCursor(CursorPos);
  Return[2] := Self.SeparatorsHor.PaintCursor(CursorPos);
  Return[3] := Self.trackNames.PaintCursor(CursorPos);
  Return[4] := Self.JCClick.PaintCursor(CursorPos);
  Return[5] := Self.texts.PaintCursor(CursorPos);
  Return[6] := Self.trainPoss.PaintCursor(CursorPos);

  if (Self.Mode = dmSepVert) then
  begin
    for var i: Integer := 0 to 6 do
    begin
      Return[i].Pos1.X := Return[i].Pos1.X + (_Symbol_Sirka div 2);
      Return[i].Pos2.X := Return[i].Pos2.X + (_Symbol_Sirka div 2);
    end;
  end else begin
    if (Self.Mode = dmSepHor) then
      for var i: Integer := 0 to 6 do
      begin
        Return[i].Pos1.Y := Return[i].Pos1.Y + (_Symbol_Vyska div 2);
        Return[i].Pos2.Y := Return[i].Pos2.Y + (_Symbol_Vyska div 2);
      end;
  end;

  // zde se zajistuje vytvoreni 1 barvy a pozice kurzoru z celkem 5 pozic a barev - osetreni priorit

  // pokud jsou vsechny stejne
  var value: TCursorDraw  := Return[0];
  var AllSame: Boolean := true;
  for var i: Integer := 1 to Length(Return)-1 do
  begin
    if ((Return[i].Color <> value.Color) or (Return[i].Pos1.X <> value.Pos1.X) or (Return[i].Pos1.Y <> value.Pos1.Y) or
      (Return[i].Pos2.X <> value.Pos2.X) or (Return[i].Pos2.Y <> value.Pos2.Y)) then
    begin
      AllSame := false;
      Break;
    end;
  end;
  if (AllSame) then
    Exit(value);

  // pokud je 1 OnObject
  for var i: Integer := 0 to Length(Return)-1 do
    if (Return[i].color = TCursorColor.ccOnObject) then
      Exit(Return[i]);

  // pokud je 1 Operation
  for var i: Integer := 0 to Length(Return)-1 do
    if (Return[i].Color = TCursorColor.ccActiveOperation) then
      Exit(Return[i]);
end;

function TPanelBitmap.IsOperation(): Boolean;
begin
  if ((Self.Symbols.addStep <> gosNone) or (Self.Symbols.moveStep > gosActive) or
    (Self.SeparatorsVert.addStep <> gosNone) or (Self.SeparatorsVert.moveStep > gosActive) or
    (Self.SeparatorsHor.addStep <> gosNone) or (Self.SeparatorsHor.moveStep > gosActive) or
    (Self.trackNames.addStep <> gosNone) or (Self.trackNames.moveStep > gosActive) or
    (Self.JCClick.addStep <> gosNone) or (Self.JCClick.moveStep > gosActive) or
    (Self.trainPoss.addStep <> gosNone) or (Self.trainPoss.moveStep > gosActive) or
    (Self.texts.addStep <> gosNone) or (Self.texts.moveStep > gosActive)) then
    Result := true
  else
    Result := false;
end;

procedure TPanelBitmap.Move();
begin
  case (Self.Mode) of
    dmBitmap:
      begin
        if (Assigned(Self.Symbols)) then
          Self.Symbols.Move;
        if (Assigned(Self.trackNames)) then
          Self.trackNames.Move;
        if (Assigned(Self.JCClick)) then
          Self.JCClick.Move;
        if (Assigned(Self.trainPoss)) then
          Self.trainPoss.Move;
        if (Assigned(Self.texts)) then
          Self.texts.Move;
      end;

    dmSepHor:
      if (Assigned(Self.SeparatorsHor)) then
        Self.SeparatorsHor.Move;
    dmSepVert:
      if (Assigned(Self.SeparatorsVert)) then
        Self.SeparatorsVert.Move;
  end;
end;

procedure TPanelBitmap.Delete();
begin
  case (Self.Mode) of
    dmBitmap:
      begin
        if (Assigned(Self.Symbols)) then
          Self.Symbols.Delete();
        if (Assigned(Self.trackNames)) then
          Self.trackNames.Delete();
        if (Assigned(Self.JCClick)) then
          Self.JCClick.Delete();
        if (Assigned(Self.trainPoss)) then
          Self.trainPoss.Delete();
        if (Assigned(Self.texts)) then
          Self.texts.Delete();
      end;
    dmSepHor:
      if (Assigned(Self.SeparatorsHor)) then
        Self.SeparatorsHor.Delete();
    dmSepVert:
      if (Assigned(Self.SeparatorsVert)) then
        Self.SeparatorsVert.Delete();
  end;
end;

procedure TPanelBitmap.PaintMove(CursorPos: TPoint);
begin
  if (Assigned(Self.Symbols)) then
    Self.Symbols.PaintBitmapMove(CursorPos);
  if (Assigned(Self.SeparatorsVert)) then
    Self.SeparatorsVert.PaintMove(CursorPos);
  if (Assigned(Self.SeparatorsHor)) then
    Self.SeparatorsHor.PaintMove(CursorPos);
  if (Assigned(Self.trackNames)) then
    Self.trackNames.PaintMove(CursorPos);
  if (Assigned(Self.JCClick)) then
    Self.JCClick.PaintMove(CursorPos);
  if (Assigned(Self.trainPoss)) then
    Self.trainPoss.PaintMove(CursorPos);
  if (Assigned(Self.texts)) then
    Self.texts.PaintTextMove(CursorPos);
end;

procedure TPanelBitmap.CheckOperations();
begin
  if ((not Self.operations.disable) and (Self.operations.opType <> botNone)) then
  begin
    case (Self.operations.opType) of
      botMove: Self.Move();
      botDelete: Self.Delete();
    end;
    Self.operations.opType := botNone;
  end;
end;

procedure TPanelBitmap.ChangeTextEvent(Sender: TObject; var popisek: TPopisek);
begin
  if (Assigned(Self.OnTextEdit)) then
    Self.OnTextEdit(Self, popisek);
end;

/// ////////////////////////////////////////////////////////////////////////////

function TPanelBitmap.ImportMyJOP(fn: string; ORs: TList<TOR>): string;
var f: TextFile;
  splitted, gsplitted: TStrings;
  linei, eloaded: Integer;
  g: TDictionary<Integer, string>;
const
  OFFSET_X = 0;
  OFFSET_Y = 0;
begin
  Result := 'INFO: OFFSET_X:' + IntToStr(OFFSET_X) + ', OFFSET_Y:' + IntToStr(OFFSET_Y) + #13#10;

  eloaded := 0;
  Self.mFileState := fsSaved;
  AssignFile(f, fn);
  Reset(f);
  ORs.Clear();

  g := TDictionary<Integer, string>.Create();
  splitted := TStringList.Create();
  gsplitted := TStringList.Create();
  linei := 1;
  try
    while (not Eof(f)) do
    begin
      var line: string;
      ReadLn(f, line);

      splitted.Clear();
      ExtractStringsEx([';'], [], line, splitted);

      if (splitted[0] = 'JOP') then
      begin
        Self.mPanelWidth := StrToInt(splitted[4]) + OFFSET_X;
        Self.mPanelHeight := StrToInt(splitted[5]) + OFFSET_Y;
        Self.Symbols.SetRozmery(Self.mPanelWidth, Self.mPanelHeight);
        Result := 'INFO: Width: ' + IntToStr(Self.mPanelWidth) + ', height: ' + IntToStr(Self.mPanelHeight) + #13#10;

      end else if (splitted[0] = 'G') then
      begin
        g.AddOrSetValue(StrToInt(splitted[1]), line);
      end else if (splitted[0] = 'E') then
      begin
        var X := StrToInt(splitted[5]) + OFFSET_X;
        var Y := StrToInt(splitted[6]) + OFFSET_Y;
        var popx := StrToInt(splitted[8]) + OFFSET_X;
        var popy := StrToInt(splitted[9]) + OFFSET_Y;

        if (splitted[3] = '1') then
          Self.Symbols.Bitmap[X][Y] := _S_TRACK_DET_B + StrToInt(splitted[12])
        else if (splitted[3] = '2') then
          Self.Symbols.Bitmap[X][Y] := _S_TRACK_DET_B + 6 + StrToInt(splitted[12])
        else if (splitted[3] = '3') then
          Self.Symbols.Bitmap[X][Y] := _S_BUMPER_R + StrToInt(splitted[12])
        else if (splitted[3] = '4') then
          Self.Symbols.Bitmap[X][Y] := _S_SIGNAL_B + StrToInt(splitted[12])
        else if (splitted[3] = '5') then
          Self.Symbols.Bitmap[X][Y] := _S_SIGNAL_B + 4 + StrToInt(splitted[12])
        else if (splitted[3] = '6') then
        begin
          Self.Symbols.Bitmap[X][Y] := _S_TURNOUT_B + StrToInt(splitted[12]);
          try
            Self.texts.AddToStructure(Point(popx, popy), splitted[7], scWhite, true);
          except
            Result := Result + 'WARN: nepodařilo se přidat popisek bloku ' + splitted[7] + #13#10;
          end;
        end else if (splitted[3] = '10') then
          Self.Symbols.Bitmap[X][Y] := _S_LOCK
        else if (splitted[3] = '7') then
        begin
          if (splitted[12] = '0') then
            Self.Symbols.Bitmap[X][Y] := _S_DERAIL_B
          else
            Self.Symbols.Bitmap[X][Y] := _S_DERAIL_B + 1;
          try
            Self.texts.AddToStructure(Point(popx, popy), splitted[7], scWhite, true);
          except
            Result := Result + 'WARN: nepodařilo se přidat popisek bloku ' + splitted[7] + #13#10;
          end;
        end else if (splitted[3] = '11') then
        begin
          var Width := StrToInt(splitted[15]);
          for var i := 0 to Width - 1 do
            Self.Symbols.Bitmap[X + i][Y] := _S_PLATFORM_B + StrToInt(splitted[12])
        end else if (splitted[3] = '20') then
          Self.Symbols.Bitmap[X][Y] := _S_LINKER_B
        else if (splitted[3] = '12') then
          Self.Symbols.Bitmap[X][Y] := _S_DISC_TRACK
        else if (splitted[3] = '21') then
          Self.Symbols.Bitmap[X][Y] := _S_LINKER_TRAIN
        else if (splitted[3] = '30') then
        begin
          var gref := StrToInt(splitted[17]);
          gsplitted.Clear();
          ExtractStringsEx([';'], [], g[gref], gsplitted);

          var OblR: TOR;
          OblR.Name := gsplitted[7];
          OblR.id := gsplitted[9];
          OblR.ShortName := gsplitted[9];
          OblR.Poss.DK := Point(X, Y);
          OblR.Poss.Queue := Point(OblR.Poss.DK.X + 10, OblR.Poss.DK.Y);
          OblR.Poss.Time := Point(OblR.Poss.DK.X + 10, OblR.Poss.DK.Y + 1);

          if ((gsplitted[12] = '0') and (gsplitted[13] = '0')) then
          begin
            popx := StrToInt(gsplitted[10]) + OFFSET_X;
            popy := StrToInt(gsplitted[11]) + OFFSET_Y;
          end else begin
            popx := StrToInt(gsplitted[12]) + OFFSET_X;
            popy := StrToInt(gsplitted[13]) + OFFSET_Y;
          end;

          try
            Self.texts.AddToStructure(Point(popx, popy), OblR.Name, scWhite, false);
          except
            Result := Result + 'WARN: nepodařilo se přidat název OŘ ' + OblR.Name + #13#10;
          end;
          ORs.Add(OblR);
        end else if (splitted[3] = '50') then
        begin
          var Height := StrToInt(splitted[15]);
          for var i := 0 to Height - 1 do
            Self.Symbols.Bitmap[X][Y + i] := _S_CROSSING
        end else if (splitted[3] = '80') then
        begin
          try
            Self.texts.AddToStructure(Point(X, Y), splitted[7], scGray, false);
          except
            Result := Result + 'WARN: nepodařilo se přidat popisek bloku ' + splitted[7] + #13#10;
          end;
        end else begin
          Result := Result + 'WARN: řádek ' + IntToStr(linei) + ': nerozpoznaný typ bloku: ' + splitted[3] + #13#10;
          Dec(eloaded);
        end;

        if ((splitted[3] = '0') or (splitted[3] = '1')) then
        begin
          if (StrToBoolDef(splitted[22], false)) then
            Self.SeparatorsVert.Add(Point(X, Y));
          if (StrToBoolDef(splitted[23], false)) then
            Self.SeparatorsHor.Add(Point(X, Y));
          if (StrToBoolDef(splitted[35], false)) then
          begin
            Self.trackNames.Add(Point(X, Y));
            Self.JCClick.Add(Point(X - 1, Y));
            Self.JCClick.Add(Point(X + 1, Y));
          end;
        end;
        if ((splitted[3] = '4') or (splitted[3] = '5') or (splitted[3] = '10')) then
        begin
          var gref := StrToInt(splitted[17]);
          gsplitted.Clear();
          ExtractStringsEx([';'], [], g[gref], gsplitted);
          if ((gsplitted[12] = '0') and (gsplitted[13] = '0')) then
          begin
            popx := StrToInt(gsplitted[10]) + OFFSET_X;
            popy := StrToInt(gsplitted[11]) + OFFSET_Y;
          end else begin
            popx := StrToInt(gsplitted[12]) + OFFSET_X;
            popy := StrToInt(gsplitted[13]) + OFFSET_Y;
          end;
          try
            Self.texts.AddToStructure(Point(popx, popy), gsplitted[8], scGray, true);
          except
            Result := Result + 'WARN: nepodařilo se přidat popisek bloku ' + gsplitted[8] + #13#10;
          end;
        end;

        Inc(eloaded);
      end;

      Inc(linei);
    end;
  finally
    splitted.Free();
    gsplitted.Free();
    g.Free();
  end;

  Close(f);
  Result := Result + 'INFO: Načteno ' + IntToStr(eloaded) + ' E řádků.' + #13#10;
end;

/// ////////////////////////////////////////////////////////////////////////////

end.// unit
