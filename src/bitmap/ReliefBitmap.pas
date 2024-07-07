﻿unit ReliefBitmap;
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

  TPanelBitmap = class
  private const

  private
    FPanelWidth, FPanelHeight: Integer;
    FMode: TMode;
    FSoubor: string;

    Operations: record
      Disable: Boolean;
      OpType: Byte;
    end;

    mFileState: TReliefFileState;
    Graphics: TPanelGraphics;
    fShowBlokPopisky: Boolean;

    FORAskEvent: TORAskEvent;
    FOnShow: TNEvent;
    FOnTextEdit: TChangeTextEvent;

    procedure SetGroup(State: Boolean);
    function GetGroup: Boolean;

    function IsOperation: Boolean;

    procedure CheckOperations;

    procedure ShowEvent;
    function IsSymbolSymbolEvent(Pos: TPoint): Boolean;
    function IsSymbolTextEvent(Pos: TPoint): Boolean;
    function IsSymbolSeparVertEvent(Pos: TPoint): Boolean;
    function IsSymbolSeparHorEvent(Pos: TPoint): Boolean;
    function IsSymbolKPopiskyJCClickSoupravyEvent(Pos: TPoint): Boolean;
    procedure NullOperationsEvent;
    procedure MoveActivateEvent;
    procedure DeleteActivateEvent;
    function IsOperationEvent: Boolean;
    procedure ChangeTextEvent(Sender: TObject; var popisek: TPopisek);
    class procedure BpnlReadAndValidateSeparator(var f: File; where: string);

  public

    Symbols: TBitmapSymbols;
    SeparatorsVert: TVBO;
    SeparatorsHor: TVBO;
    KPopisky: TVBO;
    JCClick: TVBO;
    Text: TText;
    Soupravy: TVBO;

    Bitmap: array [0 .. _MAX_WIDTH - 1, 0 .. _MAX_HEIGHT - 1] of ShortInt;

    constructor Create(SymbolIL, TextIL: TImageList; DrawCanvas: TCanvas; Width, Height: Integer; Mode: TMode;
      Parent: TForm; Graphics: TPanelGraphics);
    destructor Destroy; override;

    procedure BpnlLoad(aFile: string; var ORs: string);
    procedure BpnlSave(aFile: string; const ORs: string);

    procedure Paint();
    function PaintCursor(CursorPos: TPoint): TCursorDraw;
    procedure PaintMove(CursorPos: TPoint);

    procedure SetRozmery(aWidth, aHeight: Byte);

    procedure Escape(Group: Boolean);
    procedure ResetPanel;

    procedure Move();
    procedure Delete();

    procedure MouseUp(Position: TPoint; Button: TMouseButton);
    procedure DblClick(Position: TPoint);

    function ImportMyJOP(fn: string; ORs: TList<TOR>): string;

    property Soubor: string read FSoubor;
    property fileState: TReliefFileState read mFileState;
    property PanelWidth: Integer read FPanelWidth;
    property PanelHeight: Integer read FPanelHeight;
    property Group: Boolean read GetGroup write SetGroup;
    property Mode: TMode read FMode write FMode;

    property OnShow: TNEvent read FOnShow write FOnShow;
    property OnTextEdit: TChangeTextEvent read FOnTextEdit write FOnTextEdit;
    property OnORAsk: TORAskEvent read FORAskEvent write FORAskEvent;
    property ShowBlokPopisky: Boolean read fShowBlokPopisky write fShowBlokPopisky;
  end; // class

implementation

uses ReliefObjects, ownStrUtils;

// nacitani souboru s bitmapovymi daty
procedure TPanelBitmap.BpnlLoad(aFile: string; var ORs: string);
var buffer: array [0 .. 5] of Byte;
  aCount: Integer;
begin
  Self.mFileState := fsSaved;
  Self.FSoubor := aFile;

  Self.Symbols.Clear();
  Self.SeparatorsVert.Clear();
  Self.SeparatorsHor.Clear();
  Self.Text.Clear();

  var f: File;
  AssignFile(f, aFile);
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

    Self.FPanelWidth := buffer[3];
    Self.FPanelHeight := buffer[4];

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
      Self.Text.SetLoadedDataV32(bytesBuf);
    end else begin
      // nacteni poctu popisku
      BlockRead(f, buffer, 1, aCount);
      var len := buffer[0] * ReliefText._Block_Length;

      // nacitani popisku
      var bytesBuf: TBytes;
      SetLength(bytesBuf, len);
      BlockRead(f, bytesBuf[0], len, aCount);
      Self.Text.SetLoadedData(bytesBuf);
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

    Self.KPopisky.LoadBpnl(f, version);
    Self.BpnlReadAndValidateSeparator(f, 'mezi kolejovými popisky a JCClick');

    // -------------------------------------------

    Self.JCClick.LoadBpnl(f, version);
    Self.BpnlReadAndValidateSeparator(f, 'mezi JCClick a pozicemi pro soupravy');

    // -------------------------------------------

    if (version >= $31) then
    begin
      Self.Soupravy.LoadBpnl(f, version);
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

procedure TPanelBitmap.BpnlSave(aFile: string; const ORs: string);
var buffer: array [0 .. 4] of Byte;
  separator: array [0 .. 1] of Byte;
begin
  Self.mFileState := fsSaved;
  Self.FSoubor := aFile;

  separator[0] := $FF;
  separator[1] := $FF;

  var f: File;
  AssignFile(f, aFile);
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
    var bytesBuf := Self.Text.GetSaveData;
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
    Self.KPopisky.WriteBpnl(f);
    BlockWrite(f, separator, 2);

    // -------------------------------------------
    // JCClick
    Self.JCClick.WriteBpnl(f);
    BlockWrite(f, separator, 2);

    // -------------------------------------------
    // soupravy
    Self.Soupravy.WriteBpnl(f);
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

procedure TPanelBitmap.SetRozmery(aWidth, aHeight: Byte);
begin
  if (Self.IsOperation) then
    raise Exception.Create('Právě probíhá jiná operace!');

  Self.FPanelWidth := aWidth;
  Self.FPanelHeight := aHeight;

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

  Self.KPopisky := TVBO.Create(DrawCanvas, SymbolIL, _S_FULL, scYellow);
  Self.KPopisky.OnShow := Self.ShowEvent;
  Self.KPopisky.IsSymbol := Self.IsSymbolKPopiskyJCClickSoupravyEvent;
  Self.KPopisky.OnNullOperations := Self.NullOperationsEvent;
  Self.KPopisky.OnMoveActivate := Self.MoveActivateEvent;
  Self.KPopisky.OnDeleteActivate := Self.DeleteActivateEvent;
  Self.KPopisky.IsOp := Self.IsOperationEvent;

  Self.JCClick := TVBO.Create(DrawCanvas, SymbolIL, _S_KC, scLime);
  Self.JCClick.OnShow := Self.ShowEvent;
  Self.JCClick.IsSymbol := Self.IsSymbolKPopiskyJCClickSoupravyEvent;
  Self.JCClick.OnNullOperations := Self.NullOperationsEvent;
  Self.JCClick.OnMoveActivate := Self.MoveActivateEvent;
  Self.JCClick.OnDeleteActivate := Self.DeleteActivateEvent;
  Self.JCClick.IsOp := Self.IsOperationEvent;

  Self.Soupravy := TVBO.Create(DrawCanvas, SymbolIL, _S_FULL, scBlue);
  Self.Soupravy.OnShow := Self.ShowEvent;
  Self.Soupravy.IsSymbol := Self.IsSymbolKPopiskyJCClickSoupravyEvent;
  Self.Soupravy.OnNullOperations := Self.NullOperationsEvent;
  Self.Soupravy.OnMoveActivate := Self.MoveActivateEvent;
  Self.Soupravy.OnDeleteActivate := Self.DeleteActivateEvent;
  Self.Soupravy.IsOp := Self.IsOperationEvent;

  Self.Text := TText.Create(DrawCanvas, TextIL, Parent, Graphics);
  Self.Text.FOnShow := Self.ShowEvent;
  Self.Text.FIsSymbol := Self.IsSymbolTextEvent;
  Self.Text.FNullOperations := Self.NullOperationsEvent;
  Self.Text.FMoveActivate := Self.MoveActivateEvent;
  Self.Text.FDeleteActivate := Self.DeleteActivateEvent;
  Self.Text.FOPAsk := Self.IsOperationEvent;
  Self.Text.FOnChangeText := Self.ChangeTextEvent;

  Self.SetRozmery(Width, Height);
end;

destructor TPanelBitmap.Destroy;
begin
  FreeAndNil(Self.Symbols);
  FreeAndNil(Self.SeparatorsVert);
  FreeAndNil(Self.SeparatorsHor);
  FreeAndNil(Self.KPopisky);
  FreeAndNil(Self.JCClick);
  FreeAndNil(Self.Soupravy);
  FreeAndNil(Self.Text);

  inherited;
end;

// vykresleni vseho
procedure TPanelBitmap.Paint();
begin
  Self.JCClick.Paint;
  Self.Soupravy.Paint;
  Self.KPopisky.Paint;
  Self.Symbols.Paint;
  Self.SeparatorsVert.Paint;
  Self.SeparatorsHor.Paint;
  Self.Text.Paint(Self.fShowBlokPopisky);
end;

procedure TPanelBitmap.MouseUp(Position: TPoint; Button: TMouseButton);
begin
  Self.Operations.Disable := true;

  case (Self.Mode) of
    dmBitmap:
      begin
        Self.Text.MouseUp(Position, Button);
        Self.KPopisky.MouseUp(Position, Button);
        Self.JCClick.MouseUp(Position, Button);
        Self.Soupravy.MouseUp(Position, Button);
        Self.Symbols.MouseUp(Position, Button);
      end;

    dmSepHor:
      Self.SeparatorsHor.MouseUp(Position, Button);
    dmSepVert:
      Self.SeparatorsVert.MouseUp(Position, Button);
  end; // case

  Self.Operations.Disable := false;
  Self.CheckOperations;
end;

procedure TPanelBitmap.DblClick(Position: TPoint);
begin

end;

procedure TPanelBitmap.ShowEvent();
begin
  if (Assigned(FOnShow)) then
    FOnShow;
end;

function TPanelBitmap.IsSymbolSymbolEvent(Pos: TPoint): Boolean;
begin
  Result := false;

  if (Self.Symbols.GetSymbol(Pos) <> -1) then
    Exit(true);
  // Symbol (platform) could be placed over text
  if (Assigned(Self.FORAskEvent)) then
    Exit(Self.FORAskEvent(Pos));
end;

function TPanelBitmap.IsSymbolTextEvent(Pos: TPoint): Boolean;
begin
  Result := false;

  // Text could be placed over platform
  var symbol := Self.Symbols.GetSymbol(Pos);
  if ((symbol <> -1) and ((symbol < _S_PLATFORM_B) or (symbol > _S_PLATFORM_E))) then
    Exit(true);
  if (Self.Text.GetPopisek(Pos) <> -1) then
    Exit(true);
  if (Assigned(Self.FORAskEvent)) then
    Exit(Self.FORAskEvent(Pos));
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
  Result := (Self.KPopisky.GetObject(Pos) <> -1) or (Self.JCClick.GetObject(Pos) <> -1) or (Self.Soupravy.GetObject(Pos) <> -1);
end;

procedure TPanelBitmap.NullOperationsEvent();
begin
  Self.Escape(false);
end;

procedure TPanelBitmap.MoveActivateEvent();
begin
  if (Self.Operations.Disable) then
    Self.Operations.OpType := 1
  else
    Self.Move();
end;

procedure TPanelBitmap.DeleteActivateEvent();
begin
  if (Self.Operations.Disable) then
    Self.Operations.OpType := 2
  else
    Self.Delete;
end;

function TPanelBitmap.IsOperationEvent(): Boolean;
begin
  Result := Self.IsOperation;
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
    Self.SeparatorsVert.Escape;
  if (Assigned(Self.SeparatorsHor)) then
    Self.SeparatorsHor.Escape;
  if (Assigned(Self.KPopisky)) then
    Self.KPopisky.Escape;
  if (Assigned(Self.JCClick)) then
    Self.JCClick.Escape;
  if (Assigned(Self.Soupravy)) then
    Self.Soupravy.Escape;
  if (Assigned(Self.Text)) then
    Self.Text.Escape;
end;

procedure TPanelBitmap.ResetPanel();
begin
  if (Assigned(Self.Symbols)) then
    Self.Symbols.Clear();
  if (Assigned(Self.SeparatorsVert)) then
    Self.SeparatorsVert.Clear();
  if (Assigned(Self.SeparatorsHor)) then
    Self.SeparatorsHor.Clear();
  if (Assigned(Self.KPopisky)) then
    Self.KPopisky.Clear();
  if (Assigned(Self.JCClick)) then
    Self.JCClick.Clear();
  if (Assigned(Self.Soupravy)) then
    Self.Soupravy.Clear();
  if (Assigned(Self.Text)) then
    Self.Text.Clear();
end;

function TPanelBitmap.PaintCursor(CursorPos: TPoint): TCursorDraw;
var Return: array [0 .. 6] of TCursorDraw;
begin
  Return[0] := Self.Symbols.PaintCursor(CursorPos);
  Return[1] := Self.SeparatorsVert.PaintCursor(CursorPos);
  Return[2] := Self.SeparatorsHor.PaintCursor(CursorPos);
  Return[3] := Self.KPopisky.PaintCursor(CursorPos);
  Return[4] := Self.JCClick.PaintCursor(CursorPos);
  Return[5] := Self.Text.PaintCursor(CursorPos);
  Return[6] := Self.Soupravy.PaintCursor(CursorPos);

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
    if (Return[i].Color = 2) then
      Exit(Return[i]);

  // pokud je 1 Operation
  for var i: Integer := 0 to Length(Return)-1 do
    if (Return[i].Color = 1) then
      Exit(Return[i]);
end;

function TPanelBitmap.IsOperation(): Boolean;
begin
  if ((Self.Symbols.AddKrok <> 0) or (Self.Symbols.MoveKrok > 1) or (Self.Symbols.DeleteKrok > 1) or
    (Self.SeparatorsVert.AddKrok <> 0) or (Self.SeparatorsVert.MoveKrok > 1) or (Self.SeparatorsVert.DeleteKrok > 1) or
    (Self.SeparatorsHor.AddKrok <> 0) or (Self.SeparatorsHor.MoveKrok > 1) or (Self.SeparatorsHor.DeleteKrok > 1) or
    (Self.KPopisky.AddKrok <> 0) or (Self.KPopisky.MoveKrok > 1) or (Self.KPopisky.DeleteKrok > 1) or
    (Self.JCClick.AddKrok <> 0) or (Self.JCClick.MoveKrok > 1) or (Self.JCClick.DeleteKrok > 1) or
    (Self.Soupravy.AddKrok <> 0) or (Self.Soupravy.MoveKrok > 1) or (Self.Soupravy.DeleteKrok > 1) or
    (Self.Text.AddKrok <> 0) or (Self.Text.MoveKrok > 1) or (Self.Text.DeleteKrok > 1)) then
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
        if (Assigned(Self.KPopisky)) then
          Self.KPopisky.Move;
        if (Assigned(Self.JCClick)) then
          Self.JCClick.Move;
        if (Assigned(Self.Soupravy)) then
          Self.Soupravy.Move;
        if (Assigned(Self.Text)) then
          Self.Text.Move;
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
        if (Assigned(Self.KPopisky)) then
          Self.KPopisky.Delete();
        if (Assigned(Self.JCClick)) then
          Self.JCClick.Delete();
        if (Assigned(Self.Soupravy)) then
          Self.Soupravy.Delete();
        if (Assigned(Self.Text)) then
          Self.Text.Delete();
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
  if (Assigned(Self.KPopisky)) then
    Self.KPopisky.PaintMove(CursorPos);
  if (Assigned(Self.JCClick)) then
    Self.JCClick.PaintMove(CursorPos);
  if (Assigned(Self.Soupravy)) then
    Self.Soupravy.PaintMove(CursorPos);
  if (Assigned(Self.Text)) then
    Self.Text.PaintTextMove(CursorPos);
end;

procedure TPanelBitmap.CheckOperations();
begin
  if ((not Self.Operations.Disable) and (Self.Operations.OpType <> 0)) then
  begin
    case (Self.Operations.OpType) of
      1: Self.Move();
      2: Self.Delete();
    end;
    Self.Operations.OpType := 0;
  end;
end;

procedure TPanelBitmap.ChangeTextEvent(Sender: TObject; var popisek: TPopisek);
begin
  if (Assigned(FOnTextEdit)) then
    FOnTextEdit(Self, popisek);
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
        Self.FPanelWidth := StrToInt(splitted[4]) + OFFSET_X;
        Self.FPanelHeight := StrToInt(splitted[5]) + OFFSET_Y;
        Self.Symbols.SetRozmery(Self.FPanelWidth, Self.FPanelHeight);
        Result := 'INFO: Width: ' + IntToStr(Self.FPanelWidth) + ', height: ' + IntToStr(Self.FPanelHeight) + #13#10;

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
            Self.Text.AddToStructure(Point(popx, popy), splitted[7], scWhite, true);
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
            Self.Text.AddToStructure(Point(popx, popy), splitted[7], scWhite, true);
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
            Self.Text.AddToStructure(Point(popx, popy), OblR.Name, scWhite, false);
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
            Self.Text.AddToStructure(Point(X, Y), splitted[7], scGray, false);
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
            Self.KPopisky.Add(Point(X, Y));
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
            Self.Text.AddToStructure(Point(popx, popy), gsplitted[8], scGray, true);
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
