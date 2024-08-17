unit ReliefBitmapSymbols;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, IniFiles,
  StrUtils, ReliefText, VektorBasedObject, Global, symbolHelper, Types, ReliefCommon;

const
  _NO_SYMBOL = -1;

type
  EInvalidPosition = class(Exception);
  ENonemptyField = class(Exception);
  EEmptyField = class(Exception);
  ENoSymbol = class(Exception);
  EOperationInProgress = class(Exception);

  TBitmapSymbols = class
  private
    mOnShow: TNEvent;
    mIsConflict: TPosAskEvent;
    mNullOperations: TNEvent;
    mOPAsk: TOpAskEvent;

    Panel: record
      Width, Height: Integer;
    end;

    DrawObject: record
      Canvas: TCanvas;
      IL: TImageList;
    end;

    MoveBuf: record
      Width, Height: Cardinal;
      Symbols: array of array of ShortInt;
    end;

    Operations: record
      Add: record
        Step: TGOpStep;
        Symbol: Integer;
      end;

      Group: record
        IsGroup: Boolean;
        Start: TPoint;
      end;
    end; // Operations

    procedure PaintSymbol(X, Y: Integer; index: Integer); overload;
    procedure PaintSymbol(IL: TImageList; X, Y: Integer; index: Integer; color: SymbolColor); overload;

    procedure Add(aPos: TPoint; SymbolID: Integer); overload;
    procedure Delete(aPos: TPoint); overload;

    procedure Adding(Position: TPoint);

    function ControlCursorPos(Pos1, Pos2: TPoint): Boolean;
    procedure CheckOpInProgressAndExcept();

    function IsConflict(pos: TPoint): Boolean;

  public
    Bitmap: array [0 .. _MAX_WIDTH - 1, 0 .. _MAX_HEIGHT - 1] of ShortInt;

    constructor Create(IL: TImageList; DrawCanvas: TCanvas; Width, Height: Integer);

    procedure SetRozmery(Width, Height: Integer);
    procedure MouseUp(Position: TPoint; Button: TMouseButton);
    function GetSymbol(Position: TPoint): ShortInt;

    procedure Escape(Group: Boolean);
    procedure Clear();

    procedure Paint();
    procedure PaintMoveBuffer(pos: TPoint);
    function PaintCursor(CursorPos: TPoint): TCursorDraw;

    procedure Add(SymbolID: Integer); overload;
    function MoveDrag(pos1: TPoint; pos2: TPoint): Boolean; // returnbs if anything dragged
    procedure MoveDrop(pos: TPoint);
    function CanMoveDrop(pos: TPoint): Boolean;
    procedure Delete(pos1: TPoint; pos2: TPoint); overload;

    procedure LoadBpnl(var f: File; fileVersion: Byte; width: Integer; height: Integer);
    procedure WriteBpnl(var f: File);

    property addStep: TGOpStep read Operations.Add.Step;
    property Group: Boolean read Operations.Group.IsGroup write Operations.Group.IsGroup;

    property OnShow: TNEvent read mOnShow write mOnShow;
    property QIsConflict: TPosAskEvent read mIsConflict write mIsConflict;
    property IsOp: TOpAskEvent read mOPAsk write mOPAsk;
    property OnNullOperations: TNEvent read mNullOperations write mNullOperations;
  end; // class

implementation

// construcotr
constructor TBitmapSymbols.Create(IL: TImageList; DrawCanvas: TCanvas; Width, Height: Integer);
begin
  Self.DrawObject.Canvas := DrawCanvas;
  Self.DrawObject.IL := IL;
  Self.Panel.Width := Width;
  Self.Panel.Height := Height;
  Self.Operations.Group.IsGroup := false;

  Self.Operations.Group.Start.X := -1;
  Self.Operations.Group.Start.Y := -1;

  Self.Clear();
end;

procedure TBitmapSymbols.Escape(Group: Boolean);
begin
  Self.Operations.Add.Step := TGOpStep.gosNone;

  if (Group) then
  begin
    Self.Operations.Group.IsGroup := false;
    Self.Operations.Group.Start.X := -1;
    Self.Operations.Group.Start.Y := -1;
  end;
end;

// pridani noveho bitmapoveho symbolu
procedure TBitmapSymbols.Add(aPos: TPoint; SymbolID: Integer);
begin
  if ((aPos.X < 0) or (aPos.Y < 0) or (aPos.X > Self.Panel.Width) or (aPos.Y > Self.Panel.Height)) then
    raise EInvalidPosition.Create('Neplatná pozice!');
  if (Self.Bitmap[aPos.X, aPos.Y] <> _NO_SYMBOL) then
    raise ENonemptyField.Create('Na pozici je již symbol!');

  Self.Bitmap[aPos.X, aPos.Y] := SymbolID;
end;

// smazani bitmapovaho symbolu
procedure TBitmapSymbols.Delete(aPos: TPoint);
begin
  if ((aPos.X < 0) or (aPos.Y < 0) or (aPos.X > _MAX_WIDTH) or (aPos.Y > _MAX_HEIGHT)) then
    raise EInvalidPosition.Create('Neplatná pozice!');
  if (Self.Bitmap[aPos.X, aPos.Y] = _NO_SYMBOL) then
    raise ENonemptyField.Create('Na pozici není žádný symbol!');

  Self.Bitmap[aPos.X, aPos.Y] := _NO_SYMBOL;
end;

function TBitmapSymbols.GetSymbol(Position: TPoint): ShortInt;
begin
  if ((Position.X < 0) or (Position.Y < 0) or (Position.X > _MAX_WIDTH) or (Position.Y > _MAX_HEIGHT)) then
    Exit(-2);

  Result := Self.Bitmap[Position.X, Position.Y];
end;

procedure TBitmapSymbols.Clear();
begin
  for var i: Integer := 0 to _MAX_WIDTH - 1 do
    for var j: Integer := 0 to _MAX_HEIGHT - 1 do
      Self.Bitmap[i, j] := _NO_SYMBOL;
  Self.Escape(true);
end;

procedure TBitmapSymbols.Paint();
begin
  for var i: Integer := 0 to Self.Panel.Width - 1 do
    for var j: Integer := 0 to Self.Panel.Height - 1 do
      Self.PaintSymbol(i, j, Self.Bitmap[i, j]);
end;

procedure TBitmapSymbols.PaintMoveBuffer(pos: TPoint);
begin
  for var x: Integer := 0 to Self.MoveBuf.Width - 1 do
    for var y: Integer := 0 to Self.MoveBuf.Height - 1 do
      Self.PaintSymbol(pos.X+x, pos.Y+y, Self.MoveBuf.Symbols[x, y]);
end;

procedure TBitmapSymbols.LoadBpnl(var f: File; fileVersion: Byte; width: Integer; height: Integer);
var count: Integer;
    buf: array [0 .. (_MAX_WIDTH*_MAX_HEIGHT)] of Byte;
begin
  const total: Integer = width*height;

  BlockRead(f, buf, width*height, count);
  if (count < total) then
    raise EFileLoad.Create('Málo bitmapových dat!');

  for var i: Integer := 0 to total-1 do
    buf[i] := buf[i]-1;

  if (fileVersion < $40) then
    for var i: Integer := 0 to total-1 do
      buf[i] := TranscodeSymbolFromBpnlV3(buf[i]);

  Self.Panel.Width := width;
  Self.Panel.Height := height;

  for var i: Integer := 0 to Self.Panel.Height - 1 do
    for var j: Integer := 0 to Self.Panel.Width - 1 do
      Self.Bitmap[j, i] := buf[(i * Self.Panel.Width) + j];
end;

procedure TBitmapSymbols.WriteBpnl(var f: File);
var buf: array [0.._MAX_WIDTH] of Byte;
begin
  for var i: Integer := 0 to Self.Panel.Height - 1 do
  begin
    for var j: Integer := 0 to Self.Panel.Width - 1 do
      buf[j] := Self.Bitmap[j, i]+1;
    BlockWrite(f, buf, Self.Panel.Width);
  end;
end;

procedure TBitmapSymbols.SetRozmery(Width, Height: Integer);
begin
  Self.Panel.Width := Width;
  Self.Panel.Height := Height;
end;

function TBitmapSymbols.ControlCursorPos(Pos1, Pos2: TPoint): Boolean;
begin
  Result := ((Pos1.X <= Pos2.X) and (Pos1.Y <= Pos2.Y));
end;

procedure TBitmapSymbols.Adding(Position: TPoint);
begin
  // overovani skupiny - 2. cast podminky pridana, kdyby nekdo po 1. kroku vypl IsGroup
  if ((Self.operations.group.isGroup) or ((Self.operations.group.start.X <> -1) and
    (Self.operations.group.start.Y <> -1))) then
  begin
    // pokud je skupina
    case (Self.operations.add.step) of
      TGOpStep.gosActive:
        begin
          // pokud jsme na 1. bode
          Self.operations.group.start.X := Position.X;
          Self.operations.group.start.Y := Position.Y;

          if (Assigned(Self.OnNullOperations)) then
            Self.OnNullOperations();
          Self.operations.add.step := TGOpStep.gosSelecting;
        end; // case 1
      TGOpStep.gosSelecting:
        begin
          // pokud jsme na 2. bode
          // kontrola zapornych limitu vyberu
          if (not Self.ControlCursorPos(Self.operations.Group.Start, Position)) then
            raise EInvalidPosition.Create('Výběr musí být zleva doprava a shora dolů!');

          // kontrola obsazenosti
          for var x: Integer := Self.operations.group.start.X to Position.X do
            for var y: Integer := Self.operations.group.start.Y to Position.Y do
              if (Self.IsConflict(Point(x, y))) then
                raise ENonemptyField.Create('Na pozici je již symbol!');

          if (Assigned(Self.OnNullOperations)) then
            Self.OnNullOperations();

          // dosazeni vlastnich objektu
          for var i: Integer := Self.operations.group.start.X to Position.X do
            for var j := Self.operations.group.start.Y to Position.Y do
              Self.Add(Point(i, j), Self.operations.add.Symbol);

          Self.operations.group.start := Point(-1, -1);

          // znovu pripraveni dosazeni objektu
          Self.operations.add.step := TGOpStep.gosNone;
          if (Assigned(Self.OnShow)) then
          begin
            Self.OnShow();
            Sleep(50);
          end;
          Self.Add(Self.operations.add.symbol);
        end; // case 2
    end; // case
  end else begin
    // pokud neni skupina

    if (Self.IsConflict(Position)) then
      raise ENonemptyField.Create('Na pozici je již symbol!');

    if (Assigned(Self.OnNullOperations)) then
      Self.OnNullOperations();

    Self.Add(Position, Self.operations.add.Symbol);

    // znovu pripraveni dosazeni objektu
    Self.operations.add.step := TGOpStep.gosNone;
    if (Assigned(Self.OnShow)) then
    begin
      Self.OnShow();
      Sleep(50);
    end;
    Self.Add(Self.operations.add.Symbol);
  end; // else (Self.Group.IsGroup)
end;

procedure TBitmapSymbols.MouseUp(Position: TPoint; Button: TMouseButton);
begin
  if (Button = mbLeft) then
  begin
    if (Self.operations.add.step > TGOpStep.gosNone) then
      Self.Adding(Position);
  end;
end;

procedure TBitmapSymbols.Add(SymbolID: Integer);
begin
  Self.CheckOpInProgressAndExcept();

  Self.operations.add.step := TGOpStep.gosActive;
  Self.operations.add.Symbol := SymbolID;

  if (Assigned(Self.OnShow)) then
    Self.OnShow();
end;

function TBitmapSymbols.MoveDrag(pos1: TPoint; pos2: TPoint): Boolean;
begin
  Result := False;
  Self.MoveBuf.Width := pos2.X-pos1.X+1;
  Self.MoveBuf.Height := pos2.Y-pos1.Y+1;
  SetLength(Self.MoveBuf.Symbols, Self.MoveBuf.Width, Self.MoveBuf.Height);
  for var x := pos1.X to pos2.X do
  begin
    for var y := pos1.Y to pos2.Y do
    begin
      Self.MoveBuf.Symbols[x-pos1.X, y-pos1.Y] := Self.GetSymbol(Point(x, y));
      if (Self.Bitmap[x, y] <> _NO_SYMBOL) then
      begin
        Self.Delete(Point(x, y));
        Result := True;
      end;
    end;
  end;
end;

procedure TBitmapSymbols.MoveDrop(pos: TPoint);
begin
  for var x: Integer := 0 to Self.MoveBuf.Width-1 do
    for var y: Integer := 0 to Self.MoveBuf.Height-1 do
      if (Self.MoveBuf.Symbols[x, y] <> _NO_SYMBOL) then
        Self.Add(Point(pos.X + x, pos.Y + y), Self.MoveBuf.Symbols[x, y]);
  Self.MoveBuf.Width := 0;
  Self.MoveBuf.Height := 0;
end;

function TBitmapSymbols.CanMoveDrop(pos: TPoint): Boolean;
begin
  Result := True;
  for var x: Integer := 0 to Self.MoveBuf.Width-1 do
    for var y: Integer := 0 to Self.MoveBuf.Height-1 do
      if ((Self.MoveBuf.Symbols[x, y] <> _NO_SYMBOL) and (Self.IsConflict(Point(pos.X+x, pos.Y+y)))) then
        Exit(False);
end;

procedure TBitmapSymbols.Delete(pos1: TPoint; pos2: TPoint);
begin
  for var x := pos1.X to pos2.X do
    for var y := pos1.Y to pos2.Y do
      if (Self.Bitmap[x, y] <> _NO_SYMBOL) then
        Self.Delete(Point(x, y));
end;

// vykresleni kurzoru - vraci data PIXELECH!
function TBitmapSymbols.PaintCursor(CursorPos: TPoint): TCursorDraw;
begin
  Result.color := TCursorColor.ccDefault;
  Result.Pos1.X := CursorPos.X * _SYMBOL_WIDTH;
  Result.Pos1.Y := CursorPos.Y * _SYMBOL_HEIGHT;
  Result.Pos2.X := CursorPos.X * _SYMBOL_WIDTH;
  Result.Pos2.Y := CursorPos.Y * _SYMBOL_HEIGHT;

  if (Self.operations.add.step = TGOpStep.gosActive) then
  begin
    Result.color := TCursorColor.ccOnObject;
  end;
  if (Self.operations.add.step = TGOpStep.gosSelecting) then
  begin
    if (Self.operations.group.isGroup) then
    begin
      Result.pos1.X := (Self.operations.group.start.X) * _SYMBOL_WIDTH;
      Result.pos1.Y := (Self.operations.group.start.Y) * _SYMBOL_HEIGHT;
    end; // else IsGroup

    Result.color := TCursorColor.ccOnObject;
  end;
end;

// tato funkce ve skutenocnosti jen vykresluje veci z ImageListu
// zaroven ale resi specialni pripady velikosti, jako jsou naprikald uvazky, ci seznam souprav u uvazky
procedure TBitmapSymbols.PaintSymbol(IL: TImageList; X, Y: Integer; index: Integer; color: SymbolColor);
begin
  // specialni pripady: symbol seznamu souprav uvazky
  if (index = _S_LINKER_TRAIN) then
  begin
    Self.DrawObject.Canvas.Pen.color := clYellow;
    Self.DrawObject.Canvas.Brush.color := clBlack;

    Self.DrawObject.Canvas.Rectangle(X * _SYMBOL_WIDTH, Y * _SYMBOL_HEIGHT, (X + _Uvazka_Spr_Sirka) * _SYMBOL_WIDTH - 1,
      Y * _SYMBOL_HEIGHT + _SYMBOL_HEIGHT - 1);
  end;

  // specialni pripad: k trati a k Pst vykreslujeme druhy symbol do paru
  if (index = _S_LINKER_B) then
    SymbolDraw(IL, Self.DrawObject.Canvas, X+1, Y, _S_LINKER_B+1, color);
  if (index = _S_PST_TOP) then
    SymbolDraw(IL, Self.DrawObject.Canvas, X, Y+1, _S_PST_BOT, color);

  SymbolDraw(IL, Self.DrawObject.Canvas, X, Y, index, color);
end;

procedure TBitmapSymbols.PaintSymbol(X, Y: Integer; index: Integer);
begin
  Self.PaintSymbol(Self.DrawObject.IL, X, Y, index, SymbolDrawColor(index));
end;

procedure TBitmapSymbols.CheckOpInProgressAndExcept();
begin
  if (Assigned(Self.IsOp)) then
  begin
    if (Self.IsOp()) then
      raise EOperationInProgress.Create('Právě probíhá operace!');
  end else begin
    if (Self.operations.add.step > TGOpStep.gosNone) then
      raise EOperationInProgress.Create('Právě probíhá operace!');
  end;
end;

function TBitmapSymbols.IsConflict(pos: TPoint): Boolean;
begin
  if (Assigned(Self.mIsConflict)) then
    Result := Self.mIsConflict(pos)
  else
    Result := True; // event must be assigned
end;

end.// unit
