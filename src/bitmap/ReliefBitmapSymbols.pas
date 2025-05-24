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

  TBitmapSymbols = class
  private
    mIsConflict: TPosAskEvent;

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

    procedure PaintSymbol(X, Y: Integer; index: Integer); overload;
    procedure PaintSymbol(IL: TImageList; X, Y: Integer; index: Integer; color: SymbolColor); overload;

    function IsConflict(pos: TPoint): Boolean;

  public
    Bitmap: array [0 .. _MAX_WIDTH - 1, 0 .. _MAX_HEIGHT - 1] of ShortInt;
    addSymbol: Integer;

    constructor Create(IL: TImageList; DrawCanvas: TCanvas; Width, Height: Integer);

    procedure SetRozmery(Width, Height: Integer);
    function GetSymbol(Position: TPoint): ShortInt;

    procedure Clear();

    procedure Paint();
    procedure PaintMoveBuffer(pos: TPoint);
    procedure PaintAddSymbol(pos1: TPoint; pos2: TPoint);

    procedure Add(pos1: TPoint; pos2: TPoint; symbolID: Integer); overload;
    procedure Add(aPos: TPoint; SymbolID: Integer); overload;
    function MoveDrag(pos1: TPoint; pos2: TPoint): Boolean; // returns if anything dragged
    procedure MoveDrop(pos: TPoint); // pos = right bottom position
    function CanMoveDrop(pos: TPoint): Boolean; // pos = right bottom position
    procedure Delete(aPos: TPoint); overload;
    procedure Delete(pos1: TPoint; pos2: TPoint); overload;

    procedure LoadBpnl(var f: File; fileVersion: Byte; width: Integer; height: Integer);
    procedure WriteBpnl(var f: File);

    property QIsConflict: TPosAskEvent read mIsConflict write mIsConflict;
  end; // class

implementation

uses ReliefBitmap;

constructor TBitmapSymbols.Create(IL: TImageList; DrawCanvas: TCanvas; Width, Height: Integer);
begin
  Self.DrawObject.Canvas := DrawCanvas;
  Self.DrawObject.IL := IL;
  Self.Panel.Width := Width;
  Self.Panel.Height := Height;
  Self.Clear();
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

procedure TBitmapSymbols.Add(pos1: TPoint; pos2: TPoint; symbolID: Integer);
begin
  for var x: Integer := pos1.X to pos2.X do
    for var y: Integer := pos1.Y to pos2.Y do
      Self.Add(Point(x, y), symbolID);
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
  for var x: Integer := 0 to _MAX_WIDTH - 1 do
    for var y: Integer := 0 to _MAX_HEIGHT - 1 do
      Self.Bitmap[x, y] := _NO_SYMBOL;
end;

procedure TBitmapSymbols.Paint();
begin
  for var i: Integer := 0 to Self.Panel.Width - 1 do
    for var j: Integer := 0 to Self.Panel.Height - 1 do
      Self.PaintSymbol(i, j, Self.Bitmap[i, j]);
end;

procedure TBitmapSymbols.PaintMoveBuffer(pos: TPoint);
begin
  // pos = right down position
  for var x: Integer := 0 to Self.MoveBuf.Width - 1 do
    for var y: Integer := 0 to Self.MoveBuf.Height - 1 do
      Self.PaintSymbol(pos.X+x-Integer(Self.MoveBuf.Width)+1, pos.Y+y-Integer(Self.MoveBuf.Height)+1, Self.MoveBuf.Symbols[x, y]);
end;

procedure TBitmapSymbols.PaintAddSymbol(pos1: TPoint; pos2: TPoint);
begin
  for var x: Integer := pos1.X to pos2.X do
    for var y: Integer := pos1.Y to pos2.Y do
      Self.PaintSymbol(x, y, Self.addSymbol);
end;

procedure TBitmapSymbols.LoadBpnl(var f: File; fileVersion: Byte; width: Integer; height: Integer);
var buf: array [0 .. (_MAX_WIDTH*_MAX_HEIGHT)] of Byte;
begin
  const total: Integer = width*height;

  TPanelBitmap.BlockReadOrException(f, buf, width*height, 'bitmapová data');
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
        Self.Add(Point(pos.X+x-Integer(Self.MoveBuf.Width)+1, pos.Y+y-Integer(Self.MoveBuf.Height)+1), Self.MoveBuf.Symbols[x, y]);
  Self.MoveBuf.Width := 0;
  Self.MoveBuf.Height := 0;
end;

function TBitmapSymbols.CanMoveDrop(pos: TPoint): Boolean;
begin
  Result := True;
  for var x: Integer := 0 to Self.MoveBuf.Width-1 do
    for var y: Integer := 0 to Self.MoveBuf.Height-1 do
      if ((Self.MoveBuf.Symbols[x, y] <> _NO_SYMBOL) and
          (Self.IsConflict(Point(pos.X+x-Integer(Self.MoveBuf.Width)+1, pos.Y+y-Integer(Self.MoveBuf.Height)+1)))) then
        Exit(False);
end;

procedure TBitmapSymbols.Delete(pos1: TPoint; pos2: TPoint);
begin
  for var x := pos1.X to pos2.X do
    for var y := pos1.Y to pos2.Y do
      if (Self.Bitmap[x, y] <> _NO_SYMBOL) then
        Self.Delete(Point(x, y));
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

function TBitmapSymbols.IsConflict(pos: TPoint): Boolean;
begin
  if (Assigned(Self.mIsConflict)) then
    Result := Self.mIsConflict(pos)
  else
    Result := True; // event must be assigned
end;

end.// unit

