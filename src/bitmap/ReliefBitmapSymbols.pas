unit ReliefBitmapSymbols;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, IniFiles,
  StrUtils, ReliefText, VektorBasedObject, Global, symbolHelper, Types, ReliefCommon;

const
  _MAX_MOVE = 64;

type
  EInvalidPosition = class(Exception);
  ENonemptyField = class(Exception);
  EEmptyField = class(Exception);
  ENoSymbol = class(Exception);
  EOperationInProgress = class(Exception);

  TBitmapSymbols = class
  private
    Panel: record
      Width, Height: Integer;
    end;

    DrawObject: record
      Canvas: TCanvas;
      IL: TImageList;
    end;

    Operations: record
      Add: record
        Step: TGOpStep;
        Symbol: Integer;
      end;

      DeleteStep: TGOpStep;

      Move: record
        Step: TGOpStep;
        aWidth, aHeight: Byte;
        Symbols: array [0 .. _MAX_MOVE - 1, 0 .. _MAX_MOVE - 1] of ShortInt;
      end; // Move

      Group: record
        IsGroup: Boolean;
        Start: TPoint;
      end;
    end; // Operations

    procedure PaintSymbol(X, Y: Integer; index: Integer); overload;
    procedure PaintSymbol(IL: TImageList; X, Y: Integer; index: Integer; color: SymbolColor); overload;

    procedure AddToStructure(aPos: TPoint; SymbolID: Integer);
    procedure DeleteFromStructure(aPos: TPoint);

    procedure Adding(Position: TPoint);
    procedure Moving(Position: TPoint);
    procedure Deleting(Position: TPoint);

    function ControlCursorPos(Pos1, Pos2: TPoint): Boolean;
    procedure CheckOpInProgressAndExcept();

  public
    FOnShow: TNEvent;
    FIsSymbol: TPosAskEvent;
    FNullOperations: TNEvent;
    FMoveActivate: TNEvent;
    FDeleteActivate: TNEvent;
    FOPAsk: TOpAskEvent;

    Bitmap: array [0 .. _MAX_WIDTH - 1, 0 .. _MAX_HEIGHT - 1] of ShortInt;

    constructor Create(IL: TImageList; DrawCanvas: TCanvas; Width, Height: Integer);

    procedure SetRozmery(Width, Height: Integer);

    procedure MouseUp(Position: TPoint; Button: TMouseButton);

    function GetSymbol(Position: TPoint): ShortInt;

    procedure Escape(Group: Boolean);
    procedure Clear();

    procedure Paint();
    procedure PaintBitmapMove(KurzorPos: TPoint);
    function PaintCursor(CursorPos: TPoint): TCursorDraw;

    procedure Add(SymbolID: Integer);
    procedure Move();
    procedure Delete();

    procedure LoadBpnl(var f: File; fileVersion: Byte; width: Integer; height: Integer);
    procedure WriteBpnl(var f: File);

    property addStep: TGOpStep read Operations.Add.Step;
    property moveStep: TGOpStep read Operations.Move.Step;
    property deleteStep: TGOpStep read Operations.DeleteStep;
    property Group: Boolean read Operations.Group.IsGroup write Operations.Group.IsGroup;

    property OnShow: TNEvent read FOnShow write FOnShow;
    property IsSymbol: TPosAskEvent read FIsSymbol write FIsSymbol;
    property IsOp: TOpAskEvent read FOPAsk write FOPAsk;
    property OnNullOperations: TNEvent read FNullOperations write FNullOperations;
    property OnMoveActivate: TNEvent read FMoveActivate write FMoveActivate;
    property OnDeleteActivate: TNEvent read FDeleteActivate write FDeleteActivate;
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
  Self.Operations.Move.Step := TGOpStep.gosNone;
  Self.Operations.DeleteStep := TGOpStep.gosNone;

  if (Group) then
  begin
    Self.Operations.Group.IsGroup := false;
    Self.Operations.Group.Start.X := -1;
    Self.Operations.Group.Start.Y := -1;
  end;
end;

// pridani noveho bitmapoveho symbolu
procedure TBitmapSymbols.AddToStructure(aPos: TPoint; SymbolID: Integer);
begin
  if ((aPos.X < 0) or (aPos.Y < 0) or (aPos.X > Self.Panel.Width) or (aPos.Y > Self.Panel.Height)) then
    raise EInvalidPosition.Create('Neplatná pozice!');
  if (Self.Bitmap[aPos.X, aPos.Y] <> -1) then
    raise ENonemptyField.Create('Na pozici je již symbol!');

  Self.Bitmap[aPos.X, aPos.Y] := SymbolID;
end;

// smazani bitmapovaho symbolu
procedure TBitmapSymbols.DeleteFromStructure(aPos: TPoint);
begin
  if ((aPos.X < 0) or (aPos.Y < 0) or (aPos.X > _MAX_WIDTH) or (aPos.Y > _MAX_HEIGHT)) then
    raise EInvalidPosition.Create('Neplatná pozice!');
  if (Self.Bitmap[aPos.X, aPos.Y] = -1) then
    raise ENonemptyField.Create('Na pozici není žádný symbol!');

  Self.Bitmap[aPos.X, aPos.Y] := -1;
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
      Self.Bitmap[i, j] := -1;
  Self.Escape(true);
end;

procedure TBitmapSymbols.Paint();
begin
  for var i: Integer := 0 to Self.Panel.Width - 1 do
    for var j: Integer := 0 to Self.Panel.Height - 1 do
      Self.PaintSymbol(i, j, Self.Bitmap[i, j]);
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
  Result := true;

  if (Pos1.X > Pos2.X) or (Pos1.Y > Pos2.Y) then
    Result := false;
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
          begin
            for var y: Integer := Self.operations.group.start.Y to Position.Y do
            begin
              if (Assigned(Self.IsSymbol)) then
              begin
                if (Self.IsSymbol(Point(x, y))) then
                  raise ENonemptyField.Create('Na pozici je již symbol!');
              end else begin
                if (Self.GetSymbol(Point(x, y)) <> -1) then
                  raise ENonemptyField.Create('Na pozici je již symbol!');
              end;
            end;
          end;

          if (Assigned(Self.OnNullOperations)) then
            Self.OnNullOperations();

          // dosazeni vlastnich objektu
          for var i: Integer := Self.operations.group.start.X to Position.X do
            for var j := Self.operations.group.start.Y to Position.Y do
              Self.AddToStructure(Point(i, j), Self.operations.add.Symbol);

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

    if (Assigned(Self.IsSymbol)) then
    begin
      if (Self.IsSymbol(Position)) then
        raise ENonemptyField.Create('Na pozici je již symbol!');
    end else begin
      if (Self.GetSymbol(Position) <> -1) then
        raise ENonemptyField.Create('Na pozici je již symbol!');
    end; // else Assigned(FIsOperation)

    if (Assigned(FNullOperations)) then
      FNullOperations;

    Self.AddToStructure(Position, Self.operations.add.Symbol);

    // znovu pripraveni dosazeni objektu
    Self.operations.add.step := TGOpStep.gosNone;
    if Assigned(FOnShow) then
    begin
      FOnShow;
      Sleep(50);
    end;
    Self.Add(Self.operations.add.Symbol);
  end; // else (Self.Group.IsGroup)
end;

procedure TBitmapSymbols.Moving(Position: TPoint);
begin
  // overovani skupiny - 2. cast podminky pridana, kdyby nekdo po 1. kroku vypl IsGroup
  if ((Self.operations.Group.IsGroup) or ((Self.operations.Group.Start.X <> -1) and
    (Self.operations.Group.Start.Y <> -1))) then
  begin
    // pokud je skupina
    case (Self.operations.move.step) of
      TGOpStep.gosActive:
        begin
          Self.operations.Group.Start.X := Position.X;
          Self.operations.Group.Start.Y := Position.Y;

          if (Assigned(FNullOperations)) then
            FNullOperations;
          Self.operations.move.step := TGOpStep.gosSelecting;
        end; // case 1
      TGOpStep.gosSelecting:
        begin
          // kontrola zapornych limitu vyberu
          if (not Self.ControlCursorPos(Self.operations.Group.Start, Position)) then
            raise EInvalidPosition.Create('Výběr musí být zleva doprava a shora dolů!');

          // kontrola limitu pole
          if (((Position.X - Self.operations.Group.Start.X + 1) > _MAX_MOVE) or
            (Position.Y - Self.operations.Group.Start.Y + 1 > _MAX_MOVE)) then
            raise EInvalidPosition.Create('Přesáhnuta maximální velikosti výběru!');

          // prevzeti objektu do .Symbols[] a smazani puvodnich objektu
          Self.operations.move.aWidth := Position.X - Self.operations.Group.Start.X + 1;
          Self.operations.move.aHeight := Position.Y - Self.operations.Group.Start.Y + 1;

          for var i := Self.operations.Group.Start.X to Position.X do
          begin
            for var j := Self.operations.Group.Start.Y to Position.Y do
            begin
              Self.operations.move.Symbols[i - Self.operations.Group.Start.X, j - Self.operations.Group.Start.Y] :=
                Self.GetSymbol(Point(i, j));
              if (Self.Bitmap[i, j] <> -1) then
                Self.DeleteFromStructure(Point(i, j));
            end;
          end;

          Self.operations.move.step := TGOpStep.gosMoving;
        end; // case 2
      TGOpStep.gosMoving:
        begin
          // kontrola obsazenosti
          for var i := 0 to Self.operations.move.aWidth - 1 do
          begin
            for var j := 0 to Self.operations.move.aHeight - 1 do
            begin
              if (Assigned(FIsSymbol)) then
              begin
                if (FIsSymbol(Point(i + (Position.X - Self.operations.move.aWidth) + 1,
                  j + (Position.Y - Self.operations.move.aHeight) + 1))) then
                  raise ENonemptyField.Create('Na pozici je již symbol!');
              end else begin
                if ((Self.GetSymbol(Point(i + (Position.X - Self.operations.move.aWidth) + 1,
                  j + (Position.Y - Self.operations.move.aHeight) + 1)) <> -1)) then
                  raise ENonemptyField.Create('Na pozici je již symbol!');
              end; // else (Assigned(FIsOperation))
            end; // for j
          end; // for i

          for var i := 0 to Self.operations.move.aWidth - 1 do
          begin
            for var j := 0 to Self.operations.move.aHeight - 1 do
            begin
              Self.AddToStructure(Point(i + (Position.X - Self.operations.move.aWidth) + 1,
                j + (Position.Y - Self.operations.move.aHeight) + 1), Self.operations.move.Symbols[i, j]);
              Self.operations.move.Symbols[i, j] := 0;
            end; // for j
          end; // for i

          Self.operations.Group.Start.X := -1;
          Self.operations.Group.Start.Y := -1;

          Self.operations.move.aWidth := 0;
          Self.operations.move.aHeight := 0;
          Self.operations.move.step := TGOpStep.gosNone;

          // znovu pripraveni dosazeni objektu
          if Assigned(FOnShow) then
          begin
            FOnShow;
            Sleep(50);
          end;
          if (Assigned(Self.FMoveActivate)) then
            Self.FMoveActivate;
        end; // case 3
    end; // case

  end else begin
    // pokud neni skupina
    case (Self.operations.move.step) of
      TGOpStep.gosActive:
        begin
          // prevzeti objektu do .Symbols[] a smazani puvodnich objektu
          Self.operations.move.aWidth := 1;
          Self.operations.move.aHeight := 1;

          if (Self.GetSymbol(Position) = -1) then
            Exit();

          if (Assigned(FNullOperations)) then
            FNullOperations;

          Self.operations.move.Symbols[0, 0] := Self.GetSymbol(Position);
          Self.DeleteFromStructure(Position);

          Self.operations.move.step := TGOpStep.gosSelecting;
        end; // case 1
      TGOpStep.gosSelecting:
        begin
          // kontrola obsazenosti pozice
          if (Assigned(FIsSymbol)) then
          begin
            if (FIsSymbol(Position)) then
              raise ENonemptyField.Create('Na pozici je již symbol!');
          end else begin
            if (Self.GetSymbol(Position) <> -1) then
              raise ENonemptyField.Create('Na pozici je již symbol!');
          end;

          Self.AddToStructure(Position, Self.operations.move.Symbols[0, 0]);

          Self.operations.move.aWidth := 0;
          Self.operations.move.aHeight := 0;
          Self.operations.move.step := TGOpStep.gosNone;

          // znovu pripraveni dosazeni objektu
          if Assigned(FOnShow) then
          begin
            FOnShow;
            Sleep(50);
          end;
          if (Assigned(Self.FMoveActivate)) then
            Self.FMoveActivate;
        end; // case 3
    end; // case
  end; // else ((Self.Group.IsGroup) or ((Self.Group.Start.X <> -1) and (Self.Group.Start.Y <> -1)))
end;

procedure TBitmapSymbols.Deleting(Position: TPoint);
begin
  if ((Self.operations.Group.IsGroup) or ((Self.operations.Group.Start.X <> -1) and
    (Self.operations.Group.Start.Y <> -1))) then
  begin
    // pokud je skupina
    case (Self.operations.deleteStep) of
      TGOpStep.gosActive:
        begin
          // pokud jsme na 1. bode
          Self.operations.Group.Start.X := Position.X;
          Self.operations.Group.Start.Y := Position.Y;

          if (Assigned(Self.FNullOperations)) then
            Self.FNullOperations;
          Self.operations.deleteStep := TGOpStep.gosSelecting;
        end; // case 1
      TGOpStep.gosSelecting:
        begin
          // kontrola zapornych limitu vyberu
          if (not Self.ControlCursorPos(Self.operations.Group.Start, Position)) then
            raise EInvalidPosition.Create('Výběr musí být zleva doprava a shora dolů!');

          // pokud jsme na 2. bode
          for var i := Self.operations.Group.Start.X to Position.X do
            for var j := Self.operations.Group.Start.Y to Position.Y do
              if (Self.Bitmap[i, j] <> -1) then
                Self.DeleteFromStructure(Point(i, j));

          Self.operations.Group.Start.X := -1;
          Self.operations.Group.Start.Y := -1;

          Self.operations.deleteStep := TGOpStep.gosNone;
          if Assigned(FOnShow) then
          begin
            FOnShow;
            Sleep(50);
          end;
          if (Assigned(Self.FDeleteActivate)) then
            Self.FDeleteActivate;
        end; // case 2
    end; // case
  end else begin
    // pokud neni skupina
    if (Self.GetSymbol(Position) = -1) then
      Exit();

    if (Assigned(FNullOperations)) then
      FNullOperations;

    Self.DeleteFromStructure(Position);

    Self.operations.deleteStep := TGOpStep.gosNone;
    if Assigned(FOnShow) then
    begin
      FOnShow;
      Sleep(50);
    end;
    if (Assigned(Self.FDeleteActivate)) then
      Self.FDeleteActivate;
  end; // else (Self.Group.IsGroup)
end;

procedure TBitmapSymbols.MouseUp(Position: TPoint; Button: TMouseButton);
begin
  if (Button = mbLeft) then
  begin
    if (Self.operations.add.step > TGOpStep.gosNone) then
      Self.Adding(Position);
    if (Self.operations.move.step > TGOpStep.gosNone) then
      Self.Moving(Position);
    if (Self.operations.deleteStep > TGOpStep.gosNone) then
      Self.Deleting(Position);
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

procedure TBitmapSymbols.Move();
begin
  Self.CheckOpInProgressAndExcept();
  Self.operations.move.step := TGOpStep.gosActive;
end;

procedure TBitmapSymbols.Delete();
begin
  Self.CheckOpInProgressAndExcept();
  Self.operations.deleteStep := TGOpStep.gosActive;
end;

// zabyva se vykreslovanim posouvanych objektu v bitmape
procedure TBitmapSymbols.PaintBitmapMove(KurzorPos: TPoint);
begin
  // pohyb
  if (Self.operations.move.step > TGOpStep.gosActive) then
  begin
    if ((Self.operations.Group.Start.X = -1) and (Self.operations.Group.Start.Y = -1)) then
    begin
      // presun 1 objektu
      Self.PaintSymbol(KurzorPos.X, KurzorPos.Y, Self.operations.move.Symbols[0, 0]);
    end else begin
      // presun skupiny
      if (Self.operations.move.step = TGOpStep.gosMoving) then
      begin
        for var i := 0 to Self.operations.move.aWidth - 1 do
        begin
          for var j := 0 to Self.operations.move.aHeight - 1 do
          begin
            var aPos: TPoint;
            aPos.X := i + KurzorPos.X - Self.operations.move.aWidth + 1;
            aPos.Y := j + KurzorPos.Y - Self.operations.move.aHeight + 1;
            if (Self.GetSymbol(aPos) = -1) then
              Self.PaintSymbol(aPos.X, aPos.Y, Self.operations.move.Symbols[i, j]);
          end; // for j
        end; // for i
      end; // if (Self.move.Krok > 2)
    end; // else ((Self.Group.Start.X = -1) and (Self.Group.Start.Y = -1))
  end; // if Self.move.Krok > 0

  // pridavani
  if (Self.operations.add.step > TGOpStep.gosNone) then
  begin
    if ((Self.operations.Group.Start.X = -1) and (Self.operations.Group.Start.Y = -1)) then
    begin
      // presun 1 objektu
      Self.PaintSymbol(KurzorPos.X, KurzorPos.Y, Self.operations.add.Symbol);
    end else begin
      // presun skupiny
      for var i := Self.operations.Group.Start.X to KurzorPos.X do
        for var j := Self.operations.Group.Start.Y to KurzorPos.Y do
          if (Self.GetSymbol(Point(i, j)) = -1) then
            Self.PaintSymbol(i, j, Self.operations.add.Symbol);
    end; // else ((Self.Group.Start.X = -1) and (Self.Group.Start.Y = -1))
  end; // if Self.Add > -1
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
  if (Self.operations.add.step = TGOpStep.gosSelecting) or (Self.operations.move.step = TGOpStep.gosSelecting) or (Self.operations.deleteStep = TGOpStep.gosSelecting) then
  begin
    if (Self.operations.group.isGroup) then
    begin
      Result.pos1.X := (Self.operations.group.start.X) * _SYMBOL_WIDTH;
      Result.pos1.Y := (Self.operations.group.start.Y) * _SYMBOL_HEIGHT;
    end; // else IsGroup

    Result.color := TCursorColor.ccOnObject;
  end;
  if ((Self.operations.move.step = TGOpStep.gosActive) or (Self.operations.deleteStep = TGOpStep.gosActive)) then
  begin
    var SymbolI := Self.GetSymbol(CursorPos);
    Result.Pos2.X := CursorPos.X * _SYMBOL_WIDTH;
    Result.Pos2.Y := CursorPos.Y * _SYMBOL_HEIGHT;

    if (SymbolI = -1) then
      Result.color := TCursorColor.ccActiveOperation
    else
      Result.color := TCursorColor.ccOnObject;
  end;
  if (Self.operations.move.step = TGOpStep.gosMoving) then
  begin
    // zde uz musi byt skupina - jinak by nebylo mozne skocit do kroku 2
    Result.color := TCursorColor.ccOnObject;

    if (Self.operations.Group.IsGroup) then
    begin
      Result.Pos1.X := (CursorPos.X - Self.operations.move.aWidth + 1) * _SYMBOL_WIDTH;
      Result.Pos1.Y := (CursorPos.Y - Self.operations.move.aHeight + 1) * _SYMBOL_HEIGHT;
    end; // if (Self.operations.Group.IsGroup)
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
  if (Assigned(Self.FOPAsk)) then
  begin
    if (Self.FOPAsk) then
      raise EOperationInProgress.Create('Právě probíhá operace!');
  end else begin
    if ((Self.operations.add.step > TGOpStep.gosNone) or (Self.operations.move.step > TGOpStep.gosNone) or (Self.operations.deleteStep > TGOpStep.gosNone)) then
      raise EOperationInProgress.Create('Právě probíhá operace!');
  end;
end;

end.// unit
