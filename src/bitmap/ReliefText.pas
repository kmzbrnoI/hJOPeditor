unit ReliefText;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, IniFiles,
  StrUtils, Global, Menus, Forms, PGraphics, Generics.Collections, symbolHelper,
  Types, ReliefCommon;

const
  _MAX_POPISKY = 256;
  _MAX_TEXT_LENGTH = 32;
  _Block_Length = 35;

  _POPISEK_MAGIC_CODE = 17;

type
  EInvalidPosition = class(Exception);
  ENonemptyField = class(Exception);
  EEmptyField = class(Exception);
  EMaxReached = class(Exception);
  ENoSymbol = class(Exception);
  EOperationInProgress = class(Exception);
  ETooLongText = class(Exception);

  TPopisek = record
    Position: TPoint;
    Text: string;
    Color: SymbolColor;
    BlokPopisek: boolean;
  end;

  TChangeTextEvent = procedure(Sender: TObject; var popisek: TPopisek) of object;

  TText = class
  private
    Data: TList<TPopisek>;

    TextMenu: TPopupMenu;
    MenuPosition: TPoint;
    Graphics: TPanelGraphics;

    DrawObject: record
      Canvas: TCanvas;
      TextIL: TImageList;
    end;

    Operations: record
      AddStep: TGOpStep;
      MoveStep: TGOpStep;
      DeleteStep: TGOpStep;

      TextProperties: record
        Text: string;
        Color: SymbolColor;
        BlokPopisek: boolean;
      end;
    end; // Operations

    procedure Adding(Position: TPoint);
    procedure Moving(Position: TPoint);
    procedure Deleting(Position: TPoint);

    procedure MITextPropertiesClick(Sender: TObject);
    procedure InitializeTextMenu(var Menu: TPopupMenu; Parent: TForm);

    function GetCount(): Integer;
    procedure CheckOpInProgressAndExcept();

  public
    FOnShow: TNEvent;
    FIsSymbol: TPosAskEvent;
    FNullOperations: TNEvent;
    FMoveActivate: TNEvent;
    FDeleteActivate: TNEvent;
    FOPAsk: TOpAskEvent;
    FOnChangeText: TChangeTextEvent;

    constructor Create(DrawCanvas: TCanvas; TextIL: TImageList; Parent: TForm; Graphics: TPanelGraphics);
    destructor Destroy; override;

    procedure AddToStructure(aPos: TPoint; aText: string; aColor: SymbolColor; aBlokDesc: boolean);
    procedure DeleteFromStructure(aPos: TPoint);

    function GetPopisekData(Index: Integer): TPopisek;
    function SetPopisekData(Index: Integer; Data: TPopisek): Byte;

    procedure Paint(showPopisky: boolean);
    procedure PaintTextMove(KurzorPos: TPoint);
    function PaintCursor(CursorPos: TPoint): TCursorDraw;

    procedure Escape();

    function GetPopisek(aPos: TPoint): SmallInt;
    function IsObsazeno(Pos1, Pos2: TPoint): boolean;

    procedure Add(aText: string; aColor: SymbolColor; popisekBlok: boolean);
    procedure Move();
    procedure Delete();

    procedure SetLoadedData(LoadData: TBytes);
    procedure SetLoadedDataV32(LoadData: TBytes);
    function GetSaveData: TBytes;
    procedure Clear();

    procedure MouseUp(Position: TPoint; Button: TMouseButton);

    property addStep: TGOpStep read Operations.AddStep;
    property moveStep: TGOpStep read Operations.MoveStep;
    property deleteStep: TGOpStep read Operations.DeleteStep;
    property count: Integer read GetCount;

    property OnShow: TNEvent read FOnShow write FOnShow;
    property IsSymbol: TPosAskEvent read FIsSymbol write FIsSymbol;
    property IsOp: TOpAskEvent read FOPAsk write FOPAsk;
    property OnNullOperations: TNEvent read FNullOperations write FNullOperations;
    property OnMoveActivate: TNEvent read FMoveActivate write FMoveActivate;
    property OnDeleteActivate: TNEvent read FDeleteActivate write FDeleteActivate;
    property OnChangeText: TChangeTextEvent read FOnChangeText write FOnChangeText;
  end; // TText

implementation

constructor TText.Create(DrawCanvas: TCanvas; TextIL: TImageList; Parent: TForm; Graphics: TPanelGraphics);
begin
  inherited Create();

  Self.DrawObject.Canvas := DrawCanvas;
  Self.DrawObject.TextIL := TextIL;
  Self.Graphics := Graphics;
  Self.Data := TList<TPopisek>.Create();

  Self.InitializeTextMenu(Self.TextMenu, Parent);
  Self.Clear();
end;

destructor TText.Destroy();
begin
  Self.Data.Free();

  if (Assigned(Self.TextMenu)) then
  begin
    Self.TextMenu.Free;
    Self.TextMenu := nil;
  end;

  inherited;
end;

// pridani popisku
procedure TText.AddToStructure(aPos: TPoint; aText: string; aColor: SymbolColor; aBlokDesc: boolean);
var p: TPopisek;
begin
  if ((aPos.X < 0) or (aPos.Y < 0) or (aPos.X > (_MAX_WIDTH - 1)) or (aPos.Y > (_MAX_HEIGHT - 1))) then
    raise EInvalidPosition.Create('Neplatná pozice!');
  if (Self.GetPopisek(aPos) <> -1) then
    raise ENonemptyField.Create('Na pozici je již symbol!');
  if (Self.count >= _MAX_POPISKY) then
    raise EMaxReached.Create('Dosaženo maximálního počtu textů!');
  if (Length(aText) > _MAX_TEXT_LENGTH) then
    raise ETooLongText.Create('Text je příliš dlouhý!');

  p.Position := aPos;
  p.Text := aText;
  p.Color := aColor;
  p.BlokPopisek := aBlokDesc;

  Self.Data.Add(p);
end;

// smazani popisku
procedure TText.DeleteFromStructure(aPos: TPoint);
begin
  if ((aPos.X < 0) or (aPos.Y < 0) or (aPos.X > (_MAX_WIDTH - 1)) or (aPos.Y > (_MAX_HEIGHT - 1))) then
    raise EInvalidPosition.Create('Neplatná pozice!');

  var PIndex := Self.GetPopisek(aPos);
  if (PIndex = -1) then
    raise ENoSymbol.Create('Na této pozici není žádný symbol!');

  Self.Data.Delete(PIndex);
end;

// zjisteni, zda-li je na dane pozici popisek, popr jeho index v poli separatoru
function TText.GetPopisek(aPos: TPoint): SmallInt;
begin
  Result := -1;

  // vychazime z toho, ze 1 popisek lze zapsat pouze na 1 radek
  for var i := 0 to Self.Count - 1 do
    if (Self.Data[i].Position.Y = aPos.Y) then
      for var j := Self.Data[i].Position.X to Self.Data[i].Position.X + (Length(Self.Data[i].Text)) - 1 do
        if (j = aPos.X) then
          Exit(i);
end;

// nacteni surovych dat do struktur
procedure TText.SetLoadedData(LoadData: TBytes);
var
  Count: Integer;
begin
  Self.Data.Clear();
  Count := Length(LoadData) div _Block_Length;

  for var i := 0 to Count - 1 do
  begin
    var p: TPopisek;
    p.Position.X := LoadData[(i * _Block_Length)];
    p.Position.Y := LoadData[(i * _Block_Length) + 1];
    p.Color := SymbolColor(LoadData[(i * _Block_Length) + 2]);
    p.Text := '';
    p.BlokPopisek := false;

    for var j := 0 to _MAX_TEXT_LENGTH do
    begin
      if ((LoadData[(i * _Block_Length) + 3 + (j * 2)] = 0) and (LoadData[(i * _Block_Length) + 3 + (j * 2) + 1] = 0))
      then
        Break;
      p.Text := p.Text + chr(((LoadData[(i * _Block_Length) + 3 + (j * 2)]) shl 8) +
        LoadData[(i * _Block_Length) + 3 + (j * 2) + 1]);
    end; // for j

    Self.Data.Add(p);
  end; // for i
end;

procedure TText.SetLoadedDataV32(LoadData: TBytes);
var pos: Integer;
  i: Integer;
  len: Integer;
  p: TPopisek;
begin
  Self.Data.Clear();
  pos := 0;
  i := 0;

  while pos < Length(LoadData) do
  begin
    p.Position.X := LoadData[pos];
    p.Position.Y := LoadData[pos + 1];
    p.Color := SymbolColor(LoadData[pos + 2]);
    len := LoadData[pos + 3];
    p.BlokPopisek := (LoadData[pos + 4] = _POPISEK_MAGIC_CODE);
    if (p.BlokPopisek) then
      p.Text := TEncoding.UTF8.GetString(LoadData, pos + 5, len - 1)
    else
      p.Text := TEncoding.UTF8.GetString(LoadData, pos + 4, len);

    Self.Data.Add(p);
    i := i + 1;
    pos := pos + 4 + len;
  end; // for i

  Self.Data.Count := i;
end;

// ziskani surovych dat zapisovanych do souboru z dat programu
function TText.GetSaveData(): TBytes;
var bytesBuf: TBytes;
  len: Integer;
  currentLen: Integer;
begin
  SetLength(Result, 1024);
  currentLen := 2;

  for var p in Self.Data do
  begin
    len := TEncoding.UTF8.GetByteCount(p.Text);
    SetLength(bytesBuf, len);
    bytesBuf := TEncoding.UTF8.GetBytes(p.Text);

    if (Length(Result) < currentLen + len + 4) then
      SetLength(Result, Length(Result) * 2);

    Result[currentLen] := p.Position.X;
    Result[currentLen + 1] := p.Position.Y;
    Result[currentLen + 2] := Byte(p.Color);

    var offset: Integer;
    if (p.BlokPopisek) then
    begin
      Result[currentLen + 3] := len + 1;
      Result[currentLen + 4] := _POPISEK_MAGIC_CODE;
      offset := currentLen + 5;
    end else begin
      Result[currentLen + 3] := len;
      offset := currentLen + 4;
    end;

    CopyMemory(@Result[offset], bytesBuf, len);
    currentLen := offset + len;
  end;

  SetLength(Result, currentLen);
  Result[0] := hi(currentLen - 2);
  Result[1] := lo(currentLen - 2);
end;

procedure TText.Clear();
begin
  Self.Data.Clear();
  Self.Escape();
end;

procedure TText.Escape();
begin
  Self.Operations.addStep := gosNone;
  Self.Operations.moveStep := gosNone;
  Self.Operations.deleteStep := gosNone;
end;

procedure TText.Paint(showPopisky: boolean);
var popisek: TPopisek;
begin
  for popisek in Self.Data do
    if (showPopisky) or (not popisek.BlokPopisek) then
      Self.Graphics.TextOutputI(popisek.Position, popisek.Text, popisek.Color, clBlack, popisek.BlokPopisek, true);
end;

function TText.GetPopisekData(Index: Integer): TPopisek;
begin
  if (Index >= _MAX_POPISKY) then
  begin
    Result.Color := scPurple;
    Exit;
  end;

  Result := Self.Data[Index];
end;

function TText.SetPopisekData(Index: Integer; Data: TPopisek): Byte;
begin
  if (Index >= _MAX_POPISKY) then
  begin
    Result := 1;
    Exit;
  end; // if (Index => _MAX_POPISKY)

  Self.Data[Index] := Data;

  Result := 0;
end;

// pridavani textu
procedure TText.Adding(Position: TPoint);
begin
  // kontrola obsazenosti pozice
  if (Assigned(FIsSymbol)) then
    for var i := Position.X to Position.X + Length(Self.Operations.TextProperties.Text) - 1 do
      if (FIsSymbol(Point(i, Position.Y))) then
        raise ENonemptyField.Create('Na pozici je již symbol!');

  if (Self.IsObsazeno(Position, Point(Position.X + Length(Self.Operations.TextProperties.Text), Position.Y))) then
    raise ENonemptyField.Create('Na pozici je již symbol!');

  Self.AddToStructure(Position, Self.Operations.TextProperties.Text, Self.Operations.TextProperties.Color,
    Self.Operations.TextProperties.BlokPopisek);

  Self.Operations.addStep := gosNone;
end;

// pohyb textu
procedure TText.Moving(Position: TPoint);
begin
  // zde neni skupina pripustna
  case (Self.Operations.moveStep) of
    gosActive:
      begin
        var PopisekIndex := Self.GetPopisek(Position);
        if (PopisekIndex = -1) then
          Exit();

        // ziskani dat popisku
        Self.Operations.TextProperties.Text := Self.GetPopisekData(PopisekIndex).Text;
        Self.Operations.TextProperties.Color := Self.GetPopisekData(PopisekIndex).Color;
        Self.Operations.TextProperties.BlokPopisek := Self.GetPopisekData(PopisekIndex).BlokPopisek;

        Self.DeleteFromStructure(Position);

        if (Assigned(FNullOperations)) then
          FNullOperations;
        Self.Operations.moveStep := gosMoving;
      end;

    gosMoving:
      begin
        if (Assigned(FIsSymbol)) then
        begin
          for var i := Position.X to Position.X + Length(Self.Operations.TextProperties.Text) - 1 do
            if (FIsSymbol(Point(i, Position.Y))) then
              raise ENonemptyField.Create('Pozice obsazena!');
        end else begin
          if (Self.IsObsazeno(Position, Point(Position.X + Length(Self.Operations.TextProperties.Text), Position.Y)))
          then
            raise ENonemptyField.Create('Pozice obsazena!');
        end;

        Self.AddToStructure(Position, Self.Operations.TextProperties.Text, Self.Operations.TextProperties.Color,
          Self.Operations.TextProperties.BlokPopisek);

        Self.Operations.TextProperties.Text := '';
        Self.Operations.TextProperties.Color := scPurple;
        Self.Operations.moveStep := gosNone;

        // znovu pripraveni pohybu objektu
        if Assigned(FOnShow) then
        begin
          FOnShow;
          Sleep(50);
        end;
        if (Assigned(Self.FMoveActivate)) then
          Self.FMoveActivate;
      end;

  end; // case
end;

// mazani textu
procedure TText.Deleting(Position: TPoint);
begin
  if (Self.GetPopisek(Position) = -1) then
    Exit();

  Self.Operations.deleteStep := gosNone;

  Self.DeleteFromStructure(Position);

  Self.Operations.deleteStep := gosNone;
  if Assigned(FOnShow) then
  begin
    FOnShow;
    Sleep(50);
  end;

  if (Assigned(Self.FDeleteActivate)) then
    Self.FDeleteActivate;
end;

function TText.IsObsazeno(Pos1, Pos2: TPoint): boolean;
begin
  Result := false;

  // kontrola obsazenosti
  for var i := Pos1.X to Pos2.X do
    for var j := Pos1.Y to Pos2.Y do
      if (Self.GetPopisek(Point(i, j)) <> -1) then
        Exit(true);
end;

procedure TText.Add(aText: string; aColor: SymbolColor; popisekBlok: boolean);
begin
  if (Length(aText) > _MAX_TEXT_LENGTH) then
    raise ETooLongText.Create('Text je příliš dlouhý!');

  Self.CheckOpInProgressAndExcept();

  Self.operations.addStep := gosActive;
  Self.operations.TextProperties.Text := aText;
  Self.operations.TextProperties.Color := aColor;
  Self.operations.TextProperties.BlokPopisek := popisekBlok;
end;

procedure TText.Move();
begin
  Self.CheckOpInProgressAndExcept();
  Self.Operations.moveStep := gosActive;
end;

procedure TText.Delete();
begin
  Self.CheckOpInProgressAndExcept();
  Self.Operations.deleteStep := gosActive;
end;

procedure TText.MouseUp(Position: TPoint; Button: TMouseButton);
begin
  if (Button = mbLeft) then
  begin
    if (Self.Operations.addStep > gosNone) then
      Self.Adding(Position)
    else if (Self.Operations.moveStep > gosNone) then
      Self.Moving(Position)
    else if (Self.Operations.deleteStep > gosNone) then
      Self.Deleting(Position);
  end;

  if (Button = mbRight) then
  begin
    Self.MenuPosition := Position;
    if (Self.GetPopisek(Position) <> -1) then
      Self.TextMenu.Popup(Mouse.CursorPos.X, Mouse.CursorPos.Y);
  end;
end;

// zabyva se vykreslovanim posouvanych objektu textu
procedure TText.PaintTextMove(KurzorPos: TPoint);
begin
  // pridavani, posouvani
  if ((Self.operations.addStep > gosNone) or (Self.operations.moveStep > gosNone)) then
    Self.Graphics.TextOutputI(KurzorPos, Self.Operations.TextProperties.Text, Self.Operations.TextProperties.Color,
      clBlack, Self.Operations.TextProperties.BlokPopisek);
end;

// vykresleni kurzoru - vraci data PIXELECH!
function TText.PaintCursor(CursorPos: TPoint): TCursorDraw;
var PopisekI: Integer;
  PData: TPopisek;
begin
  Result.color := TCursorColor.ccDefault;
  Result.Pos1.X := CursorPos.X * _SYMBOL_WIDTH;
  Result.Pos1.Y := CursorPos.Y * _SYMBOL_HEIGHT;
  Result.Pos2.X := CursorPos.X * _SYMBOL_WIDTH;
  Result.Pos2.Y := CursorPos.Y * _SYMBOL_HEIGHT;

  if (Self.Operations.addStep = gosActive) then
  begin
    Result.Pos2.Y := CursorPos.Y * _SYMBOL_HEIGHT;
    Result.Pos2.X := (CursorPos.X + (Length(Self.Operations.TextProperties.Text) - 1)) * _SYMBOL_WIDTH;

    Result.color := TCursorColor.ccOnObject;
  end;
  if ((Self.Operations.moveStep = gosActive) or (Self.Operations.deleteStep = gosActive)) then
  begin
    PopisekI := Self.GetPopisek(CursorPos);
    if (PopisekI = -1) then
    begin
      Result.color := TCursorColor.ccActiveOperation;
    end else begin
      Result.color := TCursorColor.ccOnObject;
      PData := Self.GetPopisekData(PopisekI);
      Result.Pos1.X := PData.Position.X * _SYMBOL_WIDTH;
      Result.Pos1.Y := PData.Position.Y * _SYMBOL_HEIGHT;
      Result.Pos2.X := (PData.Position.X + Length(PData.Text) - 1) * _SYMBOL_WIDTH;
      Result.Pos2.Y := PData.Position.Y * _SYMBOL_HEIGHT;
    end; // else PopisekI = -1
  end;
  if (Self.Operations.moveStep = gosMoving) then
  begin
    Result.color := TCursorColor.ccOnObject;
    Result.Pos2.X := (CursorPos.X + Length(Self.Operations.TextProperties.Text) - 1) * _SYMBOL_WIDTH;
    Result.Pos2.Y := CursorPos.Y * _SYMBOL_HEIGHT;
  end;
end;

procedure TText.MITextPropertiesClick(Sender: TObject);
var PIndex: Integer;
  aPopisek: TPopisek;
begin
  PIndex := Self.GetPopisek(MenuPosition);

  aPopisek := Self.GetPopisekData(PIndex);
  if Assigned(FOnChangeText) then
    FOnChangeText(Self, aPopisek);
  Self.SetPopisekData(PIndex, aPopisek);

  if Assigned(FOnShow) then
    FOnShow();
end;

procedure TText.InitializeTextMenu(var Menu: TPopupMenu; Parent: TForm);
var MI: TMenuItem;
begin
  Menu := TPopupMenu.Create(Parent);

  MI := TMenuItem.Create(Menu);
  MI.Caption := 'Vlastnosti';
  MI.Enabled := true;
  MI.Visible := true;
  MI.OnClick := Self.MITextPropertiesClick;

  Menu.Items.Add(MI);
end;

function TText.GetCount(): Integer;
begin
  Result := Self.Data.Count;
end;

procedure TText.CheckOpInProgressAndExcept();
begin
  if (Assigned(Self.FOPAsk)) then
  begin
    if (Self.FOPAsk) then
      raise EOperationInProgress.Create('Právě probíhá operace!');
  end else begin
    if ((Self.Operations.addStep > gosNone) or (Self.Operations.moveStep > gosNone) or (Self.Operations.deleteStep > gosNone)) then
      raise EOperationInProgress.Create('Právě probíhá operace!');
  end;
end;

end.// unit
