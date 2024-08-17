﻿unit ReliefText;

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

  TPanelLabel = record
    Position: TPoint;
    Text: string;
    Color: SymbolColor;
    BlokPopisek: boolean;
  end;

  TChangeTextEvent = procedure(Sender: TObject; var popisek: TPanelLabel) of object;

  TText = class
  private
    mOnShow: TNEvent;
    mIsConflict: TPosAskEvent;
    mNullOperations: TNEvent;
    mOPAsk: TOpAskEvent;
    mOnChangeText: TChangeTextEvent;

    Data: TList<TPanelLabel>;

    TextMenu: TPopupMenu;
    MenuPosition: TPoint;
    Graphics: TPanelGraphics;

    DrawObject: record
      Canvas: TCanvas;
      TextIL: TImageList;
    end;

    Operations: record
      AddStep: TGOpStep;
      MoveBuf: TList<TPanelLabel>;

      TextProperties: record
        Text: string;
        Color: SymbolColor;
        BlokPopisek: boolean;
      end;
    end; // Operations

    procedure Adding(Position: TPoint);

    procedure MITextPropertiesClick(Sender: TObject);
    procedure InitializeTextMenu(var Menu: TPopupMenu; Parent: TForm);

    function GetCount(): Integer;
    procedure CheckOpInProgressAndExcept();

    function IsConflict(pos: TPoint): Boolean;

  public

    constructor Create(DrawCanvas: TCanvas; TextIL: TImageList; Parent: TForm; Graphics: TPanelGraphics);
    destructor Destroy(); override;

    procedure Add(aPos: TPoint; aText: string; aColor: SymbolColor; aBlokDesc: boolean); overload;
    procedure Delete(aPos: TPoint); overload;

    function GetPopisekData(Index: Integer): TPanelLabel;
    function SetPopisekData(Index: Integer; Data: TPanelLabel): Byte;

    procedure Paint(showPopisky: boolean);
    procedure PaintMoveBuffer(pos: TPoint);
    function PaintCursor(CursorPos: TPoint): TCursorDraw;

    procedure Escape();

    function GetTextI(aPos: TPoint): SmallInt;
    function IsOccupied(Pos1, Pos2: TPoint): boolean;

    procedure Add(aText: string; aColor: SymbolColor; popisekBlok: boolean); overload;
    function MoveDrag(pos1: TPoint; pos2: TPoint): Boolean; // returnbs if anything dragged
    procedure MoveDrop(pos: TPoint);
    function CanMoveDrop(pos: TPoint): Boolean;
    procedure Delete(pos1: TPoint; pos2: TPoint); overload;

    procedure SetLoadedData(LoadData: TBytes);
    procedure SetLoadedDataV32(LoadData: TBytes);
    function GetSaveData: TBytes;
    procedure Clear();

    procedure MouseUp(Position: TPoint; Button: TMouseButton);

    property addStep: TGOpStep read Operations.AddStep;
    property count: Integer read GetCount;

    property OnShow: TNEvent read mOnShow write mOnShow;
    property QIsConflict: TPosAskEvent read mIsConflict write mIsConflict;
    property IsOp: TOpAskEvent read mOPAsk write mOPAsk;
    property OnNullOperations: TNEvent read mNullOperations write mNullOperations;
    property OnChangeText: TChangeTextEvent read mOnChangeText write mOnChangeText;
  end; // TText

implementation

constructor TText.Create(DrawCanvas: TCanvas; TextIL: TImageList; Parent: TForm; Graphics: TPanelGraphics);
begin
  inherited Create();

  Self.DrawObject.Canvas := DrawCanvas;
  Self.DrawObject.TextIL := TextIL;
  Self.Graphics := Graphics;
  Self.Data := TList<TPanelLabel>.Create();
  Self.Operations.MoveBuf := TList<TPanelLabel>.Create();

  Self.InitializeTextMenu(Self.TextMenu, Parent);
  Self.Clear();
end;

destructor TText.Destroy();
begin
  Self.Operations.MoveBuf.Free();
  Self.Data.Free();

  if (Assigned(Self.TextMenu)) then
    FreeAndNil(Self.TextMenu);

  inherited;
end;

// pridani popisku
procedure TText.Add(aPos: TPoint; aText: string; aColor: SymbolColor; aBlokDesc: boolean);
begin
  if ((aPos.X < 0) or (aPos.Y < 0) or (aPos.X > (_MAX_WIDTH - 1)) or (aPos.Y > (_MAX_HEIGHT - 1))) then
    raise EInvalidPosition.Create('Neplatná pozice!');
  if (Self.GetTextI(aPos) <> -1) then
    raise ENonemptyField.Create('Na pozici je již symbol!');
  if (Self.count >= _MAX_POPISKY) then
    raise EMaxReached.Create('Dosaženo maximálního počtu textů!');
  if (Length(aText) > _MAX_TEXT_LENGTH) then
    raise ETooLongText.Create('Text je příliš dlouhý!');

  var p: TPanelLabel;
  p.Position := aPos;
  p.Text := aText;
  p.Color := aColor;
  p.BlokPopisek := aBlokDesc;

  Self.Data.Add(p);
end;

// smazani popisku
procedure TText.Delete(aPos: TPoint);
begin
  if ((aPos.X < 0) or (aPos.Y < 0) or (aPos.X > (_MAX_WIDTH - 1)) or (aPos.Y > (_MAX_HEIGHT - 1))) then
    raise EInvalidPosition.Create('Neplatná pozice!');

  var PIndex := Self.GetTextI(aPos);
  if (PIndex = -1) then
    raise ENoSymbol.Create('Na této pozici není žádný symbol!');

  Self.Data.Delete(PIndex);
end;

// zjisteni, zda-li je na dane pozici popisek, popr jeho index v poli separatoru
function TText.GetTextI(aPos: TPoint): SmallInt;
begin
  Result := -1;

  // vychazime z toho, ze 1 popisek lze zapsat pouze na 1 radek
  for var i := 0 to Self.Count - 1 do
    if ((Self.Data[i].Position.Y = aPos.Y) and (aPos.X >= Self.Data[i].Position.X) and (aPos.X < (Self.Data[i].Position.X+Length(Self.Data[i].Text)))) then
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
    var p: TPanelLabel;
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
begin
  Self.Data.Clear();
  var pos: Integer := 0;
  var i: Integer := 0;

  while pos < Length(LoadData) do
  begin
    var p: TPanelLabel;
    p.Position.X := LoadData[pos];
    p.Position.Y := LoadData[pos + 1];
    p.Color := SymbolColor(LoadData[pos + 2]);
    var len: Integer := LoadData[pos + 3];
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
  Self.Operations.MoveBuf.Clear();
end;

procedure TText.Paint(showPopisky: boolean);
begin
  for var popisek in Self.Data do
    if (showPopisky) or (not popisek.BlokPopisek) then
      Self.Graphics.TextOutputI(popisek.Position, popisek.Text, popisek.Color, clBlack, popisek.BlokPopisek, true);
end;

function TText.GetPopisekData(Index: Integer): TPanelLabel;
begin
  if (Index >= _MAX_POPISKY) then
  begin
    Result.Color := scPurple;
    Exit;
  end;

  Result := Self.Data[Index];
end;

function TText.SetPopisekData(Index: Integer; Data: TPanelLabel): Byte;
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
  for var i := Position.X to Position.X + Length(Self.Operations.TextProperties.Text) - 1 do
    if (Self.IsConflict(Point(i, Position.Y))) then
      raise ENonemptyField.Create('Na pozici je již symbol!');

  if (Self.IsOccupied(Position, Point(Position.X + Length(Self.Operations.TextProperties.Text), Position.Y))) then
    raise ENonemptyField.Create('Na pozici je již symbol!');

  Self.Add(Position, Self.Operations.TextProperties.Text, Self.Operations.TextProperties.Color,
    Self.Operations.TextProperties.BlokPopisek);

  Self.Operations.addStep := gosNone;
end;

function TText.IsOccupied(Pos1, Pos2: TPoint): boolean;
begin
  Result := false;

  // kontrola obsazenosti
  for var i := Pos1.X to Pos2.X do
    for var j := Pos1.Y to Pos2.Y do
      if (Self.GetTextI(Point(i, j)) <> -1) then
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

function TText.MoveDrag(pos1: TPoint; pos2: TPoint): Boolean;
begin
  Result := False;
  Self.Operations.MoveBuf.Clear();
  for var x := pos1.X to pos2.X do
  begin
    for var y := pos1.Y to pos2.Y do
    begin
      var i: Integer := Self.GetTextI(Point(x, y));
      if (i > -1) then
      begin
        var lbl: TPanelLabel := Self.Data[i];
        lbl.Position := Point(lbl.Position.X-pos1.X, lbl.Position.Y-pos1.Y);
        Self.Operations.MoveBuf.Add(lbl);
        Self.Data.Delete(i);
        Result := True;
      end;
    end;
  end;
end;

procedure TText.MoveDrop(pos: TPoint);
begin
  for var lbl: TPanelLabel in Self.Operations.MoveBuf do
    Self.Add(Point(pos.X+lbl.Position.X, pos.Y+lbl.Position.Y), lbl.Text, lbl.Color, lbl.BlokPopisek);
  Self.Operations.MoveBuf.Clear();
end;

function TText.CanMoveDrop(pos: TPoint): Boolean;
begin
  Result := True;
  for var lbl: TPanelLabel in Self.Operations.MoveBuf do
  begin
    for var x: Integer := 0 to Length(lbl.Text)-1 do
      if (Self.IsConflict(Point(lbl.Position.X+pos.X+x, lbl.Position.Y+pos.Y))) then
        Exit(False);
  end;
end;

procedure TText.Delete(pos1: TPoint; pos2: TPoint);
begin
  for var x := pos1.X to pos2.X do
    for var y := pos1.Y to pos2.Y do
      if (Self.GetTextI(Point(x, y)) <> -1) then
        Self.Delete(Point(x, y));
end;

procedure TText.MouseUp(Position: TPoint; Button: TMouseButton);
begin
  if (Button = mbLeft) then
  begin
    if (Self.Operations.addStep > gosNone) then
      Self.Adding(Position);
  end;

  if (Button = mbRight) then
  begin
    Self.MenuPosition := Position;
    if (Self.GetTextI(Position) <> -1) then
      Self.TextMenu.Popup(Mouse.CursorPos.X, Mouse.CursorPos.Y);
  end;
end;

procedure TText.PaintMoveBuffer(pos: TPoint);
begin
  for var lbl: TPanelLabel in Self.Operations.MoveBuf do
    Self.Graphics.TextOutputI(
      Point(pos.X+lbl.Position.X, pos.Y+lbl.Position.Y),
      lbl.Text, lbl.Color, clBlack, lbl.BlokPopisek
    );
end;

// vykresleni kurzoru - vraci data PIXELECH!
function TText.PaintCursor(CursorPos: TPoint): TCursorDraw;
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
end;

procedure TText.MITextPropertiesClick(Sender: TObject);
var PIndex: Integer;
begin
  PIndex := Self.GetTextI(MenuPosition);

  var aPopisek := Self.GetPopisekData(PIndex);
  if (Assigned(Self.OnChangeText)) then
    Self.OnChangeText(Self, aPopisek);
  Self.SetPopisekData(PIndex, aPopisek);

  if (Assigned(Self.OnShow)) then
    Self.OnShow();
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
  if (Assigned(Self.mOPAsk)) then
  begin
    if (Self.mOPAsk) then
      raise EOperationInProgress.Create('Právě probíhá operace!');
  end else begin
    if (Self.Operations.addStep > gosNone) then
      raise EOperationInProgress.Create('Právě probíhá operace!');
  end;
end;

function TText.IsConflict(pos: TPoint): Boolean;
begin
  if (Assigned(Self.mIsConflict)) then
    Result := Self.mIsConflict(pos)
  else
    Result := Self.IsOccupied(pos, pos);
end;
end.// unit
