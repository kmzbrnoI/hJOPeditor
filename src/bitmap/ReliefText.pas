unit ReliefText;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, IniFiles,
  StrUtils, Global, Menus, Forms, PGraphics, Generics.Collections, symbolHelper,
  Types, ReliefCommon;

const
  _MAX_POPISKY = 1024;
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
    Description: boolean;
  end;

  TChangeTextEvent = procedure(Sender: TObject; var popisek: TPanelLabel) of object;

  TText = class
  private
    mOnShow: TNEvent;
    mIsConflict: TPosAskEvent;
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
      MoveBuf: TList<TPanelLabel>;
    end; // Operations

    procedure MITextPropertiesClick(Sender: TObject);
    procedure InitializeTextMenu(var Menu: TPopupMenu; Parent: TForm);

    function GetCount(): Integer;
    function IsConflict(pos: TPoint): Boolean;
    procedure LoadBpnlV31OrOlder(var f: File; fileVersion: Byte);
    procedure LoadBpnlV32OrNewer(var f: File; fileVersion: Byte);


  public
    addText: TPanelLabel;

    constructor Create(DrawCanvas: TCanvas; TextIL: TImageList; Parent: TForm; Graphics: TPanelGraphics);
    destructor Destroy(); override;

    procedure Add(aPos: TPoint; aText: string; aColor: SymbolColor; aBlokDesc: boolean); overload;
    procedure Delete(aPos: TPoint); overload;

    function GetText(Index: Cardinal): TPanelLabel;
    procedure SetText(Index: Cardinal; Data: TPanelLabel);

    procedure Paint(showPopisky: boolean);
    procedure PaintMoveBuffer(pos: TPoint);
    procedure PaintAddText(pos: TPoint);

    function GetTextI(aPos: TPoint): Integer;
    function IsOccupied(Pos1, Pos2: TPoint): boolean;

    procedure Add(pos: TPoint); overload;
    function MoveDrag(pos1: TPoint; pos2: TPoint): Boolean; // returns if anything dragged
    procedure MoveDrop(pos: TPoint);
    function CanMoveDrop(pos: TPoint): Boolean;
    procedure Delete(pos1: TPoint; pos2: TPoint); overload;

    procedure LoadBpnl(var f: File; fileVersion: Byte);
    procedure WriteBpnl(var f: File);
    procedure Clear();

    procedure MouseUp(Position: TPoint; Button: TMouseButton);

    property count: Integer read GetCount;

    property QIsConflict: TPosAskEvent read mIsConflict write mIsConflict;
    property OnChangeText: TChangeTextEvent read mOnChangeText write mOnChangeText;
    property OnShow: TNEvent read mOnShow write mOnShow;
  end; // TText

implementation

uses ReliefBitmap;

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
  p.Description := aBlokDesc;

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
function TText.GetTextI(aPos: TPoint): Integer;
begin
  Result := -1;

  // vychazime z toho, ze 1 popisek lze zapsat pouze na 1 radek
  for var i := 0 to Self.Count - 1 do
    if ((Self.Data[i].Position.Y = aPos.Y) and (aPos.X >= Self.Data[i].Position.X) and (aPos.X < (Self.Data[i].Position.X+Length(Self.Data[i].Text)))) then
      Exit(i);
end;

procedure TText.LoadBpnl(var f: File; fileVersion: Byte);
begin
  if (fileVersion >= $32) then
    Self.LoadBpnlV32OrNewer(f, fileVersion)
  else
    Self.LoadBpnlV31OrOlder(f, fileVersion);
end;

procedure TText.LoadBpnlV31OrOlder(var f: File; fileVersion: Byte);
var buffer: array [0 .. 255] of Byte;
begin
  TPanelBitmap.BlockReadOrException(f, buffer, 1, 'délka bloku textů');
  const count = buffer[0];
  TPanelBitmap.BlockReadOrException(f, buffer, count*_Block_Length, 'texty');

  Self.Data.Clear();

  for var i := 0 to count-1 do
  begin
    var p: TPanelLabel;
    p.Position.X := buffer[(i * _Block_Length)];
    p.Position.Y := buffer[(i * _Block_Length) + 1];
    p.Color := SymbolColor(buffer[(i * _Block_Length) + 2]);
    p.Text := '';
    p.Description := false;

    for var j := 0 to _MAX_TEXT_LENGTH do
    begin
      if ((buffer[(i * _Block_Length) + 3 + (j * 2)] = 0) and (buffer[(i * _Block_Length) + 3 + (j * 2) + 1] = 0)) then
        Break;
      p.Text := p.Text + chr(((buffer[(i * _Block_Length) + 3 + (j * 2)]) shl 8) + buffer[(i * _Block_Length) + 3 + (j * 2) + 1]);
    end; // for j

    Self.Data.Add(p);
  end; // for i
end;

procedure TText.LoadBpnlV32OrNewer(var f: File; fileVersion: Byte);
var buffer: TBytes;
    totalLength: Integer;
begin
  SetLength(buffer, 256);

  TPanelBitmap.BlockReadOrException(f, buffer, 2, 'délka bloku textů');
  totalLength := (buffer[0] shl 8) or buffer[1];

  Self.Data.Clear();
  var pos: Integer := 0;
  var i: Integer := 0;

  while (pos < totalLength) do
  begin
    var p: TPanelLabel;
    var offset: Integer;

    if (fileVersion >= $42) then
    begin
      TPanelBitmap.BlockReadOrException(f, buffer, 6, 'hlavička textu');
      p.Position.X := (buffer[0] shl 8) or buffer[1];
      p.Position.Y := (buffer[2] shl 8) or buffer[3];
      offset := 4;
    end else begin
      TPanelBitmap.BlockReadOrException(f, buffer, 4, 'hlavička textu');
      p.Position.X := buffer[0];
      p.Position.Y := buffer[1];
      offset := 2;
    end;

    p.Color := SymbolColor(buffer[offset+0]);
    const textLen = buffer[offset+1];

    TPanelBitmap.BlockReadOrException(f, buffer, textLen, 'text');
    p.Description := (buffer[0] = _POPISEK_MAGIC_CODE);
    if (p.Description) then
      p.Text := TEncoding.UTF8.GetString(buffer, 1, textLen - 1)
    else
      p.Text := TEncoding.UTF8.GetString(buffer, 0, textLen);

    Self.Data.Add(p);
    i := i + 1;
    pos := pos + offset + 2 + textLen;
  end; // for i

  Self.Data.Count := i;
end;

// ziskani surovych dat zapisovanych do souboru z dat programu
procedure TText.WriteBpnl(var f: File);
var length: Cardinal;
    buf: array [0..7] of Byte;
begin
  var originPos: Integer := FilePos(f);
  Seek(f, originPos+2);
  length := 0;

  for var p in Self.Data do
  begin
    buf[0] := Hi(p.Position.X);
    buf[1] := Lo(p.Position.X);
    buf[2] := Hi(p.Position.Y);
    buf[3] := Lo(p.Position.Y);
    buf[4] := Byte(p.Color);

    var textLength: Cardinal := TEncoding.UTF8.GetByteCount(p.Text);
    var bytesBuf: TBytes;
    SetLength(bytesBuf, textLength);
    bytesBuf := TEncoding.UTF8.GetBytes(p.Text);

    var headerLen: Cardinal;
    if (p.Description) then
    begin
      buf[5] := textLength + 1;
      buf[6] := _POPISEK_MAGIC_CODE;
      headerLen := 7;
    end else begin
      buf[5] := textLength;
      headerLen := 6;
    end;

    if ((length + headerLen + textLength) > $FFFF) then
      break; // just to be sure

    BlockWrite(f, buf, headerLen);
    BlockWrite(f, bytesBuf[0], textLength);
    length := length + headerLen + textLength;
  end;

  var endPos := FilePos(f);
  Seek(f, originPos);
  buf[0] := Hi(length);
  buf[1] := Lo(length);
  BlockWrite(f, buf, 2);
  Seek(f, endPos);
end;

procedure TText.Clear();
begin
  Self.Data.Clear();
end;

procedure TText.Paint(showPopisky: boolean);
begin
  for var popisek in Self.Data do
    if (showPopisky) or (not popisek.Description) then
      Self.Graphics.TextOutputI(popisek.Position, popisek.Text, popisek.Color, clBlack, popisek.Description, true);
end;

function TText.GetText(Index: Cardinal): TPanelLabel;
begin
  if (Index >= Cardinal(Self.Data.Count)) then
    raise EArgumentOutOfRangeException.Create('Text index is out of range!');
  Result := Self.Data[Index];
end;

procedure TText.SetText(Index: Cardinal; Data: TPanelLabel);
begin
  if (Index >= Cardinal(Self.Data.Count)) then
    raise EArgumentOutOfRangeException.Create('Text index is out of range!');
  Self.Data[Index] := Data;
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

procedure TText.Add(pos: TPoint);
begin
  Self.Add(pos, Self.addText.Text, Self.addText.Color, Self.addText.Description)
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
        lbl.Position := Point(lbl.Position.X-pos2.X, lbl.Position.Y-pos2.Y);
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
    Self.Add(Point(pos.X+lbl.Position.X, pos.Y+lbl.Position.Y), lbl.Text, lbl.Color, lbl.Description);
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
      lbl.Text, lbl.Color, clBlack, lbl.Description
    );
end;

procedure TText.PaintAddText(pos: TPoint);
begin
  Self.Graphics.TextOutputI(pos, Self.addText.Text, Self.addText.Color, clBlack, Self.addText.Description);
end;

procedure TText.MITextPropertiesClick(Sender: TObject);
var PIndex: Integer;
begin
  PIndex := Self.GetTextI(MenuPosition);
  if (PIndex < 0) then
    Exit();
  var aLabel := Self.GetText(Cardinal(PIndex));
  if (Assigned(Self.OnChangeText)) then
    Self.OnChangeText(Self, aLabel);
  Self.SetText(PIndex, aLabel);

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

function TText.IsConflict(pos: TPoint): Boolean;
begin
  if (Assigned(Self.mIsConflict)) then
    Result := Self.mIsConflict(pos)
  else
    Result := Self.IsOccupied(pos, pos);
end;

end.// unit
