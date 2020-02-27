unit ReliefText;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, IniFiles,
  StrUtils, Global, Menus, Forms, PGraphics, Generics.Collections, symbolHelper;

const
 _MAX_POPISKY     = 256;
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
    Position:TPoint;
    Text:string;
    Color:ShortInt;
    BlokPopisek:boolean;
  end;

  TChangeTextEvent = procedure(Sender:TObject; var popisek:TPopisek) of object;

  TText = class
   private
     Data:TList<TPopisek>;

     TextMenu:TPopupMenu;
     MenuPosition:TPoint;
     Graphics:TPanelGraphics;

     DrawObject:record
      Canvas:TCanvas;
      TextIL:TImageList;
     end;

     Operations:record
      FAddKrok:Byte;
      FMoveKrok:Byte;
      FDeleteKrok:Byte;
      TextProperties:record
       Text:string;
       Color:Integer;
       blokPopisek:boolean;
      end;
     end;//Operations

    procedure Adding(Position:TPoint);
    procedure Moving(Position:TPoint);
    procedure Deleting(Position:TPoint);

    procedure MITextPropertiesClick(Sender:TObject);
    procedure InitializeTextMenu(var Menu:TPopupMenu;Parent:TForm);

    function GetCount():Integer;
    procedure CheckOpInProgressAndExcept();

   public
    FOnShow : TNEvent;
    FIsSymbol : TPosAskEvent;
    FNullOperations : TNEvent;
    FMoveActivate : TNEvent;
    FDeleteActivate : TNEvent;
    FOPAsk : TOpAskEvent;
    FOnChangeText: TCHangeTextEvent;

     constructor Create(DrawCanvas:TCanvas;TextIL:TImageList;Parent:TForm; Graphics:TPanelGraphics);
     destructor Destroy; override;

     procedure AddToStructure(aPos:TPoint; aText:string; aColor:ShortInt; aBlokDesc:boolean);
     procedure DeleteFromStructure(aPos:TPoint);

     function GetPopisekData(Index:Integer):TPopisek;
     function SetPopisekData(Index:Integer;Data:TPopisek):Byte;

     procedure Paint(showPopisky:boolean);
     procedure PaintTextMove(KurzorPos:TPoint);
     function PaintCursor(CursorPos:TPoint):TCursorDraw;

     procedure Escape;

     function GetPopisek(aPos:TPoint):SmallInt;
     function IsObsazeno(Pos1,Pos2:TPoint):boolean;

     procedure Add(aText:string; aColor:ShortInt; popisekBlok:boolean);
     procedure Move();
     procedure Delete();

     procedure SetLoadedData(LoadData:TBytes);
     procedure SetLoadedDataV32(LoadData:TBytes);
     function GetSaveData:TBytes;
     procedure Reset;

     procedure MouseUp(Position:TPoint;Button:TMouseButton);

     property AddKrok:Byte read Operations.FAddKrok;
     property MoveKrok:Byte read Operations.FMoveKrok;
     property DeleteKrok:Byte read Operations.FDeleteKrok;
     property Count:Integer read GetCount;

     property OnShow: TNEvent read FOnShow write FOnShow;
     property IsSymbol: TPosAskEvent read FIsSymbol write FIsSymbol;
     property IsOp: TOpAskEvent read FOPAsk write FOPAsk;
     property OnNullOperations: TNEvent read FNullOperations write FNullOperations;
     property OnMoveActivate: TNEvent read FMoveActivate write FMoveActivate;
     property OnDeleteActivate: TNEvent read FDeleteActivate write FDeleteActivate;
     property OnChangeText: TChangeTextEvent read FOnChangeText write FOnChangeText;
  end;//TText

implementation

constructor TText.Create(DrawCanvas:TCanvas;TextIL:TImageList;Parent:TForm; Graphics:TPanelGraphics);
begin
 inherited Create();

 Self.DrawObject.Canvas := DrawCanvas;
 Self.DrawObject.TextIL := TextIL;
 Self.Graphics := Graphics;
 Self.Data := TList<TPopisek>.Create();

 Self.InitializeTextMenu(Self.TextMenu,Parent);
 Self.Reset();
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

//pridani popisku
procedure TText.AddToStructure(aPos:TPoint; aText:string; aColor:ShortInt; aBlokDesc:boolean);
var p:TPopisek;
begin
 if ((aPos.X < 0) or (aPos.Y < 0) or (aPos.X > (_MAX_WIDTH-1)) or (aPos.Y > (_MAX_HEIGHT-1))) then
   raise EInvalidPosition.Create('Neplatná pozice!');
 if (Self.GetPopisek(aPos) <> -1) then
   raise ENonemptyField.Create('Na pozici je již symbol!');
 if (Self.Count >= _MAX_POPISKY) then
   raise EMaxReached.Create('Dosaženo maximálního počtu textů!');
 if (Length(aText) > _MAX_TEXT_LENGTH) then
   raise ETooLongText.Create('Text je příliš dlouhý!');

 p.Position := aPos;
 p.Text := aText;
 p.Color := aColor;
 p.BlokPopisek := aBlokDesc;

 Self.Data.Add(p);
end;

//smazani popisku
procedure TText.DeleteFromStructure(aPos:TPoint);
var PIndex:Integer;
begin
 if ((aPos.X < 0) or (aPos.Y < 0) or (aPos.X > (_MAX_WIDTH-1)) or (aPos.Y > (_MAX_HEIGHT-1))) then
   raise EInvalidPosition.Create('Neplatná pozice!');

 PIndex := Self.GetPopisek(aPos);
 if (PIndex = -1) then
   raise ENoSymbol.Create('Na této pozici není žádný symbol!');

 Self.Data.Delete(PIndex);
end;

//zjisteni, zda-li je na dane pozici popisek, popr jeho index v poli separatoru
function TText.GetPopisek(aPos:TPoint):SmallInt;
var i,j:Integer;
begin
 Result := -1;

 //vychazime z toho, ze 1 popisek lze zapsat pouze na 1 radek
 for i := 0 to Self.Count-1 do
   if (Self.Data[i].Position.Y = aPos.Y) then
     for j := Self.Data[i].Position.X to Self.Data[i].Position.X+(Length(Self.Data[i].Text))-1 do
       if (j = aPos.X) then
         Exit(i);
end;

//nacteni surovych dat do struktur
procedure TText.SetLoadedData(LoadData:TBytes);
var i, j:Integer;
    p:TPopisek;
    count:Integer;
begin
 Self.Data.Clear();
 count := Length(LoadData) div _Block_Length;

 for i := 0 to count-1 do
  begin
   p.Position.X := LoadData[(i*_Block_Length)];
   p.Position.Y := LoadData[(i*_Block_Length)+1];
   p.Color := LoadData[(i*_Block_Length)+2];
   p.Text := '';
   p.BlokPopisek := false;

   for j := 0 to _MAX_TEXT_LENGTH do
    begin
     if ((LoadData[(i*_Block_Length)+3+(j*2)] = 0) and (LoadData[(i*_Block_Length)+3+(j*2)+1] = 0)) then Break;
     p.Text := p.Text + chr(((LoadData[(i*_Block_Length)+3+(j*2)]) shl 8) +
                        LoadData[(i*_Block_Length)+3+(j*2)+1]);
    end;//for j

   Self.Data.Add(p);
  end;//for i
end;

procedure TText.SetLoadedDataV32(LoadData:TBytes);
var pos:Integer;
    i:Integer;
    len:Integer;
    p:TPopisek;
begin
 Self.Data.Clear();
 pos := 0;
 i := 0;

 while pos < Length(LoadData) do
  begin
   p.Position.X := LoadData[pos];
   p.Position.Y := LoadData[pos+1];
   p.Color := LoadData[pos+2];
   len := LoadData[pos+3];
   p.BlokPopisek := (LoadData[pos+4] = _POPISEK_MAGIC_CODE);
   if (p.BlokPopisek) then
     p.Text := TEncoding.UTF8.GetString(LoadData, pos+5, len-1)
   else
     p.Text := TEncoding.UTF8.GetString(LoadData, pos+4, len);

   Self.Data.Add(p);
   i := i + 1;
   pos := pos + 4 + len;
  end;//for i

 Self.Data.Count := i;
end;

//ziskani surovych dat zapisovanych do souboru z dat programu
function TText.GetSaveData():TBytes;
var bytesBuf:TBytes;
    len:Integer;
    currentLen:Integer;
    p:TPopisek;
    offset:Integer;
begin
 SetLength(Result, 1024);
 currentLen := 2;

 for p in Self.Data do
  begin
   len := TEncoding.UTF8.GetByteCount(p.Text);
   SetLength(bytesBuf, len);
   bytesBuf := TEncoding.UTF8.GetBytes(p.Text);

   if (Length(Result) < currentLen + len + 4) then
     SetLength(Result, Length(Result)*2);

   Result[currentLen] := p.Position.X;
   Result[currentLen+1] := p.Position.Y;
   Result[currentLen+2] := p.Color;

   if (p.BlokPopisek) then
    begin
     Result[currentLen+3] := len+1;
     Result[currentLen+4] := _POPISEK_MAGIC_CODE;
     offset := currentLen+5;
    end else begin
     Result[currentLen+3] := len;
     offset := currentLen+4;
    end;

   CopyMemory(@Result[offset], bytesBuf, len);
   currentLen := offset + len;
  end;//for i

 SetLength(Result, currentLen);
 Result[0] := hi(currentLen-2);
 Result[1] := lo(currentLen-2);
end;

procedure TText.Reset();
begin
 Self.Data.Count := 0;
 Self.Escape;
end;

procedure TText.Escape();
begin
 Self.Operations.FAddKrok    := 0;
 Self.Operations.FMoveKrok   := 0;
 Self.Operations.FDeleteKrok := 0;
end;

//vykresleni textu
procedure TText.Paint(showPopisky:boolean);
var popisek:TPopisek;
begin
 //vykresleni textu
 for popisek in Self.Data do
   if (showPopisky) or (not popisek.BlokPopisek) then
     Self.Graphics.TextOutputI(popisek.Position, popisek.Text, popisek.Color, clBlack, popisek.BlokPopisek);
end;

function TText.GetPopisekData(Index:Integer):TPopisek;
begin
 if (Index >= _MAX_POPISKY) then
  begin
   Result.Color := -1;
   Exit;
  end;

 Result := Self.Data[Index];
end;

function TText.SetPopisekData(Index:Integer;Data:TPopisek):Byte;
begin
 if (Index >= _MAX_POPISKY) then
  begin
   Result := 1;
   Exit;
  end;//if (Index => _MAX_POPISKY)

 Self.Data[Index] := Data;

 Result := 0;
end;

//pridavani textu
procedure TText.Adding(Position:TPoint);
var i:Integer;
begin
 //kontrola obsazenosti pozice
 if (Assigned(FIsSymbol)) then
   for i := Position.X to Position.X+Length(Self.Operations.TextProperties.Text)-1 do
     if (FIsSymbol(Point(i,Position.Y))) then
       raise ENonemptyField.Create('Na pozici je již symbol!');

 if (Self.IsObsazeno(Position,Point(Position.X+Length(Self.Operations.TextProperties.Text),Position.Y))) then
   raise ENonemptyField.Create('Na pozici je již symbol!');

 Self.AddToStructure(Position, Self.Operations.TextProperties.Text,
                     Self.Operations.TextProperties.Color, Self.Operations.TextProperties.blokPopisek);

 Self.Operations.FAddKrok := 0;
end;

//pohyb textu
procedure TText.Moving(Position:TPoint);
var PopisekIndex:Integer;
    i:Integer;
begin
 //zde neni skupina pripustna
 case (Self.Operations.FMoveKrok) of
  1:begin
     PopisekIndex := Self.GetPopisek(Position);
     if (PopisekIndex = -1) then
       Exit();

     //ziskani dat popisku
     Self.Operations.TextProperties.Text := Self.GetPopisekData(PopisekIndex).Text;
     Self.Operations.TextProperties.Color := Self.GetPopisekData(PopisekIndex).Color;
     Self.Operations.TextProperties.blokPopisek := Self.GetPopisekData(PopisekIndex).BlokPopisek;

     Self.DeleteFromStructure(Position);

     if (Assigned(FNullOperations)) then FNullOperations;
     Self.Operations.FMoveKrok := 2;
    end;

  2:begin
     if (Assigned(FIsSymbol)) then
      begin
       for i := Position.X to Position.X+Length(Self.Operations.TextProperties.Text)-1 do
         if (FIsSymbol(Point(i,Position.Y))) then
           raise ENonemptyField.Create('Pozice obsazena!');
      end else begin
       if (Self.IsObsazeno(Position,Point(Position.X+Length(Self.Operations.TextProperties.Text),Position.Y))) then
         raise ENonemptyField.Create('Pozice obsazena!');
      end;

     Self.AddToStructure(Position,Self.Operations.TextProperties.Text,
                         Self.Operations.TextProperties.Color, Self.Operations.TextProperties.blokPopisek);

     Self.Operations.TextProperties.Text := '';
     Self.Operations.TextProperties.Color := 0;
     Self.Operations.FMoveKrok := 0;

     //znovu pripraveni pohybu objektu
     if Assigned(FOnShow) then
      begin
       FOnShow;
       Sleep(50);
      end;
     if (Assigned(Self.FMoveActivate)) then Self.FMoveActivate;
    end;

  end;//case
end;

//mazani textu
procedure TText.Deleting(Position:TPoint);
begin
 if (Self.GetPopisek(Position) = -1) then
   Exit();

 Self.Operations.FDeleteKrok := 0;

 Self.DeleteFromStructure(Position);

 Self.Operations.FDeleteKrok := 0;
 if Assigned(FOnShow) then
  begin
   FOnShow;
   Sleep(50);
  end;

 if (Assigned(Self.FDeleteActivate)) then Self.FDeleteActivate;
end;

function TText.IsObsazeno(Pos1,Pos2:TPoint):Boolean;
var i,j:Integer;
begin
 Result := false;

 //kontrola obsazenosti
 for i := Pos1.X to Pos2.X do
    for j := Pos1.Y to Pos2.Y do
      if (Self.GetPopisek(Point(i,j)) <> -1) then
        Exit(true);
end;

procedure TText.Add(aText:string; aColor:ShortInt; popisekBlok:boolean);
begin
 if (Length(aText) > _MAX_TEXT_LENGTH) then
   raise ETooLongText.Create('Text je příliš dlouhý!');

 Self.CheckOpInProgressAndExcept();

 Self.Operations.FAddKrok := 1;
 Self.Operations.TextProperties.Text  := aText;
 Self.Operations.TextProperties.Color := aColor;
 Self.Operations.TextProperties.blokPopisek := popisekBlok;
end;

procedure TText.Move();
begin
 Self.CheckOpInProgressAndExcept();
 Self.Operations.FMoveKrok := 1;
end;

procedure TText.Delete();
begin
 Self.CheckOpInProgressAndExcept();
 Self.Operations.FDeleteKrok := 1;
end;

procedure TText.MouseUp(Position:TPoint;Button:TMouseButton);
begin
 if (Button = mbLeft) then
  begin
   if (Self.Operations.FAddKrok > 0) then Self.Adding(Position)
   else if (Self.Operations.FMoveKrok > 0) then Self.Moving(Position)
   else if (Self.Operations.FDeleteKrok > 0) then Self.Deleting(Position);
  end;

 if (Button = mbRight) then
  begin
   Self.MenuPosition := Position;
   if (Self.GetPopisek(Position) <> -1) then Self.TextMenu.Popup(Mouse.CursorPos.X,Mouse.CursorPos.Y);
  end;
end;

//zabyva se vykreslovanim posouvanych objektu textu
procedure TText.PaintTextMove(KurzorPos:TPoint);
begin
 //pridavani, posouvani
 if ((Self.Operations.FAddKrok = 1) or (Self.Operations.FMoveKrok = 2)) then
     Self.Graphics.TextOutputI(KurzorPos, Self.Operations.TextProperties.Text,
                               Self.Operations.TextProperties.Color, clBlack, Self.Operations.TextProperties.blokPopisek);
end;

//vykresleni kurzoru - vraci data PIXELECH!
function TText.PaintCursor(CursorPos:TPoint):TCursorDraw;
var PopisekI:Integer;
    PData:TPopisek;
begin
 Result.Color := 0;
 Result.Pos1.X := CursorPos.X*_Symbol_Sirka;
 Result.Pos1.Y := CursorPos.Y*_Symbol_Vyska;
 Result.Pos2.X := CursorPos.X*_Symbol_Sirka;
 Result.Pos2.Y := CursorPos.Y*_Symbol_Vyska;

 if (Self.Operations.FAddKrok = 1) then
  begin
   Result.Pos2.Y := CursorPos.Y*_Symbol_Vyska;
   Result.Pos2.X := (CursorPos.X+(Length(Self.Operations.TextProperties.Text)-1))*_Symbol_Sirka;

   Result.Color := 2;
  end;
 if ((Self.Operations.FMoveKrok = 1) or (Self.Operations.FDeleteKrok = 1)) then
  begin
   PopisekI := Self.GetPopisek(CursorPos);
   if (PopisekI = -1) then
    begin
     Result.Color := 1;
    end else begin
     Result.Color := 2;
     PData := Self.GetPopisekData(PopisekI);
     Result.Pos1.X := PData.Position.X*_Symbol_Sirka;
     Result.Pos1.Y := PData.Position.Y*_Symbol_Vyska;
     Result.Pos2.X := (PData.Position.X+Length(Pdata.Text)-1)*_Symbol_Sirka;
     Result.Pos2.Y := PData.Position.Y*_Symbol_Vyska;
    end;//else PopisekI = -1
  end;
 if (Self.Operations.FMoveKrok = 2) then
  begin
   Result.Color := 2;
   Result.Pos2.X := (CursorPos.X+Length(Self.Operations.TextProperties.Text)-1)*_Symbol_Sirka;
   Result.Pos2.Y := CursorPos.Y*_Symbol_Vyska;
  end;
end;

procedure TText.MITextPropertiesClick(Sender:TObject);
var PIndex:Integer;
    aPopisek:TPopisek;
begin
 PIndex := Self.GetPopisek(MenuPosition);

 aPopisek := Self.GetPopisekData(PIndex);
 if Assigned(FOnChangeText) then FOnChangeText(Self, aPopisek);
 Self.SetPopisekData(PIndex, aPopisek);

 if Assigned(FOnShow) then FOnShow();
end;

procedure TText.InitializeTextMenu(var Menu:TPopupMenu;Parent:TForm);
var MI:TMenuItem;
begin
 Menu := TPopupMenu.Create(Parent);

 MI := TMenuItem.Create(Menu);
 MI.Caption := 'Vlastnosti';
 MI.Enabled := true;
 MI.Visible := true;
 MI.OnClick := Self.MITextPropertiesClick;

 Menu.Items.Add(MI);
end;

function TText.GetCount():Integer;
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
   if ((Self.Operations.FAddKrok > 0) or (Self.Operations.FMoveKrok > 0) or (Self.Operations.FDeleteKrok > 0)) then
     raise EOperationInProgress.Create('Právě probíhá operace!');
  end;
end;

end.//unit

