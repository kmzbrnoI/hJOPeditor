unit Panel;

interface

uses DXDraws, ImgList, Controls, Windows, SysUtils, Graphics, Classes, Types,
  ReliefObjects, Forms, StdCtrls, ExtCtrls, ReliefBitmap, Menus, ReliefText,
  Global, BitmapToObj, OblastRizeni, StrUtils, DirectX, PGraphics,
  ObjBlok, symbolHelper, Generics.Collections;

type
  TObjectPointer = record
    SymbolID, ObjID: Integer;
  end;

  // error ID:
  // 0 - pretahovat lze pouze zleva doprava a zhora dolu
  // 1 - prazdne pole (neni s cim operovat)
  // 2 - chyba externi funkce
  // 3 - obsazeno
  // 4 - presazeny limity
  TErrorEvent = procedure(Sender: TObject; err: string) of object;
  TMoveEvent = procedure(Sender: TObject; Position: TPoint) of object;
  EORLoad = class(Exception);
  EGeneralFileOpen = class(Exception);
  EResourceLoad = class(Exception);

  TRelief = class
  private const
    _MAX_MOVE = 64;
    _MAX_TEXT_LENGTH = 32;
    _MIN_TEXT_LENGTH = 1;

    _Resource_Symbols = 'All8';
    _Resource_Text = 'Text8';
    _Resource_DK = 'DK8';

    // vychozi data
    _Def_Color_Pozadi = clBlack;
    _Def_Color_Mrizka = clGray;
    _Def_Color_Kurzor = clYellow;
    _Def_Color_Kurzor_OnObject = clRed;
    _Def_Color_Kurzor_Operation = clFuchsia;
    _Def_Color_Objects = 1;
    _Def_Mrizka = true;

    // rozmery DK, zasobniku, casovace
    _OR_Size: array [0 .. 5] of Byte = (5, 3, 14, 1, 16, 1);

    _Separator_BitmapIndex = 352;

    // pripony bitmapoveho a vektoroveho formatu souboru
    _Suf_bmp = '.bpnl';
    _Suf_obj = '.opnl';

  private
    DrawObject: TDXDraw;
    ParentForm: TForm;

    PanelBitmap: TPanelBitmap;
    PanelObjects: TPanelObjects;

    IL_Symbols: TImageList;
    IL_Text: TImageList;
    IL_DK: TImageList;

    DrawMode: TMode;
    Graphics: TPanelGraphics;

    Zobrazeni: record
      PanelWidth, PanelHeight: SmallInt;
      Mrizka: Boolean;
    end;

    Colors: record
      Mrizka, Pozadi, Kurzor, KurzorOnObject, KurzorOperation: TColor;
      Objects: ShortInt;
    end;

    LastPos: TPoint;

    Panel: record
      FileStav: SmallInt;
      FileCesta: string;
    end;

    DK_Menu: TPopUpMenu;

    ORMove: TORGraf;
    ORClick: TORGraf;
    FMove: Boolean;

    FOnError: TErrorEvent;
    FOnMove: TMoveEvent;
    FOnChangeText: TChangeTextEvent;
    FOnBlokEdit: TBlokAskEvent;
    FOnMsg: TMsgEvent;
    FFormBlkClose: TGlobalEvent;

    procedure Initialize(Rozmery: TPoint; Mode: TMode);

    procedure PaintMrizka(MrizkaColor: TColor);
    function PaintKurzor(CursorData: TCursorDraw): Byte;

    procedure PaintOR();
    function PaintORCursor(CursorPos: TPoint): TCursorDraw;
    function ORMouseUp(Position: TPoint; Button: TMouseButton): Byte;
    function GetORGraf(pos: TPoint): TORGraf;
    procedure DKMenuInit();
    procedure DKPropClick(Sender: TObject);
    procedure DKDeleteClick(Sender: TObject);

    function ORSave: string;
    procedure ORLoad(data: string);

    procedure DXDMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure DXDMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure DXDDoubleClick(Sender: TObject);

    procedure BitmapMouseUp(Position: TPoint; Button: TMouseButton);
    procedure ObjectMouseUp(Position: TPoint; Button: TMouseButton);

    procedure BitmapDblClick(Position: TPoint);
    procedure ObjectDblClick(Position: TPoint);

    procedure SetMrizka(aMrizka: Boolean);

    procedure FLoad(aFile: string);

    procedure ShowEvent;
    procedure BlokEditEvent(Sender: TObject; Blok: TGraphBlok);
    procedure MessageEvent(Sender: TObject; msg: string);
    procedure BlkFormCloseEvent(Sender: TObject);

    function IsOREvent(pos: TPoint): Boolean;

    procedure SetGroup(State: Boolean);
    function GetGroup: Boolean;

    procedure AssignBitmapEvents();
    procedure AssignObjectEvents();

    function GetShowBlokPopisky(): Boolean;
    procedure SetShowBlokPopisky(show: Boolean);

  public
    ORs: TList<TOR>;

    constructor Create(DDRaw: TDXDraw; aParentForm: TForm);
    destructor Destroy; override;

    procedure New(size: TPoint; firstOR: TOR);
    procedure Open(aFile: string);
    procedure Save(aFile: string);
    function Import(aFile: string): string;

    procedure show(CursorPos: TPoint);
    procedure Escape(Group: Boolean);
    procedure SetRozmery(aWidth, aHeight: Byte);

    procedure AddSymbol(SymbolID: Integer);
    procedure AddText(Text: string; Color: SymbolColor; popisekBlok: Boolean);
    procedure AddJCClick();
    procedure AddSeparatorVert();
    procedure AddSeparatorHor();
    procedure AddKPopisek();
    procedure AddSouprava();

    procedure MoveBitmapSymbol;
    procedure DeleteBitmapSymbol;
    procedure KeyPress(Key: Integer); // accepts keys from ApplicationEvent component

    procedure SwitchMode(aMode: TMode);
    procedure HideMouse();

    // OR
    procedure AddOR(oblr: TOR);
    procedure DeleteOR(pos: TPoint);

    function CheckValid(var error_cnt: Byte): TStrings; // overi validitu naeditovanych dat a vrati chybove hlasky

    property MrizkaColor: TColor read Colors.Mrizka write Colors.Mrizka;
    property PozadiColor: TColor read Colors.Pozadi write Colors.Pozadi;
    property KurzorColor: TColor read Colors.Kurzor write Colors.Kurzor;
    property KurzorOnObjectColor: TColor read Colors.KurzorOnObject write Colors.KurzorOnObject;
    property KurzorOperation: TColor read Colors.KurzorOperation write Colors.KurzorOperation;
    property ObjectColor: ShortInt read Colors.Objects write Colors.Objects;

    property Mode: TMode read DrawMode;
    property Skupina: Boolean read GetGroup write SetGroup;
    property Mrizka: Boolean read Zobrazeni.Mrizka write SetMrizka;

    property PanelWidth: SmallInt read Zobrazeni.PanelWidth;
    property PanelHeight: SmallInt read Zobrazeni.PanelHeight;

    property FileStav: SmallInt read Panel.FileStav;
    property FileCesta: string read Panel.FileCesta;

    // events
    property OnError: TErrorEvent read FOnError write FOnError;
    property OnMove: TMoveEvent read FOnMove write FOnMove;
    property OnChangeText: TChangeTextEvent read FOnChangeText write FOnChangeText;
    property OnBlokEdit: TBlokAskEvent read FOnBlokEdit write FOnBlokEdit;
    property OnMsg: TMsgEvent read FOnMsg write FOnMsg;
    property FormBlkClose: TGlobalEvent read FFormBlkClose write FFormBlkClose;
    property ShowBlokPopisky: Boolean read GetShowBlokPopisky write SetShowBlokPopisky;
  end;

implementation

{$R Resource.res}

uses fOREdit, fMain, ownStrUtils;

constructor TRelief.Create(DDRaw: TDXDraw; aParentForm: TForm);
begin
  inherited Create;

  Self.ORs := TList<TOR>.Create();

  Self.DrawObject := DDRaw;
  Self.ParentForm := aParentForm;

  Self.DrawMode := dmBitmap;

  Self.DKMenuInit();
end; // contructor

// inicializace objektu Relief
procedure TRelief.Initialize(Rozmery: TPoint; Mode: TMode);
begin
  Self.DrawObject.OnMouseUp := Self.DXDMouseUp;
  Self.DrawObject.OnMouseMove := Self.DXDMouseMove;
  Self.DrawObject.OnDblClick := Self.DXDDoubleClick;

  Self.Colors.Mrizka := _Def_Color_Mrizka;
  Self.Colors.Pozadi := _Def_Color_Pozadi;
  Self.Colors.Kurzor := _Def_Color_Kurzor;
  Self.Colors.KurzorOnObject := _Def_Color_Kurzor_OnObject;
  Self.Colors.Objects := _Def_Color_Objects;
  Self.Colors.KurzorOperation := _Def_Color_Kurzor_Operation;

  Self.DrawMode := Mode;
  Self.Zobrazeni.Mrizka := _Def_Mrizka;
  Self.Panel.FileStav := 1;

  Self.ORs.Clear();
  Self.ORMove.MovingOR := -1;
  Self.ORClick.MovingOR := -1;
  Self.FMove := false;

  Self.IL_Symbols := LoadIL(_Resource_Symbols, 8, 12);
  Self.IL_Text := LoadIL(_Resource_Text, 8, 12);
  Self.IL_DK := LoadIL(_Resource_DK, 8 * _OR_Size[0], 12 * _OR_Size[1]);

  Self.Graphics := TPanelGraphics.Create(Self.DrawObject, Self.IL_Text);

  case (Mode) of
    dmBitmap:
      begin
        Self.PanelBitmap := TPanelBitmap.Create(Self.IL_Symbols, Self.IL_Text, Self.DrawObject.Surface.Canvas,
          Rozmery.X, Rozmery.Y, dmBitmap, Self.ParentForm, Self.Graphics);
        Self.AssignBitmapEvents();
      end; // dmBitmap
    dmBloky:
      begin
        Self.PanelObjects := TPanelObjects.Create(Self.IL_Symbols, Self.IL_Text, Self.DrawObject.Surface.Canvas,
          Rozmery.X, Rozmery.Y, Self.DrawObject, Self.Graphics);
        Self.AssignObjectEvents();
      end; // dmBitmap
  end; // case

  Self.Zobrazeni.PanelWidth := Rozmery.X;
  Self.Zobrazeni.PanelHeight := Rozmery.Y;
  Self.DrawObject.Width := Self.Zobrazeni.PanelWidth * _Symbol_Sirka;
  Self.DrawObject.Height := Self.Zobrazeni.PanelHeight * _Symbol_Vyska;

  Self.show(Point(-1, -1));

  Self.DrawObject.Visible := true;
end;

// novy relief
procedure TRelief.New(size: TPoint; firstOR: TOR);
begin
  Self.Initialize(size, dmBitmap);
  Self.AddOR(firstOR);
end;

procedure TRelief.Open(aFile: string);
var Mode: TMode;
begin
  if (RightStr(aFile, 5) = _Suf_bmp) then
    Mode := dmBitmap
  else if (RightStr(aFile, 5) = _Suf_obj) then
    Mode := dmBloky
  else
    raise EGeneralFileOpen.Create('Soubor s nepodporovanou příponou!');

  Self.Initialize(Point(0, 0), Mode);
  Self.FLoad(aFile);
end;

function TRelief.Import(aFile: string): string;
var log: string;
begin
  if (RightStr(aFile, 4) <> ReliefBitmap._IMPORT_MYJOP_SUFFIX) then
    raise EGeneralFileOpen.Create('Soubor s nepodporovanou příponou!');

  log := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now) + ': spouštím import souboru ' + aFile + ' ...' + #13#10;
  Self.Initialize(Point(0, 0), dmBitmap);
  log := log + Self.PanelBitmap.ImportMyJOP(aFile, Self.ORs);

  Self.Panel.FileStav := Self.PanelBitmap.FileStav;
  Self.Zobrazeni.PanelWidth := Self.PanelBitmap.PanelWidth;
  Self.Zobrazeni.PanelHeight := Self.PanelBitmap.PanelHeight;

  Self.DrawObject.Width := Self.Zobrazeni.PanelWidth * _Symbol_Sirka;
  Self.DrawObject.Height := Self.Zobrazeni.PanelHeight * _Symbol_Vyska;

  log := log + 'Import dat proběhl úspěšně.' + #13#10;
  Result := log;
end;

destructor TRelief.Destroy();
begin
  if (Assigned(Self.PanelBitmap)) then
  begin
    Self.PanelBitmap.Destroy;
    Self.PanelBitmap := nil;
  end;

  if (Assigned(Self.PanelObjects)) then
  begin
    Self.PanelObjects.Destroy;
    Self.PanelObjects := nil;
  end;

  Self.DrawObject.Visible := false;

  Self.IL_Symbols.Free;
  Self.IL_Text.Free;
  Self.IL_DK.Free;

  Self.Graphics.Free();

  Self.DK_Menu.Free();
  Self.ORs.Free();

  inherited Destroy;
end; // destructor

// hlavni zobrazeni celeho reliefu
procedure TRelief.show(CursorPos: TPoint);
begin
  if (not Assigned(Self.DrawObject)) then
    Exit();
  if (not Self.DrawObject.CanDraw) then
    Exit();

  Self.DrawObject.Surface.Canvas.Release();
  Self.DrawObject.BeginScene;

  // smazani reliefu
  Self.DrawObject.Surface.Fill(Self.Colors.Pozadi);

  case (Self.DrawMode) of
    dmBitmap, dmSepHor, dmSepVert:
      Self.PanelBitmap.PaintMove(CursorPos);
  end; // case

  // vykresleni objektu s ohledem na DrawMode
  case (Self.DrawMode) of
    dmBitmap, dmSepHor, dmSepVert:
      Self.PanelBitmap.Paint;
    dmBloky, dmRoots:
      Self.PanelObjects.Paint;
  end;
  Self.PaintOR();
  if (Self.Zobrazeni.Mrizka) then
    Self.PaintMrizka(Self.Colors.Mrizka);

  // prioritu ma posun OR
  if (Self.PaintKurzor(Self.PaintORCursor(CursorPos)) <> 0) then
  begin
    // vykreslovani kurzoru a specialnich barev vzhledem k modum a operacim
    case (Self.DrawMode) of
      dmBitmap, dmSepHor, dmSepVert:
        Self.PaintKurzor(Self.PanelBitmap.PaintCursor(CursorPos));
      dmBloky, dmRoots:
        Self.PaintKurzor(Self.PanelObjects.PaintCursor(CursorPos));
    end; // case
  end;

  Self.DrawObject.EndScene;
  Self.DrawObject.Surface.Canvas.Release;
  Self.DrawObject.Flip;
end;

// vykresleni mrizky
procedure TRelief.PaintMrizka(MrizkaColor: TColor);
var i: Integer;
begin
  Self.DrawObject.Surface.Canvas.Pen.Color := MrizkaColor;

  for i := 1 to Self.Zobrazeni.PanelWidth - 1 do
  begin
    Self.DrawObject.Surface.Canvas.MoveTo((i * _Symbol_Sirka) - 1, 0);
    Self.DrawObject.Surface.Canvas.LineTo((i * _Symbol_Sirka) - 1, Self.Zobrazeni.PanelHeight * _Symbol_Vyska);
  end; // for i
  for i := 1 to Self.Zobrazeni.PanelHeight - 1 do
  begin
    Self.DrawObject.Surface.Canvas.MoveTo(0, (i * _Symbol_Vyska) - 1);
    Self.DrawObject.Surface.Canvas.LineTo(Self.Zobrazeni.PanelWidth * _Symbol_Sirka, (i * _Symbol_Vyska) - 1);
  end; // for i
end;

// vykresluje kurzor
function TRelief.PaintKurzor(CursorData: TCursorDraw): Byte;
var Colors: array [0 .. 2] of TColor;
begin
  if ((CursorData.Pos1.X < 0) and (CursorData.Pos1.Y < 0) and (CursorData.Pos2.X < 0) and (CursorData.Pos2.Y < 0)) then
  begin
    Result := 1;
    Exit;
  end;
  if (CursorData.Color < 0) then
  begin
    Result := 2;
    Exit;
  end;

  CursorData.Pos1.X := CursorData.Pos1.X - 1;
  CursorData.Pos1.Y := CursorData.Pos1.Y - 1;
  CursorData.Pos2.X := CursorData.Pos2.X - 1;
  CursorData.Pos2.Y := CursorData.Pos2.Y - 1;

  Colors[0] := Self.Colors.Kurzor;
  Colors[1] := Self.Colors.KurzorOperation;
  Colors[2] := Self.Colors.KurzorOnObject;

  Self.DrawObject.Surface.Canvas.Pen.Color := Colors[CursorData.Color];
  if ((CursorData.Pos2.X >= CursorData.Pos1.X) and (CursorData.Pos2.Y >= CursorData.Pos1.Y)) then
  begin
    Self.DrawObject.Surface.Canvas.MoveTo(CursorData.Pos1.X, CursorData.Pos1.Y);
    Self.DrawObject.Surface.Canvas.LineTo(CursorData.Pos2.X + 8, CursorData.Pos1.Y);

    Self.DrawObject.Surface.Canvas.MoveTo(CursorData.Pos2.X + 8, CursorData.Pos1.Y);
    Self.DrawObject.Surface.Canvas.LineTo(CursorData.Pos2.X + 8, CursorData.Pos2.Y + 12);

    Self.DrawObject.Surface.Canvas.MoveTo(CursorData.Pos2.X + 8, CursorData.Pos2.Y + 12);
    Self.DrawObject.Surface.Canvas.LineTo(CursorData.Pos1.X, CursorData.Pos2.Y + 12);

    Self.DrawObject.Surface.Canvas.MoveTo(CursorData.Pos1.X, CursorData.Pos2.Y + 12);
    Self.DrawObject.Surface.Canvas.LineTo(CursorData.Pos1.X, CursorData.Pos1.Y);
  end else begin
    if (CursorData.Pos2.X < CursorData.Pos1.X) then
    begin
      Self.DrawObject.Surface.Canvas.MoveTo(CursorData.Pos1.X, CursorData.Pos1.Y);
      Self.DrawObject.Surface.Canvas.LineTo(CursorData.Pos1.X, CursorData.Pos1.Y + 12);
    end;
    if (CursorData.Pos2.Y < CursorData.Pos1.Y) then
    begin
      Self.DrawObject.Surface.Canvas.MoveTo(CursorData.Pos1.X, CursorData.Pos1.Y);
      Self.DrawObject.Surface.Canvas.LineTo(CursorData.Pos1.X + 8, CursorData.Pos1.Y);
    end;
  end; // else ((KurzorPos2.X > KurzorPos1.X) and (KurzorPos2.Y > KurzorPos1.Y))

  Result := 0;
end;

procedure TRelief.DXDMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  LastPos.X := X div _Symbol_Sirka;
  LastPos.Y := Y div _Symbol_Vyska;

  if (Self.ORMouseUp(LastPos, Button) = 0) then
  begin
    case (Self.DrawMode) of
      dmBitmap, dmSepHor, dmSepVert:
        Self.BitmapMouseUp(LastPos, Button);
      dmBloky, dmRoots:
        Self.ObjectMouseUp(LastPos, Button);
    end; // case
  end; // ORMouseUp = 0

  Self.show(LastPos);
end;

procedure TRelief.DXDMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if ((X div _Symbol_Sirka = LastPos.X) and (Y div _Symbol_Vyska = LastPos.Y)) then
    Exit;

  LastPos.X := X div _Symbol_Sirka;
  LastPos.Y := Y div _Symbol_Vyska;

  case (Self.DrawMode) of
    dmBloky, dmRoots:
      Self.PanelObjects.MouseMove(LastPos);
  end; // case

  Self.show(LastPos);

  if Assigned(FOnMove) then
    FOnMove(Self, LastPos);
end;

procedure TRelief.DXDDoubleClick(Sender: TObject);
begin
  case (Self.DrawMode) of
    dmBitmap, dmSepHor, dmSepVert:
      Self.BitmapDblClick(LastPos);
    dmBloky, dmRoots:
      Self.ObjectDblClick(LastPos);
  end; // case

  Self.show(LastPos);
end;

procedure TRelief.KeyPress(Key: Integer);
var mouse: TPoint;
begin
  case (Key) of
    VK_LEFT, VK_RIGHT, VK_UP, VK_DOWN:
      begin
        if (not Self.ParentForm.Active) then
          Exit();

        GetCursorPos(mouse);

        case (Key) of
          VK_LEFT:
            mouse.X := mouse.X - _Symbol_Sirka;
          VK_RIGHT:
            mouse.X := mouse.X + _Symbol_Sirka;
          VK_UP:
            mouse.Y := mouse.Y - _Symbol_Vyska;
          VK_DOWN:
            mouse.Y := mouse.Y + _Symbol_Vyska;
        end;

        SetCursorPos(mouse.X, mouse.Y);
      end;

    VK_RETURN:
      Self.DXDMouseUp(Self.DrawObject, mbLeft, [], Self.LastPos.X * _Symbol_Sirka, Self.LastPos.Y * _Symbol_Vyska);
      // enter
  end;
end;

procedure TRelief.Escape(Group: Boolean);
begin
  case (Self.DrawMode) of
    dmBitmap, dmSepVert, dmSepHor:
      if (Assigned(Self.PanelBitmap)) then
        Self.PanelBitmap.Escape(Group);
    dmBloky, dmRoots:
      if (Assigned(Self.PanelObjects)) then
        Self.PanelObjects.Escape();
  end;

  Self.FMove := false;
  Self.show(LastPos);
end;

procedure TRelief.SwitchMode(aMode: TMode);
begin
  if (((Self.DrawMode = dmBitmap) or (Self.DrawMode = dmSepHor) or (Self.DrawMode = dmSepVert)) and (aMode = dmBloky))
  then
  begin
    // konverze z Bitmap na Objects

    Self.PanelObjects := TPanelObjects.Create(Self.IL_Symbols, Self.IL_Text, Self.DrawObject.Surface.Canvas,
      Self.Zobrazeni.PanelWidth, Self.Zobrazeni.PanelHeight, Self.DrawObject, Self.Graphics);
    Self.AssignObjectEvents();

    try
      Self.PanelObjects.Import(Self.PanelBitmap);
    except
      FreeAndNil(Self.PanelObjects);
      raise;
    end;

    Self.PanelObjects.ShowBlokPopisky := Self.PanelBitmap.ShowBlokPopisky;
    FreeAndNil(Self.PanelBitmap);
    Self.Panel.FileStav := 1;
  end
  else

    // konverze Bitmap <-> oddelovac hor <-> oddelovac vert
    if (((Self.DrawMode = dmBitmap) or (Self.DrawMode = dmSepHor) or (Self.DrawMode = dmSepVert)) and
      ((aMode = dmBitmap) or (aMode = dmSepHor) or (aMode = dmSepVert))) then
      Self.PanelBitmap.Mode := aMode

      // konverze Bloky <-> Koreny
    else if (((Self.DrawMode = dmBloky) and (aMode = dmRoots)) or ((Self.DrawMode = dmRoots) and (aMode = dmBloky)))
    then
      Self.PanelObjects.Mode := aMode;

  DrawMode := aMode;

  Self.show(Point(-1, -1));
end;

procedure TRelief.BitmapMouseUp(Position: TPoint; Button: TMouseButton);
begin
  try
    Self.PanelBitmap.MouseUp(Position, Button);
  except
    on E: Exception do
      if Assigned(FOnError) then
        FOnError(Self, E.Message);
  end;
end;

procedure TRelief.ObjectMouseUp(Position: TPoint; Button: TMouseButton);
begin
  try
    Self.PanelObjects.MouseUp(Position, Button);
  except
    on E: Exception do
      if Assigned(FOnError) then
        FOnError(Self, E.Message);
  end;
end;

procedure TRelief.BitmapDblClick(Position: TPoint);
begin
  try
    Self.PanelBitmap.DblClick(Position);
  except
    on E: Exception do
      if Assigned(FOnError) then
        FOnError(Self, E.Message);
  end;
end;

procedure TRelief.ObjectDblClick(Position: TPoint);
begin
  try
    Self.PanelObjects.DblClick(Position);
  except
    on E: Exception do
      if Assigned(FOnError) then
        FOnError(Self, E.Message);
  end;
end;

procedure TRelief.SetRozmery(aWidth, aHeight: Byte);
begin
  if (Self.Mode <> dmBitmap) then
    raise Exception.Create('Rozměny lze měnit jen v režimu bitmapy!');

  PanelBitmap.SetRozmery(aWidth, aHeight);

  Self.Zobrazeni.PanelWidth := aWidth;
  Self.Zobrazeni.PanelHeight := aHeight;

  Self.DrawObject.Width := aWidth * _Symbol_Sirka;
  Self.DrawObject.Height := aHeight * _Symbol_Vyska;

  Self.show(LastPos);
end;

procedure TRelief.SetMrizka(aMrizka: Boolean);
begin
  Self.Zobrazeni.Mrizka := aMrizka;

  Self.show(Point(-1, -1));
end;

procedure TRelief.FLoad(aFile: string);
var ORs: string;
begin
  case (Self.DrawMode) of
    dmBitmap, dmSepHor, dmSepVert:
      begin
        Self.PanelBitmap.FLoad(aFile, ORs);
        Self.ORLoad(ORs);
        Self.Panel.FileStav := Self.PanelBitmap.FileStav;
        Self.Zobrazeni.PanelWidth := Self.PanelBitmap.PanelWidth;
        Self.Zobrazeni.PanelHeight := Self.PanelBitmap.PanelHeight;
      end; // dmBitmap
    dmBloky, dmRoots:
      begin
        Self.PanelObjects.FLoad(aFile, ORs);
        Self.ORLoad(ORs);
        Self.Panel.FileStav := Self.PanelObjects.FileStav;
        Self.Zobrazeni.PanelWidth := Self.PanelObjects.PanelWidth;
        Self.Zobrazeni.PanelHeight := Self.PanelObjects.PanelHeight;
      end; // dmBloky
  end; // case

  Self.DrawObject.Width := Self.Zobrazeni.PanelWidth * _Symbol_Sirka;
  Self.DrawObject.Height := Self.Zobrazeni.PanelHeight * _Symbol_Vyska;

  Self.Panel.FileCesta := aFile;
end;

procedure TRelief.Save(aFile: string);
begin
  case (Self.DrawMode) of
    dmBitmap, dmSepHor, dmSepVert:
      begin
        Self.PanelBitmap.FSave(aFile, Self.ORSave());
        Self.Panel.FileStav := Self.PanelBitmap.FileStav;
      end; // dmBitmap

    dmBloky, dmRoots:
      begin
        Self.PanelObjects.FSave(aFile, Self.ORSave());
        Self.Panel.FileStav := Self.PanelObjects.FileStav;
      end; // dmBloky
  end;
  /// case

  Self.Panel.FileCesta := aFile;
end;

procedure TRelief.ShowEvent;
begin
  Self.show(LastPos);
end;

procedure TRelief.SetGroup(State: Boolean);
begin
  if (Assigned(Self.PanelBitmap)) then
    Self.PanelBitmap.Group := State;
end;

function TRelief.GetGroup: Boolean;
begin
  Result := false;

  if (Assigned(Self.PanelBitmap)) then
    Result := Self.PanelBitmap.Group;
end; // fucntion

procedure TRelief.MoveBitmapSymbol;
begin
  if (Assigned(Self.PanelBitmap)) then
    Self.PanelBitmap.Move;
  Self.FMove := true;
  Self.show(Self.LastPos);
end;

procedure TRelief.DeleteBitmapSymbol();
begin
  if (Assigned(Self.PanelBitmap)) then
    Self.PanelBitmap.Delete;
  Self.FMove := false;
  Self.show(Self.LastPos);
end;

procedure TRelief.AddSymbol(SymbolID: Integer);
begin
  if (Assigned(Self.PanelBitmap)) then
    Self.PanelBitmap.Symbols.Add(SymbolID);
end;

procedure TRelief.AddText(Text: string; Color: SymbolColor; popisekBlok: Boolean);
begin
  if (Assigned(Self.PanelBitmap)) then
    Self.PanelBitmap.Text.Add(Text, Color, popisekBlok);
end;

procedure TRelief.AddJCClick();
begin
  if (Assigned(Self.PanelBitmap)) then
    Self.PanelBitmap.JCClick.Add();
end;

procedure TRelief.AddSeparatorVert();
begin
  if (Self.Mode <> dmSepVert) then
    Self.SwitchMode(dmSepVert);
  if (Assigned(Self.PanelBitmap)) then
    Self.PanelBitmap.SeparatorsVert.Add();
end;

procedure TRelief.AddSeparatorHor();
begin
  if (Self.Mode <> dmSepHor) then
    Self.SwitchMode(dmSepHor);
  if (Assigned(Self.PanelBitmap)) then
    Self.PanelBitmap.SeparatorsHor.Add();
end;

procedure TRelief.AddKPopisek();
begin
  if (Assigned(Self.PanelBitmap)) then
    Self.PanelBitmap.KPopisky.Add();
end;

procedure TRelief.AddSouprava();
begin
  if (Assigned(Self.PanelBitmap)) then
    Self.PanelBitmap.Soupravy.Add();
end;

procedure TRelief.MessageEvent(Sender: TObject; msg: string);
begin
  if (Assigned(Self.FOnMsg)) then
    Self.FOnMsg(Self, msg);
end;

/// /////////////////////////////////////////////////////////////////////////////
// operace s oblastmi rizeni:

procedure TRelief.AddOR(oblr: TOR);
begin
  Self.ORs.Add(oblr);
  Self.ORMove.MovingOR := Self.ORs.Count - 1;
  Self.ORMove.MovingSymbol := 0; // 0 = dopravni kancelar
end;

procedure TRelief.DeleteOR(pos: TPoint);
var i: Integer;
begin
  for i := Self.ORs.Count - 1 downto 0 do
    if ((Self.ORs[i].Poss.DK.X = pos.X) and (Self.ORs[i].Poss.DK.Y = pos.Y)) then
      Self.ORs.Delete(i);
end;

// vykresli vsechny oblasti rizeni
// zatim jen baracky
// bereme ohled na posun
procedure TRelief.PaintOR();
begin
  for var i := 0 to Self.ORs.Count - 1 do
  begin
    var oblr := Self.ORs[i];
    if (Self.ORMove.MovingOR = i) then
    begin
      case (Self.ORMove.MovingSymbol) of
        0:
          oblr.Poss.DK := Self.LastPos;
        1:
          oblr.Poss.Queue := Self.LastPos;
        2:
          oblr.Poss.Time := Self.LastPos;
      end;
      Self.ORs[i] := oblr;
    end;

    Self.IL_DK.Draw(Self.DrawObject.Surface.Canvas, oblr.Poss.DK.X * _Symbol_Sirka, oblr.Poss.DK.Y * _Symbol_Vyska,
      (oblr.Poss.DKOr * 10) + 1);

    Self.Graphics.TextOutputI(oblr.Poss.Queue, '00 VZ PV EZ 00', scGray, clBlack);
    Self.Graphics.TextOutputI(oblr.Poss.Time, 'MER CASU', scGray, clBlack);
    Self.Graphics.TextOutputI(Point(oblr.Poss.Time.X + 8, oblr.Poss.Time.Y), '        ', scBlack, clWhite);
  end; // for i
end;

// vykresluje kurzor pri pohybu
function TRelief.PaintORCursor(CursorPos: TPoint): TCursorDraw;
begin
  Result.Color := -1;

  if (not Self.FMove) then
    Exit;

  if (Self.ORMove.MovingOR < 0) then
  begin
    var tmp_or := Self.GetORGraf(CursorPos);
    if (tmp_or.MovingOR = -1) then
      Exit;

    Result.Color := 2;

    case (tmp_or.MovingSymbol) of
      0:
        begin
          Result.Pos1.X := (Self.ORs[tmp_or.MovingOR].Poss.DK.X) * _Symbol_Sirka;
          Result.Pos1.Y := (Self.ORs[tmp_or.MovingOR].Poss.DK.Y) * _Symbol_Vyska;
          Result.Pos2.X := (Self.ORs[tmp_or.MovingOR].Poss.DK.X + _OR_Size[tmp_or.MovingSymbol * 2] - 1) *
            _Symbol_Sirka;
          Result.Pos2.Y := (Self.ORs[tmp_or.MovingOR].Poss.DK.Y + _OR_Size[tmp_or.MovingSymbol * 2 + 1] - 1) *
            _Symbol_Vyska;
        end;
      1:
        begin
          Result.Pos1.X := (Self.ORs[tmp_or.MovingOR].Poss.Queue.X) * _Symbol_Sirka;
          Result.Pos1.Y := (Self.ORs[tmp_or.MovingOR].Poss.Queue.Y) * _Symbol_Vyska;
          Result.Pos2.X := (Self.ORs[tmp_or.MovingOR].Poss.Queue.X + _OR_Size[tmp_or.MovingSymbol * 2] - 1) *
            _Symbol_Sirka;
          Result.Pos2.Y := (Self.ORs[tmp_or.MovingOR].Poss.Queue.Y + _OR_Size[tmp_or.MovingSymbol * 2 + 1] - 1) *
            _Symbol_Vyska;
        end;
      2:
        begin
          Result.Pos1.X := (Self.ORs[tmp_or.MovingOR].Poss.Time.X) * _Symbol_Sirka;
          Result.Pos1.Y := (Self.ORs[tmp_or.MovingOR].Poss.Time.Y) * _Symbol_Vyska;
          Result.Pos2.X := (Self.ORs[tmp_or.MovingOR].Poss.Time.X + _OR_Size[tmp_or.MovingSymbol * 2] - 1) *
            _Symbol_Sirka;
          Result.Pos2.Y := (Self.ORs[tmp_or.MovingOR].Poss.Time.Y + _OR_Size[tmp_or.MovingSymbol * 2 + 1] - 1) *
            _Symbol_Vyska;
        end;
    end; // case
  end else begin
    Result.Color := 2;

    Result.Pos1.X := CursorPos.X * _Symbol_Sirka;
    Result.Pos1.Y := CursorPos.Y * _Symbol_Vyska;

    Result.Pos2.X := (CursorPos.X + _OR_Size[Self.ORMove.MovingSymbol * 2] - 1) * _Symbol_Sirka;
    Result.Pos2.Y := (CursorPos.Y + _OR_Size[Self.ORMove.MovingSymbol * 2 + 1] - 1) * _Symbol_Vyska;
  end;
end;

function TRelief.ORMouseUp(Position: TPoint; Button: TMouseButton): Byte;
begin
  Result := 0;

  if (Self.Mode = dmBloky) then
  begin
    // tohleto umoznuje pripradit OR bloku tim, ze kliknu na prislusny baracek
    if (Button <> mbLeft) then
      Exit;

    var tmp_or := Self.GetORGraf(Position);
    if (Self.PanelObjects.selected_obj = nil) then
      Exit;
    if (tmp_or.MovingOR < 0) then
      Exit;
    if (tmp_or.MovingSymbol <> 0) then
      Exit;
    Self.PanelObjects.SetOR(tmp_or.MovingOR);
    if (Assigned(Self.FOnBlokEdit)) then
      Self.FOnBlokEdit(Self, Self.PanelObjects.selected_obj);
    Result := 1;
  end; // dmBloky

  if (Self.Mode = dmBitmap) then
  begin
    if (Button = mbLeft) then
    begin
      if (Self.ORMove.MovingOR > -1) then
      begin
        // ukonceni pohybu
        Self.ORMove.MovingOR := -1;
        Self.ORMove.MovingSymbol := 0;

        Self.FMove := false;
        Self.show(Self.LastPos);
        Self.FMove := true;
        Sleep(50);
        Self.show(Self.LastPos);
      end else begin
        var tmp_or := Self.GetORGraf(Position);
        if (tmp_or.MovingOR > -1) then
          Self.MessageEvent(Self, 'OŘ : ' + ORs[tmp_or.MovingOR].Name + ' (id = ' + ORs[tmp_or.MovingOR].id + ')');

        // zacatek pohybu
        if (Self.FMove) then
          Self.ORMove := tmp_or;
      end;
    end;

    if (Button = mbRight) then
    begin
      Self.ORClick := Self.GetORGraf(Position);
      if (Self.ORClick.MovingOR < 0) then
        Exit;
      case (Self.ORClick.MovingSymbol) of
        0:
          Self.DK_Menu.Popup(mouse.CursorPos.X, mouse.CursorPos.Y);
      end; // case
    end;
  end; // dmBitmap
end;

// vraci OR na dane pozici
function TRelief.GetORGraf(pos: TPoint): TORGraf;
begin
  Result.MovingOR := -1;
  Result.MovingSymbol := 0;

  for var i := 0 to Self.ORs.Count - 1 do
  begin
    if ((pos.X >= Self.ORs[i].Poss.DK.X) and (pos.Y >= Self.ORs[i].Poss.DK.Y) and
      (Self.ORs[i].Poss.DK.X + _OR_Size[0] > pos.X) and (Self.ORs[i].Poss.DK.Y + _OR_Size[1] > pos.Y)) then
    begin
      Result.MovingOR := i;
      Result.MovingSymbol := 0;
      Exit;
    end;
    if ((pos.X >= Self.ORs[i].Poss.Queue.X) and (pos.Y >= Self.ORs[i].Poss.Queue.Y) and
      (Self.ORs[i].Poss.Queue.X + _OR_Size[2] > pos.X) and (Self.ORs[i].Poss.Queue.Y + _OR_Size[3] > pos.Y)) then
    begin
      Result.MovingOR := i;
      Result.MovingSymbol := 1;
      Exit;
    end;
    if ((pos.X >= Self.ORs[i].Poss.Time.X) and (pos.Y >= Self.ORs[i].Poss.Time.Y) and
      (Self.ORs[i].Poss.Time.X + _OR_Size[4] > pos.X) and (Self.ORs[i].Poss.Time.Y + _OR_Size[5] > pos.Y)) then
    begin
      Result.MovingOR := i;
      Result.MovingSymbol := 2;
      Exit;
    end;
  end;
end;

procedure TRelief.DKMenuInit();
var MI: TMenuItem;
begin
  // initialize DK menu
  Self.DK_Menu := TPopUpMenu.Create(Self.DrawObject);

  MI := TMenuItem.Create(Self.DK_Menu);
  MI.Caption := 'Vlastnosti';
  MI.OnClick := Self.DKPropClick;
  Self.DK_Menu.Items.Add(MI);

  MI := TMenuItem.Create(Self.DK_Menu);
  MI.Caption := 'Smazat OŘ';
  MI.OnClick := Self.DKDeleteClick;
  Self.DK_Menu.Items.Add(MI);
end;

procedure TRelief.DKPropClick(Sender: TObject);
begin
  F_OREdit.OpenForm(Self.ORClick.MovingOR);
end;

procedure TRelief.DKDeleteClick(Sender: TObject);
begin
  if (Self.ORs.Count <= 1) then
  begin
    Application.MessageBox('Poslední OŘ nelze smazat!', 'Nelze pokračovat', MB_OK OR MB_ICONSTOP);
    Exit;
  end;

  if (Application.MessageBox('Opravdu smazat?', 'Otázka', MB_YESNO OR MB_ICONQUESTION) = mrYes) then
    Self.ORs.Delete(Self.ORClick.MovingOR);
end;

// na kazdem radku je ulozena jedna oblast rizeni ve formatu:
// nazev;nazev_zkratka;id;lichy_smer(0,1);orientace_DK(0,1);ModCasStart(0,1);ModCasStop(0,1);ModCasSet(0,1);dkposx;dkposy;qposx;qposy;timeposx;timeposy;osv_mtb|osv_port|osv_name;
function TRelief.ORSave(): string;
begin
  for var oblr in Self.ORs do
  begin
    Result := Result + oblr.Name + ';' + oblr.ShortName + ';' + oblr.id + ';' + IntToStr(oblr.Lichy) + ';' +
      IntToStr(oblr.Poss.DKOr) + ';' + BoolToStr(oblr.Rights.ModCasStart) + ';' + BoolToStr(oblr.Rights.ModCasStop) +
      ';' + BoolToStr(oblr.Rights.ModCasSet) + ';' + IntToStr(oblr.Poss.DK.X) + ';' + IntToStr(oblr.Poss.DK.Y) + ';' +
      IntToStr(oblr.Poss.Queue.X) + ';' + IntToStr(oblr.Poss.Queue.Y) + ';' + IntToStr(oblr.Poss.Time.X) + ';' +
      IntToStr(oblr.Poss.Time.Y) + ';';

    for var j := 0 to oblr.Osvetleni.Cnt - 1 do
      Result := Result + IntToStr(oblr.Osvetleni.data[j].board) + '#' + IntToStr(oblr.Osvetleni.data[j].port) + '#' +
        oblr.Osvetleni.data[j].Name + '|';

    Result := Result + #13;
  end;

  Result := Result + #13;
end;

// na kazdem radku je ulozena jedna oblast rizeni ve formatu:
// nazev;nazev_zkratka;id;lichy_smer(0,1);orientace_DK(0,1);ModCasStart(0,1);ModCasStop(0,1);ModCasSet(0,1);dkposx;dkposy;qposx;qposy;timeposx;timeposy;osv_mtb|osv_port|osv_name;
procedure TRelief.ORLoad(data: string);
var lines, data_main, data_osv, data_osv2: TStrings;
begin
  Self.ORs.Clear();

  lines := TStringList.Create();
  data_main := TStringList.Create();
  data_osv := TStringList.Create();
  data_osv2 := TStringList.Create();

  try
    if (RightStr(data, 2) <> #13#13) then
      raise EORLoad.Create('OR data nekončí dvěma symboly nového řádku!');

    ExtractStringsEx([#13], [], LeftStr(data, Length(data) - 2), lines);

    for var line in lines do
    begin
      data_main.Clear();
      ExtractStringsEx([';'], [], line, data_main);

      if (data_main.Count < 14) then
        raise EORLoad.Create('Málo položek definující OŘ!');

      var oblr: TOR;
      oblr.Name := data_main[0];
      oblr.ShortName := data_main[1];
      oblr.id := data_main[2];
      oblr.Lichy := StrToInt(data_main[3]);
      oblr.Poss.DKOr := StrToInt(data_main[4]);

      oblr.Rights.ModCasStart := StrToBool(data_main[5]);
      oblr.Rights.ModCasStop := StrToBool(data_main[6]);
      oblr.Rights.ModCasSet := StrToBool(data_main[7]);

      oblr.Poss.DK.X := StrToInt(data_main[8]);
      oblr.Poss.DK.Y := StrToInt(data_main[9]);

      oblr.Poss.Queue.X := StrToInt(data_main[10]);
      oblr.Poss.Queue.Y := StrToInt(data_main[11]);

      oblr.Poss.Time.X := StrToInt(data_main[12]);
      oblr.Poss.Time.Y := StrToInt(data_main[13]);

      oblr.Osvetleni.Cnt := 0;
      data_osv.Clear();
      if (data_main.Count >= 15) then
      begin
        ExtractStringsEx(['|'], [], data_main[14], data_osv);
        oblr.Osvetleni.Cnt := 0;
        for var j := 0 to data_osv.Count - 1 do
        begin
          data_osv2.Clear();
          ExtractStringsEx(['#'], [], data_osv[j], data_osv2);

          if (data_osv2.Count < 2) then
            raise EORLoad.Create('Málo položek definující osvětlení!');

          oblr.Osvetleni.Cnt := oblr.Osvetleni.Cnt + 1;
          oblr.Osvetleni.data[oblr.Osvetleni.Cnt - 1].board := StrToInt(data_osv2[0]);
          oblr.Osvetleni.data[oblr.Osvetleni.Cnt - 1].port := StrToInt(data_osv2[1]);
          if (data_osv2.Count > 2) then
            oblr.Osvetleni.data[oblr.Osvetleni.Cnt - 1].Name := data_osv2[2]
          else
            oblr.Osvetleni.data[oblr.Osvetleni.Cnt - 1].Name := '';
        end;
      end;

      Self.ORs.Add(oblr);
    end;
  finally
    FreeAndNil(lines);
    FreeAndNil(data_main);
    FreeAndNil(data_osv);
    FreeAndNil(data_osv2);
  end;
end;

// konec operaci s oblastmi rizeni
/// /////////////////////////////////////////////////////////////////////////////

function TRelief.CheckValid(var error_cnt: Byte): TStrings;
begin
  if (Integer(Self.Mode) < 2) then
  begin
    Result := nil;
    Exit;
  end;
  Result := Self.PanelObjects.CheckValid(error_cnt);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TRelief.AssignBitmapEvents();
begin
  Self.PanelBitmap.OnShow := Self.ShowEvent;
  Self.PanelBitmap.OnTextEdit := Self.OnChangeText;
  Self.PanelBitmap.OnORAsk := Self.IsOREvent;
end;

procedure TRelief.AssignObjectEvents();
begin
  Self.PanelObjects.OnBlokEdit := Self.BlokEditEvent;
  Self.PanelObjects.OnMsg := Self.MessageEvent;
  Self.PanelObjects.OnShow := Self.ShowEvent;
  Self.PanelObjects.OnFormBlkClose := Self.BlkFormCloseEvent;
end;

/// /////////////////////////////////////////////////////////////////////////////

// vola se z bitmapoveho modu
// vraci true, pokud je na dane pozici DK, zasobnik, ci mereni casu
function TRelief.IsOREvent(pos: TPoint): Boolean;
begin
  for var oblr in Self.ORs do
  begin
    if ((pos.X >= oblr.Poss.DK.X) and (pos.X <= oblr.Poss.DK.X + 4) and (pos.Y >= oblr.Poss.DK.Y) and
      (pos.Y <= oblr.Poss.DK.Y + 2)) then
      Exit(true);

    if ((pos.X >= oblr.Poss.Queue.X) and (pos.X <= oblr.Poss.Queue.X + 13) and (pos.Y = oblr.Poss.Queue.Y)) then
      Exit(true);

    if ((pos.X >= oblr.Poss.Time.X) and (pos.X <= oblr.Poss.Time.X + 15) and (pos.Y = oblr.Poss.Time.Y)) then
      Exit(true);
  end;

  Result := false;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TRelief.BlkFormCloseEvent(Sender: TObject);
begin
  if (Assigned(Self.FFormBlkClose)) then
    Self.FFormBlkClose(Self);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TRelief.HideMouse();
begin
  if (Self.LastPos.X > 0) then
  begin
    Self.show(Point(-1, -1));

    LastPos.X := -1;
    LastPos.Y := -1;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TRelief.BlokEditEvent(Sender: TObject; Blok: TGraphBlok);
begin
  if (Assigned(Self.FOnBlokEdit)) then
    Self.FOnBlokEdit(Self, Blok);
end;

/// /////////////////////////////////////////////////////////////////////////////

function TRelief.GetShowBlokPopisky(): Boolean;
begin
  case (Self.Mode) of
    dmBitmap, dmSepVert, dmSepHor:
      Result := Self.PanelBitmap.ShowBlokPopisky;
    dmBloky, dmRoots:
      Result := Self.PanelObjects.ShowBlokPopisky;
  else
    Result := true;
  end;
end;

procedure TRelief.SetShowBlokPopisky(show: Boolean);
begin
  case (Self.Mode) of
    dmBitmap, dmSepVert, dmSepHor:
      Self.PanelBitmap.ShowBlokPopisky := show;
    dmBloky, dmRoots:
      Self.PanelObjects.ShowBlokPopisky := show;
  end;
  Self.show(Point(-1, -1));
end;

/// /////////////////////////////////////////////////////////////////////////////

end.// unit
