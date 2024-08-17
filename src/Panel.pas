unit Panel;

interface

uses DXDraws, ImgList, Controls, Windows, SysUtils, Graphics, Classes, Types,
  ReliefObjects, Forms, StdCtrls, ExtCtrls, ReliefBitmap, Menus, ReliefText,
  Global, BitmapToObj, OblastRizeni, StrUtils, DirectX, PGraphics,
  ObjBlok, symbolHelper, Generics.Collections, ReliefCommon;

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
    _MAX_TEXT_LENGTH = 32;
    _MIN_TEXT_LENGTH = 1;

    _RESOURCE_SYMBOLS = 'All8';
    _RESOURCE_TEXT = 'Text8';
    _RESOURCE_DK = 'DK8';

    // default colors
    _DEF_COLOR_BACK = clBlack;
    _DEF_COLOR_GRID = clGray;
    _DEF_COLOR_CURSOR = clYellow;
    _DEF_COLOR_CURSOR_ON_OBJECT = clRed;
    _DEF_COLOR_CURSOR_OPERATION = clFuchsia;
    _DEF_COLOR_OBJECTS = 1;
    _DEF_GRID = true;

    // file suffixes
    _FILESUFFIX_BPNL = '.bpnl';
    _FILESUFFIX_OBJ = '.opnl';

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

    mPanelWidth, mPanelHeight: SmallInt;
    mGrid: Boolean;

    Colors: record
      Grid, Back, Cursor, CursorOnObject, CursorOperation: TColor;
      Objects: ShortInt;
    end;

    LastPos: TPoint;

    Panel: record
      FileState: TReliefFileState;
      FilePath: string;
    end;

    DK_Menu: TPopUpMenu;

    ORMove: TORGraf;
    ORClick: TORGraf;
    FMove: Boolean;

    mOnError: TErrorEvent;
    mOnMove: TMoveEvent;
    mOnChangeText: TChangeTextEvent;
    mOnBlokEdit: TBlokAskEvent;
    mOnMsg: TMsgEvent;
    mFormBlkClose: TGlobalEvent;

    procedure Initialize(Rozmery: TPoint; Mode: TMode);

    procedure PaintGrid(MrizkaColor: TColor);
    function PaintCursor(CursorData: TCursorDraw): Byte;

    procedure PaintOR();
    function PaintORCursor(CursorPos: TPoint): TCursorDraw;
    function ORMouseUp(Position: TPoint; Button: TMouseButton): Byte;
    function GetORGraf(pos: TPoint): TORGraf;
    procedure DKMenuInit();
    procedure DKPropClick(Sender: TObject);
    procedure DKDeleteClick(Sender: TObject);

    function ORSave(): string;
    procedure ORLoad(data: string);

    procedure DXDMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure DXDMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure DXDDoubleClick(Sender: TObject);

    procedure BitmapMouseUp(Position: TPoint; Button: TMouseButton);
    procedure ObjectMouseUp(Position: TPoint; Button: TMouseButton);

    procedure BitmapDblClick(Position: TPoint);
    procedure ObjectDblClick(Position: TPoint);

    procedure SetGrid(aMrizka: Boolean);

    procedure FileLoad(aFile: string);

    procedure ShowEvent();
    procedure BlokEditEvent(Sender: TObject; Blok: TGraphBlok);
    procedure MessageEvent(Sender: TObject; msg: string);
    procedure BlkFormCloseEvent(Sender: TObject);

    function IsAreaStuffPresent(pos: TPoint): Boolean;

    procedure SetGroup(State: Boolean);
    function GetGroup(): Boolean;

    procedure AssignBitmapEvents();
    procedure AssignObjectEvents();

    function GetShowBlokPopisky(): Boolean;
    procedure SetShowBlokPopisky(show: Boolean);

  public
    ORs: TObjectList<TOR>;

    constructor Create(DDRaw: TDXDraw; aParentForm: TForm);
    destructor Destroy; override;

    procedure New(size: TPoint; firstOR: TOR);
    procedure Open(aFile: string);
    procedure Save(aFile: string);
    function Import(aFile: string): string;
    procedure ImportOldOpnl(aFile: string);

    procedure Show(CursorPos: TPoint);
    procedure Escape(Group: Boolean);
    procedure SetSize(aWidth, aHeight: Byte);

    procedure AddSymbol(SymbolID: Integer);
    procedure AddText(Text: string; Color: SymbolColor; popisekBlok: Boolean);
    procedure AddJCClick();
    procedure AddSeparatorVert();
    procedure AddSeparatorHor();
    procedure AddTrackName();
    procedure AddTrainPos();

    procedure MoveBitmapSymbol();
    procedure DeleteBitmapSymbol();
    procedure KeyPress(Key: Integer); // accepts keys from ApplicationEvent component

    procedure SwitchMode(aMode: TMode);
    procedure HideMouse();

    // OR
    procedure AddOR(oblr: TOR);
    procedure DeleteOR(pos: TPoint);

    function CheckValid(var error_cnt: Byte): TStrings; // overi validitu naeditovanych dat a vrati chybove hlasky

    property GridColor: TColor read Colors.Grid write Colors.Grid;
    property BackColor: TColor read Colors.Back write Colors.Back;
    property CursorColor: TColor read Colors.Cursor write Colors.Cursor;
    property CursorOnObjectColor: TColor read Colors.CursorOnObject write Colors.CursorOnObject;
    property CursorOperationColor: TColor read Colors.CursorOperation write Colors.CursorOperation;
    property ObjectColor: ShortInt read Colors.Objects write Colors.Objects;

    property Mode: TMode read DrawMode;
    property Group: Boolean read GetGroup write SetGroup;
    property Grid: Boolean read mGrid write SetGrid;

    property PanelWidth: SmallInt read mPanelWidth;
    property PanelHeight: SmallInt read mPanelHeight;

    property FileState: TReliefFileState read Panel.FileState;
    property FilePath: string read Panel.FilePath;

    // events
    property OnError: TErrorEvent read mOnError write mOnError;
    property OnMove: TMoveEvent read mOnMove write mOnMove;
    property OnChangeText: TChangeTextEvent read mOnChangeText write mOnChangeText;
    property OnBlokEdit: TBlokAskEvent read mOnBlokEdit write mOnBlokEdit;
    property OnMsg: TMsgEvent read mOnMsg write mOnMsg;
    property FormBlkClose: TGlobalEvent read mFormBlkClose write mFormBlkClose;
    property ShowBlokPopisky: Boolean read GetShowBlokPopisky write SetShowBlokPopisky;
  end;

implementation

{$R Resource.res}

uses fOREdit, fMain, ownStrUtils;

constructor TRelief.Create(DDRaw: TDXDraw; aParentForm: TForm);
begin
  inherited Create;

  Self.ORs := TObjectList<TOR>.Create();

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

  Self.Colors.Grid := _DEF_COLOR_GRID;
  Self.Colors.Back := _DEF_COLOR_BACK;
  Self.Colors.Cursor := _DEF_COLOR_CURSOR;
  Self.Colors.CursorOnObject := _DEF_COLOR_CURSOR_ON_OBJECT;
  Self.Colors.Objects := _DEF_COLOR_OBJECTS;
  Self.Colors.CursorOperation := _DEF_COLOR_CURSOR_OPERATION;

  Self.DrawMode := Mode;
  Self.mGrid := _DEF_GRID;
  Self.Panel.FileState := fsUnsaved;

  Self.ORs.Clear();
  Self.ORMove.areai := -1;
  Self.ORClick.areai := -1;
  Self.FMove := false;

  Self.IL_Symbols := LoadIL(_RESOURCE_SYMBOLS, 8, 12);
  Self.IL_Text := LoadIL(_Resource_Text, 8, 12);
  Self.IL_DK := LoadIL(_Resource_DK, _OR_DK_SIZE.X*_SYMBOL_WIDTH, _OR_DK_SIZE.Y*_SYMBOL_HEIGHT);

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

  Self.mPanelWidth := Rozmery.X;
  Self.mPanelHeight := Rozmery.Y;
  Self.DrawObject.Width := Self.mPanelWidth * _SYMBOL_WIDTH;
  Self.DrawObject.Height := Self.mPanelHeight * _SYMBOL_HEIGHT;

  Self.Show(Point(-1, -1));

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
  if (RightStr(aFile, 5) = _FILESUFFIX_BPNL) then
    Mode := dmBitmap
  else if (RightStr(aFile, 5) = _FILESUFFIX_OBJ) then
    Mode := dmBloky
  else
    raise EGeneralFileOpen.Create('Soubor s nepodporovanou příponou!');

  Self.Initialize(Point(0, 0), Mode);
  Self.FileLoad(aFile);
end;

function TRelief.Import(aFile: string): string;
var log: string;
begin
  if (RightStr(aFile, 4) <> ReliefBitmap._IMPORT_MYJOP_SUFFIX) then
    raise EGeneralFileOpen.Create('Soubor s nepodporovanou příponou!');

  log := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now) + ': spouštím import souboru ' + aFile + ' ...' + #13#10;
  Self.Initialize(Point(0, 0), dmBitmap);
  log := log + Self.PanelBitmap.ImportMyJOP(aFile, Self.ORs);

  Self.Panel.FileState := Self.PanelBitmap.fileState;
  Self.mPanelWidth := Self.PanelBitmap.PanelWidth;
  Self.mPanelHeight := Self.PanelBitmap.PanelHeight;

  Self.DrawObject.Width := Self.mPanelWidth * _SYMBOL_WIDTH;
  Self.DrawObject.Height := Self.mPanelHeight * _SYMBOL_HEIGHT;

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

  Self.IL_Symbols.Free();
  Self.IL_Text.Free();
  Self.IL_DK.Free();

  Self.Graphics.Free();

  Self.DK_Menu.Free();
  Self.ORs.Free();

  inherited Destroy();
end; // destructor

// hlavni zobrazeni celeho reliefu
procedure TRelief.Show(CursorPos: TPoint);
begin
  if (not Assigned(Self.DrawObject)) then
    Exit();
  if (not Self.DrawObject.CanDraw) then
    Exit();

  Self.DrawObject.Surface.Canvas.Release();
  Self.DrawObject.BeginScene();

  // smazani reliefu
  Self.DrawObject.Surface.Fill(Self.Colors.Back);

  case (Self.DrawMode) of
    dmBitmap, dmSepHor, dmSepVert:
      Self.PanelBitmap.PaintMove(CursorPos);
  end; // case

  // vykresleni objektu s ohledem na DrawMode
  case (Self.DrawMode) of
    dmBitmap, dmSepHor, dmSepVert:
      Self.PanelBitmap.Paint();
    dmBloky, dmRoots:
      Self.PanelObjects.Paint();
  end;
  Self.PaintOR();
  if (Self.Grid) then
    Self.PaintGrid(Self.Colors.Grid);

  if ((Self.ORMove.areai >= 0) or (Self.GetORGraf(CursorPos).areai > -1)) then
  begin
    // prioritu ma posun OR
    Self.PaintCursor(Self.PaintORCursor(CursorPos));
  end else begin
    // vykreslovani kurzoru a specialnich barev vzhledem k modum a operacim
    case (Self.DrawMode) of
      dmBitmap, dmSepHor, dmSepVert:
        Self.PaintCursor(Self.PanelBitmap.PaintCursor(CursorPos));
      dmBloky, dmRoots:
        Self.PaintCursor(Self.PanelObjects.PaintCursor(CursorPos));
    end; // case
  end;

  Self.DrawObject.EndScene();
  Self.DrawObject.Surface.Canvas.Release();
  Self.DrawObject.Flip();
end;

// vykresleni mrizky
procedure TRelief.PaintGrid(MrizkaColor: TColor);
begin
  Self.DrawObject.Surface.Canvas.Pen.Color := MrizkaColor;

  for var i: Integer := 1 to Self.PanelWidth - 1 do
  begin
    Self.DrawObject.Surface.Canvas.MoveTo((i * _SYMBOL_WIDTH) - 1, 0);
    Self.DrawObject.Surface.Canvas.LineTo((i * _SYMBOL_WIDTH) - 1, Self.PanelHeight * _SYMBOL_HEIGHT);
  end; // for i
  for var i: Integer := 1 to Self.PanelHeight - 1 do
  begin
    Self.DrawObject.Surface.Canvas.MoveTo(0, (i * _SYMBOL_HEIGHT) - 1);
    Self.DrawObject.Surface.Canvas.LineTo(Self.PanelWidth * _SYMBOL_WIDTH, (i * _SYMBOL_HEIGHT) - 1);
  end; // for i
end;

// vykresluje kurzor
function TRelief.PaintCursor(CursorData: TCursorDraw): Byte;
begin
  if ((CursorData.pos1.X < 0) and (CursorData.pos1.Y < 0) and (CursorData.pos2.X < 0) and (CursorData.pos2.Y < 0)) then
    Exit(1);

  CursorData.Pos1.X := CursorData.Pos1.X - 1;
  CursorData.Pos1.Y := CursorData.Pos1.Y - 1;
  CursorData.Pos2.X := CursorData.Pos2.X - 1;
  CursorData.Pos2.Y := CursorData.Pos2.Y - 1;

  case (CursorData.color) of
    TCursorColor.ccDefault: Self.DrawObject.Surface.Canvas.Pen.Color := Self.colors.cursor;
    TCursorColor.ccActiveOperation: Self.DrawObject.Surface.Canvas.Pen.Color := Self.colors.cursorOperation;
    TCursorColor.ccOnObject: Self.DrawObject.Surface.Canvas.Pen.Color := Self.colors.cursorOnObject;
  end;

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
  LastPos.X := X div _SYMBOL_WIDTH;
  LastPos.Y := Y div _SYMBOL_HEIGHT;

  if (Self.ORMouseUp(LastPos, Button) = 0) then
  begin
    case (Self.DrawMode) of
      dmBitmap, dmSepHor, dmSepVert:
        Self.BitmapMouseUp(LastPos, Button);
      dmBloky, dmRoots:
        Self.ObjectMouseUp(LastPos, Button);
    end; // case
  end; // ORMouseUp = 0

  Self.Show(LastPos);
end;

procedure TRelief.DXDMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if ((X div _SYMBOL_WIDTH = LastPos.X) and (Y div _SYMBOL_HEIGHT = LastPos.Y)) then
    Exit;

  LastPos.X := X div _SYMBOL_WIDTH;
  LastPos.Y := Y div _SYMBOL_HEIGHT;

  case (Self.DrawMode) of
    dmBloky, dmRoots:
      Self.PanelObjects.MouseMove(LastPos);
  end; // case

  Self.Show(LastPos);

  if Assigned(Self.OnMove) then
    Self.OnMove(Self, LastPos);
end;

procedure TRelief.DXDDoubleClick(Sender: TObject);
begin
  case (Self.DrawMode) of
    dmBitmap, dmSepHor, dmSepVert:
    begin
      // Open DK window od DK double-click
      var orGraf: TORGRaf := Self.GetORGraf(LastPos);
      if ((orGraf.areai > -1) and (orGraf.objType = TORGraphSymbol.orsDK)) then
        F_OREdit.EditOR(orGraf.areai);

      Self.BitmapDblClick(LastPos);
    end;
    dmBloky, dmRoots:
    begin
      Self.ObjectDblClick(LastPos);
    end;
  end; // case

  Self.Show(LastPos);
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
            mouse.X := mouse.X - _SYMBOL_WIDTH;
          VK_RIGHT:
            mouse.X := mouse.X + _SYMBOL_WIDTH;
          VK_UP:
            mouse.Y := mouse.Y - _SYMBOL_HEIGHT;
          VK_DOWN:
            mouse.Y := mouse.Y + _SYMBOL_HEIGHT;
        end;

        SetCursorPos(mouse.X, mouse.Y);
      end;

    VK_RETURN:
      Self.DXDMouseUp(Self.DrawObject, mbLeft, [], Self.LastPos.X * _SYMBOL_WIDTH, Self.LastPos.Y * _SYMBOL_HEIGHT);
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
  Self.Show(LastPos);
end;

procedure TRelief.SwitchMode(aMode: TMode);
begin
  if (((Self.DrawMode = dmBitmap) or (Self.DrawMode = dmSepHor) or (Self.DrawMode = dmSepVert)) and (aMode = dmBloky))
  then
  begin
    // konverze z Bitmap na Objects

    Self.PanelObjects := TPanelObjects.Create(Self.IL_Symbols, Self.IL_Text, Self.DrawObject.Surface.Canvas,
      Self.PanelWidth, Self.PanelHeight, Self.DrawObject, Self.Graphics);
    Self.AssignObjectEvents();

    try
      Self.PanelObjects.Import(Self.PanelBitmap);
    except
      FreeAndNil(Self.PanelObjects);
      raise;
    end;

    Self.PanelObjects.ShowBlokPopisky := Self.PanelBitmap.ShowBlokPopisky;
    FreeAndNil(Self.PanelBitmap);
    Self.Panel.fileState := fsUnsaved;
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

  Self.Show(Point(-1, -1));
end;

procedure TRelief.BitmapMouseUp(Position: TPoint; Button: TMouseButton);
begin
  try
    Self.PanelBitmap.MouseUp(Position, Button);
  except
    on E: Exception do
      if Assigned(Self.OnError) then
        Self.OnError(Self, E.Message);
  end;
end;

procedure TRelief.ObjectMouseUp(Position: TPoint; Button: TMouseButton);
begin
  try
    Self.PanelObjects.MouseUp(Position, Button);
  except
    on E: Exception do
      if Assigned(Self.OnError) then
        Self.OnError(Self, E.Message);
  end;
end;

procedure TRelief.BitmapDblClick(Position: TPoint);
begin
  try
    Self.PanelBitmap.DblClick(Position);
  except
    on E: Exception do
      if Assigned(Self.OnError) then
        Self.OnError(Self, E.Message);
  end;
end;

procedure TRelief.ObjectDblClick(Position: TPoint);
begin
  try
    Self.PanelObjects.DblClick(Position);
  except
    on E: Exception do
      if Assigned(Self.OnError) then
        Self.OnError(Self, E.Message);
  end;
end;

procedure TRelief.SetSize(aWidth, aHeight: Byte);
begin
  if (Self.Mode <> dmBitmap) then
    raise Exception.Create('Rozměny lze měnit jen v režimu bitmapy!');

  PanelBitmap.SetSize(aWidth, aHeight);

  Self.mPanelWidth := aWidth;
  Self.mPanelHeight := aHeight;

  Self.DrawObject.Width := aWidth * _SYMBOL_WIDTH;
  Self.DrawObject.Height := aHeight * _SYMBOL_HEIGHT;

  Self.Show(LastPos);
end;

procedure TRelief.SetGrid(aMrizka: Boolean);
begin
  Self.mGrid := aMrizka;
  Self.Show(Point(-1, -1));
end;

procedure TRelief.FileLoad(aFile: string);
begin
  case (Self.DrawMode) of
    dmBitmap, dmSepHor, dmSepVert:
      begin
        var ORs: string;
        Self.PanelBitmap.BpnlLoad(aFile, ORs);
        Self.ORLoad(ORs);
        Self.Panel.fileState := Self.PanelBitmap.fileState;
        Self.mPanelWidth := Self.PanelBitmap.PanelWidth;
        Self.mPanelHeight := Self.PanelBitmap.PanelHeight;
      end; // dmBitmap
    dmBloky, dmRoots:
      begin
        var ORs: string;
        Self.PanelObjects.OpnlLoad(aFile, ORs);
        Self.ORLoad(ORs);
        Self.Panel.fileState := Self.PanelObjects.FileStav;
        Self.mPanelWidth := Self.PanelObjects.PanelWidth;
        Self.mPanelHeight := Self.PanelObjects.PanelHeight;
      end; // dmBloky
  end; // case

  Self.DrawObject.Width := Self.PanelWidth * _SYMBOL_WIDTH;
  Self.DrawObject.Height := Self.PanelHeight * _SYMBOL_HEIGHT;

  Self.Panel.filePath := aFile;
end;

procedure TRelief.Save(aFile: string);
begin
  case (Self.DrawMode) of
    dmBitmap, dmSepHor, dmSepVert:
      begin
        Self.PanelBitmap.BpnlSave(aFile, Self.ORSave());
        Self.Panel.fileState := Self.PanelBitmap.fileState;
      end; // dmBitmap

    dmBloky, dmRoots:
      begin
        Self.PanelObjects.OpnlSave(aFile, Self.ORSave());
        Self.Panel.fileState := Self.PanelObjects.FileStav;
      end; // dmBloky
  end;
  /// case

  Self.Panel.filePath := aFile;
end;

procedure TRelief.ShowEvent;
begin
  Self.Show(LastPos);
end;

procedure TRelief.SetGroup(State: Boolean);
begin
  if (Assigned(Self.PanelBitmap)) then
    Self.PanelBitmap.Group := State;
end;

function TRelief.GetGroup(): Boolean;
begin
  Result := false;

  if (Assigned(Self.PanelBitmap)) then
    Result := Self.PanelBitmap.Group;
end;

procedure TRelief.MoveBitmapSymbol();
begin
  if (Assigned(Self.PanelBitmap)) then
    Self.PanelBitmap.Move();
  Self.FMove := true;
  Self.Show(Self.LastPos);
end;

procedure TRelief.DeleteBitmapSymbol();
begin
  if (Assigned(Self.PanelBitmap)) then
    Self.PanelBitmap.Delete;
  Self.FMove := false;
  Self.Show(Self.LastPos);
end;

procedure TRelief.AddSymbol(SymbolID: Integer);
begin
  if (Assigned(Self.PanelBitmap)) then
    Self.PanelBitmap.AddSymbol(SymbolID);
end;

procedure TRelief.AddText(Text: string; Color: SymbolColor; popisekBlok: Boolean);
begin
  if (Assigned(Self.PanelBitmap)) then
    Self.PanelBitmap.AddText(Text, Color, popisekBlok);
end;

procedure TRelief.AddJCClick();
begin
  if (Assigned(Self.PanelBitmap)) then
    Self.PanelBitmap.AddJCClick();
end;

procedure TRelief.AddSeparatorVert();
begin
  if (Self.Mode <> dmSepVert) then
    Self.SwitchMode(dmSepVert);
  if (Assigned(Self.PanelBitmap)) then
    Self.PanelBitmap.AddSeparVert();
end;

procedure TRelief.AddSeparatorHor();
begin
  if (Self.Mode <> dmSepHor) then
    Self.SwitchMode(dmSepHor);
  if (Assigned(Self.PanelBitmap)) then
    Self.PanelBitmap.AddSeparHor();
end;

procedure TRelief.AddTrackName();
begin
  if (Assigned(Self.PanelBitmap)) then
    Self.PanelBitmap.AddTrackName();
end;

procedure TRelief.AddTrainPos();
begin
  if (Assigned(Self.PanelBitmap)) then
    Self.PanelBitmap.AddTrainPos();
end;

procedure TRelief.MessageEvent(Sender: TObject; msg: string);
begin
  if (Assigned(Self.OnMsg)) then
    Self.OnMsg(Self, msg);
end;

/// /////////////////////////////////////////////////////////////////////////////
// operace s oblastmi rizeni:

procedure TRelief.AddOR(oblr: TOR);
begin
  Self.ORs.Add(oblr);
  Self.ORMove.areai := Self.ORs.Count - 1;
  Self.ORMove.objType := TORGraphSymbol.orsDK;
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
    if (Self.ORMove.areai = i) then
    begin
      case (Self.ORMove.objType) of
        TORGraphSymbol.orsDK:
          oblr.Poss.DK := Self.LastPos;
        TORGraphSymbol.orsQueue:
          oblr.Poss.Queue := Self.LastPos;
        TORGraphSymbol.orsTime:
          oblr.Poss.Time := Self.LastPos;
      end;
    end;

    Self.IL_DK.Draw(Self.DrawObject.Surface.Canvas, oblr.Poss.DK.X * _SYMBOL_WIDTH, oblr.Poss.DK.Y * _SYMBOL_HEIGHT,
      (Integer(oblr.Poss.DKOr) * 10) + 1);

    Self.Graphics.TextOutputI(oblr.Poss.Queue, '00 VZ PV EZ 00', scGray, clBlack);
    Self.Graphics.TextOutputI(oblr.Poss.Time, 'MER CASU', scGray, clBlack);
    Self.Graphics.TextOutputI(Point(oblr.Poss.Time.X + 8, oblr.Poss.Time.Y), '        ', scBlack, clWhite);
  end; // for i
end;

// vykresluje kurzor pri pohybu
function TRelief.PaintORCursor(CursorPos: TPoint): TCursorDraw;
begin
  if (Self.ORMove.areai < 0) then
  begin
    var tmp_or := Self.GetORGraf(CursorPos);
    if (tmp_or.areai = -1) then
      Exit();

    Result.color := TCursorColor.ccOnObject;

    case (tmp_or.objType) of
      TORGraphSymbol.orsDK:
        begin
          Result.pos1.X := (Self.ORs[tmp_or.areai].Poss.DK.X) * _SYMBOL_WIDTH;
          Result.pos1.Y := (Self.ORs[tmp_or.areai].Poss.DK.Y) * _SYMBOL_HEIGHT;
          Result.pos2.X := (Self.ORs[tmp_or.areai].Poss.DK.X+_OR_DK_SIZE.X-1) * _SYMBOL_WIDTH;
          Result.Pos2.Y := (Self.ORs[tmp_or.areai].Poss.DK.Y+_OR_DK_SIZE.Y-1) * _SYMBOL_HEIGHT;
        end;
      TORGraphSymbol.orsQueue:
        begin
          Result.pos1.X := (Self.ORs[tmp_or.areai].Poss.Queue.X) * _SYMBOL_WIDTH;
          Result.pos1.Y := (Self.ORs[tmp_or.areai].Poss.Queue.Y) * _SYMBOL_HEIGHT;
          Result.pos2.X := (Self.ORs[tmp_or.areai].Poss.Queue.X+_OR_QUEUE_SIZE.X-1) * _SYMBOL_WIDTH;
          Result.Pos2.Y := (Self.ORs[tmp_or.areai].Poss.Queue.Y+_OR_QUEUE_SIZE.Y-1) * _SYMBOL_HEIGHT;
        end;
      TORGraphSymbol.orsTime:
        begin
          Result.pos1.X := (Self.ORs[tmp_or.areai].Poss.Time.X) * _SYMBOL_WIDTH;
          Result.pos1.Y := (Self.ORs[tmp_or.areai].Poss.Time.Y) * _SYMBOL_HEIGHT;
          Result.pos2.X := (Self.ORs[tmp_or.areai].Poss.Time.X+_OR_TIME_SIZE.X-1) * _SYMBOL_WIDTH;
          Result.pos2.Y := (Self.ORs[tmp_or.areai].Poss.Time.Y+_OR_TIME_SIZE.Y-1) * _SYMBOL_HEIGHT;
        end;
    end; // case
  end else begin
    Result.color := TCursorColor.ccOnObject;

    Result.pos1.X := CursorPos.X * _SYMBOL_WIDTH;
    Result.pos1.Y := CursorPos.Y * _SYMBOL_HEIGHT;

    case (Self.ORMove.objType) of
      TORGraphSymbol.orsDK:
        begin
          Result.pos2.X := (CursorPos.X+_OR_DK_SIZE.X-1) * _SYMBOL_WIDTH;
          Result.pos2.Y := (CursorPos.Y+_OR_DK_SIZE.Y-1) * _SYMBOL_HEIGHT;
        end;
      TORGraphSymbol.orsQueue:
        begin
          Result.pos2.X := (CursorPos.X+_OR_QUEUE_SIZE.X-1) * _SYMBOL_WIDTH;
          Result.pos2.Y := (CursorPos.Y+_OR_QUEUE_SIZE.Y-1) * _SYMBOL_HEIGHT;
        end;
      TORGraphSymbol.orsTime:
        begin
          Result.pos2.X := (CursorPos.X+_OR_TIME_SIZE.X-1) * _SYMBOL_WIDTH;
          Result.pos2.Y := (CursorPos.Y+_OR_TIME_SIZE.Y-1) * _SYMBOL_HEIGHT;
        end;
    end;
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
      Exit();
    if (tmp_or.areai < 0) then
      Exit();
    Self.PanelObjects.SetOR(tmp_or.areai);
    if (Assigned(Self.OnBlokEdit)) then
      Self.OnBlokEdit(Self, Self.PanelObjects.selected_obj);
    Result := 1;
  end; // dmBloky

  if (Self.Mode = dmBitmap) then
  begin
    if (Button = mbLeft) then
    begin
      if (Self.ORMove.areai > -1) then
      begin
        // ukonceni pohybu
        Self.ORMove.areai := -1;

        Self.FMove := false;
        Self.Show(Self.LastPos);
        Self.FMove := true;
        Sleep(50);
        Self.Show(Self.LastPos);
      end else begin
        var tmp_or := Self.GetORGraf(Position);
        if (tmp_or.areai > -1) then
          Self.MessageEvent(Self, 'OŘ : ' + ORs[tmp_or.areai].Name + ' (id = ' + ORs[tmp_or.areai].id + ')');

        // zacatek pohybu
        if (Self.FMove) then
          Self.ORMove := tmp_or;
      end;
    end;

    if (Button = mbRight) then
    begin
      Self.ORClick := Self.GetORGraf(Position);
      if (Self.ORClick.areai < 0) then
        Exit;
      case (Self.ORClick.objType) of
       TORGraphSymbol.orsDK:
          Self.DK_Menu.Popup(mouse.CursorPos.X, mouse.CursorPos.Y);
      end; // case
    end;
  end; // dmBitmap
end;

// vraci OR na dane pozici
function TRelief.GetORGraf(pos: TPoint): TORGraf;
begin
  Result.areai := -1;

  for var i := 0 to Self.ORs.Count - 1 do
  begin
    if ((pos.X >= Self.ORs[i].Poss.DK.X) and (pos.Y >= Self.ORs[i].Poss.DK.Y) and
      (Self.ORs[i].Poss.DK.X + _OR_DK_SIZE.X > pos.X) and (Self.ORs[i].Poss.DK.Y + _OR_DK_SIZE.Y > pos.Y)) then
    begin
      Result.areai := i;
      Result.objType := TORGraphSymbol.orsDK;
      Exit();
    end;
    if ((pos.X >= Self.ORs[i].Poss.Queue.X) and (pos.Y >= Self.ORs[i].Poss.Queue.Y) and
      (Self.ORs[i].Poss.Queue.X + _OR_QUEUE_SIZE.X > pos.X) and (Self.ORs[i].Poss.Queue.Y + _OR_QUEUE_SIZE.Y > pos.Y)) then
    begin
      Result.areai := i;
      Result.objType := TORGraphSymbol.orsQueue;
      Exit();
    end;
    if ((pos.X >= Self.ORs[i].Poss.Time.X) and (pos.Y >= Self.ORs[i].Poss.Time.Y) and
      (Self.ORs[i].Poss.Time.X + _OR_TIME_SIZE.X > pos.X) and (Self.ORs[i].Poss.Time.Y + _OR_TIME_SIZE.Y > pos.Y)) then
    begin
      Result.areai := i;
      Result.objType := TORGraphSymbol.orsTime;
      Exit();
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
  F_OREdit.EditOR(Self.ORClick.areai);
end;

procedure TRelief.DKDeleteClick(Sender: TObject);
begin
  if (Self.ORs.Count <= 1) then
  begin
    Application.MessageBox('Poslední OŘ nelze smazat!', 'Nelze pokračovat', MB_OK OR MB_ICONSTOP);
    Exit;
  end;

  if (Application.MessageBox('Opravdu smazat?', 'Otázka', MB_YESNO OR MB_ICONQUESTION) = mrYes) then
    Self.ORs.Delete(Self.ORClick.areai);
end;

// na kazdem radku je ulozena jedna oblast rizeni ve formatu:
// nazev;nazev_zkratka;id;lichy_smer(0,1);orientace_DK(0,1);ModCasStart(0,1);ModCasStop(0,1);ModCasSet(0,1);dkposx;dkposy;qposx;qposy;timeposx;timeposy;osv_mtb|osv_port|osv_name;
function TRelief.ORSave(): string;
begin
  for var oblr in Self.ORs do
  begin
    Result := Result + oblr.Name + ';' + oblr.ShortName + ';' + oblr.id + ';' + IntToStr(Integer(oblr.oddDirection)) + ';' +
      IntToStr(Integer(oblr.Poss.DKOr)) + ';' + BoolToStr(oblr.Rights.ModCasStart) + ';' + BoolToStr(oblr.Rights.ModCasStop) +
      ';' + BoolToStr(oblr.Rights.ModCasSet) + ';' + IntToStr(oblr.Poss.DK.X) + ';' + IntToStr(oblr.Poss.DK.Y) + ';' +
      IntToStr(oblr.Poss.Queue.X) + ';' + IntToStr(oblr.Poss.Queue.Y) + ';' + IntToStr(oblr.Poss.Time.X) + ';' +
      IntToStr(oblr.Poss.Time.Y) + ';';

    for var light in oblr.Lights do
      Result := Result + IntToStr(light.board) + '#' + IntToStr(light.port) + '#' + light.Name + '|';

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

      var area: TOR;
      area := TOR.Create();
      area.Name := data_main[0];
      area.ShortName := data_main[1];
      area.id := data_main[2];

      var dir: Integer := StrToInt(data_main[3]);
      case (dir) of
        Integer(TOROddDirection.ordLeftToRight): area.oddDirection := TOROddDirection.ordLeftToRight;
        Integer(TOROddDirection.ordRightToLeft): area.oddDirection := TOROddDirection.ordRightToLeft;
      else
        area.oddDirection := TOROddDirection.ordLeftToRight;
      end;

      var dkor: Integer := StrToInt(data_main[4]);
      case (dkor) of
        Integer(TDKOrientation.dkoDown): area.Poss.DKOr := TDKOrientation.dkoDown;
        Integer(TDKOrientation.dkoUp): area.Poss.DKOr := TDKOrientation.dkoUp;
      else
        area.Poss.DKOr := TDKOrientation.dkoDown; // default
      end;

      area.Rights.ModCasStart := StrToBool(data_main[5]);
      area.Rights.ModCasStop := StrToBool(data_main[6]);
      area.Rights.ModCasSet := StrToBool(data_main[7]);

      area.Poss.DK.X := StrToInt(data_main[8]);
      area.Poss.DK.Y := StrToInt(data_main[9]);

      area.Poss.Queue.X := StrToInt(data_main[10]);
      area.Poss.Queue.Y := StrToInt(data_main[11]);

      area.Poss.Time.X := StrToInt(data_main[12]);
      area.Poss.Time.Y := StrToInt(data_main[13]);

      data_osv.Clear();
      if (data_main.Count >= 15) then
      begin
        ExtractStringsEx(['|'], [], data_main[14], data_osv);
        for var j := 0 to data_osv.Count - 1 do
        begin
          data_osv2.Clear();
          ExtractStringsEx(['#'], [], data_osv[j], data_osv2);

          if (data_osv2.Count < 2) then
            raise EORLoad.Create('Málo položek definující osvětlení!');

          var light: TORLight;
          light.Board := StrToInt(data_osv2[0]);
          light.Port := StrToInt(data_osv2[1]);
          if (data_osv2.Count > 2) then
            light.Name := data_osv2[2]
          else
            light.Name := '';
          area.Lights.Add(light);
        end;
      end;

      Self.ORs.Add(area);
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
  Self.PanelBitmap.IsAreaStuffPresent := Self.IsAreaStuffPresent;
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
function TRelief.IsAreaStuffPresent(pos: TPoint): Boolean;
begin
  for var area in Self.ORs do
  begin
    if ((pos.X >= area.Poss.DK.X) and (pos.X <= area.Poss.DK.X + 4) and (pos.Y >= area.Poss.DK.Y) and
      (pos.Y <= area.Poss.DK.Y + 2)) then
      Exit(true);

    if ((pos.X >= area.Poss.Queue.X) and (pos.X <= area.Poss.Queue.X + 13) and (pos.Y = area.Poss.Queue.Y)) then
      Exit(true);

    if ((pos.X >= area.Poss.Time.X) and (pos.X <= area.Poss.Time.X + 15) and (pos.Y = area.Poss.Time.Y)) then
      Exit(true);
  end;

  Result := false;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TRelief.BlkFormCloseEvent(Sender: TObject);
begin
  if (Assigned(Self.FormBlkClose)) then
    Self.FormBlkClose(Self);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TRelief.HideMouse();
begin
  if (Self.LastPos.X > 0) then
  begin
    Self.Show(Point(-1, -1));

    LastPos.X := -1;
    LastPos.Y := -1;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TRelief.BlokEditEvent(Sender: TObject; Blok: TGraphBlok);
begin
  if (Assigned(Self.OnBlokEdit)) then
    Self.OnBlokEdit(Self, Blok);
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
  Self.Show(Point(-1, -1));
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TRelief.ImportOldOpnl(aFile: string);
begin
  if ((Self.Mode <> dmBloky) and (Self.Mode <> dmRoots)) then
    raise Exception.Create('Panel musí být v režimu bloků nebo kořenů!');
  Self.PanelObjects.ImportOldOpnl(aFile);
end;

/// /////////////////////////////////////////////////////////////////////////////

end.// unit
