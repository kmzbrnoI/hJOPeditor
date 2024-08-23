unit ObjBlokVyhybka;

interface

uses ObjBlok, Types, IniFiles, Global, symbolHelper, PGraphics, Generics.Collections;

type

  TTurnout = class(TGraphBlok)
    PolohaPlus: Byte;
    Position: TPoint;
    SymbolID: Integer;
    obj: Integer; // index useku, na kterem vyhybka je

    constructor Create(index: Integer);
    procedure Load(ini: TMemIniFile; key: string; version: Word); override;
    procedure Save(ini: TMemIniFile; key: string); override;
    procedure Paint(DrawObject: TDrawObject; panelGraphics: TPanelGraphics; colors: TObjColors; selected: boolean;
      mode: TMode); override;

    function GetEqTurnout(blocks: TList<TGraphBlok>): TTurnout;
  end;

implementation

/// /////////////////////////////////////////////////////////////////////////////

constructor TTurnout.Create(index: Integer);
begin
  inherited;
  Self.typ := TBlkType.turnout;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TTurnout.Load(ini: TMemIniFile; key: string; version: Word);
begin
  inherited;

  Self.SymbolID := ini.ReadInteger(key, 'S', 0);
  Self.PolohaPlus := ini.ReadInteger(key, 'P', 0);
  Self.Position.X := ini.ReadInteger(key, 'X', 0);
  Self.Position.Y := ini.ReadInteger(key, 'Y', 0);
  Self.obj := ini.ReadInteger(key, 'O', -1);
end;

procedure TTurnout.Save(ini: TMemIniFile; key: string);
begin
  inherited;

  ini.WriteInteger(key, 'S', Self.SymbolID);
  ini.WriteInteger(key, 'P', Self.PolohaPlus);
  ini.WriteInteger(key, 'X', Self.Position.X);
  ini.WriteInteger(key, 'Y', Self.Position.Y);
  ini.WriteInteger(key, 'O', Self.obj);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TTurnout.Paint(DrawObject: TDrawObject; panelGraphics: TPanelGraphics; colors: TObjColors; selected: boolean;
  mode: TMode);
begin
  SymbolDraw(DrawObject.SymbolIL, DrawObject.Canvas, Self.Position, Self.SymbolID,
    Self.StandardColor(colors, selected, mode));
end;

/// /////////////////////////////////////////////////////////////////////////////

function TTurnout.GetEqTurnout(blocks: TList<TGraphBlok>): TTurnout;
begin
  for var block: TGraphBlok in blocks do
    if ((block.typ = TBlkType.turnout) and (TTurnout(block).Position = Self.Position) and (TTurnout(block).SymbolID = Self.SymbolID)) then
      Exit(TTurnout(block));
  Result := nil;
end;

/// /////////////////////////////////////////////////////////////////////////////

end.
