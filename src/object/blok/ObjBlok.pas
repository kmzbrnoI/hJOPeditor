unit ObjBlok;

interface

uses IniFiles, Global, PGraphics, symbolHelper;

type

  TBlkType = (
    track, signal, other, text, description, turnout, crossing, linker,
    linker_train, lock, derail, disconnector, pst
  );

  TGraphBlok = class
  protected
    function StandardColor(colors: TObjColors; selected: boolean; mode: TMode): SymbolColor;

  public
    typ: TBlkType;

    block: Integer;
    area: Integer;
    index: Integer; // index bloku daneho typu v poli vsech bloku (tzn. kazdy typ ma svou indexaci - pocinaje 0)

    constructor Create(index: Integer);
    procedure Load(ini: TMemIniFile; key: string; version: Word); virtual;
    procedure Save(ini: TMemIniFile; key: string); virtual;
    procedure Paint(DrawObject: TDrawObject; panelGraphics: TPanelGraphics; colors: TObjColors; selected: boolean;
      mode: TMode); virtual; abstract;

    class function TypeToFileStr(typ: TBlkType): string;
  end;

implementation

constructor TGraphBlok.Create(index: Integer);
begin
  inherited Create();
  Self.index := index;
  Self.block := -1;
  Self.area := 0;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TGraphBlok.Load(ini: TMemIniFile; key: string; version: Word);
begin
  Self.block := ini.ReadInteger(key, 'B', -1);
  Self.area := ini.ReadInteger(key, 'OR', -1);
end;

procedure TGraphBlok.Save(ini: TMemIniFile; key: string);
begin
  ini.WriteInteger(key, 'B', Self.block);
  ini.WriteInteger(key, 'OR', Self.area);
end;

/// /////////////////////////////////////////////////////////////////////////////

class function TGraphBlok.TypeToFileStr(typ: TBlkType): string;
begin
  case (typ) of
    TBlkType.track:
      Result := 'U';
    TBlkType.signal:
      Result := 'N';
    TBlkType.turnout:
      Result := 'V';
    TBlkType.crossing:
      Result := 'PRJ';
    TBlkType.text:
      Result := 'T';
    TBlkType.other:
      Result := 'P';
    TBlkType.linker:
      Result := 'Uv';
    TBlkType.linker_train:
      Result := 'UvS';
    TBlkType.lock:
      Result := 'Z';
    TBlkType.derail:
      Result := 'Vyk';
    TBlkType.disconnector:
      Result := 'R';
    TBlkType.description:
      Result := 'TP';
    TBlkType.pst:
      Result := 'PSt';
  else
    Result := '?';
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TGraphBlok.StandardColor(colors: TObjColors; selected: boolean; mode: TMode): SymbolColor;
begin
  Result := colors.normal;

  if (selected) then
    Result := colors.selected
  else if (mode = dmBlocks) then
  begin
    case (Self.block) of
      -1: Result := colors.alert;
      -2: Result := colors.intUnassigned;
    else
      Result := colors.normal;
    end;
  end else if (mode = dmAreas) then
    Result := AreaColor(Self.area);
end;

////////////////////////////////////////////////////////////////////////////////

end.
