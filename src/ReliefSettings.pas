unit ReliefSettings;

interface

uses SysUtils, Graphics, IniFiles, Panel;

type
  TReliefOptions = class

  private const
    _DEF_COLOR_BACK = clBlack;
    _DEF_COLOR_GRID = clGray;
    _DEF_COLOR_CURSOR = clYellow;
    _DEF_COLOR_CURSOR_ON_OBJECT = clRed;
    _DEF_COLOR_CURSOR_OPERATION = clFuchsia;
    _DEF_COLOR_OBJECTS = 1;
    _DEF_GRID = true;

  private
    Colors: record
      Grid, Back, Cursor, CursorOnObject, CursorOperation: TColor;
    end; // Colors

    FBlockFile: String;
    mGrid: Boolean;

  public
    procedure LoadData(FileName: string);
    procedure SaveData(FileName: string);
    procedure UseData(Relief: TRelief);

    property GridColor: TColor read Colors.Grid write Colors.Grid;
    property BackColor: TColor read Colors.Back write Colors.Back;
    property CursorColor: TColor read Colors.Cursor write Colors.Cursor;
    property CursorOnObjectColor: TColor read Colors.CursorOnObject write Colors.CursorOnObject;
    property CursorOperationColor: TColor read Colors.CursorOperation write Colors.CursorOperation;
    property Grid: boolean read mGrid write mGrid;
    property BlockFile: string read FBlockFile write FBlockFile;
  end; // TReliefOptions

var
  ReliefOptions: TReliefOptions;

implementation

procedure TReliefOptions.LoadData(FileName: string);
var ini: TMemIniFile;
begin
  ini := TMemIniFile.Create(FileName, TEncoding.UTF8);

  try
    Self.Colors.Grid := StringToColor(ini.ReadString('Colors', 'Mrizka',
      ColorToString(_DEF_COLOR_GRID)));
    Self.Colors.Back := StringToColor(ini.ReadString('Colors', 'Pozadi',
      ColorToString(_DEF_COLOR_BACK)));
    Self.Colors.Cursor := StringToColor(ini.ReadString('Colors', 'Kurzor',
      ColorToString(_DEF_COLOR_CURSOR)));
    Self.Colors.CursorOnObject := StringToColor(ini.ReadString('Colors', 'KurzorOnObject',
      ColorToString(_DEF_COLOR_CURSOR_ON_OBJECT)));
    Self.Colors.CursorOperation := StringToColor(ini.ReadString('Colors', 'KurzorOperation',
      ColorToString(_DEF_COLOR_CURSOR_OPERATION)));
    Self.Grid := ini.ReadBool('Obecne', 'Mrizka', _DEF_GRID);
    Self.FBlockFile := ini.ReadString('Obecne', 'BlockFile', '');
  finally
    ini.Free();
  end;
end;

procedure TReliefOptions.SaveData(FileName: string);
var ini: TMemIniFile;
begin
  ini := TMemIniFile.Create(FileName, TEncoding.UTF8);

  try
    ini.WriteString('Colors', 'Mrizka', ColorToString(Self.Colors.Grid));
    ini.WriteString('Colors', 'Pozadi', ColorToString(Self.Colors.Back));
    ini.WriteString('Colors', 'Kurzor', ColorToString(Self.Colors.Cursor));
    ini.WriteString('Colors', 'KurzorOnObject', ColorToString(Self.Colors.CursorOnObject));
    ini.WriteString('Colors', 'KurzorOperation', ColorToString(Self.Colors.CursorOperation));
    ini.WriteBool('Obecne', 'Mrizka', Self.Grid);
    ini.WriteString('Obecne', 'BlockFile', Self.FBlockFile);

    ini.UpdateFile();
  finally
    ini.Free();
  end;
end;

procedure TReliefOptions.UseData(Relief: TRelief);
begin
  Relief.GridColor := Self.GridColor;
  Relief.BackColor := Self.BackColor;
  Relief.CursorColor := Self.CursorColor;
  Relief.CursorOnObjectColor := Self.CursorOnObjectColor;
  Relief.CursorOperationColor := Self.CursorOperationColor;
  Relief.Grid := Self.Grid;
end;

initialization
  ReliefOptions := TReliefOptions.Create();
finalization
  FreeAndNil(ReliefOptions);

end.// unit
