﻿unit fMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, ReliefObjects, Buttons, ToolWin, ComCtrls, ExtCtrls, Panel,
  ImgList, DXDraws, StdCtrls, AppEvnts, StrUtils, ReliefBitmap, Global, Math,
  ReliefSettings, fBlockEdit, DXSprite, DIB, ObjBlok, ReliefText, Types,
  System.ImageList, symbolHelper, ReliefCommon;

type
  TF_Hlavni = class(TForm)
    MM_Hlavni: TMainMenu;
    MI_File: TMenuItem;
    PM_Open: TMenuItem;
    PM_New: TMenuItem;
    PM_Save: TMenuItem;
    PM_SaveAs: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    MI_Help: TMenuItem;
    PM_About: TMenuItem;
    OD_Open: TOpenDialog;
    SD_Save: TSaveDialog;
    IL_Menu: TImageList;
    TB_Turnout: TToolBar;
    ToolButton0: TToolButton;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton19: TToolButton;
    ToolButton18: TToolButton;
    ToolButton17: TToolButton;
    ToolButton12: TToolButton;
    ToolButton13: TToolButton;
    ToolButton14: TToolButton;
    ToolButton15: TToolButton;
    ToolButton16: TToolButton;
    ToolButton24: TToolButton;
    ToolButton25: TToolButton;
    ToolButton28: TToolButton;
    ToolButton29: TToolButton;
    TB_Track_Detected: TToolBar;
    TB_Signal: TToolBar;
    AE_Main: TApplicationEvents;
    MI_Draw: TMenuItem;
    PM_Bitmap: TMenuItem;
    PM_Sep_Vert: TMenuItem;
    PM_Bloky: TMenuItem;
    P_Menu: TPanel;
    TB_BitmapTools: TToolBar;
    B_Move: TButton;
    B_Delete: TButton;
    CHB_Group: TCheckBox;
    ToolButton51: TToolButton;
    SB_Main: TStatusBar;
    TB_Separator: TToolBar;
    TB_Separator_Vert: TToolButton;
    MI_Relief: TMenuItem;
    PM_ChangeRozmery: TMenuItem;
    TB_Bitmap_Other: TToolBar;
    ToolButton30: TToolButton;
    ToolButton31: TToolButton;
    ToolButton32: TToolButton;
    ToolButton33: TToolButton;
    ToolButton34: TToolButton;
    MI_Zobrazit: TMenuItem;
    MI_Mrizka: TMenuItem;
    N3: TMenuItem;
    MI_CloseFile: TMenuItem;
    N4: TMenuItem;
    PM_CloseApp: TMenuItem;
    PM_ReliefOptions: TMenuItem;
    MI_SaveShowOptions: TMenuItem;
    TB_Other: TToolBar;
    TB_Text: TToolButton;
    TB_EndJC: TToolButton;
    TB_KCislo: TToolButton;
    N5: TMenuItem;
    PM_ORAdd: TMenuItem;
    MI_Data: TMenuItem;
    MI_CheckData: TMenuItem;
    PM_Roots: TMenuItem;
    ToolButton35: TToolButton;
    TB_Railway: TToolBar;
    TB_Uvazka: TToolButton;
    IL_Trat: TImageList;
    TB_UvazkaSpr: TToolButton;
    ToolButton48: TToolButton;
    TB_Derail: TToolBar;
    ToolButton49: TToolButton;
    ToolButton50: TToolButton;
    ToolButton55: TToolButton;
    TB_Separator_Horiz: TToolButton;
    PM_Sep_Hor: TMenuItem;
    TB_SoupravaPos: TToolButton;
    N6: TMenuItem;
    PM_Reload_Blocks: TMenuItem;
    PM_Show_Blk_Descriptions: TMenuItem;
    OD_Import: TOpenDialog;
    MI_Import: TMenuItem;
    ToolButton56: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    TB_Track_Undetected: TToolBar;
    ToolButton36: TToolButton;
    ToolButton37: TToolButton;
    ToolButton38: TToolButton;
    ToolButton39: TToolButton;
    ToolButton40: TToolButton;
    ToolButton41: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton20: TToolButton;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure PM_NewClick(Sender: TObject);
    procedure PM_OpenClick(Sender: TObject);
    procedure PM_SaveClick(Sender: TObject);
    procedure PM_SaveAsClick(Sender: TObject);
    procedure AE_MainMessage(var Msg: tagMSG; var Handled: Boolean);
    procedure ToolButton0Click(Sender: TObject);
    procedure PM_BitmapClick(Sender: TObject);
    procedure B_DeleteClick(Sender: TObject);
    procedure CHB_GroupClick(Sender: TObject);
    procedure B_MoveClick(Sender: TObject);
    procedure PM_ChangeRozmeryClick(Sender: TObject);
    procedure TB_Separator_VertClick(Sender: TObject);
    procedure MI_MrizkaClick(Sender: TObject);
    procedure MI_CloseFileClick(Sender: TObject);
    procedure PM_CloseAppClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure PM_ReliefOptionsClick(Sender: TObject);
    procedure MI_SaveShowOptionsClick(Sender: TObject);
    procedure PM_AboutClick(Sender: TObject);
    procedure TB_TextClick(Sender: TObject);
    procedure TB_EndJCClick(Sender: TObject);
    procedure TB_KCisloClick(Sender: TObject);
    procedure PM_ORAddClick(Sender: TObject);
    procedure MI_CheckDataClick(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure TB_Separator_HorizClick(Sender: TObject);
    procedure TB_SoupravaPosClick(Sender: TObject);
    procedure PM_Reload_BlocksClick(Sender: TObject);
    procedure PM_Show_Blk_DescriptionsClick(Sender: TObject);
    procedure MI_ImportClick(Sender: TObject);
  private
    pushedButton: TToolButton; // last pushed button

  const
    _PanelErrors: array [0 .. 4] of string = ('Vybírat lze pouze zleva doprava a zhora dolů',
      'Prázdné pole (není s čím operovat)', 'Chyba externí funkce', 'Cílové pole obsazeno',
      'Překročeny maximální limity');
    _Caption = 'hJOPeditor';
    _Config_File = 'Config.ini';

  public
    DXD_main: TDXDraw;
    Relief: TRelief;

    procedure LoadFileUpdateGUI(fname: string);
    procedure OpenFile(fname: string);
    procedure ImportFile(fname: string);
    procedure DesignOpen(fname: string);
    procedure DesignClose;

    procedure RepaintModes(cur: TMode);

    procedure AssignReliefEvents();

    procedure ReliefErrorEvent(Sender: TObject; err: string);
    procedure ReliefMoveEvent(Sender: TObject; Position: TPoint);
    procedure ReliefChangeTextEvent(Sender: TObject; var popisek: TPopisek);
    procedure BlokEditEvent(Sender: TObject; Blok: TGraphBlok);

    procedure MessageEvent(Sender: TObject; Msg: string);
    procedure FormBlkCloseEvent(Sender: TObject);
  end;

var
  F_Hlavni: TF_Hlavni;

implementation

uses fNewRelief, fChangeRelief, fReliefSettings, fPopiskek, OblastRizeni,
  fOREdit, fDataCheck, fImportLog;

{$R *.dfm}

procedure TF_Hlavni.AE_MainMessage(var Msg: tagMSG; var Handled: Boolean);
begin
  if (Msg.message = WM_KeyDown) then // pokud je stisknuta klavesa
  begin
    case Msg.wParam of // case moznosti stisknutych klaves
      VK_ESCAPE:
        begin
          Self.CHB_Group.Checked := false;
          if (Assigned(Relief)) then
            Relief.Escape(true);
          Self.SB_Main.Panels.Items[3].Text := '';

          if (Assigned(Self.pushedButton)) then
            Self.pushedButton.Down := false;
        end; // VK_Escape
    end; // case

    if ((Assigned(Self.Relief)) and (Self.Active)) then
    begin
      Self.ActiveControl := nil;
      Self.Relief.KeyPress(Msg.wParam);
    end;
  end else if (Msg.message = WM_MOUSELEAVE) then
  begin
    if (Msg.hwnd = Self.DXD_main.Handle) then
    begin
      Self.SB_Main.Panels.Items[2].Text := '---;---';
      if (Assigned(Self.Relief)) then
        Self.Relief.HideMouse();
    end;
  end;
end;

procedure TF_Hlavni.B_DeleteClick(Sender: TObject);
begin
  Relief.Escape(false);

  if (Assigned(Self.pushedButton)) then
    Self.pushedButton.Down := false;

  case (Relief.Mode) of
    dmBitmap, dmSepVert, dmSepHor:
      Relief.DeleteBitmapSymbol;
  end; // case
end;

// move
procedure TF_Hlavni.B_MoveClick(Sender: TObject);
begin
  Relief.Escape(false);

  if (Assigned(Self.pushedButton)) then
    Self.pushedButton.Down := false;

  case (Relief.Mode) of
    dmBitmap, dmSepVert, dmSepHor:
      Relief.MoveBitmapSymbol;
  end; // case
end;

procedure TF_Hlavni.CHB_GroupClick(Sender: TObject);
begin
  Relief.Skupina := Self.CHB_Group.Checked;
end;

procedure TF_Hlavni.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  try
    if (Assigned(ReliefOptions)) then
      ReliefOptions.SaveData(IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName)) + _Config_File);

    FreeAndNil(Self.Relief);

    if (Assigned(Self.DXD_main)) then
      FreeAndNil(Self.DXD_main);
  except

  end;
end;

procedure TF_Hlavni.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if (Assigned(Relief)) then
  begin
    if (Application.MessageBox('Uzavřením projektu bez jeho uložení ztratíte projektová data, přesto pokračovat?',
      'Uzavření projektu', MB_YESNO OR MB_ICONQUESTION) = mrNo) then
      CanClose := false;
  end; // if (Assigned(Relief))
end;

procedure TF_Hlavni.FormCreate(Sender: TObject);
begin
  Self.Relief := nil;

  Self.DXD_main := TDXDraw.Create(Self);
  Self.DXD_main.Parent := Self;
  Self.DXD_main.Left := 8;
  Self.DXD_main.Top := 80;
  Self.DXD_main.Visible := false;
  Self.DXD_main.Initialize(); // tohleto tady musi byt, jinak nefunguje nacitani souboru jako argumentu !!

  ReliefOptions.LoadData(IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName)) + 'Config.ini');
  Self.MI_Mrizka.Checked := ReliefOptions.Mrizka;

  Self.DXD_main.Cursor := crNone;
  Self.Caption := _Caption + '     v' + GetVersion(Application.ExeName);

  Self.pushedButton := nil;
end;

procedure TF_Hlavni.FormKeyPress(Sender: TObject; var Key: Char);
begin
  case (Key) of
    'g':
      begin
        // group
        if (not Assigned(Self.Relief)) then
          Exit;
        if (Self.Relief.Mode <> dmBitmap) then
          Exit;

        Self.CHB_Group.Checked := not Self.CHB_Group.Checked;
        Self.CHB_GroupClick(Self);
      end;

    'd':
      begin
        // delete
        if (not Assigned(Self.Relief)) then
          Exit;
        if (Self.Relief.Mode = dmBitmap) then
          Self.B_DeleteClick(Self);
      end;

    'm':
      begin
        // move
        if (not Assigned(Self.Relief)) then
          Exit;
        if (Self.Relief.Mode = dmBitmap) then
          Self.B_MoveClick(Self);
      end;

    't':
      begin
        if (not Assigned(Self.Relief)) then
          Exit;
        if (Self.Relief.Mode <> dmBitmap) then
          Exit;
        Self.TB_TextClick(Self.TB_Text);
      end;

  end;
end;

// novy soubor
procedure TF_Hlavni.PM_NewClick(Sender: TObject);
begin
  if (Assigned(Relief)) then
  begin
    if (Relief.fileState <> fsClosed) then
      if (Application.MessageBox('Otevřením nového projektu ztratíte všechna neuložená data, pokračovat?',
        'Pokračovat?', MB_YESNO OR MB_ICONQUESTION) <> mrYes) then
        Exit();
  end;

  if (Assigned(Relief)) then
    FreeAndNil(Relief);

  F_NewRelief.OpenForm;
end;

procedure TF_Hlavni.LoadFileUpdateGUI(fname: string);
begin
  Self.RepaintModes(Relief.Mode);

  case (Self.Relief.Mode) of
    dmBitmap:
      begin
        Self.PM_BitmapClick(Self.PM_Bitmap);
        Self.PM_ORAdd.Enabled := true;
      end;

    dmBloky:
      begin
        Self.PM_ORAdd.Enabled := false;
        Self.PM_BitmapClick(Self.PM_Bloky);
      end;
  end;

  ReliefOptions.UseData(F_Hlavni.Relief);
  Self.DesignOpen(ExtractFileName(fname));
end;

procedure TF_Hlavni.OpenFile(fname: string);
begin
  Relief := TRelief.Create(Self.DXD_main, Self);
  Self.AssignReliefEvents();
  Screen.Cursor := crHourGlass;

  try
    Relief.Open(fname);
  except
    on E: Exception do
    begin
      if (Assigned(Relief)) then
        FreeAndNil(Relief);
      Self.DesignClose();
      Screen.Cursor := crDefault;
      Application.MessageBox(PChar('Otevření souboru skončilo s chybou:' + #13#10 + E.message), 'Chyba',
        MB_OK OR MB_ICONERROR);
      Exit();
    end;
  end;

  Screen.Cursor := crDefault;
  Self.LoadFileUpdateGUI(fname);
end;

procedure TF_Hlavni.ImportFile(fname: string);
var log: string;
begin
  Relief := TRelief.Create(Self.DXD_main, Self);
  Self.AssignReliefEvents();
  Screen.Cursor := crHourGlass;
  try
    log := Relief.Import(fname);
  except
    on E: Exception do
    begin
      if (Assigned(Relief)) then
        FreeAndNil(Relief);
      Self.DesignClose();
      Screen.Cursor := crDefault;
      Application.MessageBox(PChar('Import suboru skončil s chybou:' + #13#10 + E.message), 'Chyba',
        MB_OK OR MB_ICONERROR);
      Exit();
    end;
  end;

  Screen.Cursor := crDefault;
  Self.LoadFileUpdateGUI('Nový projekt');
  F_ImportLog.Open(log);
end;

procedure TF_Hlavni.PM_OpenClick(Sender: TObject);
begin
  if (Assigned(Relief)) then
    if (Application.MessageBox('Otevřením nového projektu ztratíte všechna neuložená data, pokračovat?', 'Pokračovat?',
      MB_YESNO OR MB_ICONQUESTION) <> mrYes) then
      Exit();

  if (Self.OD_Open.Execute(Self.Handle)) then
  begin
    if (Assigned(Relief)) then
    begin
      if (Relief.fileState <> fsClosed) then
        FreeAndNil(Relief);
    end;

    Self.OpenFile(Self.OD_Open.FileName);
  end;
end;

procedure TF_Hlavni.PM_ORAddClick(Sender: TObject);
var tmpOR: TOR;
begin
  tmpOR.Osvetleni.Cnt := 0;
  tmpOR.Poss.DK := Point(0, 0);
  tmpOR.Poss.Queue := Point(0, 0);
  tmpOR.Poss.Time := Point(0, 0);
  tmpOR.Rights.ModCasStart := true;
  tmpOR.Rights.ModCasStop := true;
  tmpOR.Rights.ModCasSet := true;

  F_OREdit.OpenForm(tmpOR);
end;

procedure TF_Hlavni.PM_ReliefOptionsClick(Sender: TObject);
begin
  F_ReliefOptions.OpenForm();
end;

procedure TF_Hlavni.PM_Reload_BlocksClick(Sender: TObject);
begin
  try
    F_BlockEdit.Bloky.LoadData(ReliefOptions.BlockFile);
    if (F_BlockEdit.Showing) then
      F_BlockEdit.FormShow(F_BlockEdit);
    Application.MessageBox(PChar('Soubor ' + ReliefOptions.BlockFile + ' úspěšně načten.'), 'Info',
      MB_OK OR MB_ICONINFORMATION);
  except
    on E: Exception do
      Application.MessageBox(PChar('Chyba při načítáni souboru s bloky technologie:' + #13#10 + E.message), 'Chyba',
        MB_OK OR MB_ICONWARNING);
  end;
end;

// ukladani souboru jako
procedure TF_Hlavni.PM_SaveAsClick(Sender: TObject);
begin
  if ((Relief.Mode = dmBitmap) or (Relief.Mode = dmSepVert) or (Relief.Mode = dmSepHor)) then
  begin
    Self.SD_Save.Filter := 'Bitmapové soubory panelu (*.bpnl)|*.bpnl';

    if (not Self.SD_Save.Execute(Self.Handle)) then
      Exit;
    if (RightStr(Self.SD_Save.FileName, 5) <> '.bpnl') then
      Relief.Save(Self.SD_Save.FileName + '.bpnl')
    else
      Relief.Save(Self.SD_Save.FileName);
  end; // bitmapovy mod

  if (Relief.Mode = dmBloky) then
  begin
    Self.SD_Save.Filter := 'Objektové soubory panelu (*.opnl)|*.opnl';

    if (not Self.SD_Save.Execute(Self.Handle)) then
      Exit;
    if (RightStr(Self.SD_Save.FileName, 5) <> '.opnl') then
      Relief.Save(Self.SD_Save.FileName + '.opnl')
    else
      Relief.Save(Self.SD_Save.FileName);
  end; // objektovy mod

  Self.SB_Main.Panels.Items[1].Text := 'Soubor uložen';
  Self.Caption := ExtractFileName(Self.SD_Save.FileName) + ' - ' + _Caption + '     v' +
    GetVersion(Application.ExeName);
end;

// ukladani souboru
procedure TF_Hlavni.PM_SaveClick(Sender: TObject);
begin
  try
    if ((Relief.Mode = dmBitmap) or (Relief.Mode = dmSepVert) or (Relief.Mode = dmSepHor)) then
    begin
      Self.SD_Save.Filter := 'Bitmapové soubory panelu (*.bpnl)|*.bpnl';

      if (Relief.fileState = fsUnsaved) then
      begin
        if (not Self.SD_Save.Execute(Self.Handle)) then
          Exit;
        if (RightStr(Self.SD_Save.FileName, 5) <> '.bpnl') then
          Relief.Save(Self.SD_Save.FileName + '.bpnl')
        else
          Relief.Save(Self.SD_Save.FileName);
      end else begin
        if (Relief.fileState = fsSaved) then
          Relief.Save(Relief.filePath);
      end; // else .Stav = 1
    end; // bitmapovy mod

    if ((Relief.Mode = dmBloky) or (Relief.Mode = dmRoots)) then
    begin
      Self.SD_Save.Filter := 'Objektové soubory panelu (*.opnl)|*.opnl';

      if (Relief.fileState = fsUnsaved) then
      begin
        if (not Self.SD_Save.Execute(Self.Handle)) then
          Exit;
        if (RightStr(Self.SD_Save.FileName, 5) <> '.opnl') then
          Relief.Save(Self.SD_Save.FileName + '.opnl')
        else
          Relief.Save(Self.SD_Save.FileName);
      end else begin
        if (Relief.fileState = fsSaved) then
          Relief.Save(Relief.filePath);
      end; // else .Stav = 1
    end; // objektovy mod
  except
    on E: Exception do
    begin
      Application.MessageBox(PChar('Uložení suboru skončilo s chybou:' + #13#10 + E.message), 'Chyba',
        MB_OK OR MB_ICONERROR);
      Exit();
    end;
  end;

  Self.SB_Main.Panels.Items[1].Text := 'Soubor uložen';
  Self.Caption := ExtractFileName(Relief.filePath) + ' - ' + _Caption + '     v' + GetVersion(Application.ExeName);
end;

procedure TF_Hlavni.PM_AboutClick(Sender: TObject);
begin
  Application.MessageBox(PChar('hJOPeditor' + #13#10 + 'v' + GetVersion(Application.ExeName) + #13#10 +
    'Vytvořil Jan Horáček 2011–2023'), 'Info', MB_OK OR MB_ICONINFORMATION);
end;

procedure TF_Hlavni.PM_BitmapClick(Sender: TObject);
var Return: Integer;
begin
  (Sender as TMenuItem).Checked := true;

  if (Assigned(Self.pushedButton)) then
    Self.pushedButton.Down := false;

  F_BlockEdit.Close();

  Self.TB_Turnout.Visible := false;
  Self.TB_Track_Undetected.Visible := false;
  Self.TB_Track_Detected.Visible := false;
  Self.TB_Signal.Visible := false;
  Self.TB_BitmapTools.Visible := false;
  Self.TB_Separator.Visible := false;
  Self.TB_Bitmap_Other.Visible := false;
  Self.TB_Railway.Visible := false;
  Self.TB_Derail.Visible := false;
  Self.CHB_Group.Enabled := false;
  Self.TB_Other.Visible := false;
  Self.MI_Relief.Visible := false;
  Self.MI_Data.Visible := false;

  case (Sender as TMenuItem).Tag of
    0:
      begin
        if (Relief.Mode = dmBloky) then
        begin
          Application.MessageBox('Tato funkce zatím není dostupná', 'Nelze převést', MB_OK OR MB_ICONERROR);
          Self.PM_Bloky.Checked := true;
          Exit;
        end;

        try
          if (Relief.Mode <> dmBitmap) then
            Relief.SwitchMode(dmBitmap);
        except
          on E: Exception do
          begin
            Application.MessageBox(PChar(E.message), 'Chyba', MB_OK OR MB_ICONWARNING);
            Exit();
          end;
        end;

        Self.TB_Turnout.Visible := true;
        Self.TB_Track_Detected.Visible := true;
        Self.TB_Track_Undetected.Visible := true;
        Self.TB_Signal.Visible := true;
        Self.TB_BitmapTools.Visible := true;
        Self.TB_Bitmap_Other.Visible := true;
        Self.TB_Railway.Visible := true;
        Self.CHB_Group.Enabled := true;
        Self.TB_Other.Visible := true;
        Self.MI_Relief.Visible := true;
        Self.TB_Derail.Visible := true;
      end; // case 0

    1, 2:
      begin
        if (Relief.Mode = dmBloky) then
        begin
          Application.MessageBox('Tato funkce zatím není dostupná', 'Nelze převést', MB_OK OR MB_ICONERROR);
          Self.PM_Bloky.Checked := true;
          Exit;
        end;

        try
          if ((TMenuItem(Sender).Tag = 1) and (Relief.Mode <> dmSepVert)) then
            Relief.SwitchMode(dmSepVert)
          else if ((TMenuItem(Sender).Tag = 2) and (Relief.Mode <> dmSepHor)) then
            Relief.SwitchMode(dmSepHor);
        except
          on E: Exception do
          begin
            Application.MessageBox(PChar(E.message), 'Chyba', MB_OK OR MB_ICONWARNING);
            Exit();
          end;
        end;

        Self.TB_Separator.Visible := true;
        Self.TB_BitmapTools.Visible := true;
        Self.MI_Relief.Visible := true;
      end;

    3:
      begin
        Return := 0;

        if ((Relief.Mode = dmBitmap) or (Relief.Mode = dmSepVert) or (Relief.Mode = dmSepHor)) then
        begin
          if (Application.MessageBox
            (PChar('Po přepnutí na režim bloky není možné se vrátit zpět do bitmapového režimu. Zkontrolujte si, že máte uložený soubor s daty panelu.'
            + #13#10 + 'Pokračovat?'), 'Změna režimu projektu', MB_YESNO OR MB_ICONQUESTION OR MB_DEFBUTTON2) <> mrYes)
          then
          begin
            Self.PM_BitmapClick(Self.PM_Bitmap);
            Exit();
          end;
        end;

        try
          if (Relief.Mode <> dmBloky) then
            Relief.SwitchMode(dmBloky);
        except
          on E: Exception do
          begin
            Application.MessageBox(PChar(E.message), 'Chyba', MB_OK OR MB_ICONWARNING);
            Exit();
          end;
        end;

        if (Return <> 0) then
        begin
          Application.MessageBox(PChar('Při změně módu se vyskytla chyba - chyba: ' + IntToStr(Return)), 'Chyba',
            MB_OK OR MB_ICONWARNING);
          Exit;
        end;

        if (not F_BlockEdit.Bloky.triedLoad) then
        begin
          try
            F_BlockEdit.Bloky.LoadData(ReliefOptions.BlockFile);
          except
            on E: Exception do
              Application.MessageBox(PChar('Chyba při načítáni souboru s bloky technologie:' + #13#10 + E.message),
                'Chyba', MB_OK OR MB_ICONWARNING);
          end;
        end;

        Self.MI_Data.Visible := true;
      end;

    4:
      begin
        try
          if (Relief.Mode <> dmRoots) then
            Relief.SwitchMode(dmRoots);
        except
          on E: Exception do
          begin
            Application.MessageBox(PChar(E.message), 'Chyba', MB_OK OR MB_ICONWARNING);
            Exit();
          end;
        end;

        F_BlockEdit.Close();
        Self.MI_Data.Visible := true;
      end;
  end; // case (Sender as TMenuItem).Tag

  Self.RepaintModes(Relief.Mode);
end;

procedure TF_Hlavni.PM_ChangeRozmeryClick(Sender: TObject);
begin
  F_ReliefProperties.OpenForm;
end;

procedure TF_Hlavni.MI_CheckDataClick(Sender: TObject);
var error_cnt: Byte;
  LI: TListItem;
begin
  if ((Assigned(Self.Relief)) and ((Self.Relief.Mode = dmBloky) or (Self.Relief.Mode = dmRoots))) then
  begin
    F_DataCheck.OpenForm(Self.Relief.CheckValid(error_cnt));
    LI := F_DataCheck.LV_Errors.Items.Add;
    LI.Caption := IntToStr(F_DataCheck.LV_Errors.Items.Count);
    if (error_cnt = 0) then
      LI.SubItems.Add('OK: Zkouška validity proběhla úspěšně')
    else
      LI.SubItems.Add('ERR: Zkouška validity skončila s ' + IntToStr(error_cnt) + ' chybami!');
  end;
end;

procedure TF_Hlavni.PM_CloseAppClick(Sender: TObject);
begin
  Self.Close;
end;

procedure TF_Hlavni.MI_MrizkaClick(Sender: TObject);
begin
  (Sender as TMenuItem).Checked := not(Sender as TMenuItem).Checked;

  ReliefOptions.Mrizka := (Sender as TMenuItem).Checked;
  ReliefOptions.SaveData(IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName)) + 'Config.ini');

  if (Assigned(Relief)) then
    ReliefOptions.UseData(F_Hlavni.Relief);
end;

procedure TF_Hlavni.MI_SaveShowOptionsClick(Sender: TObject);
begin
  ReliefOptions.SaveData(IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName)) + _Config_File);
end;

procedure TF_Hlavni.TB_Separator_HorizClick(Sender: TObject);
begin
  if (Relief.Mode <> dmSepHor) then
    Self.PM_BitmapClick(Self.PM_Sep_Hor);

  Relief.Escape(false);
  try
    Relief.AddSeparatorHor();
  except
    on E: Exception do
      Application.MessageBox(PChar('Chyba při přidávání objektu:' + #13#10 + E.message), 'Chyba',
        MB_OK OR MB_ICONWARNING);
  end;
end;

procedure TF_Hlavni.TB_Separator_VertClick(Sender: TObject);
begin
  if (Relief.Mode <> dmSepVert) then
    Self.PM_BitmapClick(Self.PM_Sep_Vert);

  Relief.Escape(false);
  try
    Relief.AddSeparatorVert();
  except
    on E: Exception do
      Application.MessageBox(PChar('Chyba při přidávání objektu:' + #13#10 + E.message), 'Chyba',
        MB_OK OR MB_ICONWARNING);
  end;
end;

procedure TF_Hlavni.ToolButton0Click(Sender: TObject);
begin
  Relief.Escape(false);
  try
    Relief.AddSymbol((Sender as TToolButton).Tag);
  except
    on E: Exception do
      Application.MessageBox(PChar('Chyba při přidávání symbolu:' + #13#10 + E.message), 'Chyba',
        MB_OK OR MB_ICONWARNING);
  end;

  if (Assigned(Self.pushedButton)) then
    Self.pushedButton.Down := false;

  (Sender as TToolButton).Down := true;
  Self.pushedButton := (Sender as TToolButton);
end;

procedure TF_Hlavni.TB_TextClick(Sender: TObject);
begin
  Relief.Escape(false);
  F_Popisek.NewPopisek();

  if (F_Popisek.PopisekColor <> -1) then
    Relief.AddText(F_Popisek.PopisekText, SymbolColor(F_Popisek.PopisekColor), F_Popisek.PopisekBlok);
end;

procedure TF_Hlavni.TB_EndJCClick(Sender: TObject);
begin
  Relief.Escape(false);
  try
    Relief.AddJCClick();
  except
    on E: Exception do
      Application.MessageBox(PChar('Chyba při přidávání objektu:' + #13#10 + E.message), 'Chyba',
        MB_OK OR MB_ICONWARNING);
  end;
end;

procedure TF_Hlavni.TB_KCisloClick(Sender: TObject);
begin
  Relief.Escape(false);
  try
    Relief.AddKPopisek();
  except
    on E: Exception do
      Application.MessageBox(PChar('Chyba při přidávání objektu:' + #13#10 + E.message), 'Chyba',
        MB_OK OR MB_ICONWARNING);
  end;
end;

procedure TF_Hlavni.TB_SoupravaPosClick(Sender: TObject);
begin
  Relief.Escape(false);

  try
    Relief.AddSouprava();
  except
    on E: Exception do
      Application.MessageBox(PChar('Chyba při přidávání objektu:' + #13#10 + E.message), 'Chyba',
        MB_OK OR MB_ICONWARNING);
  end;
end;

procedure TF_Hlavni.PM_Show_Blk_DescriptionsClick(Sender: TObject);
begin
  (Sender as TMenuItem).Checked := not(Sender as TMenuItem).Checked;

  if (Assigned(Relief)) then
    Relief.ShowBlokPopisky := (Sender as TMenuItem).Checked;
end;

procedure TF_Hlavni.MI_CloseFileClick(Sender: TObject);
begin
  if (Application.MessageBox('Uzavřením projektu ztratíte všechna neuložená data, pokračovat?', 'Uzavření projektu',
    MB_YESNO OR MB_ICONQUESTION) <> mrYes) then
    Exit();

  if (Assigned(ReliefOptions)) then
    ReliefOptions.SaveData(IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName)) + _Config_File);

  Self.DesignClose();
  FreeAndNil(Self.Relief);
end;

procedure TF_Hlavni.MI_ImportClick(Sender: TObject);
begin
  if (Assigned(Relief)) then
    if (Application.MessageBox('Importováním projektu ztratíte všechna neuložená data, pokračovat?', 'Pokračovat?',
      MB_YESNO OR MB_ICONQUESTION) <> mrYes) then
      Exit();

  if (Self.OD_Import.Execute(Self.Handle)) then
  begin
    if (Assigned(Relief)) then
      if (Relief.fileState <> fsClosed) then
        FreeAndNil(Relief);

    Self.ImportFile(Self.OD_Import.FileName);
  end;
end;

procedure TF_Hlavni.DesignOpen(fname: string);
begin
  Self.PM_Save.Enabled := true;
  Self.PM_SaveAs.Enabled := true;
  Self.MI_Draw.Visible := true;
  Self.MI_CloseFile.Enabled := true;
  Self.SB_Main.Panels.Items[0].Text := 'Soubor otevřen';
  Self.SB_Main.Panels.Items[1].Text := 'Soubor není uložen';

  Self.Caption := fname + ' – ' + _Caption + '     v' + GetVersion(Application.ExeName);

  Self.Constraints.MinWidth := Max(Self.DXD_main.Width + 2 * Self.DXD_main.Left + 20,
    Self.TB_BitmapTools.Left + Self.TB_BitmapTools.Width + 30);
  Self.Constraints.MinHeight := Self.DXD_main.Height + Self.DXD_main.Top + Self.SB_Main.Height + 70;
end;

procedure TF_Hlavni.DesignClose();
begin
  Self.PM_Save.Enabled := false;
  Self.PM_SaveAs.Enabled := false;
  Self.MI_Draw.Visible := false;
  Self.MI_Relief.Visible := false;
  Self.MI_CloseFile.Enabled := false;
  Self.MI_Data.Visible := false;

  Self.TB_Turnout.Visible := false;
  Self.TB_Track_Detected.Visible := false;
  Self.TB_Track_Undetected.Visible := false;
  Self.TB_Signal.Visible := false;
  Self.TB_BitmapTools.Visible := false;
  Self.TB_Separator.Visible := false;
  Self.TB_Bitmap_Other.Visible := false;
  Self.TB_Other.Visible := false;
  Self.TB_Railway.Visible := false;
  Self.TB_Derail.Visible := false;

  if (Assigned(Self.pushedButton)) then
    Self.pushedButton.Down := false;

  Self.SB_Main.Panels.Items[2].Text := '---;---';
  Self.SB_Main.Panels.Items[3].Text := '';

  Self.SB_Main.Panels.Items[0].Text := 'Soubor uzavřen';
  Self.SB_Main.Panels.Items[1].Text := '';
  Self.Caption := _Caption + '     v' + GetVersion(Application.ExeName);

  F_BlockEdit.Close();
end;

procedure TF_Hlavni.ReliefErrorEvent(Sender: TObject; err: string);
begin
  Self.SB_Main.Panels.Items[3].Text := 'Chyba: ' + err;
end;

procedure TF_Hlavni.ReliefMoveEvent(Sender: TObject; Position: TPoint);
begin
  Self.SB_Main.Panels.Items[2].Text := Format('%.3d', [Position.X]) + ' ; ' + Format('%.3d', [Position.Y]);
  Self.SB_Main.Panels.Items[3].Text := '';
end;

procedure TF_Hlavni.ReliefChangeTextEvent(Sender: TObject; var popisek: TPopisek);
begin
  F_Popisek.OpenPopisek(Text, popisek);

  if (F_Popisek.PopisekColor = -1) then
    Exit;
  popisek.Text := F_Popisek.PopisekText;
  popisek.Color := SymbolColor(F_Popisek.PopisekColor);
  popisek.BlokPopisek := F_Popisek.PopisekBlok;
end;

procedure TF_Hlavni.BlokEditEvent(Sender: TObject; Blok: TGraphBlok);
begin
  F_BlockEdit.OpenForm(Blok);
end;

procedure TF_Hlavni.MessageEvent(Sender: TObject; Msg: string);
begin
  Self.SB_Main.Panels.Items[3].Text := Msg;
end;

procedure TF_Hlavni.RepaintModes(cur: TMode);
begin
  case (cur) of
    dmBitmap, dmSepVert, dmSepHor:
      begin
        Self.PM_Bitmap.Enabled := true;
        Self.PM_Sep_Vert.Enabled := true;
        Self.PM_Sep_Hor.Enabled := true;
        Self.PM_Bloky.Enabled := true;
        Self.PM_Roots.Enabled := false;
      end;

    dmBloky, dmRoots:
      begin
        Self.PM_Bitmap.Enabled := false;
        Self.PM_Sep_Vert.Enabled := false;
        Self.PM_Sep_Hor.Enabled := false;
        Self.PM_Bloky.Enabled := true;
        Self.PM_Roots.Enabled := true;
      end;

  end; // case
end;

procedure TF_Hlavni.AssignReliefEvents();
begin
  Relief.OnError := Self.ReliefErrorEvent;
  Relief.OnMove := Self.ReliefMoveEvent;
  Relief.OnChangeText := Self.ReliefChangeTextEvent;
  Relief.OnBlokEdit := Self.BlokEditEvent;

  Relief.OnMsg := Self.MessageEvent;
  Relief.FormBlkClose := Self.FormBlkCloseEvent;
end;

procedure TF_Hlavni.FormBlkCloseEvent(Sender: TObject);
begin
  F_BlockEdit.Close();
end;

end.// unit
