unit fMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, ReliefObjects, Buttons, ToolWin, ComCtrls, ExtCtrls, Panel,
  ImgList, DXDraws, StdCtrls, AppEvnts, StrUtils, ReliefBitmap, Global,
  ReliefSettings, fBlockEdit, DXSprite, DIB;

const
  _open_file_errors: array [1..5] of string =
    ('Zadaný soubor neexistuje',
     'Zadaný soubor má pøíponu, kterou program nepodporuje',
     'Nepodaøilo se inicializavat panel',
     'Nepodaøilo se naèíst symboly',
     'Nepodaøilo se naèíst oblasti øízení');

  _close_file_errors: array [1..1] of string =
    ('Nelze pøistoupit k výstupnímu souboru');

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
    TB_Vyhybka: TToolBar;
    ToolButton0: TToolButton;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton21: TToolButton;
    ToolButton20: TToolButton;
    ToolButton19: TToolButton;
    ToolButton18: TToolButton;
    ToolButton17: TToolButton;
    ToolButton12: TToolButton;
    ToolButton13: TToolButton;
    ToolButton14: TToolButton;
    ToolButton15: TToolButton;
    ToolButton16: TToolButton;
    ToolButton22: TToolButton;
    ToolButton23: TToolButton;
    ToolButton24: TToolButton;
    ToolButton25: TToolButton;
    ToolButton28: TToolButton;
    ToolButton29: TToolButton;
    TB_Usek: TToolBar;
    TB_SCom: TToolBar;
    AE_Main: TApplicationEvents;
    MI_Draw: TMenuItem;
    PM_Bitmap: TMenuItem;
    PM_Oddelovac: TMenuItem;
    PM_Bloky: TMenuItem;
    P_Menu: TPanel;
    TB_BitmapTools: TToolBar;
    B_Move: TButton;
    B_Delete: TButton;
    CHB_Group: TCheckBox;
    ToolButton51: TToolButton;
    SB_Main: TStatusBar;
    TB_Oddelovac: TToolBar;
    TBt_Separator: TToolButton;
    MI_Relief: TMenuItem;
    PM_ChangeRozmery: TMenuItem;
    TB_BitmapOstatni: TToolBar;
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
    ToolButton40: TToolButton;
    ToolButton41: TToolButton;
    ToolButton42: TToolButton;
    N5: TMenuItem;
    PM_ORAdd: TMenuItem;
    MI_Data: TMenuItem;
    MI_CheckData: TMenuItem;
    PM_Roots: TMenuItem;
    ToolButton35: TToolButton;
    TB_Trat: TToolBar;
    TB_Uvazka: TToolButton;
    IL_Trat: TImageList;
    TB_UvazkaSpr: TToolButton;
    ToolButton48: TToolButton;
    TB_Vykolejka: TToolBar;
    ToolButton49: TToolButton;
    ToolButton50: TToolButton;
    ToolButton55: TToolButton;
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
    procedure TBt_SeparatorClick(Sender: TObject);
    procedure MI_MrizkaClick(Sender: TObject);
    procedure MI_CloseFileClick(Sender: TObject);
    procedure PM_CloseAppClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure PM_ReliefOptionsClick(Sender: TObject);
    procedure MI_SaveShowOptionsClick(Sender: TObject);
    procedure PM_AboutClick(Sender: TObject);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure ToolButton40Click(Sender: TObject);
    procedure ToolButton41Click(Sender: TObject);
    procedure ToolButton42Click(Sender: TObject);
    procedure PM_ORAddClick(Sender: TObject);
    procedure MI_CheckDataClick(Sender: TObject);
    procedure MI_ExportServerClick(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
  private
    pushedButton:TToolButton;                   // last pushed button

   const
    _PanelErrors: array [0..4] of string = ('Vybírat lze pouze zleva doprava a zhora dolù','Prázdné pole (není s èím operovat)',
                                            'Chyba externí funkce','Cílové pole obsazeno','Pøekroèeny maximální limity');
    _Caption = 'hJOPeditor';
    _Config_File = 'Config.ini';

    procedure ActivateSymbol(index:Integer);

  public
   DXD_main:TDXDraw;
   Relief:TRelief;

    procedure CreateClasses;
    procedure CreateReliefClasses;
    procedure DestroyClasses;
    procedure DestroyReliefClasses;

    function OpenFile(fname:string):Byte;
    procedure DesignOpen(FName:string);
    procedure DesignClose;

    procedure RepaintModes(cur:TMode);

    procedure AssignReliefEvents();

    procedure ReliefErrorEvent(Sender:TObject; ErrorID:Integer);
    procedure ReliefMoveEvent(Sender:TObject; Position:TPoint);
    procedure ReliefChangeTextEvent(Sender:TObject; var Text:string;var Color:Integer);
    procedure BlokEditEvent(Sender:TObject; Blok:TGraphBlok);

    procedure MessageEvent(Sender:TObject; msg:string);
    procedure FormBlkCloseEvent(Sender:TObject);
  end;


var
  F_Hlavni: TF_Hlavni;


implementation

uses fNewRelief, fChangeRelief, fReliefSettings, fPopiskek, OblastRizeni,
  fOREdit, fDataCheck;

{$R *.dfm}

procedure TF_Hlavni.AE_MainMessage(var Msg: tagMSG; var Handled: Boolean);
begin
 if (msg.message = WM_KeyDown) then                          //pokud je stisknuta klavesa
  begin
   case msg.wParam of                                     //case moznosti stisknutych klaves
     VK_ESCAPE:begin
                Self.CHB_Group.Checked := false;
                if (Assigned(Relief)) then Relief.Escape(true);
                Self.SB_Main.Panels.Items[3].Text := '';

                if (Assigned(Self.pushedButton)) then
                 Self.pushedButton.Down := false;
               end;//VK_Escape
    end;//case

   if ((Assigned(Self.Relief)) and (Self.Active)) then
    begin
     Self.ActiveControl := nil;
     Self.Relief.KeyPress(msg.wParam);
    end;
  end;//if (msg.message = WM_KeyDown)
end;//procedure

procedure TF_Hlavni.B_DeleteClick(Sender: TObject);
begin
 Relief.Escape(false);

 if (Assigned(Self.pushedButton)) then
  Self.pushedButton.Down := false;

 case (Relief.Mode) of
  dmBitmap,dmOddelovace:Relief.DeleteBitmapSymbol;
 end;//case
end;//procedure

//move
procedure TF_Hlavni.B_MoveClick(Sender: TObject);
begin
 Relief.Escape(false);

 if (Assigned(Self.pushedButton)) then
  Self.pushedButton.Down := false;

 case (Relief.Mode) of
  dmBitmap,dmOddelovace:Relief.MoveBitmapSymbol;
 end;//case
end;//procedure

procedure TF_Hlavni.CHB_GroupClick(Sender: TObject);
begin
 Relief.Skupina := Self.CHB_Group.Checked;
end;//procedure

procedure TF_Hlavni.CreateClasses;
begin
 Self.DXD_main := TDXDraw.Create(Self);
// Self.DXD_main.AutoInitialize := true;
 Self.DXD_main.Parent  := Self;
 Self.DXD_main.Left    := 8;
 Self.DXD_main.Top     := 80;
 Self.DXD_main.Visible := false;
 Self.DXD_main.Initialize;               // tohleto tady musi byt, jinak nefunguje nacitani souboru jako argumentu !!

 Self.CreateReliefClasses;
end;

procedure TF_Hlavni.CreateReliefClasses;
begin
 ReliefOptions  := TReliefOptions.Create;
end;

procedure TF_Hlavni.DestroyClasses;
begin
 if (Assigned(ReliefOptions)) then
  begin
   ReliefOptions.Destroy;
   ReliefOptions := nil;
  end;

 Self.DestroyReliefClasses;

 if (Assigned(Self.DXD_main)) then
   FreeAndNil(Self.DXD_main);
end;

procedure TF_Hlavni.DestroyReliefClasses;
begin
 if (Assigned(Relief)) then
  begin
   Relief.Destroy;
   Relief := nil;
  end;
end;

procedure TF_Hlavni.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 if (Assigned(ReliefOptions)) then ReliefOptions.SaveData(IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName))+_Config_File);

 DestroyClasses;
end;

procedure TF_Hlavni.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
 if (Assigned(Relief)) then
  begin
   if (Application.MessageBox('Uzavøením projektu bez jeho uložení ztratíte projektová data, pøesto pokraèovat?','Uzavøení projektu',MB_YESNO OR MB_ICONQUESTION) = mrNo) then CanClose := false;
  end;//if (Assigned(Relief))
end;//procedure

procedure TF_Hlavni.FormCreate(Sender: TObject);
begin
 CreateClasses;

 ReliefOptions.LoadData(IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName))+'Config.ini');

 Self.DXD_Main.Cursor := crNone;
 Self.Caption := _Caption + '     v' + GetVersion(Application.ExeName);

 Self.pushedButton := nil;
end;

procedure TF_Hlavni.FormKeyPress(Sender: TObject; var Key: Char);
var index:Integer;
begin
 case (Key) of
  'g':begin
    // group
    if (not Assigned(Self.Relief)) then Exit;
    if (Self.Relief.Mode <> dmBitmap) then Exit;

    Self.CHB_Group.Checked := not Self.CHB_Group.Checked;
    Self.CHB_GroupClick(Self);
  end;

  'd':begin
    // delete
    if (not Assigned(Self.Relief)) then Exit;
    if ((Self.Relief.Mode <> dmBitmap) and (Self.Relief.Mode <> dmOddelovace)) then Exit;

    Self.B_DeleteClick(Self);
  end;

  'm':begin
    // move
    if (not Assigned(Self.Relief)) then Exit;
    if ((Self.Relief.Mode <> dmBitmap) and (Self.Relief.Mode <> dmOddelovace)) then Exit;

    Self.B_MoveClick(Self);
  end;

  'r':begin
    // rotate symbol
    if (not Assigned(Self.pushedButton)) then Exit;
    index := Self.pushedButton.Tag;   // index of the symbol

    case (index) of
      0..3:begin
        // vyhybky
        index := index + 1;
        if (index > 3) then index := 0;
        Self.ActivateSymbol(index);
      end;

      12..17:begin
        // detekovany usek
        index := index + 1;
        if (index > 17) then index := 12;
        Self.ActivateSymbol(index);
      end;

      18..23:begin
        // nedetekovany usek
        index := index + 1;
        if (index > 23) then index := 18;
        Self.ActivateSymbol(index);
      end;

      // jizdni navestidla
      24:Self.ActivateSymbol(25);
      25:Self.ActivateSymbol(24);

      // manipulacni navestidla
      28:Self.ActivateSymbol(29);
      29:Self.ActivateSymbol(28);

      // zarazedla
      30:Self.ActivateSymbol(31);
      31:Self.ActivateSymbol(30);

      32..34:begin
        // peron
        index := index + 1;
        if (index > 34) then index := 32;
        Self.ActivateSymbol(index);
      end;

      // vykolejka
      49: Self.ActivateSymbol(50);
      50: Self.ActivateSymbol(49);

    end;
  end;

  's':begin
    // change symbol
    if (not Assigned(Self.Relief)) then Exit;
    if (Self.Relief.Mode <> dmBitmap) then Exit;

    if (Assigned(Self.pushedButton)) then
      index := Self.pushedButton.Tag
    else index := -1;

    case (index) of
     0..3  : Self.ActivateSymbol(12);
     12..17: Self.ActivateSymbol(18);
     18..23: Self.ActivateSymbol(24);
     24..25: Self.ActivateSymbol(28);
     28..29: Self.ActivateSymbol(49);
     30..31: Self.ActivateSymbol(32);
     32..34: Self.ActivateSymbol(35);
     40:     Self.ActivateSymbol(48);
     48:     Self.ActivateSymbol(55);
     49..50: Self.ActivateSymbol(30);
     35    : Self.ActivateSymbol(30);
    else
     Self.ActivateSymbol(0);
    end;
  end;//'s'


 end;
end;//procedure

procedure TF_Hlavni.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
 Self.SB_Main.Panels.Items[2].Text := '---;---';
 if (Assigned(Self.Relief)) then
   Self.Relief.HideMouse();
end;//procedure

//novy soubor
procedure TF_Hlavni.PM_NewClick(Sender: TObject);
begin
 if (Assigned(Relief)) then
  begin
   if (Relief.FileStav <> 0) then if (Application.MessageBox('Otevøením nového projektu ztratíte všechna pøedešlá data, pokraèovat?','Pokraèovat?',MB_YESNO OR MB_ICONQUESTION) <> mrYes) then Exit;
  end;//if Assigned(Relief)

 if (Assigned(Relief)) then FreeAndNil(Relief);

 F_NewRelief.OpenForm;
end;

//otevirani souboru
function TF_Hlavni.OpenFile(fname:string):Byte;
var aReturn:Integer;
begin
 Relief := TRelief.Create(Self.DXD_Main, Self);

 Self.AssignReliefEvents();

 aReturn := Relief.Open(fname);

 if (aReturn <> 0) then
  begin
   FreeAndNil(Relief);
   Application.MessageBox(PChar('Otevøení suboru skonèilo s chybou:'+#13#10+_open_file_errors[aReturn]), 'Chyba', MB_OK OR MB_ICONERROR);
   Result := 1;
   Exit;
  end;

 Self.RepaintModes(Relief.Mode);

 case (Self.Relief.Mode) of
  dmBitmap:begin
   Self.PM_BitmapClick(Self.PM_Bitmap);
   Self.PM_ORAdd.Enabled := true;
  end;//dmBitmap

  dmOddelovace:begin
   Self.PM_BitmapClick(Self.PM_Oddelovac);
  end;//dmBitmap

  dmBloky:begin
   Self.PM_ORAdd.Enabled := false;
   Self.PM_BitmapClick(Self.PM_Bloky);
  end;//dmBitmap

  dmRoots:begin
   Self.PM_BitmapClick(Self.PM_Roots);
  end;//dmBitmap
 end;

 ReliefOptions.UseData(F_Hlavni.Relief);
 Self.DesignOpen(ExtractFileName(fname));
 Result := 0;
end;//function

procedure TF_Hlavni.PM_OpenClick(Sender: TObject);
begin
 if (Assigned(Relief)) then
   if (Application.MessageBox('Otevøením nového projektu ztratíte všechna pøedešlá data, pokraèovat?','Pokraèovat?',MB_YESNO OR MB_ICONQUESTION) <> mrYes) then
     Exit();

 if (Self.OD_Open.Execute(Self.Handle)) then
  begin
   if (Assigned(Relief)) then
    begin
     if (Relief.FileStav <> 0) then
     FreeAndNil(Relief);
    end;//if Assigned(Relief)

   Self.OpenFile(Self.OD_Open.FileName);
  end;
end;

procedure TF_Hlavni.PM_ORAddClick(Sender: TObject);
var tmpOR:TOR;
begin
 tmpOR.Osvetleni.Cnt := 0;
 tmpOR.Poss.DK    := Point(0,0);
 tmpOR.Poss.Queue := Point(0,0);
 tmpOR.Poss.Time  := Point(0,0);
 tmpOR.Rights.ModCasStart := true;
 tmpOR.Rights.ModCasStop  := true;
 tmpOR.Rights.ModCasSet   := true;

 F_OREdit.OpenForm(tmpOR);
end;//procedure

procedure TF_Hlavni.PM_ReliefOptionsClick(Sender: TObject);
begin
 F_ReliefOptions.OpenForm;
end;//procedure

//ukladani souboru jako
procedure TF_Hlavni.PM_SaveAsClick(Sender: TObject);
begin
 if ((Relief.Mode = dmBitmap) or (Relief.Mode = dmOddelovace)) then
  begin
   Self.SD_Save.Filter := 'Bitmapove soubory panelu (*.bpnl)|*.bpnl';

   if (not Self.SD_Save.Execute(Self.Handle)) then Exit;
   if (RightStr(Self.SD_Save.FileName,5) <> '.bpnl') then Relief.Save(Self.SD_Save.FileName+'.bpnl') else Relief.Save(Self.SD_Save.FileName);
  end;//bitmapovy mod

 if (Relief.Mode = dmBloky) then
  begin
   Self.SD_Save.Filter := 'Objektove soubory panelu (*.opnl)|*.opnl';

   if (not Self.SD_Save.Execute(Self.Handle)) then Exit;
   if (RightStr(Self.SD_Save.FileName,5) <> '.opnl') then Relief.Save(Self.SD_Save.FileName+'.opnl') else Relief.Save(Self.SD_Save.FileName);
  end;//objektovy mod

 Self.SB_Main.Panels.Items[1].Text := 'Soubor uložen';
 Self.Caption := ExtractFileName(Self.SD_Save.FileName)+' - '+_Caption + '     v' + GetVersion(Application.ExeName);
end;

//ukladani souboru
procedure TF_Hlavni.PM_SaveClick(Sender: TObject);
var return:Byte;
begin
 return := 0;

 if ((Relief.Mode = dmBitmap) or (Relief.Mode = dmOddelovace)) then
  begin
   Self.SD_Save.Filter := 'Bitmapove soubory panelu (*.bpnl)|*.bpnl';

   if (Relief.FileStav = 1) then
    begin
     if (not Self.SD_Save.Execute(Self.Handle)) then Exit;
     if (RightStr(Self.SD_Save.FileName,5) <> '.bpnl') then return := Relief.Save(Self.SD_Save.FileName+'.bpnl') else return := Relief.Save(Self.SD_Save.FileName);
    end else begin
     if (Relief.FileStav = 2) then return := Relief.Save(Relief.FileCesta);
    end;//else .Stav = 1
  end;//bitmapovy mod

 if ((Relief.Mode = dmBloky) or (Relief.Mode = dmRoots)) then
  begin
   Self.SD_Save.Filter := 'Objektove soubory panelu (*.opnl)|*.opnl';

   if (Relief.FileStav = 1) then
    begin
     if (not Self.SD_Save.Execute(Self.Handle)) then Exit;
     if (RightStr(Self.SD_Save.FileName,5) <> '.opnl') then return := Relief.Save(Self.SD_Save.FileName+'.opnl') else return := Relief.Save(Self.SD_Save.FileName);
    end else begin
     if (Relief.FileStav = 2) then return := Relief.Save(Relief.FileCesta);
    end;//else .Stav = 1
  end;//objektovy mod

 if (return <> 0) then
  begin
   Application.MessageBox(PChar('Uložení suboru skonèilo s chybou:'+#13#10+_close_file_errors[Return]), 'Chyba', MB_OK OR MB_ICONERROR);
   Exit;
  end;

 Self.SB_Main.Panels.Items[1].Text := 'Soubor uložen';
 Self.Caption := ExtractFileName(Relief.FileCesta)+' - '+_Caption + '     v' + GetVersion(Application.ExeName);
end;

procedure TF_Hlavni.PM_AboutClick(Sender: TObject);
begin
 Application.MessageBox(PChar('hJOPeditor'+#13#10+'v'+GetVersion(Application.ExeName)+#13#10+'(c) Jan Horáèek 2011-2017'),'Info',MB_OK OR MB_ICONINFORMATION);
end;//procedure

procedure TF_Hlavni.PM_BitmapClick(Sender: TObject);
var Return:Integer;
begin
 (Sender as TMenuItem).Checked := true;

 if (Assigned(Self.pushedButton)) then
  Self.pushedButton.Down := false;

 F_BlockEdit.Close();

 Self.TB_Vyhybka.Visible        := false;
 Self.TB_Usek.Visible           := false;
 Self.TB_SCom.Visible           := false;
 Self.TB_BitmapTools.Visible    := false;
 Self.TB_Oddelovac.Visible      := false;
 Self.TB_BitmapOstatni.Visible  := false;
 Self.TB_Trat.Visible           := false;
 Self.TB_Vykolejka.Visible      := false;
 Self.CHB_Group.Enabled         := false;
 Self.TB_Other.Visible          := false;
 Self.MI_Relief.Visible         := false;
 Self.MI_Data.Visible           := false;

 case (Sender as TMenuItem).Tag of
  0:begin
     if (Relief.Mode = dmBloky) then
      begin
       Application.MessageBox('Tato funkce zatím není dostupná', 'Nelze pøevést', MB_OK OR MB_ICONERROR);
       Self.PM_Bloky.Checked := true;
       Exit;
      end;

     Return := 0;
     if (Relief.Mode <> dmBitmap) then Return := Relief.SwitchMode(dmBitmap);

     if (Return <> 0) then
      begin
       Application.MessageBox(PChar('Pri zmene modu se vykytla chyba - chyba: '+IntToStr(Return)),'Chyba',MB_OK OR MB_ICONWARNING);
       Exit;
      end;

     Self.TB_Vyhybka.Visible        := true;
     Self.TB_Usek.Visible           := true;
     Self.TB_SCom.Visible           := true;
     Self.TB_BitmapTools.Visible    := true;
     Self.TB_BitmapOstatni.Visible  := true;
     Self.TB_Trat.Visible           := true;
     Self.CHB_Group.Enabled         := true;
     Self.TB_Other.Visible          := true;
     Self.MI_Relief.Visible         := true;
     Self.TB_Vykolejka.Visible      := true;
    end;//case 0
  1:begin
     if (Relief.Mode = dmBloky) then
      begin
       Application.MessageBox('Tato funkce zatím není dostupná', 'Nelze pøevést', MB_OK OR MB_ICONERROR);
       Self.PM_Bloky.Checked := true;
       Exit;
      end;

     Return := 0;
     if (Relief.Mode <> dmOddelovace) then Return := Relief.SwitchMode(dmOddelovace);

     if (Return <> 0) then
      begin
       Application.MessageBox(PChar('Pri zmene modu se vyskytla chyba - chyba: '+IntToStr(Return)),'Chyba',MB_OK OR MB_ICONWARNING);
       Exit;
      end;

     Self.TB_Oddelovac.Visible   := true;
     Self.TB_BitmapTools.Visible := true;
     Self.MI_Relief.Visible      := true;
    end;//case 1
  2:begin
     Return := 0;
     if (Relief.Mode <> dmBloky) then Return := Relief.SwitchMode(dmBloky);

     if (Return <> 0) then
      begin
       Application.MessageBox(PChar('Pri zmene modu se vyskytla chyba - chyba: '+IntToStr(Return)),'Chyba',MB_OK OR MB_ICONWARNING);
       Exit;
      end;

     Self.MI_Data.Visible := true;
    end;//case 2
  3:begin
     Return := 0;
     if (Relief.Mode <> dmRoots) then Return := Relief.SwitchMode(dmRoots);

     if (Return <> 0) then
      begin
       Application.MessageBox(PChar('Pri zmene modu se vyskytla chyba - chyba: '+IntToStr(Return)),'Chyba',MB_OK OR MB_ICONWARNING);
       Exit;
      end;

     F_BlockEdit.Close();
     Self.MI_Data.Visible := true;
    end;//case 2
 end;//case (Sender as TMenuItem).Tag

 Self.RepaintModes(Relief.Mode);
end;//procedure

procedure TF_Hlavni.PM_ChangeRozmeryClick(Sender: TObject);
begin
 F_ReliefProperties.OpenForm;
end;

procedure TF_Hlavni.MI_CheckDataClick(Sender: TObject);
var error_cnt:Byte;
    LI:TListItem;
begin
 if ((Self.Relief.Mode = dmBloky) or (Self.Relief.Mode = dmRoots)) then
  begin
   F_DataCheck.OpenForm(Self.Relief.CheckValid(error_cnt));
   LI := F_DataCheck.LV_Errors.Items.Add;
   LI.Caption := IntToStr(F_DataCheck.LV_Errors.Items.Count);
   if (error_cnt = 0) then
     LI.SubItems.Add('OK: Zkouška validity probìhla úspìšnì')
   else
     LI.SubItems.Add('ERR: Zkouška validity skonèila s '+IntToStr(error_cnt)+' chybami!');
  end;
end;

//procedure

procedure TF_Hlavni.PM_CloseAppClick(Sender: TObject);
begin
 Self.Close;
end;//procedure

procedure TF_Hlavni.MI_MrizkaClick(Sender: TObject);
begin
 (Sender as TMenuItem).Checked := not (Sender as TMenuItem).Checked;

 ReliefOptions.Mrizka := (Sender as TMenuItem).Checked;
 ReliefOptions.SaveData(IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName))+'Config.ini');

 if (Assigned(Relief)) then
   ReliefOptions.UseData(F_Hlavni.Relief);
end;//ppocedure

procedure TF_Hlavni.MI_SaveShowOptionsClick(Sender: TObject);
begin
 ReliefOptions.SaveData(IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName))+_Config_File);
end;//ppocedure

procedure TF_Hlavni.TBt_SeparatorClick(Sender: TObject);
begin
 Relief.Escape(false);
 if (Relief.AddSeparator <> 0) then Application.MessageBox('Chyba pri pridavani objektu','Chyba',MB_OK OR MB_ICONWARNING);
end;

procedure TF_Hlavni.ToolButton0Click(Sender: TObject);
begin
 Relief.Escape(false);
 if (Relief.AddSymbol((Sender as TToolButton).Tag) <> 0) then
  begin
   Application.MessageBox('Chyba pri pridavani objektu','Chyba',MB_OK OR MB_ICONWARNING);
   Exit;
  end;

 if (Assigned(Self.pushedButton)) then
   Self.pushedButton.Down := false;

 (Sender as TToolButton).Down := true;
 Self.pushedButton := (Sender as TToolButton);
end;//procedure

procedure TF_Hlavni.ToolButton40Click(Sender: TObject);
begin
 Relief.Escape(false);
 F_Popisek.NewPopisek;

 if (F_Popisek.PopisekColor <> -1) then Relief.AddText(F_Popisek.PopisekText,F_Popisek.PopisekColor);
end;//procedure

procedure TF_Hlavni.ToolButton41Click(Sender: TObject);
begin
 Relief.Escape(false);
 if (Relief.AddJCClick <> 0) then Application.MessageBox('Chyba pri pridavani objektu','Chyba',MB_OK OR MB_ICONWARNING);
end;//procedure

procedure TF_Hlavni.ToolButton42Click(Sender: TObject);
begin
 Relief.Escape(false);
 if (Relief.AddKPopisek <> 0) then Application.MessageBox('Chyba pri pridavani objektu','Chyba',MB_OK OR MB_ICONWARNING);
end;//procedure

procedure TF_Hlavni.MI_CloseFileClick(Sender: TObject);
begin
 if (Application.MessageBox('Uzavøením projektu bez jeho uložení ztratíte projektová data, pøesto pokraèovat?','Uzavøení projektu',MB_YESNO OR MB_ICONQUESTION) = mrNo) then Exit;

 if (Assigned(ReliefOptions)) then ReliefOptions.SaveData(IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName))+_Config_File);

 Self.DesignClose;

 Self.DestroyReliefClasses;
end;

procedure TF_Hlavni.MI_ExportServerClick(Sender: TObject);
begin
 //execute Merger
end;//procedure

procedure TF_Hlavni.DesignOpen(FName:string);
begin
 Self.PM_Save.Enabled      := true;
 Self.PM_SaveAs.Enabled    := true;
 Self.MI_Draw.Visible      := true;
 Self.MI_Mrizka.Checked    := ReliefOptions.Mrizka;
 Self.MI_CloseFile.Enabled := true;
 Self.SB_Main.Panels.Items[0].Text := 'Soubor otevøen';
 Self.SB_Main.Panels.Items[1].Text := 'Soubor není uložen';

 Self.Caption := FName+' - '+_Caption + '     v' + GetVersion(Application.ExeName);
end;//procedure

procedure TF_Hlavni.DesignClose;
begin
 Self.PM_Save.Enabled      := false;
 Self.PM_SaveAs.Enabled    := false;
 Self.MI_Draw.Visible      := false;
 Self.MI_Relief.Visible    := false;
 Self.MI_CloseFile.Enabled := false;
 Self.MI_Data.Visible      := false;

 Self.TB_Vyhybka.Visible        := false;
 Self.TB_Usek.Visible           := false;
 Self.TB_SCom.Visible           := false;
 Self.TB_BitmapTools.Visible    := false;
 Self.TB_Oddelovac.Visible      := false;
 Self.TB_BitmapOstatni.Visible  := false;
 Self.TB_Other.Visible          := false;
 Self.TB_Trat.Visible           := false;
 Self.TB_Vykolejka.Visible      := false;

 if (Assigned(Self.pushedButton)) then
  Self.pushedButton.Down := false;

 Self.SB_Main.Panels.Items[2].Text := '---;---';
 Self.SB_Main.Panels.Items[3].Text := '';

 Self.SB_Main.Panels.Items[0].Text := 'Soubor uzavøen';
 Self.SB_Main.Panels.Items[1].Text := '';
 Self.Caption := _Caption + '     v' + GetVersion(Application.ExeName);

 F_BlockEdit.Close();
end;//procedure

procedure TF_Hlavni.ReliefErrorEvent(Sender:TObject; ErrorID:Integer);
begin
 Self.SB_Main.Panels.Items[3].Text := 'Chyba: '+_PanelErrors[ErrorID]+'(ID:'+IntToStr(ErrorID)+')';
end;//procedure

procedure TF_Hlavni.ReliefMoveEvent(Sender:TObject; Position:TPoint);
begin
 Self.SB_Main.Panels.Items[2].Text := Format('%.3d',[Position.X])+' ; '+Format('%.3d',[Position.Y]);
 Self.SB_Main.Panels.Items[3].Text := '';
end;//procedure

procedure TF_Hlavni.ReliefChangeTextEvent(Sender:TObject; var Text:string;var Color:Integer);
begin
 F_Popisek.OpenPopisek(Text,Color);

 if (F_Popisek.PopisekColor = -1) then Exit;
 Text  := F_Popisek.PopisekText;
 Color := F_Popisek.PopisekColor;
end;//procedure

procedure TF_Hlavni.BlokEditEvent(Sender:TObject; Blok:TGraphBlok);
begin
 F_BlockEdit.OpenForm(Blok);
end;//procedure

procedure TF_Hlavni.MessageEvent(Sender:TObject; msg:string);
begin
 Self.SB_Main.Panels.Items[3].Text := msg;
end;//procedure

procedure TF_Hlavni.RepaintModes(cur:TMode);
begin
 case (cur) of
   dmBitmap, dmOddelovace:begin
    Self.PM_Bitmap.Enabled    := true;
    Self.PM_Oddelovac.Enabled := true;
    Self.PM_Bloky.Enabled     := true;
    Self.PM_Roots.Enabled     := false;
   end;

   dmBloky, dmRoots:begin
    Self.PM_Bitmap.Enabled    := false;
    Self.PM_Oddelovac.Enabled := false;
    Self.PM_Bloky.Enabled     := true;
    Self.PM_Roots.Enabled     := true;
   end;

 end;//case
end;//procedure

procedure TF_Hlavni.ActivateSymbol(index:Integer);
begin
 if (not Self.TB_BitmapTools.Visible) then
   Exit;

 if (Assigned(Self.pushedButton)) then
   Self.pushedButton.Down := false;

 case (index) of
   0: Self.ToolButton0.OnClick(Self.ToolButton0);
   1: Self.ToolButton1.OnClick(Self.ToolButton1);
   2: Self.ToolButton2.OnClick(Self.ToolButton2);
   3: Self.ToolButton3.OnClick(Self.ToolButton3);
  12: Self.ToolButton12.OnClick(Self.ToolButton12);
  13: Self.ToolButton13.OnClick(Self.ToolButton13);
  14: Self.ToolButton14.OnClick(Self.ToolButton14);
  15: Self.ToolButton15.OnClick(Self.ToolButton15);
  16: Self.ToolButton16.OnClick(Self.ToolButton16);
  17: Self.ToolButton17.OnClick(Self.ToolButton17);
  18: Self.ToolButton18.OnClick(Self.ToolButton18);
  19: Self.ToolButton19.OnClick(Self.ToolButton19);
  20: Self.ToolButton20.OnClick(Self.ToolButton20);
  21: Self.ToolButton21.OnClick(Self.ToolButton21);
  22: Self.ToolButton22.OnClick(Self.ToolButton22);
  23: Self.ToolButton23.OnClick(Self.ToolButton23);
  24: Self.ToolButton24.OnClick(Self.ToolButton24);
  25: Self.ToolButton25.OnClick(Self.ToolButton25);
  28: Self.ToolButton28.OnClick(Self.ToolButton28);
  29: Self.ToolButton29.OnClick(Self.ToolButton29);
  30: Self.ToolButton30.OnClick(Self.ToolButton30);
  31: Self.ToolButton31.OnClick(Self.ToolButton31);
  32: Self.ToolButton32.OnClick(Self.ToolButton32);
  33: Self.ToolButton33.OnClick(Self.ToolButton33);
  34: Self.ToolButton34.OnClick(Self.ToolButton34);
  35: Self.ToolButton35.OnClick(Self.ToolButton35);

  41: Self.ToolButton34.OnClick(Self.ToolButton41);
  42: Self.ToolButton35.OnClick(Self.ToolButton42);

  48: Self.ToolButton48.OnClick(Self.ToolButton48);
  55: Self.ToolButton55.OnClick(Self.ToolButton55);

  49: Self.ToolButton49.OnClick(Self.ToolButton49);
  50: Self.ToolButton50.OnClick(Self.ToolButton50);
 end;
end;//procedure

procedure TF_Hlavni.AssignReliefEvents();
begin
 Relief.OnError         := Self.ReliefErrorEvent;
 Relief.OnMove          := Self.ReliefMoveEvent;
 Relief.OnChangeText    := Self.ReliefChangeTextEvent;
 Relief.OnBlokEdit      := Self.BlokEditEvent;

 Relief.OnMsg         := Self.MessageEvent;
 Relief.FormBlkClose  := Self.FormBlkCloseEvent;
end;//procedure

procedure TF_Hlavni.FormBlkCloseEvent(Sender:TObject);
begin
 F_BlockEdit.Close();
end;//procedure

end.//unit
