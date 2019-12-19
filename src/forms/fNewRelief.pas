unit fNewRelief;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Panel, Global, ReliefSettings, OblastRizeni, Spin, ExtCtrls;

type
  TF_NewRelief = class(TForm)
    B_Create: TButton;
    B_Storno: TButton;
    GB_OR: TGroupBox;
    Label3: TLabel;
    E_Name: TEdit;
    Label4: TLabel;
    E_NameShort: TEdit;
    Label5: TLabel;
    E_NameUniq: TEdit;
    CHB_ModCasStart: TCheckBox;
    CHB_ModCasStop: TCheckBox;
    CHB_ModCasSet: TCheckBox;
    GB_Osv: TGroupBox;
    Label6: TLabel;
    SE_OsvCnt: TSpinEdit;
    LB_Osv: TListBox;
    GB_OsvOne: TGroupBox;
    Label7: TLabel;
    E_OsvAddr: TEdit;
    Label8: TLabel;
    E_OsvPort: TEdit;
    Label9: TLabel;
    E_OsvName: TEdit;
    GB_Relief: TGroupBox;
    Label2: TLabel;
    Label1: TLabel;
    Label10: TLabel;
    CB_Lichy: TComboBox;
    Label11: TLabel;
    Image1: TImage;
    RB_OR1: TRadioButton;
    RB_OR2: TRadioButton;
    StaticText1: TStaticText;
    StaticText2: TStaticText;
    SE_Width: TSpinEdit;
    SE_Height: TSpinEdit;
    procedure B_CreateClick(Sender: TObject);
    procedure B_StornoClick(Sender: TObject);
    procedure SE_OsvCntChange(Sender: TObject);
    procedure LB_OsvClick(Sender: TObject);
    procedure E_OsvAddrExit(Sender: TObject);
    procedure E_NameKeyPress(Sender: TObject; var Key: Char);
  private
    firstOR:TOR;
  public
   procedure OpenForm;
  end;

var
  F_NewRelief: TF_NewRelief;

implementation

uses fMain, ReliefObjects, ReliefBitmap;

{$R *.dfm}

procedure TF_NewRelief.B_CreateClick(Sender: TObject);
begin
 if ((Self.SE_Height.Value > 255) or (Self.SE_Width.Value > 255)) then
  begin
   Application.MessageBox('Maximální rozměry panelu jsou 255x255', 'Nelze pokračovat', MB_OK OR MB_ICONERROR);
   Exit();
  end;
 if ((Self.E_Name.Text = '') or (Self.E_NameShort.Text = '') or (Self.E_NameUniq.Text = '')) then
  begin
   Application.MessageBox('Název, zkratka názvu a unikátní název (ID) oblasti řízení musí být vyplněno!',
                          'Nelze pokračovat', MB_OK OR MB_ICONERROR);
   Exit();
  end;
 if (Self.CB_Lichy.ItemIndex = -1) then
  begin
   Application.MessageBox('Vyberte lichý směr', 'Nelze pokračovat', MB_OK OR MB_ICONERROR);
   Exit();
  end;

 if ((not Self.RB_OR1.Checked) and (not Self.RB_OR2.Checked)) then
  begin
   Application.MessageBox('Vyberte orientaci DK!', 'Nelze pokračovat', MB_OK OR MB_ICONERROR);
   Exit;
  end;

 Self.firstOR.Name      := Self.E_Name.Text;
 Self.firstOR.ShortName := Self.E_NameShort.Text;
 Self.firstOR.id        := Self.E_NameUniq.Text;
 Self.firstOR.Lichy     := Self.CB_Lichy.ItemIndex;

 if (Self.RB_OR1.Checked) then Self.firstOR.Poss.DKOr := 0;
 if (Self.RB_OR2.Checked) then Self.firstOR.Poss.DKOr := 1;

 Self.firstOR.Rights.ModCasStart  := Self.CHB_ModCasStart.Checked;
 Self.firstOR.Rights.ModCasStop   := Self.CHB_ModCasStop.Checked;
 Self.firstOR.Rights.ModCasSet    := Self.CHB_ModCasSet.Checked;

 Self.Close;

 //vytvareni objektu relief
 F_Hlavni.Relief := TRelief.Create(F_Hlavni.DXD_Main,F_Hlavni);

 F_Hlavni.AssignReliefEvents();

 try
   F_Hlavni.Relief.New(Point(Self.SE_Width.Value, Self.SE_Height.Value), firstOR);
 except
   on E:Exception do
    begin
     Application.MessageBox(PChar('Vyskytla se chyba pri inicializaci panelu:#13#10'+e.Message),
                            'Chyba', MB_OK OR MB_ICONWARNING);
     Exit();
    end;
 end;

 F_Hlavni.PM_BitmapClick(F_Hlavni.PM_Bitmap);
 ReliefOptions.UseData(F_Hlavni.Relief);
 F_Hlavni.DesignOpen('Nový projekt');
end;

procedure TF_NewRelief.B_StornoClick(Sender: TObject);
begin
 Self.Close;
end;

procedure TF_NewRelief.E_NameKeyPress(Sender: TObject; var Key: Char);
begin
 //obrana proti injection do souboru
 if (Key = ';') then Key := #0;
end;

procedure TF_NewRelief.E_OsvAddrExit(Sender: TObject);
begin
 Self.firstOR.Osvetleni.Data[Self.LB_Osv.ItemIndex].board := StrToInt(Self.E_OsvAddr.Text);
 Self.firstOR.Osvetleni.Data[Self.LB_Osv.ItemIndex].port  := StrToInt(Self.E_OsvPort.Text);
 Self.firstOR.Osvetleni.Data[Self.LB_Osv.ItemIndex].name  := Self.E_OsvName.Text;
end;

procedure TF_NewRelief.LB_OsvClick(Sender: TObject);
begin
 if ((Sender as TListBox).ItemIndex = -1) then
  begin
   Self.GB_OsvOne.Visible := false;
   Exit;
  end;

 Self.E_OsvAddr.Text := IntToStr(Self.firstOR.Osvetleni.Data[(Sender as TListBox).ItemIndex].board);
 Self.E_OsvPort.Text := IntToStr(Self.firstOR.Osvetleni.Data[(Sender as TListBox).ItemIndex].port);
 Self.E_OsvName.Text := Self.firstOR.Osvetleni.Data[(Sender as TListBox).ItemIndex].name;

 Self.GB_OsvOne.Visible := true;
end;

procedure TF_NewRelief.OpenForm;
var i:Integer;
begin
 Self.SE_Height.Value := 20;
 Self.SE_Width.Value  := 40;

 Self.firstOR.Osvetleni.Cnt := 0;
 for i := 0 to _MAX_OSV-1 do
  begin
   Self.firstOR.Osvetleni.Data[i].board := 0;
   Self.firstOR.Osvetleni.Data[i].port  := 0;
   Self.firstOR.Osvetleni.Data[i].name  := '';
  end;

 Self.E_Name.Text      := '';
 Self.E_NameShort.Text := '';
 Self.E_NameUniq.Text  := '';
 Self.CB_Lichy.ItemIndex := -1;
 Self.CHB_ModCasStart.Checked := true;
 Self.CHB_ModCasStop.Checked  := true;
 Self.CHB_ModCasSet.Checked   := true;
 Self.SE_OsvCnt.Value   := 0;
 Self.GB_OsvOne.Visible := false;
 Self.LB_Osv.Clear();

 Self.ShowModal;
end;

procedure TF_NewRelief.SE_OsvCntChange(Sender: TObject);
var i:Integer;
begin
 Self.firstOR.Osvetleni.Cnt := (Sender as TSpinEdit).Value;
 Self.LB_Osv.Clear();
 for i := 0 to (Sender as TSpinEdit).Value-1 do Self.LB_Osv.Items.Add(IntToStr(i+1));
 Self.GB_OsvOne.Visible := false;
end;

end.//unit
