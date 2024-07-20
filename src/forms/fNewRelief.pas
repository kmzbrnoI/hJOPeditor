unit fNewRelief;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Panel, Global, ReliefSettings, OblastRizeni, Spin, ExtCtrls,
  Types, Generics.Collections;

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
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    lights: TList<TORLight>;

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

  var area: TOR;
  area := TOR.Create();

  area.Name := Self.E_Name.Text;
  area.ShortName := Self.E_NameShort.Text;
  area.Id := Self.E_NameUniq.Text;
  area.OddDirection := TOROddDirection(Self.CB_Lichy.ItemIndex);

  if (Self.RB_OR1.Checked) then
    area.Poss.DKOr := TDKOrientation.dkoDown;
  if (Self.RB_OR2.Checked) then
    area.Poss.DKOr := TDKOrientation.dkoUp;

  area.Rights.ModCasStart := Self.CHB_ModCasStart.Checked;
  area.Rights.ModCasStop := Self.CHB_ModCasStop.Checked;
  area.Rights.ModCasSet := Self.CHB_ModCasSet.Checked;

  area.Lights.AddRange(Self.lights);

  try
    F_Main.Relief := TRelief.Create(F_Main.DXD_Main, F_Main);
    F_Main.AssignReliefEvents();
    F_Main.Relief.New(Point(Self.SE_Width.Value, Self.SE_Height.Value), area);
  except
    on E: Exception do
    begin
      try
        if (F_Main.Relief <> nil) then
          FreeAndNil(F_Main.Relief);
      except
        F_Main.Relief := nil;
      end;

      Application.MessageBox(PChar('Vyskytla se chyba pri inicializaci panelu:#13#10' + E.Message), 'Chyba',
        MB_OK OR MB_ICONWARNING);
      Exit();
    end;
  end;

  Self.Close();
  F_Main.PM_BitmapClick(F_Main.PM_Bitmap);
  ReliefOptions.UseData(F_Main.Relief);
  F_Main.DesignOpen('Nový projekt');
end;

procedure TF_NewRelief.B_StornoClick(Sender: TObject);
begin
  Self.Close();
end;

procedure TF_NewRelief.E_NameKeyPress(Sender: TObject; var Key: Char);
begin
  // obrana proti injection do souboru
  if (Key = ';') then
    Key := #0;
end;

procedure TF_NewRelief.E_OsvAddrExit(Sender: TObject);
begin
  var i := Self.LB_Osv.ItemIndex;
  if ((i >= 0) and (i < Self.lights.Count)) then
  begin
    var light := Self.lights[i];
    light.Board := StrToInt(Self.E_OsvAddr.Text);
    light.Port := StrToInt(Self.E_OsvPort.Text);
    light.Name := Self.E_OsvName.Text;
    Self.lights[i] := light;
  end;
end;

procedure TF_NewRelief.FormCreate(Sender: TObject);
begin
  Self.lights := TList<TORLight>.Create();
end;

procedure TF_NewRelief.FormDestroy(Sender: TObject);
begin
  Self.lights.Free();
end;

procedure TF_NewRelief.LB_OsvClick(Sender: TObject);
begin
  if ((Sender as TListBox).ItemIndex = -1) then
  begin
    Self.GB_OsvOne.Visible := false;
    Exit();
  end;

  var i: Integer := (Sender as TListBox).ItemIndex;
  if (i < Self.lights.Count) then
  begin
    Self.E_OsvAddr.Text := IntToStr(Self.lights[i].board);
    Self.E_OsvPort.Text := IntToStr(Self.lights[i].port);
    Self.E_OsvName.Text := Self.lights[i].Name;
  end;

  Self.GB_OsvOne.Visible := true;
end;

procedure TF_NewRelief.OpenForm();
begin
  Self.SE_Height.Value := 20;
  Self.SE_Width.Value := 40;

  Self.lights.Clear();

  Self.E_Name.Text := '';
  Self.E_NameShort.Text := '';
  Self.E_NameUniq.Text := '';
  Self.CB_Lichy.ItemIndex := -1;
  Self.CHB_ModCasStart.Checked := true;
  Self.CHB_ModCasStop.Checked := true;
  Self.CHB_ModCasSet.Checked := true;
  Self.SE_OsvCnt.Value := 0;
  Self.GB_OsvOne.Visible := false;
  Self.LB_Osv.Clear();

  Self.ShowModal;
end;

procedure TF_NewRelief.SE_OsvCntChange(Sender: TObject);
begin
  while (Self.lights.Count < Self.SE_OsvCnt.Value) do
  begin
    var empty: TORLight;
    empty.Board := 0;
    empty.Port := 0;
    empty.Name := '';
    Self.lights.Add(empty);
    Self.LB_Osv.Items.Add(IntToStr(Self.lights.Count));
  end;

  while (Self.lights.Count > Self.SE_OsvCnt.Value) do
  begin
    Self.lights.Delete(Self.lights.Count-1);
    Self.LB_Osv.Items.Delete(Self.lights.Count);
  end;
end;

end.// unit
