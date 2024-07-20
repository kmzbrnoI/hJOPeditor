unit fOREdit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Spin, OblastRizeni, Generics.Collections;

type
  TF_OREdit = class(TForm)
    GB_Osv: TGroupBox;
    Label6: TLabel;
    SE_OsvCnt: TSpinEdit;
    LB_Osv: TListBox;
    GB_OsvOne: TGroupBox;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    E_OsvAddr: TEdit;
    E_OsvPort: TEdit;
    E_OsvName: TEdit;
    CHB_ModCasSet: TCheckBox;
    CHB_ModCasStop: TCheckBox;
    CHB_ModCasStart: TCheckBox;
    Image1: TImage;
    RB_OR1: TRadioButton;
    RB_OR2: TRadioButton;
    CB_Lichy: TComboBox;
    E_NameUniq: TEdit;
    E_NameShort: TEdit;
    E_Name: TEdit;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    B_Apply: TButton;
    B_Storno: TButton;
    procedure B_StornoClick(Sender: TObject);
    procedure SE_OsvCntChange(Sender: TObject);
    procedure LB_OsvClick(Sender: TObject);
    procedure E_OsvAddrExit(Sender: TObject);
    procedure B_ApplyClick(Sender: TObject);
    procedure E_NameUniqKeyPress(Sender: TObject; var Key: Char);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
  private
    openindex: Integer;
    area: TOR;
    lights: TList<TORLight>;

    procedure EditOR(oblRizeni: TOR); overload;

  public

    procedure NewOR();
    procedure EditOR(orindex: Integer); overload;
  end;

var
  F_OREdit: TF_OREdit;

implementation

uses fMain;

{$R *.dfm}

// ulozi vsechna data
procedure TF_OREdit.B_ApplyClick(Sender: TObject);
begin
  if ((Self.E_Name.Text = '') or (Self.E_NameShort.Text = '') or (Self.E_NameUniq.Text = '')) then
  begin
    Application.MessageBox('Název, zkratka názvu a unikátní název (ID) oblasti řízení musí být vyplněno!',
      'Nelze pokračovat', MB_OK OR MB_ICONERROR);
    Exit();
  end;
  if (Self.CB_Lichy.ItemIndex = -1) then
  begin
    Application.MessageBox('Vyberte lichý směr', 'Nelze pokračovat', MB_OK OR MB_ICONERROR);
    Exit;
  end;

  if ((not Self.RB_OR1.Checked) and (not Self.RB_OR2.Checked)) then
  begin
    Application.MessageBox('Vyberte orientaci DK!', 'Nelze pokračovat', MB_OK OR MB_ICONERROR);
    Exit;
  end;

  if (Self.area = nil) then
    Self.area := TOR.Create();

  Self.area.Name := Self.E_Name.Text;
  Self.area.ShortName := Self.E_NameShort.Text;
  Self.area.Id := Self.E_NameUniq.Text;
  Self.area.OddDirection := TOROddDirection(Self.CB_Lichy.ItemIndex);

  if (Self.RB_OR1.Checked) then
    Self.area.Poss.DKOr := TDKOrientation.dkoDown;
  if (Self.RB_OR2.Checked) then
    Self.area.Poss.DKOr := TDKOrientation.dkoUp;

  Self.area.Rights.ModCasStart := Self.CHB_ModCasStart.Checked;
  Self.area.Rights.ModCasStop := Self.CHB_ModCasStop.Checked;
  Self.area.Rights.ModCasSet := Self.CHB_ModCasSet.Checked;

  Self.area.Lights.Clear();
  Self.area.Lights.AddRange(Self.lights);

  if (Self.openindex = -1) then
    F_Hlavni.Relief.AddOR(Self.area);

  Self.Close();
end;

procedure TF_OREdit.B_StornoClick(Sender: TObject);
begin
  Self.Close();
end;

procedure TF_OREdit.E_NameUniqKeyPress(Sender: TObject; var Key: Char);
begin
  // obrana proti injection do souboru
  if (Key = ';') then
    Key := #0;
end;

procedure TF_OREdit.E_OsvAddrExit(Sender: TObject);
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

procedure TF_OREdit.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Self.openindex := -1;
end;

procedure TF_OREdit.FormCreate(Sender: TObject);
begin
  Self.lights := TList<TORLight>.Create();
  Self.openindex := -1;
end;

procedure TF_OREdit.FormDestroy(Sender: TObject);
begin
  Self.lights.Free();
end;

procedure TF_OREdit.LB_OsvClick(Sender: TObject);
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

procedure TF_OREdit.EditOR(orindex: Integer);
begin
  Self.openindex := orindex;
  Self.EditOR(F_Hlavni.Relief.ORs[orindex]);
end;

procedure TF_OREdit.EditOR(oblRizeni: TOR);
begin
  Self.area := oblRizeni;

  Self.GB_OsvOne.Visible := false;

  Self.E_Name.Text := Self.area.Name;
  Self.E_NameShort.Text := Self.area.ShortName;
  Self.E_NameUniq.Text := Self.area.Id;

  Self.CB_Lichy.ItemIndex := Integer(Self.area.OddDirection);
  if (Self.area.Poss.DKOr = TDKOrientation.dkoDown) then
    Self.RB_OR1.Checked := true;
  if (Self.area.Poss.DKOr = TDKOrientation.dkoUp) then
    Self.RB_OR2.Checked := true;

  Self.CHB_ModCasStart.Checked := Self.area.Rights.ModCasStart;
  Self.CHB_ModCasStop.Checked := Self.area.Rights.ModCasStop;
  Self.CHB_ModCasSet.Checked := Self.area.Rights.ModCasSet;

  Self.lights.Clear();
  Self.lights.AddRange(oblRizeni.Lights);

  Self.LB_Osv.Clear();
  for var i: Integer := 0 to oblRizeni.Lights.Count-1 do
    Self.LB_Osv.Items.Add(IntToStr(i+1));

  Self.SE_OsvCnt.Value := Self.area.Lights.Count;

  Self.Caption := 'Oblast řízení ' + Self.area.Id;
  Self.ActiveControl := Self.E_Name;
  Self.ShowModal();
end;

procedure TF_OREdit.NewOR();
begin
  Self.area := nil;
  Self.GB_OsvOne.Visible := false;

  Self.E_Name.Text := '';
  Self.E_NameShort.Text := '';
  Self.E_NameUniq.Text := '';

  Self.CB_Lichy.ItemIndex := -1;
  Self.RB_OR1.Checked := False;
  Self.RB_OR2.Checked := False;

  Self.CHB_ModCasStart.Checked := False;
  Self.CHB_ModCasStop.Checked := False;
  Self.CHB_ModCasSet.Checked := False;

  Self.lights.Clear();
  Self.LB_Osv.Clear();
  Self.SE_OsvCnt.Value := 0;

  Self.Caption := 'Nová oblast řízení';
  Self.ActiveControl := Self.E_Name;
  Self.ShowModal();
end;

procedure TF_OREdit.SE_OsvCntChange(Sender: TObject);
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

  Self.GB_OsvOne.Visible := false;
end;

end.// unit
