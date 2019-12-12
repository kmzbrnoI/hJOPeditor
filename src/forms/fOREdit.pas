unit fOREdit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Spin, OblastRizeni;

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
  private
    openindex:Integer;
    orData:TOR;
  public
    procedure OpenForm(orindex:Integer); overload;
    procedure OpenForm(oblRizeni:TOR); overload;
  end;

var
  F_OREdit: TF_OREdit;

implementation

uses fMain;

{$R *.dfm}

//ulozi vsechna data
procedure TF_OREdit.B_ApplyClick(Sender: TObject);
begin
 if ((Self.E_Name.Text = '') or (Self.E_NameShort.Text = '') or (Self.E_NameUniq.Text = '')) then
  begin
   Application.MessageBox('Název, zkratka názvu a unikátní název (ID) oblasti øízení musí být vyplnìno!',
                          'Nelze pokraèovat', MB_OK OR MB_ICONERROR);
   Exit();
  end;
 if (Self.CB_Lichy.ItemIndex = -1) then
  begin
   Application.MessageBox('Vyberte lichý smìr','Nelze pokraèovat',MB_OK OR MB_ICONERROR);
   Exit;
  end;

 if ((not Self.RB_OR1.Checked) and (not Self.RB_OR2.Checked)) then
  begin
   Application.MessageBox('Vyberte orientaci DK!','Nelze pokraèovat',MB_OK OR MB_ICONERROR);
   Exit;
  end;

 Self.orData.Name      := Self.E_Name.Text;
 Self.orData.ShortName := Self.E_NameShort.Text;
 Self.orData.id        := Self.E_NameUniq.Text;
 Self.orData.Lichy     := Self.CB_Lichy.ItemIndex;

 if (Self.RB_OR1.Checked) then Self.orData.Poss.DKOr := 0;
 if (Self.RB_OR2.Checked) then Self.orData.Poss.DKOr := 1;

 Self.orData.Rights.ModCasStart  := Self.CHB_ModCasStart.Checked;
 Self.orData.Rights.ModCasStop   := Self.CHB_ModCasStop.Checked;
 Self.orData.Rights.ModCasSet    := Self.CHB_ModCasSet.Checked;

 if (Self.openindex = -1) then
  begin
   //vytvorit nove OR
   F_Hlavni.Relief.AddOR(Self.orData);
  end else begin
   //aktualizovat exsitujici OR
   F_Hlavni.Relief.ORs[openindex] := Self.orData;
  end;

 Self.Close();
end;

procedure TF_OREdit.B_StornoClick(Sender: TObject);
begin
 Self.Close();
end;

procedure TF_OREdit.E_NameUniqKeyPress(Sender: TObject; var Key: Char);
begin
 //obrana proti injection do souboru
 if (Key = ';') then Key := #0;
end;

procedure TF_OREdit.E_OsvAddrExit(Sender: TObject);
begin
 Self.orData.Osvetleni.Data[Self.LB_Osv.ItemIndex].board := StrToInt(Self.E_OsvAddr.Text);
 Self.orData.Osvetleni.Data[Self.LB_Osv.ItemIndex].port  := StrToInt(Self.E_OsvPort.Text);
 Self.orData.Osvetleni.Data[Self.LB_Osv.ItemIndex].name  := Self.E_OsvName.Text;
end;

procedure TF_OREdit.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 Self.openindex := -1;
end;

procedure TF_OREdit.FormCreate(Sender: TObject);
begin
 Self.openindex := -1;
end;

procedure TF_OREdit.LB_OsvClick(Sender: TObject);
begin
 if ((Sender as TListBox).ItemIndex = -1) then
  begin
   Self.GB_OsvOne.Visible := false;
   Exit;
  end;

 Self.E_OsvAddr.Text := IntToStr(Self.orData.Osvetleni.Data[(Sender as TListBox).ItemIndex].board);
 Self.E_OsvPort.Text := IntToStr(Self.orData.Osvetleni.Data[(Sender as TListBox).ItemIndex].port);
 Self.E_OsvName.Text := Self.orData.Osvetleni.Data[(Sender as TListBox).ItemIndex].name;

 Self.GB_OsvOne.Visible := true;
end;

procedure TF_OREdit.OpenForm(orindex:Integer);
begin
 Self.openindex := orindex;
 Self.OpenForm(F_Hlavni.Relief.ORs[orindex]);
end;

procedure TF_OREdit.OpenForm(oblRizeni:TOR);
begin
 Self.orData    := oblRizeni;

 Self.GB_OsvOne.Visible := false;

 Self.E_Name.Text      := Self.orData.Name;
 Self.E_NameShort.Text := Self.orData.ShortName;
 Self.E_NameUniq.Text  := Self.orData.id;

 Self.CB_Lichy.ItemIndex := Self.orData.Lichy;
 if (Self.orData.Poss.DKOr = 0) then Self.RB_OR1.Checked := true;
 if (Self.orData.Poss.DKOr = 1) then Self.RB_OR2.Checked := true;

 Self.CHB_ModCasStart.Checked := Self.orData.Rights.ModCasStart;
 Self.CHB_ModCasStop.Checked  := Self.orData.Rights.ModCasStop;
 Self.CHB_ModCasSet.Checked   := Self.orData.Rights.ModCasSet;

 Self.SE_OsvCnt.Value := Self.orData.Osvetleni.Cnt;
 Self.SE_OsvCntChange(Self.SE_OsvCnt);

 Self.Caption := 'OØ '+Self.orData.id;
 Self.ShowModal();
end;

procedure TF_OREdit.SE_OsvCntChange(Sender: TObject);
var i:Integer;
begin
 Self.orData.Osvetleni.Cnt := (Sender as TSpinEdit).Value;
 Self.LB_Osv.Clear();
 for i := 0 to (Sender as TSpinEdit).Value-1 do Self.LB_Osv.Items.Add(IntToStr(i+1));
 Self.GB_OsvOne.Visible := false;
end;

end.//unit
