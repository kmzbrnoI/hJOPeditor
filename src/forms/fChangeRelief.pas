unit fChangeRelief;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, fMain, Panel, ReliefBitmap;

type
  TF_ReliefProperties = class(TForm)
    Label2: TLabel;
    Label1: TLabel;
    E_Width: TEdit;
    E_Height: TEdit;
    B_Storno: TButton;
    B_Apply: TButton;
    procedure E_WidthKeyPress(Sender: TObject; var Key: Char);
    procedure B_StornoClick(Sender: TObject);
    procedure B_ApplyClick(Sender: TObject);
  private
    { Private declarations }
  public
   procedure OpenForm;
  end;

var
  F_ReliefProperties: TF_ReliefProperties;

implementation

{$R *.dfm}

procedure TF_ReliefProperties.OpenForm;
begin
 Self.E_Width.Text  := IntToStr(F_Hlavni.Relief.PanelWidth);
 Self.E_Height.Text := IntToStr(F_Hlavni.Relief.PanelHeight);

 Self.ShowModal;
end;

procedure TF_ReliefProperties.B_ApplyClick(Sender: TObject);
begin
 try
   F_Hlavni.Relief.SetRozmery(StrToIntDef(E_Width.Text,0), StrToIntDef(E_Height.Text,0));
   Self.Close();
 except
   on E:Exception do
     Application.MessageBox(PChar('Chyba: ' + E.Message), 'Chyba', MB_OK OR MB_ICONWARNING);
 end;
end;

procedure TF_ReliefProperties.B_StornoClick(Sender: TObject);
begin
 Self.Close;
end;

procedure TF_ReliefProperties.E_WidthKeyPress(Sender: TObject;
  var Key: Char);
begin
 case Key of
  '0'..'9',#9,#8:;
  #$1B:B_StornoClick(Self);
  else
   Key := #0;
  end;
end;

end.//unit
