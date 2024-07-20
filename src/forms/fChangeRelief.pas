unit fChangeRelief;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, fMain, Panel, ReliefBitmap, Spin;

type
  TF_ReliefProperties = class(TForm)
    Label2: TLabel;
    Label1: TLabel;
    B_Storno: TButton;
    B_Apply: TButton;
    SE_Width: TSpinEdit;
    SE_Height: TSpinEdit;
    procedure B_StornoClick(Sender: TObject);
    procedure B_ApplyClick(Sender: TObject);
  private
    { Private declarations }
  public
    procedure OpenForm();
  end;

var
  F_ReliefProperties: TF_ReliefProperties;

implementation

{$R *.dfm}

procedure TF_ReliefProperties.OpenForm();
begin
  Self.SE_Width.Value := F_Hlavni.Relief.PanelWidth;
  Self.SE_Height.Value := F_Hlavni.Relief.PanelHeight;

  Self.ShowModal();
end;

procedure TF_ReliefProperties.B_ApplyClick(Sender: TObject);
begin
  try
    F_Hlavni.Relief.SetSize(SE_Width.Value, SE_Height.Value);
    Self.Close();
  except
    on E: Exception do
      Application.MessageBox(PChar('Chyba: ' + E.Message), 'Chyba', MB_OK OR MB_ICONWARNING);
  end;
end;

procedure TF_ReliefProperties.B_StornoClick(Sender: TObject);
begin
  Self.Close();
end;

end.// unit
