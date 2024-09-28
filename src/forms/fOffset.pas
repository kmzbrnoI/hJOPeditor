unit fOffset;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Samples.Spin;

type
  TFormOffsetResult = record
    success: Boolean;
    offset: TPoint;
  end;

  TF_Offset = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    SE_X: TSpinEdit;
    SE_Y: TSpinEdit;
    Label3: TLabel;
    B_OK: TButton;
    B_Cancel: TButton;
    procedure B_OKClick(Sender: TObject);
    procedure B_CancelClick(Sender: TObject);
  private
    success: Boolean;

  public
    function Offset(): TFormOffsetResult;

  end;

var
  F_Offset: TF_Offset;

implementation

{$R *.dfm}

procedure TF_Offset.B_CancelClick(Sender: TObject);
begin
  Self.Close();
end;

procedure TF_Offset.B_OKClick(Sender: TObject);
begin
  Self.success := True;
  Self.Close();
end;

function TF_Offset.Offset(): TFormOffsetResult;
begin
  Self.success := False;
  Self.SE_X.Value := 0;
  Self.SE_Y.Value := 0;
  Self.ShowModal();

  Result.success := Self.success;
  Result.offset.X := Self.SE_X.Value;
  Result.offset.Y := Self.SE_Y.Value;
end;

end.
