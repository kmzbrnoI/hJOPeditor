unit fDataCheck;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, StrUtils;

type
  TF_DataCheck = class(TForm)
    LV_Errors: TListView;
    B_Update: TButton;
    GB_Prop: TGroupBox;
    procedure B_UpdateClick(Sender: TObject);
    procedure LV_ErrorsCustomDrawItem(Sender: TCustomListView; Item: TListItem;
      State: TCustomDrawState; var DefaultDraw: Boolean);
  private
    { Private declarations }
  public
    procedure OpenForm(str:TStrings);
  end;

var
  F_DataCheck: TF_DataCheck;

implementation

uses fMain;

{$R *.dfm}

procedure TF_DataCheck.B_UpdateClick(Sender: TObject);
begin
 F_Hlavni.MI_CheckDataClick(F_Hlavni.MI_CheckData);
end;//procedure

procedure TF_DataCheck.LV_ErrorsCustomDrawItem(Sender: TCustomListView;
  Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
begin
 Self.LV_Errors.Canvas.Brush.Color := $FFFFFF;

 if (LeftStr(Item.SubItems.Strings[0],4) = 'WARN') then
   Self.LV_Errors.Canvas.Brush.Color := $AAFFFF
 else if (LeftStr(Item.SubItems.Strings[0],3) = 'ERR') then
   Self.LV_Errors.Canvas.Brush.Color := $CCCCFF
 else if (LeftStr(Item.SubItems.Strings[0],2) = 'OK') then
   Self.LV_Errors.Canvas.Brush.Color := $CCFFCC;
end;//procedure

procedure TF_DataCheck.OpenForm(str:TStrings);
var i:Integer;
    LI:TListItem;
begin
 Self.LV_Errors.Clear();

 for i := 0 to str.Count-1 do
  begin
   LI := Self.LV_Errors.Items.Add;
   LI.Caption := IntToStr(i+1);
   LI.SubItems.Add(str[i]);
  end;//for i

 str.Free;
 Self.Show();
end;//procedure

end.//unit
