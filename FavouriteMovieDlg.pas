unit FavouriteMovieDlg;

interface

uses Winapi.Windows, System.SysUtils, System.Classes, Vcl.Graphics, Vcl.Forms,
  Vcl.Controls, Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls, Vcl.ComCtrls;

type
  TFavoriteMovieDlg = class(TForm)
    OKBtn: TButton;
    CancelBtn: TButton;
    Label1: TLabel;
    Edit1: TEdit;
    Label2: TLabel;
    ComboBoxEx1: TComboBoxEx;
    Label3: TLabel;
    Memo1: TMemo;
    Label4: TLabel;
    Edit2: TEdit;
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  FavoriteMovieDlg: TFavoriteMovieDlg;

implementation

{$R *.dfm}

end.
