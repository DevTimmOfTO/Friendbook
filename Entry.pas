unit Entry;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls,
  Vcl.ExtCtrls, Vcl.Menus;

type
  TAddFriendFrame = class(TFrame)
    ScrollBox1: TScrollBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    EditFirstName: TEdit;
    EditSurname: TEdit;
    MemoNicknames: TMemo;
    Label4: TLabel;
    DateTimePickerBirthday: TDateTimePicker;
    Label5: TLabel;
    Button1: TButton;
    ListView1: TListView;
    Image1: TImage;
    GroupBox1: TGroupBox;
    Button2: TButton;
    PaintBox1: TPaintBox;
    PopupMenuFavMov: TPopupMenu;
    AddEntry1: TMenuItem;
    AddEntry2: TMenuItem;
    procedure Button2Click(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

implementation

{$R *.dfm}

procedure TAddFriendFrame.Button2Click(Sender: TObject);
var
  OpenDlg: TOpenDialog;
begin
// Image
   OpenDlg := TOpenDialog.Create(Self);
  try
    OpenDlg.Filter := 'Image Files|*.bmp;*.jpg;*.jpeg;*.png;*.gif';
    if OpenDlg.Execute then
      Image1.Picture.LoadFromFile(OpenDlg.FileName);
  finally
    OpenDlg.Free;
  end;
end;

end.
