unit ProfilSelection;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  System.IOUtils; // Notwendige Units für Pfade und Typen

type
  TForm2 = class(TForm)
    ScrollBox1: TScrollBox;
    Panel1: TPanel;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  Form2: TForm2;


implementation

{$R *.dfm}


//-----------------------------------------------------------------------------
// Event-Handler für den Button-Klick
//-----------------------------------------------------------------------------
procedure TForm2.FormCreate(Sender: TObject);
begin
  // Setzen Sie ScrollBox1 so, dass es das Formular füllt
  ScrollBox1.Align := alClient;
end;

end.
