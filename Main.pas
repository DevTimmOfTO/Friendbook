unit Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Entry, View,
  Winapi.WebView2, Winapi.ActiveX, Vcl.Edge, Vcl.OleCtrls, SHDocVw,
  Vcl.WinXCtrls, Vcl.Menus;

type
  TForm1 = class(TForm)
    ButtonAddAFriend: TButton;
    Panel1: TPanel;
    ButtonEditOwnSite: TButton;
    ButtonExport: TButton;
    ButtonLookFriendbook: TButton;
    GroupBox1: TGroupBox;
    ButtonEditFriendSite: TButton;
    ButtonPrint: TButton;
    Panel2: TPanel;
    SearchBox1: TSearchBox;
    MainMenu1: TMainMenu;
    Datei1: TMenuItem;
    Datei2: TMenuItem;
    Profilladen1: TMenuItem;
    N1: TMenuItem;
    RecentProjects1: TMenuItem;
    N2: TMenuItem;
    Exit1: TMenuItem;
    Language1: TMenuItem;
    GermanDeutsch1: TMenuItem;
    EnglishEnglisch1: TMenuItem;
    Extras1: TMenuItem;
    Pickrandompersonthefriendshipbook1: TMenuItem;
    GivearandomTorontoFunFact1: TMenuItem;
    N3: TMenuItem;
    About1: TMenuItem;
    imm1: TMenuItem;
    Toronto1: TMenuItem;
    procedure ButtonSaveClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ButtonViewNextClick(Sender: TObject);
    procedure ButtonViewPreviouseClick(Sender: TObject);
    procedure ButtonExitAllClick(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure ButtonAddAFriendClick(Sender: TObject);
  private
    { Private-Deklarationen }
  public

    { Public-Deklarationen }
  end;

  TPersonality = class
    Surname: string;
    Prename: string;
  end;

var
  Form1: TForm1;
  Persona: array of TPersonality;
  i1, IndexOfView : integer;


implementation

{$R *.dfm}

procedure TForm1.ButtonAddAFriendClick(Sender: TObject);
var
  AddFriendFrame: TAddFriendFrame;
begin
//

   AddFriendFrame := TAddFriendFrame.Create(Self);
   AddFriendFrame.Parent := Panel2;
   AddFriendFrame.Align := alClient;
end;

procedure TForm1.ButtonExitAllClick(Sender: TObject);
begin
  Application.Terminate()
end;

procedure TForm1.ButtonSaveClick(Sender: TObject);

begin



  // Add Save func here
end;

procedure TForm1.ButtonViewNextClick(Sender: TObject);
begin
  IndexOfView:=IndexOfView+1;
end;

procedure TForm1.ButtonViewPreviouseClick(Sender: TObject);
begin
  IndexOfView:=IndexOfView-1;
end;


procedure TForm1.Exit1Click(Sender: TObject);
begin
 //

end;

procedure TForm1.FormCreate(Sender: TObject);
begin
//
end;

end.
