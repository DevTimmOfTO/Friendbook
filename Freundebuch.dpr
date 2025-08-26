program Freundebuch;

uses
  Vcl.Forms,
  Main in 'Main.pas' {Form1},
  Entry in 'Entry.pas' {AddFriendFrame: TFrame},
  View in 'View.pas' {FrameView: TFrame},
  FavouriteMovieDlg in 'FavouriteMovieDlg.pas' {FavoriteMovieDlg};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TFavoriteMovieDlg, FavoriteMovieDlg);
  Application.Run;
end.
