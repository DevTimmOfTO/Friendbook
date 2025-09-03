{*******************************************************************}
{* This file is part of Friendshipbook.                            *}
{*                                                                 *}
{* Copyright (c) 2025 Timm Johannes Göring                         *}
{* This software is licensed under the MIT License.                *}
{* For the full license text, see the LICENSE file in the          *}
{* project root directory.                                         *}
{*******************************************************************}

program friendshipbook;

uses
  Vcl.Forms,
  Main in 'Main.pas' {Form1},
  Entry in 'Entry.pas' {AddFriendFrame: TFrame},
  View in 'View.pas' {FrameView: TFrame},
  FavouriteMovieDlg in 'FavouriteMovieDlg.pas' {FavoriteMovieDlg},
  FavouriteSeriesDlg in 'FavouriteSeriesDlg.pas' {FavouriteSeriesDlg},
  PersonData in 'PersonData.pas',
  ABOUT in 'ABOUT.pas' {AboutBox},
  LanguageConfigurator in 'LanguageConfigurator.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TFavoriteMovieDlg, FavoriteMovieDlg);
  Application.CreateForm(TFavouriteSeriesDlg, FavoriteSeriesDlg);
  Application.CreateForm(TAboutBox, AboutBox);
  Application.Run;
end.

