unit Entry;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls,
  Vcl.ExtCtrls, Vcl.Menus, Vcl.ImgList, System.ImageList,
  System.Net.HttpClient, System.Net.HttpClientComponent,
  Vcl.Imaging.pngimage, Vcl.Imaging.jpeg, System.Math, FavouriteMovieDlg, FavouriteSeriesDlg, ImageUploadDlg, PersonData;
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
    ImageList1: TImageList;
    Label6: TLabel;
    ListView2: TListView;
    Button3: TButton;
    PopupMenuFavSeries: TPopupMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    GroupBox2: TGroupBox;
    Label7: TLabel;
    Memo1: TMemo;
    GroupBox3: TGroupBox;
    Label8: TLabel;
    Edit1: TEdit;
    Label9: TLabel;
    Edit2: TEdit;
    Label10: TLabel;
    Edit3: TEdit;
    Label11: TLabel;
    Edit4: TEdit;
    Label12: TLabel;
    Edit5: TEdit;
    GroupBox4: TGroupBox;
    Label13: TLabel;
    ComboBox1: TComboBox;
    Label14: TLabel;
    Edit6: TEdit;
    Label15: TLabel;
    ComboBox2: TComboBox;
    Memo2: TMemo;
    Memo3: TMemo;
    Label16: TLabel;
    Memo4: TMemo;
    Label17: TLabel;
    Button4: TButton;
    Label18: TLabel;
    Memo5: TMemo;
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure AddEntry2Click(Sender: TObject);
    procedure AddEntry3Click(Sender: TObject);
    //procedure FrameCreate(Sender: TObject);
    procedure FrameDestroy(Sender: TObject);
    constructor Create(AOwner: TComponent); override;
    procedure Button4Click(Sender: TObject);
  private
    FMoviePosterImages, FSeriesPosterImages: TImageList;
    FNetHttpClient: TNetHTTPClient;
    FEditMode: Boolean;
    FEditingPersonIndex: Integer;
    FEditingPerson: TPerson;
    function LoadPosterFromURL(const PosterURL: string; TargetImageList: TImageList): Integer;  // Parameter hinzugefügt
    procedure SetupMovieNSeriesListView;
    procedure ClearForm;
  public
    procedure LoadPersonForEdit(Person: TPerson; PersonIndex: Integer);
    procedure SetEditMode(EditMode: Boolean);
    { Public declarations }
  end;
  //var


implementation

{$R *.dfm}



constructor TAddFriendFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

    FEditMode := False;
  FEditingPersonIndex := -1;
  FEditingPerson := nil;
  // Create ImageList for movie posters
  FMoviePosterImages := TImageList.Create(Self);
  FMoviePosterImages.Width := 92;   // Smaller for the entry list
  FMoviePosterImages.Height := 138;  // Proportional
  FMoviePosterImages.ColorDepth := cd32Bit;

  // Create ImageList for movie posters
  FSeriesPosterImages := TImageList.Create(Self);
  FSeriesPosterImages.Width := 92;   // Smaller for the entry list
  FSeriesPosterImages.Height := 138;  // Proportional
  FSeriesPosterImages.ColorDepth := cd32Bit;

  // Create HTTP client for poster loading
  FNetHttpClient := TNetHTTPClient.Create(Self);

  // Setup ListView properly
  SetupMovieNSeriesListView;
end;

procedure TAddFriendFrame.LoadPersonForEdit(Person: TPerson; PersonIndex: Integer);
var
  I: Integer;
  ListItem: TListItem;
  MovieEntry: TMovieSeriesEntry;
  SeriesEntry: TMovieSeriesEntry;
  PosterIndex: Integer;
  Overview: string;
begin
  if not Assigned(Person) then
    Exit;

  FEditMode := True;
  FEditingPersonIndex := PersonIndex;
  FEditingPerson := Person;

  // Lade die Basisdaten
  EditFirstName.Text := Person.FirstName;
  EditSurname.Text := Person.Surname;
  DateTimePickerBirthday.Date := Person.Birthday;

  // Nicknames
  MemoNicknames.Text := Person.Nicknames.Text;

  // Profilbild
  if Assigned(Person.ProfilePicture) and not Person.ProfilePicture.Empty then
  begin
    Image1.Picture.Assign(Person.ProfilePicture);
    Image1.Stretch := True;
    Image1.Proportional := True;
  end;

  // Beschreibung
  Memo5.Text := Person.Description;

  // Adresse
  Edit1.Text := Person.Address1;
  Edit2.Text := Person.Address2;
  Edit3.Text := Person.Address3;
  Edit4.Text := Person.Address4;
  Edit5.Text := Person.Address5;

  // Beziehung & Beruf
  ComboBox1.Text := Person.RelationshipStatus;
  Edit6.Text := Person.Profession;
  ComboBox2.Text := Person.Education;

  // Zusätzliche Infos
  Memo2.Text := Person.ThoughtsAbout;
  Memo3.Text := Person.Memories;
  Memo4.Text := Person.Wishes;

  // Filme laden
  ListView1.Clear;
  FMoviePosterImages.Clear;
  for I := 0 to Person.FavoriteMovies.Count - 1 do
  begin
    MovieEntry := Person.FavoriteMovies[I];

    PosterIndex := -1;
    if MovieEntry.PosterPath <> '' then
      PosterIndex := LoadPosterFromURL(MovieEntry.PosterPath, FMoviePosterImages);

    ListItem := ListView1.Items.Add;
    ListItem.Caption := '';
    ListItem.SubItems.Add(MovieEntry.Title);
    ListItem.SubItems.Add(MovieEntry.Year);

    Overview := MovieEntry.Overview;
    if Length(Overview) > 80 then
      Overview := Copy(Overview, 1, 77) + '...';
    ListItem.SubItems.Add(Overview);

    ListItem.ImageIndex := PosterIndex;

    if MovieEntry.PosterPath <> '' then
      ListItem.Data := Pointer(StrNew(PChar(MovieEntry.PosterPath)))
    else
      ListItem.Data := nil;
  end;

  // Serien laden
  ListView2.Clear;
  FSeriesPosterImages.Clear;
  for I := 0 to Person.FavoriteSeries.Count - 1 do
  begin
    SeriesEntry := Person.FavoriteSeries[I];

    PosterIndex := -1;
    if SeriesEntry.PosterPath <> '' then
      PosterIndex := LoadPosterFromURL(SeriesEntry.PosterPath, FSeriesPosterImages);

    ListItem := ListView2.Items.Add;
    ListItem.Caption := '';
    ListItem.SubItems.Add(SeriesEntry.Title);
    ListItem.SubItems.Add(SeriesEntry.Year);

    Overview := SeriesEntry.Overview;
    if Length(Overview) > 80 then
      Overview := Copy(Overview, 1, 77) + '...';
    ListItem.SubItems.Add(Overview);

    ListItem.ImageIndex := PosterIndex;

    if SeriesEntry.PosterPath <> '' then
      ListItem.Data := Pointer(StrNew(PChar(SeriesEntry.PosterPath)))
    else
      ListItem.Data := nil;
  end;

  // Button Text ändern
  Button4.Caption := 'Änderungen speichern';
end;

procedure TAddFriendFrame.SetEditMode(EditMode: Boolean);
begin
  FEditMode := EditMode;
  if EditMode then
    Button4.Caption := 'Änderungen speichern'
  else
  begin
    Button4.Caption := 'Person speichern';
    FEditingPerson := nil;
    FEditingPersonIndex := -1;
  end;
end;

procedure TAddFriendFrame.FrameDestroy(Sender: TObject);
var
  I: Integer;
begin
  // Cleanup movie poster paths stored in ListView.Data
  if Assigned(ListView1) and Assigned(ListView1.Items) then
  begin
    for I := 0 to ListView1.Items.Count - 1 do
    begin
      if Assigned(ListView1.Items[I]) and (ListView1.Items[I].Data <> nil) then
        StrDispose(PChar(ListView1.Items[I].Data));
    end;
  end;

  // Cleanup components (automatic with Owner, but to be safe)
  if Assigned(FNetHttpClient) then
    FNetHttpClient := nil;
  if Assigned(FMoviePosterImages) then
    FMoviePosterImages := nil;
end;

procedure TAddFriendFrame.SetupMovieNSeriesListView;
begin
  if not Assigned(ListView1) then
    Exit;

  if not Assigned(ListView2) then
    Exit;

  ListView1.ViewStyle := vsReport;
  ListView1.SmallImages := FMoviePosterImages;
  ListView1.RowSelect := True;
  ListView1.GridLines := True;
  ListView1.DoubleBuffered := True;

  // Setup columns
  ListView1.Columns.Clear;
  with ListView1.Columns.Add do
  begin
    Caption := 'Poster';
    Width := 92;
  end;
  with ListView1.Columns.Add do
  begin
    Caption := 'Title';
    Width := 150;
  end;
  with ListView1.Columns.Add do
  begin
    Caption := 'Year';
    Width := 60;
  end;
  with ListView1.Columns.Add do
  begin
    Caption := 'Description';
    Width := 200;
  end;


  ListView2.ViewStyle := vsReport;
  ListView2.SmallImages := FSeriesPosterImages;
  ListView2.RowSelect := True;
  ListView2.GridLines := True;
  ListView2.DoubleBuffered := True;

    // Setup columns
  ListView2.Columns.Clear;
  with ListView2.Columns.Add do
  begin
    Caption := 'Poster';
    Width := 92;
  end;
  with ListView2.Columns.Add do
  begin
    Caption := 'Title';
    Width := 150;
  end;
  with ListView2.Columns.Add do
  begin
    Caption := 'Year';
    Width := 60;
  end;
  with ListView2.Columns.Add do
  begin
    Caption := 'Description';
    Width := 200;
  end;

end;

// Updated function signature to accept ImageList parameter
function TAddFriendFrame.LoadPosterFromURL(const PosterURL: string; TargetImageList: TImageList): Integer;
var
  PosterStream: TMemoryStream;
  Jpg: TJPEGImage;
  PNG: TPngImage;
  Bmp: TBitmap;
  SourceBmp: TBitmap;
  Buffer: array[0..1] of Byte;
  ScaleX, ScaleY, Scale: Double;
  NewWidth, NewHeight, OffsetX, OffsetY: Integer;
begin
  Result := -1; // No image

  if (PosterURL = '') or not Assigned(FNetHttpClient) or not Assigned(TargetImageList) then
    Exit;

  PosterStream := TMemoryStream.Create;
  try
    try
      FNetHttpClient.Get('https://image.tmdb.org/t/p/w92' + PosterURL, PosterStream);
      if PosterStream.Size < 2 then
        Exit;

      PosterStream.Position := 0;
      SourceBmp := TBitmap.Create;
      try
        // Read first 2 bytes to detect image type
        PosterStream.ReadBuffer(Buffer, 2);
        PosterStream.Position := 0;

        // Check if it's a JPEG (starts with FF D8)
        if (Buffer[0] = $FF) and (Buffer[1] = $D8) then
        begin
          Jpg := TJPEGImage.Create;
          try
            Jpg.LoadFromStream(PosterStream);
            SourceBmp.Assign(Jpg);
          finally
            Jpg.Free;
          end;
        end
        else
        begin
          PNG := TPngImage.Create;
          try
            PNG.LoadFromStream(PosterStream);
            SourceBmp.Assign(PNG);
          finally
            PNG.Free;
          end;
        end;

        // Create target bitmap with correct size
        Bmp := TBitmap.Create;
        try
          Bmp.PixelFormat := pf32bit;
          Bmp.SetSize(TargetImageList.Width, TargetImageList.Height);
          Bmp.Canvas.Brush.Color := clWhite;
          Bmp.Canvas.FillRect(Rect(0, 0, Bmp.Width, Bmp.Height));

          // Calculate proportional scaling
          if (SourceBmp.Width > 0) and (SourceBmp.Height > 0) then
          begin
            ScaleX := TargetImageList.Width / SourceBmp.Width;
            ScaleY := TargetImageList.Height / SourceBmp.Height;
            Scale := Min(ScaleX, ScaleY);

            NewWidth := Round(SourceBmp.Width * Scale);
            NewHeight := Round(SourceBmp.Height * Scale);
            OffsetX := (TargetImageList.Width - NewWidth) div 2;
            OffsetY := (TargetImageList.Height - NewHeight) div 2;

            Bmp.Canvas.StretchDraw(
              Rect(OffsetX, OffsetY, OffsetX + NewWidth, OffsetY + NewHeight),
              SourceBmp);
          end;

          // Add to the correct ImageList
          Result := TargetImageList.Add(Bmp, nil);
        finally
          Bmp.Free;
        end;
      finally
        SourceBmp.Free;
      end;
    except
      // Silently handle poster loading errors
      Result := -1;
    end;
  finally
    PosterStream.Free;
  end;
end;

procedure TAddFriendFrame.Button1Click(Sender: TObject);
var
  MovieDlg: TFavoriteMovieDlg;
  ListItem: TListItem;
  PosterImageIndex: Integer;
  Overview: string;
begin
  MovieDlg := TFavoriteMovieDlg.Create(Self);
  try
    if MovieDlg.ShowModal = mrOK then
    begin
      if MovieDlg.SelectedMovieTitle = '' then
      begin
        ShowMessage('No movie data received!');
        Exit;
      end;

      // Load poster image for MOVIES using FMoviePosterImages
      PosterImageIndex := -1;
      if MovieDlg.SelectedMoviePosterPath <> '' then
        PosterImageIndex := LoadPosterFromURL(MovieDlg.SelectedMoviePosterPath, FMoviePosterImages);

      ListItem := ListView1.Items.Add;
      ListItem.Caption := '';
      ListItem.SubItems.Add(MovieDlg.SelectedMovieTitle);
      ListItem.SubItems.Add(MovieDlg.SelectedMovieYear);

      Overview := MovieDlg.SelectedMovieOverview;
      if Length(Overview) > 80 then
        Overview := Copy(Overview, 1, 77) + '...';
      ListItem.SubItems.Add(Overview);

      ListItem.ImageIndex := PosterImageIndex;

      if MovieDlg.SelectedMoviePosterPath <> '' then
        ListItem.Data := Pointer(StrNew(PChar(MovieDlg.SelectedMoviePosterPath)))
      else
        ListItem.Data := nil;

      ShowMessage(Format('Added movie: %s (%s)', [MovieDlg.SelectedMovieTitle, MovieDlg.SelectedMovieYear]));
    end;
  finally
    MovieDlg.Free;
  end;
end;

procedure TAddFriendFrame.Button3Click(Sender: TObject);
var
  SeriesDlg: TFavouriteSeriesDlg;
  ListItem: TListItem;
  PosterImageIndex: Integer;
  Overview: string;
begin
  SeriesDlg := TFavouriteSeriesDlg.Create(Self);
  try
    if SeriesDlg.ShowModal = mrOK then
    begin
      if SeriesDlg.SelectedSeriesTitle = '' then
      begin
        ShowMessage('No series data received!');
        Exit;
      end;

      // Load poster image for SERIES using FSeriesPosterImages
      PosterImageIndex := -1;
      if SeriesDlg.SelectedSeriesPosterPath <> '' then
        PosterImageIndex := LoadPosterFromURL(SeriesDlg.SelectedSeriesPosterPath, FSeriesPosterImages);

      ListItem := ListView2.Items.Add;
      ListItem.Caption := '';
      ListItem.SubItems.Add(SeriesDlg.SelectedSeriesTitle);
      ListItem.SubItems.Add(SeriesDlg.SelectedSeriesYear);

      Overview := SeriesDlg.SelectedSeriesOverview;
      if Length(Overview) > 80 then
        Overview := Copy(Overview, 1, 77) + '...';
      ListItem.SubItems.Add(Overview);

      ListItem.ImageIndex := PosterImageIndex;

      if SeriesDlg.SelectedSeriesPosterPath <> '' then
        ListItem.Data := Pointer(StrNew(PChar(SeriesDlg.SelectedSeriesPosterPath)))
      else
        ListItem.Data := nil;

      ShowMessage(Format('Added series: %s (%s)', [SeriesDlg.SelectedSeriesTitle, SeriesDlg.SelectedSeriesYear]));
    end;
  finally
    SeriesDlg.Free;
  end;
end;

procedure TAddFriendFrame.Button4Click(Sender: TObject);
var
  NewPerson: TPerson;
  MovieEntry, SeriesEntry: TMovieSeriesEntry;
  I: Integer;
  ListItem: TListItem;
begin
  // Validation - check required fields
  if Trim(EditFirstName.Text) = '' then
  begin
    ShowMessage('Bitte Vorname eingeben!');
    EditFirstName.SetFocus;
    Exit;
  end;

  if Trim(EditSurname.Text) = '' then
  begin
    ShowMessage('Bitte Nachname eingeben!');
    EditSurname.SetFocus;
    Exit;
  end;

  if FEditMode and Assigned(FEditingPerson) then
  begin
    // Edit Mode - update existing person
    try
      // Update basic info
      FEditingPerson.FirstName := Trim(EditFirstName.Text);
      FEditingPerson.Surname := Trim(EditSurname.Text);
      FEditingPerson.Birthday := DateTimePickerBirthday.Date;

      // Update Nicknames
      FEditingPerson.Nicknames.Text := MemoNicknames.Text;

      // Update profile picture
      if Assigned(Image1.Picture.Graphic) then
        FEditingPerson.ProfilePicture.Assign(Image1.Picture.Graphic)
      else
        FEditingPerson.ProfilePicture.Destroy;

      // Update description
      FEditingPerson.Description := Memo5.Text;

      // Update address fields
      FEditingPerson.Address1 := Edit1.Text;
      FEditingPerson.Address2 := Edit2.Text;
      FEditingPerson.Address3 := Edit3.Text;
      FEditingPerson.Address4 := Edit4.Text;
      FEditingPerson.Address5 := Edit5.Text;

      // Update relationship & job info
      FEditingPerson.RelationshipStatus := ComboBox1.Text;
      FEditingPerson.Profession := Edit6.Text;
      FEditingPerson.Education := ComboBox2.Text;

      // Update additional info
      FEditingPerson.ThoughtsAbout := Memo2.Text;
      FEditingPerson.Memories := Memo3.Text;
      FEditingPerson.Wishes := Memo4.Text;

      // Clear and rebuild favorite movies
      FEditingPerson.FavoriteMovies.Clear;
      for I := 0 to ListView1.Items.Count - 1 do
      begin
        ListItem := ListView1.Items[I];
        if ListItem.SubItems.Count >= 3 then
        begin
          MovieEntry.Title := ListItem.SubItems[0];
          MovieEntry.Year := ListItem.SubItems[1];
          MovieEntry.Overview := ListItem.SubItems[2];
          if ListItem.Data <> nil then
            MovieEntry.PosterPath := string(PChar(ListItem.Data))
          else
            MovieEntry.PosterPath := '';
          FEditingPerson.FavoriteMovies.Add(MovieEntry);
        end;
      end;

      // Clear and rebuild favorite series
      FEditingPerson.FavoriteSeries.Clear;
      for I := 0 to ListView2.Items.Count - 1 do
      begin
        ListItem := ListView2.Items[I];
        if ListItem.SubItems.Count >= 3 then
        begin
          SeriesEntry.Title := ListItem.SubItems[0];
          SeriesEntry.Year := ListItem.SubItems[1];
          SeriesEntry.Overview := ListItem.SubItems[2];
          if ListItem.Data <> nil then
            SeriesEntry.PosterPath := string(PChar(ListItem.Data))
          else
            SeriesEntry.PosterPath := '';
          FEditingPerson.FavoriteSeries.Add(SeriesEntry);
        end;
      end;

      ShowMessage(Format('Person "%s" erfolgreich aktualisiert!',
        [FEditingPerson.GetFullName]));

      // Clear form and exit edit mode
      ClearForm;
      SetEditMode(False);

    except
      on E: Exception do
      begin
        ShowMessage('Fehler beim Aktualisieren: ' + E.Message);
      end;
    end;
  end
  else
  begin
    // Create Mode - add new person (existing code)
    NewPerson := TPerson.Create;
    try
      // Basic info
      NewPerson.FirstName := Trim(EditFirstName.Text);
      NewPerson.Surname := Trim(EditSurname.Text);
      NewPerson.Birthday := DateTimePickerBirthday.Date;

      // Nicknames
      NewPerson.Nicknames.Text := MemoNicknames.Text;

      // Profile picture
      if Assigned(Image1.Picture.Graphic) then
        NewPerson.ProfilePicture.Assign(Image1.Picture.Graphic);

      // Description
      NewPerson.Description := Memo5.Text;

      // Address fields
      NewPerson.Address1 := Edit1.Text;
      NewPerson.Address2 := Edit2.Text;
      NewPerson.Address3 := Edit3.Text;
      NewPerson.Address4 := Edit4.Text;
      NewPerson.Address5 := Edit5.Text;

      // Relationship & Job info
      NewPerson.RelationshipStatus := ComboBox1.Text;
      NewPerson.Profession := Edit6.Text;
      NewPerson.Education := ComboBox2.Text;

      // Additional info
      NewPerson.ThoughtsAbout := Memo2.Text;
      NewPerson.Memories := Memo3.Text;
      NewPerson.Wishes := Memo4.Text;

      // Copy favorite movies
      for I := 0 to ListView1.Items.Count - 1 do
      begin
        ListItem := ListView1.Items[I];
        if ListItem.SubItems.Count >= 3 then
        begin
          MovieEntry.Title := ListItem.SubItems[0];
          MovieEntry.Year := ListItem.SubItems[1];
          MovieEntry.Overview := ListItem.SubItems[2];
          if ListItem.Data <> nil then
            MovieEntry.PosterPath := string(PChar(ListItem.Data))
          else
            MovieEntry.PosterPath := '';
          NewPerson.FavoriteMovies.Add(MovieEntry);
        end;
      end;

      // Copy favorite series
      for I := 0 to ListView2.Items.Count - 1 do
      begin
        ListItem := ListView2.Items[I];
        if ListItem.SubItems.Count >= 3 then
        begin
          SeriesEntry.Title := ListItem.SubItems[0];
          SeriesEntry.Year := ListItem.SubItems[1];
          SeriesEntry.Overview := ListItem.SubItems[2];
          if ListItem.Data <> nil then
            SeriesEntry.PosterPath := string(PChar(ListItem.Data))
          else
            SeriesEntry.PosterPath := '';
          NewPerson.FavoriteSeries.Add(SeriesEntry);
        end;
      end;

      // Add to global list
      PersonList.Add(NewPerson);

      ShowMessage(Format('Person "%s" erfolgreich gespeichert! (Total: %d Personen)',
        [NewPerson.GetFullName, PersonList.Count]));

      // Clear form for next entry
      ClearForm;

    except
      on E: Exception do
      begin
        NewPerson.Free;
        ShowMessage('Fehler beim Speichern: ' + E.Message);
      end;
    end;
  end;
end;

procedure TAddFriendFrame.ClearForm;
begin
  EditFirstName.Clear;
  EditSurname.Clear;
  MemoNicknames.Clear;
  DateTimePickerBirthday.Date := Now;
  Image1.Picture := nil;
  ListView1.Clear;
  ListView2.Clear;
  Memo5.Clear;
  Edit1.Clear;
  Edit2.Clear;
  Edit3.Clear;
  Edit4.Clear;
  Edit5.Clear;
  ComboBox1.ItemIndex := -1;
  Edit6.Clear;
  ComboBox2.ItemIndex := -1;
  Memo2.Clear;
  Memo3.Clear;
  Memo4.Clear;

  // Clear ImageLists to free memory
  if Assigned(FMoviePosterImages) then
    FMoviePosterImages.Clear;
  if Assigned(FSeriesPosterImages) then
    FSeriesPosterImages.Clear;
end;

procedure TAddFriendFrame.AddEntry2Click(Sender: TObject);
begin
  if not Assigned(ListView1) or not Assigned(ListView1.Selected) then
  begin
    ShowMessage('Please select a movie to remove.');
    Exit;
  end;

  if MessageDlg('Remove selected movie from favorites?',
                mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    // Memory cleanup for stored poster path
    if ListView1.Selected.Data <> nil then
      StrDispose(PChar(ListView1.Selected.Data));

    ListView1.Selected.Delete;
  end;
end;

procedure TAddFriendFrame.AddEntry3Click(Sender: TObject);
begin
  if not Assigned(ListView2) or not Assigned(ListView2.Selected) then
  begin
    ShowMessage('Please select a series to remove.');
    Exit;
  end;

  if MessageDlg('Remove selected series from favorites?',
                mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    // Memory cleanup for stored poster path
    if ListView2.Selected.Data <> nil then
      StrDispose(PChar(ListView2.Selected.Data));

    ListView2.Selected.Delete;
  end;
end;


procedure TAddFriendFrame.Button2Click(Sender: TObject);
var
  OpenDlg: TOpenDialog;
begin
  OpenDlg := TOpenDialog.Create(Self);
  try
    OpenDlg.Title := 'Profilbild auswählen';
    OpenDlg.Filter := 'Bilddateien|*.bmp;*.jpg;*.jpeg;*.png;*.gif|' +
                     'JPEG (*.jpg;*.jpeg)|*.jpg;*.jpeg|' +
                     'PNG (*.png)|*.png|' +
                     'Bitmap (*.bmp)|*.bmp|' +
                     'Alle Dateien (*.*)|*.*';
    OpenDlg.FilterIndex := 1;
    OpenDlg.Options := [ofPathMustExist, ofFileMustExist, ofEnableSizing];

    if OpenDlg.Execute then
    begin
      try
        Image1.Picture.LoadFromFile(OpenDlg.FileName);

        // Optional: Bildgröße anpassen
        Image1.Stretch := True;
        Image1.Proportional := True;
        Image1.Center := True;

        // Debug-Info (kann später entfernt werden)
        ShowMessage('Bild erfolgreich geladen: ' + ExtractFileName(OpenDlg.FileName));

      except
        on E: Exception do
        begin
          ShowMessage('Fehler beim Laden des Bildes: ' + E.Message);
        end;
      end;
    end;
  finally
    OpenDlg.Free;
  end;
end;
end.
