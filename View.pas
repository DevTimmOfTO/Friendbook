unit View;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Math,
  Vcl.ComCtrls, PersonData, Vcl.ImgList, System.ImageList, pngimage, Vcl.Imaging.jpeg, System.Net.HttpClient, System.Net.HttpClientComponent;

type
  TFrameView = class(TFrame)
    ScrollBox1: TScrollBox;
    Panel1: TPanel;
    Image1: TImage;
    Label1: TLabel;
    LabelFullName: TLabel;
    Label3: TLabel;
    LabelAge: TLabel;
    GroupBox1: TGroupBox;
    LabelNicknames: TLabel;
    GroupBox2: TGroupBox;
    LabelDescription: TLabel;
    GroupBox3: TGroupBox;
    LabelAddress: TLabel;
    GroupBox4: TGroupBox;
    ListView1: TListView;
    GroupBox5: TGroupBox;
    ListView2: TListView;
    GroupBox6: TGroupBox;
    LabelPersonalInfo: TLabel;
    GroupBox7: TGroupBox;
    LabelThoughtsAbout: TLabel;
    GroupBox8: TGroupBox;
    LabelMemories: TLabel;
    GroupBox9: TGroupBox;
    LabelWishes: TLabel;
    ImageList1: TImageList;
    ImageList2: TImageList;
  private
    FMoviePosterImages: TImageList;
    FSeriesPosterImages: TImageList;
    procedure SetupListViews;
    procedure LoadMoviesIntoListView(Person: TPerson);
    procedure LoadSeriesIntoListView(Person: TPerson);
    function LoadPosterFromURL(const PosterURL: string; TargetImageList: TImageList): Integer;

  public
    constructor Create(AOwner: TComponent); override;
    procedure LoadPersonData(Person: TPerson);
  end;

  var
    FNetHttpClient: TNetHTTPClient;

implementation

{$R *.dfm}

constructor TFrameView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  // HTTP Client initialisieren, falls noch nicht
  if not Assigned(FNetHttpClient) then
    FNetHttpClient := TNetHTTPClient.Create(nil);

  // Dynamische ImageLists für Poster erstellen
  FMoviePosterImages := TImageList.Create(Self);
  FMoviePosterImages.Width := 92;   // entspricht ListView-Spalte
  FMoviePosterImages.Height := 138; // Verhältnis ca. 2:3 wie Poster

  FSeriesPosterImages := TImageList.Create(Self);
  FSeriesPosterImages.Width := 92;
  FSeriesPosterImages.Height := 138;

  // Jetzt die ListViews aufbauen
  SetupListViews;
end;


function TFrameView.LoadPosterFromURL(const PosterURL: string; TargetImageList: TImageList): Integer;
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


procedure TFrameView.SetupListViews;
begin
  // Movies ListView
  ListView1.ViewStyle := vsReport;
  ListView1.SmallImages := FMoviePosterImages; // dynamische ImageList
  ListView1.RowSelect := True;
  ListView1.GridLines := True;
  ListView1.ReadOnly := True;
  ListView1.Columns.Clear;

  with ListView1.Columns.Add do
    Caption := 'Poster'; Width := 92;
  with ListView1.Columns.Add do
    Caption := 'Titel'; Width := 200;
  with ListView1.Columns.Add do
    Caption := 'Jahr'; Width := 60;
  with ListView1.Columns.Add do
    Caption := 'Beschreibung'; Width := 300;

  // Series ListView
  ListView2.ViewStyle := vsReport;
  ListView2.SmallImages := FSeriesPosterImages; // dynamische ImageList
  ListView2.RowSelect := True;
  ListView2.GridLines := True;
  ListView2.ReadOnly := True;
  ListView2.Columns.Clear;

  with ListView2.Columns.Add do
    Caption := 'Poster'; Width := 92;
  with ListView2.Columns.Add do
    Caption := 'Titel'; Width := 200;
  with ListView2.Columns.Add do
    Caption := 'Jahr'; Width := 60;
  with ListView2.Columns.Add do
    Caption := 'Beschreibung'; Width := 300;
end;
procedure TFrameView.LoadPersonData(Person: TPerson);
var
  NicknamesText, AddressText, PersonalInfoText: string;
begin
  if not Assigned(Person) then
    Exit;

  // Basic info
  LabelFullName.Caption := Person.GetFullName;
  if Person.GetAge > 0 then
    LabelAge.Caption := Format('%d Jahre', [Person.GetAge])
  else
    LabelAge.Caption := 'Alter unbekannt';

  // Profile picture
  if Assigned(Person.ProfilePicture) and not Person.ProfilePicture.Empty then
  begin
    Image1.Picture.Assign(Person.ProfilePicture);
    Image1.Stretch := True;
    Image1.Proportional := True;
  end
  else
  begin
    Image1.Picture := nil;
  end;

  // Nicknames
  if Person.Nicknames.Count > 0 then
    NicknamesText := Person.Nicknames.Text
  else
    NicknamesText := 'No nicknames';
  LabelNicknames.Caption := NicknamesText;

  // Description
  if Person.Description <> '' then
    LabelDescription.Caption := Person.Description
  else
    LabelDescription.Caption := 'No description';

  // Address
  AddressText := '';
  if Person.Address1 <> '' then AddressText := AddressText + Person.Address1 + #13#10;
  if Person.Address2 <> '' then AddressText := AddressText + Person.Address2 + #13#10;
  if Person.Address3 <> '' then AddressText := AddressText + Person.Address3 + #13#10;
  if Person.Address4 <> '' then AddressText := AddressText + Person.Address4 + #13#10;
  if Person.Address5 <> '' then AddressText := AddressText + Person.Address5;

  if Trim(AddressText) = '' then
    AddressText := 'no address provided';
  LabelAddress.Caption := Trim(AddressText);

  // Personal info
  PersonalInfoText := '';
  if Person.RelationshipStatus <> '' then
    PersonalInfoText := PersonalInfoText + 'religious affiliation: ' + Person.RelationshipStatus + #13#10;
  if Person.Profession <> '' then
    PersonalInfoText := PersonalInfoText + 'profession/job/career: ' + Person.Profession + #13#10;
  if Person.Education <> '' then
    PersonalInfoText := PersonalInfoText + 'Marital status: ' + Person.Education;

  if Trim(PersonalInfoText) = '' then
    PersonalInfoText := 'Keine weiteren Informationen';
  LabelPersonalInfo.Caption := Trim(PersonalInfoText);

  // Thoughts, memories, wishes
if Person.ThoughtsAbout <> '' then
  LabelThoughtsAbout.Caption := Person.ThoughtsAbout
else
  LabelThoughtsAbout.Caption := 'Keine Gedanken eingetragen';

if Person.Memories <> '' then
  LabelMemories.Caption := Person.Memories
else
  LabelMemories.Caption := 'Keine Erinnerungen eingetragen';

if Person.Wishes <> '' then
  LabelWishes.Caption := Person.Wishes
else
  LabelWishes.Caption := 'Keine Wünsche eingetragen';


  // Load movies and series
  LoadMoviesIntoListView(Person);
  LoadSeriesIntoListView(Person);
end;

procedure TFrameView.LoadMoviesIntoListView(Person: TPerson);
var
  I: Integer;
  MovieEntry: TMovieSeriesEntry;
  ListItem: TListItem;
  PosterIndex: Integer;
  OverviewText: string;
begin
  ListView1.Clear;

  for I := 0 to Person.FavoriteMovies.Count - 1 do
  begin
    MovieEntry := Person.FavoriteMovies[I];

    // Poster laden
    PosterIndex := -1;
    if MovieEntry.PosterPath <> '' then
      PosterIndex := LoadPosterFromURL(MovieEntry.PosterPath, FMoviePosterImages);

    ListItem := ListView1.Items.Add;
    ListItem.Caption := ''; // Poster wird in erster Spalte angezeigt
    ListItem.ImageIndex := PosterIndex;

    ListItem.SubItems.Add(MovieEntry.Title);
    ListItem.SubItems.Add(MovieEntry.Year);

    OverviewText := MovieEntry.Overview;
    if Length(OverviewText) > 80 then
      OverviewText := Copy(OverviewText, 1, 77) + '...';
    ListItem.SubItems.Add(OverviewText);

    ListItem.Data := nil; // keine neuen Pfade speichern
  end;

  if Person.FavoriteMovies.Count = 0 then
  begin
    ListItem := ListView1.Items.Add;
    ListItem.Caption := 'Keine Lieblingsfilme eingetragen';
    ListItem.SubItems.Add('');
    ListItem.SubItems.Add('');
    ListItem.SubItems.Add('');
    ListItem.ImageIndex := -1;
    ListItem.Data := nil;
  end;
end;

procedure TFrameView.LoadSeriesIntoListView(Person: TPerson);
var
  I: Integer;
  SeriesEntry: TMovieSeriesEntry;
  ListItem: TListItem;
  PosterIndex: Integer;
  OverviewText: string;
begin
  ListView2.Clear;

  for I := 0 to Person.FavoriteSeries.Count - 1 do
  begin
    SeriesEntry := Person.FavoriteSeries[I];

    // Poster laden
    PosterIndex := -1;
    if SeriesEntry.PosterPath <> '' then
      PosterIndex := LoadPosterFromURL(SeriesEntry.PosterPath, FSeriesPosterImages);

    ListItem := ListView2.Items.Add;
    ListItem.Caption := ''; // Poster in erster Spalte
    ListItem.ImageIndex := PosterIndex;

    ListItem.SubItems.Add(SeriesEntry.Title);
    ListItem.SubItems.Add(SeriesEntry.Year);

    OverviewText := SeriesEntry.Overview;
    if Length(OverviewText) > 80 then
      OverviewText := Copy(OverviewText, 1, 77) + '...';
    ListItem.SubItems.Add(OverviewText);

    ListItem.Data := nil; // keine neuen Pfade speichern
  end;

  if Person.FavoriteSeries.Count = 0 then
  begin
    ListItem := ListView2.Items.Add;
    ListItem.Caption := 'Keine Lieblingsserien eingetragen';
    ListItem.SubItems.Add('');
    ListItem.SubItems.Add('');
    ListItem.SubItems.Add('');
    ListItem.ImageIndex := -1;
    ListItem.Data := nil;
  end;
end;

end.
