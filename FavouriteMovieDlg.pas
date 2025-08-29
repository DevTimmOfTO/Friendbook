unit FavouriteMovieDlg;

interface

uses
  System.SysUtils, System.Classes, System.JSON, System.Math,
  System.NetEncoding,  Winapi.Windows,
  System.Net.URLClient, System.Net.HttpClient, System.Net.HttpClientComponent,
  Vcl.Forms, Vcl.Controls, Vcl.Graphics, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls,
  Vcl.ImgList, Vcl.Imaging.pngimage, Vcl.Imaging.jpeg, System.ImageList;

type
  TFavoriteMovieDlg = class(TForm)
    OKBtn: TButton;
    CancelBtn: TButton;
    Label1: TLabel;
    Edit1: TEdit;
    Button1: TButton;
    NetHTTPClientFavMov: TNetHTTPClient;
    ListView1: TListView;
    ImageList1: TImageList;
    Label2: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ListView1Data(Sender: TObject; Item: TListItem);
    procedure ListView1SelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure OKBtnClick(Sender: TObject);
    procedure Edit1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    FPosterImages: TImageList;
    FMovieData: TArray<TJSONObject>; // Store movie data for virtual ListView
    FPosterCache: TArray<Integer>;   // Cache for poster image indices
    procedure ClearMovieData;
    function LoadPosterFromURL(const PosterURL: string): Integer;
    procedure SetupListView;
  public
    // Properties to get selected movie data
    SelectedMovieTitle: string;
    SelectedMovieYear: string;
    SelectedMovieOverview: string;
    SelectedMoviePosterPath: string;
    function GetSelectedMovieData: Boolean;
  end;

var
  FavoriteMovieDlg: TFavoriteMovieDlg;

implementation

{$R *.dfm}

procedure TFavoriteMovieDlg.FormCreate(Sender: TObject);
begin
  // Create poster ImageList
  FPosterImages := TImageList.Create(Self);
  FPosterImages.Width := 92;   // TMDb w92 poster width
  FPosterImages.Height := 138; // Proportional height for movie posters
  FPosterImages.ColorDepth := cd32Bit;

  SetupListView;
end;

procedure TFavoriteMovieDlg.FormDestroy(Sender: TObject);
begin
  ClearMovieData;
  if Assigned(FPosterImages) then
    FPosterImages.Free;
end;

procedure TFavoriteMovieDlg.SetupListView;
begin
  ListView1.ViewStyle := vsReport;
  ListView1.SmallImages := FPosterImages;
  ListView1.RowSelect := True;
  ListView1.ReadOnly := True;
  ListView1.GridLines := True;
  ListView1.FullDrag := False;
  ListView1.DoubleBuffered := True;  // Anti-Flicker!

  // Setup columns
  ListView1.Columns.Clear;
  with ListView1.Columns.Add do
  begin
    Caption := 'Poster';
    Width := 100;
  end;
  with ListView1.Columns.Add do
  begin
    Caption := 'Title';
    Width := 200;
  end;
  with ListView1.Columns.Add do
  begin
    Caption := 'Release Year';
    Width := 100;
  end;
  with ListView1.Columns.Add do
  begin
    Caption := 'Overview';
    Width := 300;
  end;
end;

procedure TFavoriteMovieDlg.ClearMovieData;
var
  I: Integer;
begin
  for I := 0 to High(FMovieData) do
    if Assigned(FMovieData[I]) then
      FMovieData[I].Free;
  SetLength(FMovieData, 0);
  SetLength(FPosterCache, 0);  // Clear poster cache too
end;

procedure TFavoriteMovieDlg.Edit1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
 if Key = VK_RETURN then
  begin
    Key := 0; // prevent "ding" sound
    Button1Click(Self); // trigger your search
  end;
end;

function TFavoriteMovieDlg.LoadPosterFromURL(const PosterURL: string): Integer;
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

  if PosterURL = '' then
    Exit;

  PosterStream := TMemoryStream.Create;
  try
    try
      NetHTTPClientFavMov.Get(PosterURL, PosterStream);
      if PosterStream.Size < 2 then
        Exit;

      PosterStream.Position := 0;

      // Create source bitmap to load image into
      SourceBmp := TBitmap.Create;
      try
        // Read first 2 bytes to detect image type
        PosterStream.ReadBuffer(Buffer, 2);
        PosterStream.Position := 0;

        // Check if it's a JPEG (starts with FF D8)
        if (Buffer[0] = $FF) and (Buffer[1] = $D8) then
        begin
          // It's a JPEG
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
          // Try as PNG
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
          Bmp.SetSize(FPosterImages.Width, FPosterImages.Height);
          Bmp.Canvas.Brush.Color := clWhite;
          Bmp.Canvas.FillRect(Rect(0, 0, Bmp.Width, Bmp.Height));

          // Calculate proportional scaling
          if (SourceBmp.Width > 0) and (SourceBmp.Height > 0) then
          begin
            ScaleX := FPosterImages.Width / SourceBmp.Width;
            ScaleY := FPosterImages.Height / SourceBmp.Height;
            Scale := Min(ScaleX, ScaleY);

            NewWidth := Round(SourceBmp.Width * Scale);
            NewHeight := Round(SourceBmp.Height * Scale);
            OffsetX := (FPosterImages.Width - NewWidth) div 2;
            OffsetY := (FPosterImages.Height - NewHeight) div 2;

            Bmp.Canvas.StretchDraw(
              Rect(OffsetX, OffsetY, OffsetX + NewWidth, OffsetY + NewHeight),
              SourceBmp);
          end;

          Result := FPosterImages.Add(Bmp, nil);
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

procedure TFavoriteMovieDlg.ListView1Data(Sender: TObject; Item: TListItem);
var
  MovieObj: TJSONObject;
  Title, ReleaseDate, Overview, PosterPath, PosterURL: string;
  V: TJSONValue;
  ImageIndex: Integer;
begin
  if (Item.Index >= 0) and (Item.Index < Length(FMovieData)) then
  begin
    MovieObj := FMovieData[Item.Index];

    // Get movie data safely
    if not MovieObj.TryGetValue<string>('title', Title) then
      Title := 'Unknown Title';
    if not MovieObj.TryGetValue<string>('release_date', ReleaseDate) then
      ReleaseDate := '';
    if not MovieObj.TryGetValue<string>('overview', Overview) then
      Overview := '';

    // Set item data
    Item.Caption := ''; // Poster column
    Item.SubItems.Clear;
    Item.SubItems.Add(Title);

    if ReleaseDate <> '' then
      Item.SubItems.Add(Copy(ReleaseDate, 1, 4))
    else
      Item.SubItems.Add('Unknown');

    // Truncate long overviews
    if Length(Overview) > 100 then
      Overview := Copy(Overview, 1, 97) + '...';
    Item.SubItems.Add(Overview);

    // Use cached poster index if available
    if (Item.Index < Length(FPosterCache)) then
      ImageIndex := FPosterCache[Item.Index]
    else
    begin
      // Load poster if not cached yet
      ImageIndex := -1;
      V := MovieObj.Values['poster_path'];
      if (V <> nil) and not (V is TJSONNull) then
      begin
        PosterPath := V.Value;
        if PosterPath <> '' then
        begin
          PosterURL := 'https://image.tmdb.org/t/p/w92' + PosterPath;
          ImageIndex := LoadPosterFromURL(PosterURL);

          // Cache the result
          if Length(FPosterCache) <= Item.Index then
            SetLength(FPosterCache, Item.Index + 1);
          FPosterCache[Item.Index] := ImageIndex;
        end;
      end;
    end;

    Item.ImageIndex := ImageIndex;
  end;
end;

procedure TFavoriteMovieDlg.Button1Click(Sender: TObject);
const
  API_KEY = '49db8bf4953ba543a91b2567082a58d6'; // Keep this private!
var
  SearchTitle, URL, ResponseText: string;
  Resp: IHTTPResponse;
  Root: TJSONObject;
  Results: TJSONArray;
  I: Integer;
  MovieObj: TJSONObject;
begin
  SearchTitle := Trim(Edit1.Text);
  if SearchTitle = '' then
  begin
    Exit;
  end;

  // Clear previous results
  ClearMovieData;
  FPosterImages.Clear;
  ListView1.Items.Count := 0;

  // Encode search query
  SearchTitle := TNetEncoding.URL.Encode(SearchTitle);
  URL := Format('https://api.themoviedb.org/3/search/movie?query=%s&api_key=%s&language=en-US',
    [SearchTitle, API_KEY]);

  try
    Screen.Cursor := crHourGlass;
    try
      // DEBUG: Show the URL we're calling

      Resp := NetHTTPClientFavMov.Get(URL);

      // DEBUG: Show response details

      if Resp.StatusCode <> 200 then
        raise Exception.CreateFmt('TMDb API Error %d: %s',
          [Resp.StatusCode, Resp.StatusText]);

      ResponseText := Resp.ContentAsString(TEncoding.UTF8);

      // DEBUG: Show first 500 chars of response

      Root := TJSONObject(TJSONObject.ParseJSONValue(ResponseText));

      if not Assigned(Root) then
        raise Exception.Create('Invalid JSON response from TMDb API.');

      try
        // DEBUG: Show JSON structure


        Results := Root.GetValue<TJSONArray>('results');
        if not Assigned(Results) then
          raise Exception.Create('No results array found in API response.');

        // Store movie data for virtual ListView
        SetLength(FMovieData, Results.Count);
        for I := 0 to Results.Count - 1 do
        begin
          MovieObj := Results.Items[I] as TJSONObject;
          // Clone the JSON object to store it
          FMovieData[I] := TJSONObject(TJSONObject.ParseJSONValue(MovieObj.ToJSON));
        end;

        // Update ListView - with proper flicker prevention
        ListView1.Items.BeginUpdate;
        try
          ListView1.OwnerData := True;  // This was missing!
          ListView1.Items.Count := Length(FMovieData);
        finally
          ListView1.Items.EndUpdate;
        end;
        ListView1.Invalidate;


      finally
        Root.Free;
      end;

    finally
      Screen.Cursor := crDefault;
    end;

  except
    on E: Exception do
    begin
      Screen.Cursor := crDefault;
    end;
  end;
end;

procedure TFavoriteMovieDlg.ListView1SelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
  // Enable/Disable OK button based on selection
  OKBtn.Enabled := Selected ;                  // Not needed and (ListView1.Selected <> nil)
end;

procedure TFavoriteMovieDlg.OKBtnClick(Sender: TObject);
begin
  if GetSelectedMovieData then
  begin
    ModalResult := mrOK;
  end
  else
  begin

  end;
end;

function TFavoriteMovieDlg.GetSelectedMovieData: Boolean;
var
  MovieObj: TJSONObject;
  V: TJSONValue;
begin
  Result := False;

  if (ListView1.Selected = nil) or
     (ListView1.Selected.Index >= Length(FMovieData)) then
    Exit;

  MovieObj := FMovieData[ListView1.Selected.Index];

  // Get selected movie data
  if not MovieObj.TryGetValue<string>('title', SelectedMovieTitle) then
    SelectedMovieTitle := 'Unknown Title';

  if not MovieObj.TryGetValue<string>('release_date', SelectedMovieYear) then
    SelectedMovieYear := ''
  else
    SelectedMovieYear := Copy(SelectedMovieYear, 1, 4); // Just the year

  if not MovieObj.TryGetValue<string>('overview', SelectedMovieOverview) then
    SelectedMovieOverview := '';

  // Get poster path
  SelectedMoviePosterPath := '';
  V := MovieObj.Values['poster_path'];
  if (V <> nil) and not (V is TJSONNull) then
    SelectedMoviePosterPath := V.Value;

  Result := True;
end;

end.
