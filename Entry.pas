unit Entry;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls,
  Vcl.ExtCtrls, Vcl.Menus, Vcl.ImgList, System.ImageList,
  System.Net.HttpClient, System.Net.HttpClientComponent,
  Vcl.Imaging.pngimage, Vcl.Imaging.jpeg, System.Math;

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
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure AddEntry2Click(Sender: TObject);
    //procedure FrameCreate(Sender: TObject);
    procedure FrameDestroy(Sender: TObject);
    constructor Create(AOwner: TComponent); override;
  private
    FMoviePosterImages: TImageList;
    FNetHttpClient: TNetHTTPClient;
    function LoadPosterFromURL(const PosterURL: string): Integer;
    procedure SetupMovieListView;
  public

    { Public declarations }
  end;

implementation

{$R *.dfm}

uses FavouriteMovieDlg;

constructor TAddFriendFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  // Create ImageList for movie posters
  FMoviePosterImages := TImageList.Create(Self);
  FMoviePosterImages.Width := 92;   // Smaller for the entry list
  FMoviePosterImages.Height := 138;  // Proportional
  FMoviePosterImages.ColorDepth := cd32Bit;

  // Create HTTP client for poster loading
  FNetHttpClient := TNetHTTPClient.Create(Self);

  // Setup ListView properly
  SetupMovieListView;
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

procedure TAddFriendFrame.SetupMovieListView;
begin
  if not Assigned(ListView1) then
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
end;

function TAddFriendFrame.LoadPosterFromURL(const PosterURL: string): Integer;
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

  if (PosterURL = '') or not Assigned(FNetHttpClient) or not Assigned(FMoviePosterImages) then
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
          Bmp.SetSize(FMoviePosterImages.Width, FMoviePosterImages.Height);
          Bmp.Canvas.Brush.Color := clWhite;
          Bmp.Canvas.FillRect(Rect(0, 0, Bmp.Width, Bmp.Height));

          // Calculate proportional scaling
          if (SourceBmp.Width > 0) and (SourceBmp.Height > 0) then
          begin
            ScaleX := FMoviePosterImages.Width / SourceBmp.Width;
            ScaleY := FMoviePosterImages.Height / SourceBmp.Height;
            Scale := Min(ScaleX, ScaleY);

            NewWidth := Round(SourceBmp.Width * Scale);
            NewHeight := Round(SourceBmp.Height * Scale);
            OffsetX := (FMoviePosterImages.Width - NewWidth) div 2;
            OffsetY := (FMoviePosterImages.Height - NewHeight) div 2;

            Bmp.Canvas.StretchDraw(
              Rect(OffsetX, OffsetY, OffsetX + NewWidth, OffsetY + NewHeight),
              SourceBmp);
          end;

          Result := FMoviePosterImages.Add(Bmp, nil);
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
      // Debug: Check if we got data
      if MovieDlg.SelectedMovieTitle = '' then
      begin
        ShowMessage('No movie data received!');
        Exit;
      end;

      // Load poster image if available
      PosterImageIndex := -1;
      if MovieDlg.SelectedMoviePosterPath <> '' then
        PosterImageIndex := LoadPosterFromURL(MovieDlg.SelectedMoviePosterPath);

      // Add movie to ListView with proper structure
      ListItem := ListView1.Items.Add;
      ListItem.Caption := ''; // Poster column (empty, image shows here)

      // Add SubItems for each column
      ListItem.SubItems.Add(MovieDlg.SelectedMovieTitle);  // Title
      ListItem.SubItems.Add(MovieDlg.SelectedMovieYear);   // Year

      // Truncate overview if too long
      Overview := MovieDlg.SelectedMovieOverview;
      if Length(Overview) > 80 then
        Overview := Copy(Overview, 1, 77) + '...';
      ListItem.SubItems.Add(Overview);  // Description

      // Set poster image
      ListItem.ImageIndex := PosterImageIndex;

      // Store poster path for cleanup
      if MovieDlg.SelectedMoviePosterPath <> '' then
        ListItem.Data := Pointer(StrNew(PChar(MovieDlg.SelectedMoviePosterPath)))
      else
        ListItem.Data := nil;

      // Debug info
      ShowMessage(Format('Added movie: %s (%s)', [MovieDlg.SelectedMovieTitle, MovieDlg.SelectedMovieYear]));
    end;
  finally
    MovieDlg.Free;
  end;
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

procedure TAddFriendFrame.Button2Click(Sender: TObject);
var
  OpenDlg: TOpenDialog;
begin
  // Image upload
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
