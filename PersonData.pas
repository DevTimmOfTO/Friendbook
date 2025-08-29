unit PersonData;

interface

uses
  System.Classes, System.SysUtils, Vcl.Graphics, System.Generics.Collections,
  System.JSON, System.NetEncoding, System.DateUtils;

type
  TMovieSeriesEntry = record
    Title: string;
    Year: string;
    Overview: string;
    PosterPath: string;

    // Hilfsmethoden für JSON
    function ToJSON: TJSONObject;
    procedure FromJSON(JSON: TJSONObject);
  end;

  TPerson = class
  private
    FFirstName: string;
    FSurname: string;
    FNicknames: TStringList;
    FBirthday: TDateTime;
    FProfilePicture: TBitmap;
    FFavoriteMovies: TList<TMovieSeriesEntry>;
    FFavoriteSeries: TList<TMovieSeriesEntry>;
    FDescription: string;
    FAddress1: string;
    FAddress2: string;
    FAddress3: string;
    FAddress4: string;
    FAddress5: string;
    FRelationshipStatus: string;
    FProfession: string;
    FEducation: string;
    FThoughtsAbout: string;
    FMemories: string;
    FWishes: string;

    // Hilfsmethoden für Bitmap-Konvertierung
    function BitmapToBase64(Bitmap: TBitmap): string;
    procedure Base64ToBitmap(const Base64String: string; Bitmap: TBitmap);

  public
    constructor Create;
    destructor Destroy; override;

    // Properties
    property FirstName: string read FFirstName write FFirstName;
    property Surname: string read FSurname write FSurname;
    property Nicknames: TStringList read FNicknames;
    property Birthday: TDateTime read FBirthday write FBirthday;
    property ProfilePicture: TBitmap read FProfilePicture write FProfilePicture;
    property FavoriteMovies: TList<TMovieSeriesEntry> read FFavoriteMovies;
    property FavoriteSeries: TList<TMovieSeriesEntry> read FFavoriteSeries;
    property Description: string read FDescription write FDescription;
    property Address1: string read FAddress1 write FAddress1;
    property Address2: string read FAddress2 write FAddress2;
    property Address3: string read FAddress3 write FAddress3;
    property Address4: string read FAddress4 write FAddress4;
    property Address5: string read FAddress5 write FAddress5;
    property RelationshipStatus: string read FRelationshipStatus write FRelationshipStatus;
    property Profession: string read FProfession write FProfession;
    property Education: string read FEducation write FEducation;
    property ThoughtsAbout: string read FThoughtsAbout write FThoughtsAbout;
    property Memories: string read FMemories write FMemories;
    property Wishes: string read FWishes write FWishes;

    // Existing methods
    function GetFullName: string;
    function GetAge: Integer;

    // JSON Serialization methods
    function ToJSON: TJSONObject;
    procedure FromJSON(JSON: TJSONObject);

    // Class methods für einfachere Verwendung
    class function CreateFromJSON(JSON: TJSONObject): TPerson;
  end;

var
  PersonList: TObjectList<TPerson>;

implementation

{ TMovieSeriesEntry }

function TMovieSeriesEntry.ToJSON: TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.AddPair('title', Title);
  Result.AddPair('year', Year);
  Result.AddPair('overview', Overview);
  Result.AddPair('posterPath', PosterPath);
end;

procedure TMovieSeriesEntry.FromJSON(JSON: TJSONObject);
begin
  if Assigned(JSON.GetValue('title')) then
    Title := JSON.GetValue('title').Value;
  if Assigned(JSON.GetValue('year')) then
    Year := JSON.GetValue('year').Value;
  if Assigned(JSON.GetValue('overview')) then
    Overview := JSON.GetValue('overview').Value;
  if Assigned(JSON.GetValue('posterPath')) then
    PosterPath := JSON.GetValue('posterPath').Value;
end;

{ TPerson }

constructor TPerson.Create;
begin
  inherited Create;
  FNicknames := TStringList.Create;
  FProfilePicture := TBitmap.Create;
  FFavoriteMovies := TList<TMovieSeriesEntry>.Create;
  FFavoriteSeries := TList<TMovieSeriesEntry>.Create;
  FBirthday := 0; // Empty date
end;

destructor TPerson.Destroy;
begin
  FNicknames.Free;
  FProfilePicture.Free;
  FFavoriteMovies.Free;
  FFavoriteSeries.Free;
  inherited Destroy;
end;

function TPerson.GetFullName: string;
begin
  Result := Trim(FFirstName + ' ' + FSurname);
end;

function TPerson.GetAge: Integer;
begin
  if FBirthday > 0 then
    Result := Trunc((Now - FBirthday) / 365.25)
  else
    Result := 0;
end;

function TPerson.BitmapToBase64(Bitmap: TBitmap): string;
var
  MemStream: TMemoryStream;
  Base64Stream: TStringStream;
begin
  Result := '';
  if not Assigned(Bitmap) or Bitmap.Empty then
    Exit;

  MemStream := TMemoryStream.Create;
  Base64Stream := TStringStream.Create;
  try
    Bitmap.SaveToStream(MemStream);
    MemStream.Position := 0;
    TNetEncoding.Base64.Encode(MemStream, Base64Stream);
    Result := Base64Stream.DataString;
  finally
    MemStream.Free;
    Base64Stream.Free;
  end;
end;

procedure TPerson.Base64ToBitmap(const Base64String: string; Bitmap: TBitmap);
var
  MemStream: TMemoryStream;
  Base64Stream: TStringStream;
begin
  if (Base64String = '') or not Assigned(Bitmap) then
    Exit;

  MemStream := TMemoryStream.Create;
  Base64Stream := TStringStream.Create(Base64String);
  try
    TNetEncoding.Base64.Decode(Base64Stream, MemStream);
    MemStream.Position := 0;
    Bitmap.LoadFromStream(MemStream);
  finally
    MemStream.Free;
    Base64Stream.Free;
  end;
end;

function TPerson.ToJSON: TJSONObject;
var
  I: Integer;
  NicknamesArray: TJSONArray;
  MoviesArray: TJSONArray;
  SeriesArray: TJSONArray;
  MovieEntry: TMovieSeriesEntry;
begin
  Result := TJSONObject.Create;

  // Basic Info
  Result.AddPair('firstName', FFirstName);
  Result.AddPair('surname', FSurname);
  Result.AddPair('description', FDescription);

  // Birthday als ISO 8601 String
  if FBirthday > 0 then
    Result.AddPair('birthday', DateToISO8601(FBirthday))
  else
    Result.AddPair('birthday', '');

  // Nicknames Array
  NicknamesArray := TJSONArray.Create;
  for I := 0 to FNicknames.Count - 1 do
    NicknamesArray.AddElement(TJSONString.Create(FNicknames[I]));
  Result.AddPair('nicknames', NicknamesArray);

  // Address
  Result.AddPair('address1', FAddress1);
  Result.AddPair('address2', FAddress2);
  Result.AddPair('address3', FAddress3);
  Result.AddPair('address4', FAddress4);
  Result.AddPair('address5', FAddress5);

  // Personal Info
  Result.AddPair('relationshipStatus', FRelationshipStatus);
  Result.AddPair('profession', FProfession);
  Result.AddPair('education', FEducation);

  // Personal Thoughts
  Result.AddPair('thoughtsAbout', FThoughtsAbout);
  Result.AddPair('memories', FMemories);
  Result.AddPair('wishes', FWishes);

  // Profile Picture als Base64
  Result.AddPair('profilePicture', BitmapToBase64(FProfilePicture));

  // Favorite Movies
  MoviesArray := TJSONArray.Create;
  for I := 0 to FFavoriteMovies.Count - 1 do
  begin
    MovieEntry := FFavoriteMovies[I];
    MoviesArray.AddElement(MovieEntry.ToJSON);
  end;
  Result.AddPair('favoriteMovies', MoviesArray);

  // Favorite Series
  SeriesArray := TJSONArray.Create;
  for I := 0 to FFavoriteSeries.Count - 1 do
  begin
    MovieEntry := FFavoriteSeries[I];
    SeriesArray.AddElement(MovieEntry.ToJSON);
  end;
  Result.AddPair('favoriteSeries', SeriesArray);
end;

procedure TPerson.FromJSON(JSON: TJSONObject);
var
  I: Integer;
  NicknamesArray: TJSONArray;
  MoviesArray: TJSONArray;
  SeriesArray: TJSONArray;
  MovieEntry: TMovieSeriesEntry;
  JSONValue: TJSONValue;
  BirthdayStr: string;
begin
  if not Assigned(JSON) then
    Exit;

  // Basic Info
  JSONValue := JSON.GetValue('firstName');
  if Assigned(JSONValue) then FFirstName := JSONValue.Value;

  JSONValue := JSON.GetValue('surname');
  if Assigned(JSONValue) then FSurname := JSONValue.Value;

  JSONValue := JSON.GetValue('description');
  if Assigned(JSONValue) then FDescription := JSONValue.Value;

  // Birthday
  JSONValue := JSON.GetValue('birthday');
  if Assigned(JSONValue) then
  begin
    BirthdayStr := JSONValue.Value;
    if BirthdayStr <> '' then
      FBirthday := ISO8601ToDate(BirthdayStr)
    else
      FBirthday := 0;
  end;

  // Nicknames
  FNicknames.Clear;
  JSONValue := JSON.GetValue('nicknames');
  if Assigned(JSONValue) and (JSONValue is TJSONArray) then
  begin
    NicknamesArray := JSONValue as TJSONArray;
    for I := 0 to NicknamesArray.Count - 1 do
      FNicknames.Add(NicknamesArray.Items[I].Value);
  end;

  // Address
  JSONValue := JSON.GetValue('address1');
  if Assigned(JSONValue) then FAddress1 := JSONValue.Value;

  JSONValue := JSON.GetValue('address2');
  if Assigned(JSONValue) then FAddress2 := JSONValue.Value;

  JSONValue := JSON.GetValue('address3');
  if Assigned(JSONValue) then FAddress3 := JSONValue.Value;

  JSONValue := JSON.GetValue('address4');
  if Assigned(JSONValue) then FAddress4 := JSONValue.Value;

  JSONValue := JSON.GetValue('address5');
  if Assigned(JSONValue) then FAddress5 := JSONValue.Value;

  // Personal Info
  JSONValue := JSON.GetValue('relationshipStatus');
  if Assigned(JSONValue) then FRelationshipStatus := JSONValue.Value;

  JSONValue := JSON.GetValue('profession');
  if Assigned(JSONValue) then FProfession := JSONValue.Value;

  JSONValue := JSON.GetValue('education');
  if Assigned(JSONValue) then FEducation := JSONValue.Value;

  // Personal Thoughts
  JSONValue := JSON.GetValue('thoughtsAbout');
  if Assigned(JSONValue) then FThoughtsAbout := JSONValue.Value;

  JSONValue := JSON.GetValue('memories');
  if Assigned(JSONValue) then FMemories := JSONValue.Value;

  JSONValue := JSON.GetValue('wishes');
  if Assigned(JSONValue) then FWishes := JSONValue.Value;

  // Profile Picture
  JSONValue := JSON.GetValue('profilePicture');
  if Assigned(JSONValue) and (JSONValue.Value <> '') then
    Base64ToBitmap(JSONValue.Value, FProfilePicture);

  // Favorite Movies
  FFavoriteMovies.Clear;
  JSONValue := JSON.GetValue('favoriteMovies');
  if Assigned(JSONValue) and (JSONValue is TJSONArray) then
  begin
    MoviesArray := JSONValue as TJSONArray;
    for I := 0 to MoviesArray.Count - 1 do
    begin
      MovieEntry.FromJSON(MoviesArray.Items[I] as TJSONObject);
      FFavoriteMovies.Add(MovieEntry);
    end;
  end;

  // Favorite Series
  FFavoriteSeries.Clear;
  JSONValue := JSON.GetValue('favoriteSeries');
  if Assigned(JSONValue) and (JSONValue is TJSONArray) then
  begin
    SeriesArray := JSONValue as TJSONArray;
    for I := 0 to SeriesArray.Count - 1 do
    begin
      MovieEntry.FromJSON(SeriesArray.Items[I] as TJSONObject);
      FFavoriteSeries.Add(MovieEntry);
    end;
  end;
end;

class function TPerson.CreateFromJSON(JSON: TJSONObject): TPerson;
begin
  Result := TPerson.Create;
  Result.FromJSON(JSON);
end;

initialization
  PersonList := TObjectList<TPerson>.Create(True); // True = owns objects

finalization
  PersonList.Free;

end.
