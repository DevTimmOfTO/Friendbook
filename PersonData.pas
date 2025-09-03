{*******************************************************************}
{* This file is part of Friendshipbook.                            *}
{*                                                                 *}
{* Copyright (c) 2025 Timm Johannes Göring                         *}
{* This software is licensed under the MIT License.                *}
{* For the full license text, see the LICENSE file in the          *}
{* project root directory.                                         *}
{*******************************************************************}

unit PersonData;

interface

uses
  System.Classes, System.SysUtils, Vcl.Graphics, System.Generics.Collections,
  System.JSON, System.NetEncoding, System.DateUtils, Vcl.Imaging.jpeg,
  Vcl.Imaging.pngimage, System.Math, System.IOUtils;

type
  TMovieSeriesEntry = record
    Title: string;
    Year: string;
    Overview: string;
    PosterPath: string;

    function ToJSON: TJSONObject;
    procedure FromJSON(JSON: TJSONObject);
  end;

  TPerson = class
  private
    FFirstName: string;
    FSurname: string;
    FNicknames: TStringList;
    FBirthday: TDateTime;
    FProfilePicture: TPicture;
    FFavoriteMovies: TList<TMovieSeriesEntry>;
    FFavoriteSeries: TList<TMovieSeriesEntry>;
    FDescription: string;
    FAddress1: string;
    FAddress2: string;
    FAddress3: string;
    FAddress4: string;
    FAddress5: string;
    FreligiousAffiliation: string;
    FProfession: string;
    FMaritalStatus: string;
    FHobbies: string;
    FVolunteerActivities: string;
    FFunFact: string;
    FProfileImageFileName: string; // Neues Feld für lokalen Dateipfad

    // LOKALE BILDVERARBEITUNG
    function GenerateImageFileName: string;
    function GetProfileImagesFolder: string;
    procedure SaveProfileImageToFile(const BasePath: string);
    procedure LoadProfileImageFromFile(const BasePath: string);
    procedure DeleteProfileImageFile(const BasePath: string);

  public
    constructor Create;
    destructor Destroy; override;

    // Properties (gleich wie vorher)
    property FirstName: string read FFirstName write FFirstName;
    property Surname: string read FSurname write FSurname;
    property Nicknames: TStringList read FNicknames;
    property Birthday: TDateTime read FBirthday write FBirthday;
    property ProfilePicture: TPicture read FProfilePicture write FProfilePicture;
    property FavoriteMovies: TList<TMovieSeriesEntry> read FFavoriteMovies;
    property FavoriteSeries: TList<TMovieSeriesEntry> read FFavoriteSeries;
    property SomethingElse: string read FDescription write FDescription;
    property Address1: string read FAddress1 write FAddress1;
    property Address2: string read FAddress2 write FAddress2;
    property Address3: string read FAddress3 write FAddress3;
    property Address4: string read FAddress4 write FAddress4;
    property Address5: string read FAddress5 write FAddress5;
    property ReligionsAfflication: string read FreligiousAffiliation write FreligiousAffiliation;
    property Profession: string read FProfession write FProfession;
    property MaritalStatus: string read FMaritalStatus write FMaritalStatus;
    property Hobbies: string read FHobbies write FHobbies;
    property VolunteerActivities: string read FVolunteerActivities write FVolunteerActivities;
    property FunFact: string read FFunFact write FFunFact;
    property ProfileImageFileName: string read FProfileImageFileName write FProfileImageFileName;

    function GetFullName: string;
    function GetAge: Integer;

    // Lokale JSON Serialization (ohne Bilder)
    function ToJSON(const BasePath: string = ''): TJSONObject;
    procedure FromJSON(JSON: TJSONObject; const BasePath: string = '');
    class function CreateFromJSON(JSON: TJSONObject; const BasePath: string = ''): TPerson;
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
  // Overview wird gekürzt um Platz zu sparen
  if Length(Overview) > 100 then
    Result.AddPair('overview', Copy(Overview, 1, 97) + '...')
  else
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
  FProfilePicture := TPicture.Create;
  FFavoriteMovies := TList<TMovieSeriesEntry>.Create;
  FFavoriteSeries := TList<TMovieSeriesEntry>.Create;
  FBirthday := 0;
  FProfileImageFileName := '';
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

// === LOKALE BILDVERARBEITUNG ===

function TPerson.GenerateImageFileName: string;
var
  SafeName: string;
  I: Integer;
  Ch: Char;
begin
  // Sichere Dateinamen aus Name generieren
  SafeName := GetFullName;
  if SafeName = '' then
    SafeName := 'Unknown_Person';

  // Ungültige Zeichen ersetzen
  for I := 1 to Length(SafeName) do
  begin
    Ch := SafeName[I];
    if not (Ch in ['A'..'Z', 'a'..'z', '0'..'9', '_', '-', ' ']) then
      SafeName[I] := '_';
  end;

  SafeName := StringReplace(SafeName, ' ', '_', [rfReplaceAll]);

  // Eindeutigen Namen mit Timestamp erzeugen
  Result := SafeName + '_' + FormatDateTime('yyyy-mm-dd_hh-nn-ss', Now) + '.bmp';
end;

function TPerson.GetProfileImagesFolder: string;
begin
  Result := TPath.Combine(TPath.GetDocumentsPath, 'FreundschaftsbuchApp');
  Result := TPath.Combine(Result, 'ProfileImages');
end;

procedure TPerson.SaveProfileImageToFile(const BasePath: string);
var
  ImageFolder, FullPath: string;
  BmpImage: TBitmap;
begin
  if not Assigned(FProfilePicture) or not Assigned(FProfilePicture.Graphic) or FProfilePicture.Graphic.Empty then
  begin
    FProfileImageFileName := '';
    Exit;
  end;

  // Bestimme Zielpfad
  if BasePath <> '' then
    ImageFolder := TPath.Combine(ExtractFilePath(BasePath), 'ProfileImages')
  else
    ImageFolder := GetProfileImagesFolder;

  // Ordner erstellen falls nicht vorhanden
  if not TDirectory.Exists(ImageFolder) then
    TDirectory.CreateDirectory(ImageFolder);

  // Altes Bild löschen falls vorhanden
  if FProfileImageFileName <> '' then
    DeleteProfileImageFile(BasePath);

  // Neuen Dateinamen generieren
  FProfileImageFileName := GenerateImageFileName;
  FullPath := TPath.Combine(ImageFolder, FProfileImageFileName);

  // Bild als BMP speichern
  BmpImage := TBitmap.Create;
  try
    BmpImage.Assign(FProfilePicture.Graphic);
    BmpImage.SaveToFile(FullPath);
  finally
    BmpImage.Free;
  end;
end;

procedure TPerson.LoadProfileImageFromFile(const BasePath: string);
var
  ImageFolder, FullPath: string;
begin
  if FProfileImageFileName = '' then
    Exit;

  // Bestimme Quellpfad
  if BasePath <> '' then
    ImageFolder := TPath.Combine(ExtractFilePath(BasePath), 'ProfileImages')
  else
    ImageFolder := GetProfileImagesFolder;

  FullPath := TPath.Combine(ImageFolder, FProfileImageFileName);

  // Datei laden falls vorhanden
  if TFile.Exists(FullPath) then
  begin
    try
      FProfilePicture.LoadFromFile(FullPath);
    except
      // Fehler beim Laden ignorieren, Bild bleibt leer
      FProfileImageFileName := '';
    end;
  end
  else
  begin
    // Datei nicht gefunden, Verweis löschen
    FProfileImageFileName := '';
  end;
end;

procedure TPerson.DeleteProfileImageFile(const BasePath: string);
var
  ImageFolder, FullPath: string;
begin
  if FProfileImageFileName = '' then
    Exit;

  // Bestimme Pfad
  if BasePath <> '' then
    ImageFolder := TPath.Combine(ExtractFilePath(BasePath), 'ProfileImages')
  else
    ImageFolder := GetProfileImagesFolder;

  FullPath := TPath.Combine(ImageFolder, FProfileImageFileName);

  // Datei löschen falls vorhanden
  if TFile.Exists(FullPath) then
  begin
    try
      TFile.Delete(FullPath);
    except
      // Fehler beim Löschen ignorieren
    end;
  end;

  FProfileImageFileName := '';
end;

// === LOKALE JSON SERIALIZATION ===

function TPerson.ToJSON(const BasePath: string = ''): TJSONObject;
var
  I: Integer;
  NicknamesArray, MoviesArray, SeriesArray: TJSONArray;
  MovieEntry: TMovieSeriesEntry;
begin
  Result := TJSONObject.Create;

  // Profilbild separat speichern
  SaveProfileImageToFile(BasePath);

  // Nur die wichtigsten Felder speichern
  Result.AddPair('fn', FFirstName); // Gekürzte Feldnamen
  Result.AddPair('sn', FSurname);

  // Beschreibung kürzen wenn zu lang
  if Length(FDescription) > 500 then
    Result.AddPair('desc', Copy(FDescription, 1, 497) + '...')
  else
    Result.AddPair('desc', FDescription);

  // Birthday kompakt speichern
  if FBirthday > 0 then
    Result.AddPair('bd', DateToISO8601(FBirthday))
  else
    Result.AddPair('bd', '');

  // Nicknames (nur die ersten 3)
  NicknamesArray := TJSONArray.Create;
  for I := 0 to Min(FNicknames.Count - 1, 2) do
    NicknamesArray.AddElement(TJSONString.Create(FNicknames[I]));
  Result.AddPair('nn', NicknamesArray);

  // Adresse kompakt (nur die wichtigsten Felder)
  if Trim(FAddress1 + FAddress2 + FAddress3) <> '' then
  begin
    Result.AddPair('addr', Trim(FAddress1 + ' ' + FAddress2 + ' ' + FAddress3));
  end;

  // Nur die wichtigsten persönlichen Infos
  if Trim(FProfession) <> '' then
    Result.AddPair('prof', FProfession);
  if Trim(FHobbies) <> '' then
    Result.AddPair('hob', FHobbies);

  // Profilbild-Dateiname (nicht das Bild selbst)
  if FProfileImageFileName <> '' then
    Result.AddPair('imgFile', FProfileImageFileName);

  // Religionszugehörigkeit
if Trim(FreligiousAffiliation) <> '' then
  Result.AddPair('religion', FreligiousAffiliation);

// Familienstand
if Trim(FMaritalStatus) <> '' then
  Result.AddPair('maritalStatus', FMaritalStatus);

// Freiwilligenarbeit
if Trim(FVolunteerActivities) <> '' then
  Result.AddPair('volunteer', FVolunteerActivities);

// Fun Facts
if Trim(FFunFact) <> '' then
  Result.AddPair('funFact', FFunFact);

// Adresse sauber aufteilen
var AddrArray: TJSONArray := TJSONArray.Create;
if Trim(FAddress1) <> '' then AddrArray.Add(FAddress1);
if Trim(FAddress2) <> '' then AddrArray.Add(FAddress2);
if Trim(FAddress3) <> '' then AddrArray.Add(FAddress3);
if Trim(FAddress4) <> '' then AddrArray.Add(FAddress4);
if Trim(FAddress5) <> '' then AddrArray.Add(FAddress5);
if AddrArray.Count > 0 then
  Result.AddPair('address', AddrArray);


  // Nur die Top-3 Lieblingsfilme
  if FFavoriteMovies.Count > 0 then
  begin
    MoviesArray := TJSONArray.Create;
    for I := 0 to FFavoriteMovies.Count - 1 do
    begin
      MovieEntry := FFavoriteMovies[I];
      MoviesArray.AddElement(MovieEntry.ToJSON);
    end;
    Result.AddPair('mov', MoviesArray);
  end;

  // Nur die Top-3 Lieblingsserien
  if FFavoriteSeries.Count > 0 then
  begin
    SeriesArray := TJSONArray.Create;
    for I := 0 to FFavoriteSeries.Count - 1 do
    begin
      MovieEntry := FFavoriteSeries[I];
      SeriesArray.AddElement(MovieEntry.ToJSON);
    end;
    Result.AddPair('ser', SeriesArray);
  end;
end;

procedure TPerson.FromJSON(JSON: TJSONObject; const BasePath: string = '');
var
  I: Integer;
  NicknamesArray, MoviesArray, SeriesArray: TJSONArray;
  MovieEntry: TMovieSeriesEntry;
  JSONValue: TJSONValue;
  BirthdayStr: string;
begin
  if not Assigned(JSON) then
    Exit;

  // Gekürzte Feldnamen lesen
  JSONValue := JSON.GetValue('fn');
  if Assigned(JSONValue) then FFirstName := JSONValue.Value;

  JSONValue := JSON.GetValue('sn');
  if Assigned(JSONValue) then FSurname := JSONValue.Value;

  JSONValue := JSON.GetValue('desc');
  if Assigned(JSONValue) then FDescription := JSONValue.Value;

  // Birthday
  JSONValue := JSON.GetValue('bd');
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
  JSONValue := JSON.GetValue('nn');
  if Assigned(JSONValue) and (JSONValue is TJSONArray) then
  begin
    NicknamesArray := JSONValue as TJSONArray;
    for I := 0 to NicknamesArray.Count - 1 do
      FNicknames.Add(NicknamesArray.Items[I].Value);
  end;

  // Kompakte Adresse
  JSONValue := JSON.GetValue('addr');
  if Assigned(JSONValue) then
    FAddress1 := JSONValue.Value;



  // Persönliche Infos
  JSONValue := JSON.GetValue('prof');
  if Assigned(JSONValue) then FProfession := JSONValue.Value;

  JSONValue := JSON.GetValue('hob');
  if Assigned(JSONValue) then FHobbies := JSONValue.Value;

  // Profilbild-Dateiname laden
  JSONValue := JSON.GetValue('imgFile');
  if Assigned(JSONValue) and (JSONValue.Value <> '') then
  begin
    FProfileImageFileName := JSONValue.Value;
    LoadProfileImageFromFile(BasePath);
  end;

  // Religionszugehörigkeit
JSONValue := JSON.GetValue('religion');
if Assigned(JSONValue) then FreligiousAffiliation := JSONValue.Value;

// Familienstand
JSONValue := JSON.GetValue('maritalStatus');
if Assigned(JSONValue) then FMaritalStatus := JSONValue.Value;

// Freiwilligenarbeit
JSONValue := JSON.GetValue('volunteer');
if Assigned(JSONValue) then FVolunteerActivities := JSONValue.Value;

// Fun Facts
JSONValue := JSON.GetValue('funFact');
if Assigned(JSONValue) then FFunFact := JSONValue.Value;

// Adresse
FAddress1 := ''; FAddress2 := ''; FAddress3 := ''; FAddress4 := ''; FAddress5 := '';
JSONValue := JSON.GetValue('address');
if Assigned(JSONValue) and (JSONValue is TJSONArray) then
begin
  var AddrArray: TJSONArray := JSONValue as TJSONArray;
  if AddrArray.Count > 0 then
    FAddress1 := AddrArray.Items[0].Value;
  if AddrArray.Count > 1 then
    FAddress2 := AddrArray.Items[1].Value;
  if AddrArray.Count > 2 then
    FAddress3 := AddrArray.Items[2].Value;
  if AddrArray.Count > 3 then
    FAddress4 := AddrArray.Items[3].Value;
  if AddrArray.Count > 4 then
    FAddress5 := AddrArray.Items[4].Value;
end;


  // Top Movies
  FFavoriteMovies.Clear;
  JSONValue := JSON.GetValue('mov');
  if Assigned(JSONValue) and (JSONValue is TJSONArray) then
  begin
    MoviesArray := JSONValue as TJSONArray;
    for I := 0 to MoviesArray.Count - 1 do
    begin
      MovieEntry.FromJSON(MoviesArray.Items[I] as TJSONObject);
      FFavoriteMovies.Add(MovieEntry);
    end;
  end;

  // Top Series
  FFavoriteSeries.Clear;
  JSONValue := JSON.GetValue('ser');
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

class function TPerson.CreateFromJSON(JSON: TJSONObject; const BasePath: string = ''): TPerson;
begin
  Result := TPerson.Create;
  Result.FromJSON(JSON, BasePath);
end;

initialization
  PersonList := TObjectList<TPerson>.Create(True);

finalization
  PersonList.Free;

end.
