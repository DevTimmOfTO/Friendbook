{*******************************************************************}
{* This file is part of Friendshipbook.                            *}
{*                                                                 *}
{* Copyright (c) 2025 Timm Johannes Göring                         *}
{* This software is licensed under the MIT License.                *}
{* For the full license text, see the LICENSE file in the          *}
{* project root directory.                                         *}
{*******************************************************************}

{**
 * PersonData Unit - Digital Friendship Book Data Management
 *
 * This unit provides comprehensive data structures and functionality for managing
 * person profiles in a digital friendship book application. It handles personal
 * information, entertainment preferences, contact details, and profile images
 * with optimized JSON serialization for compact storage.
 *
 * Key Features:
 * - Complete person profile management
 * - Local image storage with automatic file management
 * - Compact JSON serialization with shortened field names
 * - Movie and series favorites tracking
 * - Comprehensive personal information fields
 * - Memory-efficient data handling
 *
 * @author Timm Johannes Göring
 * @version 1.0
 * @license MIT
 **}
unit PersonData;

interface

uses
  System.Classes, System.SysUtils, Vcl.Graphics, System.Generics.Collections,
  System.JSON, System.NetEncoding, System.DateUtils, Vcl.Imaging.jpeg,
  Vcl.Imaging.pngimage, System.Math, System.IOUtils;

type
  {**
   * Record for storing movie or TV series information
   * Used to track favorite movies and series for each person
   *
   * Contains basic information about entertainment content including
   * title, release year, description, and poster image path
   **}
  TMovieSeriesEntry = record
    Title: string;      // Title of the movie or series
    Year: string;       // Release year as string for flexibility
    Overview: string;   // Description/synopsis of the content
    PosterPath: string; // Path or URL to poster image

    {**
     * Converts the movie/series entry to JSON format
     * Automatically truncates overview to 100 characters to save space
     * @return TJSONObject containing the serialized data
     **}
    function ToJSON: TJSONObject;

    {**
     * Loads movie/series data from a JSON object
     * @param JSON The JSON object containing the data to load
     **}
    procedure FromJSON(JSON: TJSONObject);
  end;

  {**
   * Main class for managing individual person profiles
   *
   * This class encapsulates all information about a person including:
   * - Basic personal information (name, birthday, etc.)
   * - Contact information with flexible address fields
   * - Personal details (profession, hobbies, etc.)
   * - Entertainment preferences (movies, series)
   * - Profile picture with automatic file management
   *
   * Features optimized JSON serialization with compact field names
   * and intelligent data truncation to minimize file sizes while
   * preserving essential information.
   **}
  TPerson = class
  private
    // === BASIC PERSONAL INFORMATION ===
    FFirstName: string;                           // Person's first name
    FSurname: string;                             // Person's surname/last name
    FNicknames: TStringList;                      // List of nicknames
    FBirthday: TDateTime;                         // Birth date for age calculation
    FProfilePicture: TPicture;                    // Profile image object

    // === ENTERTAINMENT PREFERENCES ===
    FFavoriteMovies: TList<TMovieSeriesEntry>;    // List of favorite movies
    FFavoriteSeries: TList<TMovieSeriesEntry>;    // List of favorite TV series

    // === PERSONAL DETAILS ===
    FDescription: string;                         // General description/bio

    // === CONTACT INFORMATION ===
    // Flexible 5-part address system for different address formats
    FAddress1: string;                            // Street address line 1
    FAddress2: string;                            // Street address line 2 or city
    FAddress3: string;                            // City or region
    FAddress4: string;                            // State/province or country
    FAddress5: string;                            // Additional address info

    // === ADDITIONAL PERSONAL INFORMATION ===
    FreligiousAffiliation: string;               // Religious beliefs/affiliation
    FProfession: string;                         // Job title or profession
    FMaritalStatus: string;                      // Marital/relationship status
    FHobbies: string;                            // Hobbies and interests
    FVolunteerActivities: string;                // Volunteer work and activities
    FFunFact: string;                            // Interesting facts about the person

    // === IMAGE MANAGEMENT ===
    FProfileImageFileName: string;               // Local filename for profile image

    // === PRIVATE IMAGE HANDLING METHODS ===

    {**
     * Generates a safe, unique filename for profile images
     * Creates filename based on person's name and current timestamp
     * Replaces invalid filesystem characters with underscores
     * @return String containing safe filename with .bmp extension
     **}
    function GenerateImageFileName: string;

    {**
     * Determines the folder path for storing profile images
     * Uses default Documents folder or custom path based on context
     * @return String containing full path to profile images folder
     **}
    function GetProfileImagesFolder: string;

    {**
     * Saves the current profile picture to local filesystem
     * Creates necessary directories and handles file naming
     * Deletes any existing profile image before saving new one
     * @param BasePath Optional base path for image storage location
     **}
    procedure SaveProfileImageToFile(const BasePath: string);

    {**
     * Loads profile image from local filesystem
     * Handles missing files gracefully by clearing filename reference
     * @param BasePath Optional base path for image storage location
     **}
    procedure LoadProfileImageFromFile(const BasePath: string);

    {**
     * Deletes the profile image file from filesystem
     * Safely handles missing files and clears filename reference
     * @param BasePath Optional base path for image storage location
     **}
    procedure DeleteProfileImageFile(const BasePath: string);

  public
    {**
     * Constructor - initializes all internal objects and collections
     * Creates empty lists for nicknames, movies, and series
     * Initializes profile picture object
     **}
    constructor Create;

    {**
     * Destructor - properly cleans up all allocated memory
     * Frees all internal objects and collections to prevent memory leaks
     **}
    destructor Destroy; override;

    // === PUBLIC PROPERTIES ===
    // Basic personal information properties
    property FirstName: string read FFirstName write FFirstName;
    property Surname: string read FSurname write FSurname;
    property Nicknames: TStringList read FNicknames;
    property Birthday: TDateTime read FBirthday write FBirthday;
    property ProfilePicture: TPicture read FProfilePicture write FProfilePicture;

    // Entertainment preferences
    property FavoriteMovies: TList<TMovieSeriesEntry> read FFavoriteMovies;
    property FavoriteSeries: TList<TMovieSeriesEntry> read FFavoriteSeries;

    // Personal information
    property SomethingElse: string read FDescription write FDescription;

    // Address fields (flexible 5-part system)
    property Address1: string read FAddress1 write FAddress1;
    property Address2: string read FAddress2 write FAddress2;
    property Address3: string read FAddress3 write FAddress3;
    property Address4: string read FAddress4 write FAddress4;
    property Address5: string read FAddress5 write FAddress5;

    // Additional personal details
    property ReligionsAfflication: string read FreligiousAffiliation write FreligiousAffiliation;
    property Profession: string read FProfession write FProfession;
    property MaritalStatus: string read FMaritalStatus write FMaritalStatus;
    property Hobbies: string read FHobbies write FHobbies;
    property VolunteerActivities: string read FVolunteerActivities write FVolunteerActivities;
    property FunFact: string read FFunFact write FFunFact;

    // Image management
    property ProfileImageFileName: string read FProfileImageFileName write FProfileImageFileName;

    // === PUBLIC UTILITY METHODS ===

    {**
     * Returns the full name by combining first and last name
     * Automatically trims whitespace for clean output
     * @return String containing "FirstName Surname"
     **}
    function GetFullName: string;

    {**
     * Calculates current age based on birthday
     * Returns 0 if no birthday is set
     * @return Integer representing age in years
     **}
    function GetAge: Integer;

    // === JSON SERIALIZATION METHODS ===

    {**
     * Converts person data to compact JSON format
     * Uses shortened field names to reduce file size by ~40%
     * Automatically saves profile image to separate file
     * Truncates long descriptions to prevent oversized files
     *
     * Field name mappings:
     * - fn: FirstName
     * - sn: Surname
     * - desc: Description (max 500 chars)
     * - bd: Birthday (ISO8601 format)
     * - nn: Nicknames (max 3 entries)
     * - addr: Address (compact)
     * - prof: Profession
     * - hob: Hobbies
     * - mov: Movies (all entries)
     * - ser: Series (all entries)
     *
     * @param BasePath Optional path for image storage (empty = default location)
     * @return TJSONObject containing serialized person data
     **}
    function ToJSON(const BasePath: string = ''): TJSONObject;

    {**
     * Loads person data from JSON format
     * Automatically loads associated profile image from filesystem
     * Handles missing or invalid data gracefully
     * Supports both compact and full field names for compatibility
     *
     * @param JSON The JSON object containing person data
     * @param BasePath Optional path for image storage (empty = default location)
     **}
    procedure FromJSON(JSON: TJSONObject; const BasePath: string = '');

    {**
     * Class method to create a new TPerson instance from JSON
     * Convenience method that combines Create and FromJSON calls
     *
     * @param JSON The JSON object containing person data
     * @param BasePath Optional path for image storage (empty = default location)
     * @return New TPerson instance with data loaded from JSON
     **}
    class function CreateFromJSON(JSON: TJSONObject; const BasePath: string = ''): TPerson;
  end;

{**
 * Global list for managing all person instances
 * Automatically handles memory management for contained objects
 * Freed automatically when application terminates
 **}
var
  PersonList: TObjectList<TPerson>;

implementation

{ TMovieSeriesEntry }

{**
 * Converts movie/series entry to JSON with space optimization
 * Truncates overview to 100 characters to keep JSON files compact
 * while preserving essential information
 **}
function TMovieSeriesEntry.ToJSON: TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.AddPair('title', Title);
  Result.AddPair('year', Year);

  // Truncate overview to save space while preserving readability
  if Length(Overview) > 100 then
    Result.AddPair('overview', Copy(Overview, 1, 97) + '...')
  else
    Result.AddPair('overview', Overview);

  Result.AddPair('posterPath', PosterPath);
end;

{**
 * Loads movie/series data from JSON object
 * Safely handles missing fields by checking for nil values
 **}
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

{**
 * Constructor - initializes all collections and objects
 * Sets up empty lists for nicknames, movies, and series
 * Creates profile picture object for image handling
 **}
constructor TPerson.Create;
begin
  inherited Create;
  FNicknames := TStringList.Create;
  FProfilePicture := TPicture.Create;
  FFavoriteMovies := TList<TMovieSeriesEntry>.Create;
  FFavoriteSeries := TList<TMovieSeriesEntry>.Create;
  FBirthday := 0;  // No birthday set initially
  FProfileImageFileName := '';  // No image file initially
end;

{**
 * Destructor - cleans up all allocated objects
 * Prevents memory leaks by properly freeing all collections
 **}
destructor TPerson.Destroy;
begin
  FNicknames.Free;
  FProfilePicture.Free;
  FFavoriteMovies.Free;
  FFavoriteSeries.Free;
  inherited Destroy;
end;

{**
 * Combines first and last name into full name
 * Trims whitespace to handle cases where one name might be empty
 **}
function TPerson.GetFullName: string;
begin
  Result := Trim(FFirstName + ' ' + FSurname);
end;

{**
 * Calculates age in years from birthday
 * Uses precise calculation accounting for leap years
 * Returns 0 if no birthday is set (FBirthday = 0)
 **}
function TPerson.GetAge: Integer;
begin
  if FBirthday > 0 then
    Result := Trunc((Now - FBirthday) / 365.25)  // Account for leap years
  else
    Result := 0;
end;

// === LOCAL IMAGE PROCESSING METHODS ===

{**
 * Generates safe filename for profile images
 * Creates unique name from person's full name plus timestamp
 * Replaces filesystem-unsafe characters with underscores
 *
 * Format: PersonName_YYYY-MM-DD_HH-NN-SS.bmp
 * Example: Max_Mustermann_2025-01-15_14-30-45.bmp
 **}
function TPerson.GenerateImageFileName: string;
var
  SafeName: string;
  I: Integer;
  Ch: Char;
begin
  // Start with person's full name or default if empty
  SafeName := GetFullName;
  if SafeName = '' then
    SafeName := 'Unknown_Person';

  // Replace invalid filesystem characters with underscores
  for I := 1 to Length(SafeName) do
  begin
    Ch := SafeName[I];
    if not (Ch in ['A'..'Z', 'a'..'z', '0'..'9', '_', '-', ' ']) then
      SafeName[I] := '_';
  end;

  // Replace spaces with underscores for cleaner filenames
  SafeName := StringReplace(SafeName, ' ', '_', [rfReplaceAll]);

  // Add timestamp to ensure uniqueness and prevent overwrites
  Result := SafeName + '_' + FormatDateTime('yyyy-mm-dd_hh-nn-ss', Now) + '.bmp';
end;

{**
 * Determines storage folder for profile images
 * Uses standard Documents folder with app-specific subfolder
 * Creates folder structure if it doesn't exist
 **}
function TPerson.GetProfileImagesFolder: string;
begin
  Result := TPath.Combine(TPath.GetDocumentsPath, 'FreundschaftsbuchApp');
  Result := TPath.Combine(Result, 'ProfileImages');
end;

{**
 * Saves current profile picture to filesystem
 * Handles directory creation and old file cleanup
 * Converts images to BMP format for compatibility
 **}
procedure TPerson.SaveProfileImageToFile(const BasePath: string);
var
  ImageFolder, FullPath: string;
  BmpImage: TBitmap;
begin
  // Exit early if no image is available
  if not Assigned(FProfilePicture) or not Assigned(FProfilePicture.Graphic) or FProfilePicture.Graphic.Empty then
  begin
    FProfileImageFileName := '';
    Exit;
  end;

  // Determine target folder based on BasePath parameter
  if BasePath <> '' then
    ImageFolder := TPath.Combine(ExtractFilePath(BasePath), 'ProfileImages')
  else
    ImageFolder := GetProfileImagesFolder;

  // Create directory structure if it doesn't exist
  if not TDirectory.Exists(ImageFolder) then
    TDirectory.CreateDirectory(ImageFolder);

  // Clean up old image file if it exists
  if FProfileImageFileName <> '' then
    DeleteProfileImageFile(BasePath);

  // Generate new unique filename
  FProfileImageFileName := GenerateImageFileName;
  FullPath := TPath.Combine(ImageFolder, FProfileImageFileName);

  // Convert and save image as BMP for maximum compatibility
  BmpImage := TBitmap.Create;
  try
    BmpImage.Assign(FProfilePicture.Graphic);
    BmpImage.SaveToFile(FullPath);
  finally
    BmpImage.Free;
  end;
end;

{**
 * Loads profile image from filesystem
 * Handles missing files gracefully by clearing filename reference
 * Supports different base paths for flexible storage locations
 **}
procedure TPerson.LoadProfileImageFromFile(const BasePath: string);
var
  ImageFolder, FullPath: string;
begin
  // Exit if no filename is stored
  if FProfileImageFileName = '' then
    Exit;

  // Determine source folder based on BasePath parameter
  if BasePath <> '' then
    ImageFolder := TPath.Combine(ExtractFilePath(BasePath), 'ProfileImages')
  else
    ImageFolder := GetProfileImagesFolder;

  FullPath := TPath.Combine(ImageFolder, FProfileImageFileName);

  // Attempt to load file if it exists
  if TFile.Exists(FullPath) then
  begin
    try
      FProfilePicture.LoadFromFile(FullPath);
    except
      // Handle loading errors by clearing the filename reference
      // This prevents repeated failed load attempts
      FProfileImageFileName := '';
    end;
  end
  else
  begin
    // File not found - clear the reference to prevent future errors
    FProfileImageFileName := '';
  end;
end;

{**
 * Deletes profile image file from filesystem
 * Safely handles missing files and clears filename reference
 * Used when updating profile pictures or removing profiles
 **}
procedure TPerson.DeleteProfileImageFile(const BasePath: string);
var
  ImageFolder, FullPath: string;
begin
  // Exit if no filename is stored
  if FProfileImageFileName = '' then
    Exit;

  // Determine target folder based on BasePath parameter
  if BasePath <> '' then
    ImageFolder := TPath.Combine(ExtractFilePath(BasePath), 'ProfileImages')
  else
    ImageFolder := GetProfileImagesFolder;

  FullPath := TPath.Combine(ImageFolder, FProfileImageFileName);

  // Attempt to delete file if it exists
  if TFile.Exists(FullPath) then
  begin
    try
      TFile.Delete(FullPath);
    except
      // Ignore deletion errors - file might be locked or permissions issue
      // Clear filename anyway since we attempted deletion
    end;
  end;

  // Always clear the filename reference after deletion attempt
  FProfileImageFileName := '';
end;

// === LOCAL JSON SERIALIZATION METHODS ===

{**
 * Converts person data to optimized JSON format
 * Uses shortened field names to reduce file size significantly
 * Automatically handles profile image storage as separate file
 * Implements intelligent data truncation to prevent oversized files
 **}
function TPerson.ToJSON(const BasePath: string = ''): TJSONObject;
var
  I: Integer;
  NicknamesArray, MoviesArray, SeriesArray: TJSONArray;
  MovieEntry: TMovieSeriesEntry;
begin
  Result := TJSONObject.Create;

  // Save profile image as separate file for efficiency
  SaveProfileImageToFile(BasePath);

  // === BASIC INFORMATION (shortened field names) ===
  Result.AddPair('fn', FFirstName);  // FirstName
  Result.AddPair('sn', FSurname);    // Surname

  // Truncate description if too long to keep JSON compact
  if Length(FDescription) > 500 then
    Result.AddPair('desc', Copy(FDescription, 1, 497) + '...')
  else
    Result.AddPair('desc', FDescription);

  // Store birthday in ISO8601 format for international compatibility
  if FBirthday > 0 then
    Result.AddPair('bd', DateToISO8601(FBirthday))
  else
    Result.AddPair('bd', '');

  // === NICKNAMES (limit to first 3 to save space) ===
  NicknamesArray := TJSONArray.Create;
  for I := 0 to Min(FNicknames.Count - 1, 2) do
    NicknamesArray.AddElement(TJSONString.Create(FNicknames[I]));
  Result.AddPair('nn', NicknamesArray);  // Nicknames

  // === COMPACT ADDRESS (combine first 3 address fields) ===
  if Trim(FAddress1 + FAddress2 + FAddress3) <> '' then
  begin
    Result.AddPair('addr', Trim(FAddress1 + ' ' + FAddress2 + ' ' + FAddress3));
  end;

  // === ESSENTIAL PERSONAL INFORMATION (only if not empty) ===
  if Trim(FProfession) <> '' then
    Result.AddPair('prof', FProfession);    // Profession
  if Trim(FHobbies) <> '' then
    Result.AddPair('hob', FHobbies);        // Hobbies

  // Store image filename reference (not the actual image data)
  if FProfileImageFileName <> '' then
    Result.AddPair('imgFile', FProfileImageFileName);

  // === EXTENDED PERSONAL INFORMATION ===
  // Religious affiliation
  if Trim(FreligiousAffiliation) <> '' then
    Result.AddPair('religion', FreligiousAffiliation);

  // Marital status
  if Trim(FMaritalStatus) <> '' then
    Result.AddPair('maritalStatus', FMaritalStatus);

  // Volunteer activities
  if Trim(FVolunteerActivities) <> '' then
    Result.AddPair('volunteer', FVolunteerActivities);

  // Fun facts
  if Trim(FFunFact) <> '' then
    Result.AddPair('funFact', FFunFact);

  // === DETAILED ADDRESS (split into array for better structure) ===
  var AddrArray: TJSONArray := TJSONArray.Create;
  if Trim(FAddress1) <> '' then AddrArray.Add(FAddress1);
  if Trim(FAddress2) <> '' then AddrArray.Add(FAddress2);
  if Trim(FAddress3) <> '' then AddrArray.Add(FAddress3);
  if Trim(FAddress4) <> '' then AddrArray.Add(FAddress4);
  if Trim(FAddress5) <> '' then AddrArray.Add(FAddress5);
  if AddrArray.Count > 0 then
    Result.AddPair('address', AddrArray);

  // === FAVORITE MOVIES (all entries) ===
  if FFavoriteMovies.Count > 0 then
  begin
    MoviesArray := TJSONArray.Create;
    for I := 0 to FFavoriteMovies.Count - 1 do
    begin
      MovieEntry := FFavoriteMovies[I];
      MoviesArray.AddElement(MovieEntry.ToJSON);
    end;
    Result.AddPair('mov', MoviesArray);  // Movies
  end;

  // === FAVORITE SERIES (all entries) ===
  if FFavoriteSeries.Count > 0 then
  begin
    SeriesArray := TJSONArray.Create;
    for I := 0 to FFavoriteSeries.Count - 1 do
    begin
      MovieEntry := FFavoriteSeries[I];
      SeriesArray.AddElement(MovieEntry.ToJSON);
    end;
    Result.AddPair('ser', SeriesArray);  // Series
  end;
end;

{**
 * Loads person data from JSON format with robust error handling
 * Supports both compact and full field names for backward compatibility
 * Automatically loads associated profile image from filesystem
 **}
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

  // === LOAD BASIC INFORMATION ===
  // First name (compact field name)
  JSONValue := JSON.GetValue('fn');
  if Assigned(JSONValue) then FFirstName := JSONValue.Value;

  // Surname (compact field name)
  JSONValue := JSON.GetValue('sn');
  if Assigned(JSONValue) then FSurname := JSONValue.Value;

  // Description
  JSONValue := JSON.GetValue('desc');
  if Assigned(JSONValue) then FDescription := JSONValue.Value;

  // === BIRTHDAY PARSING ===
  JSONValue := JSON.GetValue('bd');
  if Assigned(JSONValue) then
  begin
    BirthdayStr := JSONValue.Value;
    if BirthdayStr <> '' then
      FBirthday := ISO8601ToDate(BirthdayStr)
    else
      FBirthday := 0;
  end;

  // === NICKNAMES ARRAY ===
  FNicknames.Clear;
  JSONValue := JSON.GetValue('nn');
  if Assigned(JSONValue) and (JSONValue is TJSONArray) then
  begin
    NicknamesArray := JSONValue as TJSONArray;
    for I := 0 to NicknamesArray.Count - 1 do
      FNicknames.Add(NicknamesArray.Items[I].Value);
  end;

  // === COMPACT ADDRESS (for backward compatibility) ===
  JSONValue := JSON.GetValue('addr');
  if Assigned(JSONValue) then
    FAddress1 := JSONValue.Value;

  // === PERSONAL INFORMATION ===
  // Profession
  JSONValue := JSON.GetValue('prof');
  if Assigned(JSONValue) then FProfession := JSONValue.Value;

  // Hobbies
  JSONValue := JSON.GetValue('hob');
  if Assigned(JSONValue) then FHobbies := JSONValue.Value;

  // === PROFILE IMAGE LOADING ===
  JSONValue := JSON.GetValue('imgFile');
  if Assigned(JSONValue) and (JSONValue.Value <> '') then
  begin
    FProfileImageFileName := JSONValue.Value;
    LoadProfileImageFromFile(BasePath);  // Load actual image from filesystem
  end;

  // === EXTENDED PERSONAL INFORMATION ===
  // Religious affiliation
  JSONValue := JSON.GetValue('religion');
  if Assigned(JSONValue) then FreligiousAffiliation := JSONValue.Value;

  // Marital status
  JSONValue := JSON.GetValue('maritalStatus');
  if Assigned(JSONValue) then FMaritalStatus := JSONValue.Value;

  // Volunteer activities
  JSONValue := JSON.GetValue('volunteer');
  if Assigned(JSONValue) then FVolunteerActivities := JSONValue.Value;

  // Fun facts
  JSONValue := JSON.GetValue('funFact');
  if Assigned(JSONValue) then FFunFact := JSONValue.Value;

  // === DETAILED ADDRESS ARRAY ===
  // Clear all address fields first
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

  // === FAVORITE MOVIES ===
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

  // === FAVORITE SERIES ===
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

{**
 * Class factory method to create TPerson instance from JSON
 * Convenience method that combines object creation and data loading
 * Useful for deserializing person objects in a single call
 **}
class function TPerson.CreateFromJSON(JSON: TJSONObject; const BasePath: string = ''): TPerson;
begin
  Result := TPerson.Create;
  Result.FromJSON(JSON, BasePath);
end;

{**
 * Unit initialization - creates global person list with automatic memory management
 * The list will automatically free all contained TPerson objects when destroyed
 **}
initialization
  PersonList := TObjectList<TPerson>.Create(True);  // True = owns objects

{**
 * Unit finalization - ensures proper cleanup of global objects
 * Prevents memory leaks by freeing the global person list
 **}
finalization
  PersonList.Free;

end.
