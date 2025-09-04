//does not work, to complex in the late game

unit LanguageConfigurator;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections, System.JSON,
  System.IOUtils, Registry;

type
  TLanguage = (lnGerman, lnEnglish);

  TLanguageConfigurator = class
  private
    FCurrentLanguage: TLanguage;
    FStrings: TDictionary<string, TDictionary<TLanguage, string>>;

    procedure InitializeDefaultStrings;

  public
    constructor Create;
    destructor Destroy; override;

    // Language management
    property CurrentLanguage: TLanguage read FCurrentLanguage write FCurrentLanguage;
    procedure SetLanguage(Language: TLanguage);
    function GetLanguageName(Language: TLanguage): string;

    // String retrieval
    function GetString(const Key: string): string; overload;
    function GetString(const Key: string; Language: TLanguage): string; overload;
    function GetStringf(const Key: string; const Args: array of const): string;

    // String management
    procedure AddString(const Key: string; const GermanText, EnglishText: string);
    function HasString(const Key: string): Boolean;

    // Utility
    procedure ExportToJSON(const FileName: string);
  end;

var
  Lang: TLanguageConfigurator;

implementation

const
  REGISTRY_KEY = 'Software\FreundschaftsbuchApp';
  LANGUAGE_VALUE = 'Language';

{ TLanguageConfigurator }

constructor TLanguageConfigurator.Create;
begin
  inherited Create;
  FStrings := TDictionary<string, TDictionary<TLanguage, string>>.Create;
  FCurrentLanguage := lnGerman; // Default

  InitializeDefaultStrings;
end;

destructor TLanguageConfigurator.Destroy;
var
  Pair: TPair<string, TDictionary<TLanguage, string>>;
begin

  // Cleanup dictionaries
  for Pair in FStrings do
    Pair.Value.Free;
  FStrings.Free;

  inherited Destroy;
end;

procedure TLanguageConfigurator.InitializeDefaultStrings;
begin
  // === MAIN FORM ===
  AddString('app_title', 'Freundschaftsbuch', 'Friendship Book');
  AddString('btn_add_friend', 'Freund hinzufügen', 'Add Friend');
  AddString('btn_view_book', 'Freundschaftsbuch anzeigen', 'View Friendship Book');
  AddString('btn_edit_friend', 'Freund bearbeiten', 'Edit Friend');
  AddString('btn_export', 'Exportieren', 'Export');
  AddString('btn_print', 'Drucken', 'Print');
  AddString('search_placeholder', 'Suche nach Namen...', 'Search for names...');

  // === MENU ===
  AddString('menu_file', 'Datei', 'File');
  AddString('menu_new', 'Neu', 'New');
  AddString('menu_open', #$00D6'ffnen', 'Open');
  AddString('menu_save', 'Speichern', 'Save');
  AddString('menu_save_as', 'Speichern unter...', 'Save as...');
  AddString('menu_recent', 'Zuletzt verwendet', 'Recent Files');
  AddString('menu_exit', 'Beenden', 'Exit');

  AddString('menu_extras', 'Extras', 'Extras');
  AddString('menu_random_person', 'Zuf'#$00E4'llige Person w'#$00E4'hlen', 'Pick Random Person');
  AddString('menu_fun_fact', 'Zuf'#$00E4'lliger Fun Fact', 'Random Fun Fact');
  AddString('menu_about', #$00DC'ber', 'About');

  // === ENTRY FORM ===
  AddString('entry_title', 'Neuen Freund hinzuf'#$00FC'gen', 'Add New Friend');
  AddString('entry_edit_title', 'Freund bearbeiten', 'Edit Friend');
  AddString('lbl_first_name', 'Vorname:', 'First Name:');
  AddString('lbl_surname', 'Nachname:', 'Surname:');
  AddString('lbl_nicknames', 'Spitznamen:', 'Nicknames:');
  AddString('lbl_birthday', 'Geburtstag:', 'Birthday:');
  AddString('lbl_profile_picture', 'Profilbild:', 'Profile Picture:');
  AddString('btn_browse_image', 'Durchsuchen...', 'Browse...');

  // === ADDRESS GROUP ===
  AddString('grp_address', 'Adresse', 'Address');
  AddString('lbl_address1', 'Stra'#$00DF'e:', 'Street:');
  AddString('lbl_address2', 'Hausnummer:', 'House Number:');
  AddString('lbl_address3', 'PLZ:', 'Postal Code:');
  AddString('lbl_address4', 'Stadt:', 'City:');
  AddString('lbl_address5', 'Land:', 'Country:');

  // === PERSONAL INFO ===
  AddString('grp_personal', 'Pers'#$00F6'nliche Informationen', 'Personal Information');
  AddString('lbl_relationship', 'Beziehungsstatus:', 'Relationship Status:');
  AddString('lbl_profession', 'Beruf:', 'Profession:');
  AddString('lbl_education', 'Bildung:', 'Education:');
  AddString('lbl_hobbies', 'Hobbys:', 'Hobbies:');
  AddString('lbl_volunteer', 'Ehrenamt:', 'Volunteer Activities:');
  AddString('lbl_fun_fact', 'Fun Fact:', 'Fun Fact:');
  AddString('lbl_description', 'Beschreibung:', 'Description:');

  // === FAVORITES ===
  AddString('grp_movies', 'Lieblingsfilme', 'Favorite Movies');
  AddString('grp_series', 'Lieblingsserien', 'Favorite TV Series');
  AddString('btn_add_movie', 'Film hinzuf'#$00FC'gen', 'Add Movie');
  AddString('btn_add_series', 'Serie hinzuf'#$00FC'gen', 'Add Series');
  AddString('btn_remove', 'Entfernen', 'Remove');

  // === BUTTONS ===
  AddString('btn_save', 'Speichern', 'Save');
  AddString('btn_save_changes', #$00C4'nderungen speichern', 'Save Changes');
  AddString('btn_cancel', 'Abbrechen', 'Cancel');
  AddString('btn_ok', 'OK', 'OK');
  AddString('btn_yes', 'Ja', 'Yes');
  AddString('btn_no', 'Nein', 'No');

  // === MESSAGES ===
  AddString('msg_save_success', 'Person erfolgreich gespeichert!', 'Person saved successfully!');
  AddString('msg_no_persons', 'Keine Personen im Freundschaftsbuch!', 'No people in the friendship book!');
  AddString('msg_select_person', 'Bitte Person ausw'#$00E4'hlen!', 'Please select a person!');
  AddString('msg_enter_first_name', 'Bitte Vorname eingeben!', 'Please enter first name!');
  AddString('msg_enter_surname', 'Bitte Nachname eingeben!', 'Please enter surname!');
  AddString('msg_file_modified', 'Das Freundschaftsbuch wurde ge'#$00E4'ndert. M'#$00F6'chten Sie die '#$00C4'nderungen speichern?', 'The friendship book has been changed. Would you like to save the changes?');
  AddString('msg_print_success', 'Erfolgreich gedruckt!', 'Successfully printed!');
  AddString('msg_no_search_results', 'Keine Ergebnisse f'#$00FC'r "%s" gefunden.', 'No results found for "%s".');

  // === DIALOGS ===
  AddString('dlg_open_title', 'Freundschaftsbuch '#$00F6'ffnen', 'Open Friendship Book');
  AddString('dlg_save_title', 'Freundschaftsbuch speichern', 'Save Friendship Book');
  AddString('dlg_image_title', 'Profilbild ausw'#$00E4'hlen', 'Select Profile Picture');
  AddString('dlg_confirm_delete', 'Sind Sie sicher, dass Sie diese Person l'#$00F6'schen m'#$00F6'chten?', 'Are you sure you want to delete this person?');

  // === STATUS ===
  AddString('status_ready', 'Bereit', 'Ready');
  AddString('status_loading', 'Lade...', 'Loading...');
  AddString('status_saving', 'Speichere...', 'Saving...');
  AddString('status_persons_count', '%d Personen', '%d people');

  // === PRINT ===
  AddString('print_title', 'Freundschaftsbuch', 'Friendship Book');
  AddString('print_printed_at', 'Gedruckt am:', 'Printed at:');
  AddString('print_age', 'Alter:', 'Age:');
  AddString('print_address', 'Adresse:', 'Address:');
  AddString('print_no_address', 'Keine Adresse angegeben', 'No address provided');
  AddString('print_nicknames', 'Spitznamen:', 'Nicknames:');
  AddString('print_movies', 'Lieblingsfilme:', 'Favorite Movies:');
  AddString('print_series', 'Lieblingsserien:', 'Favorite TV Series:');
  AddString('print_not_provided', 'Nicht angegeben', 'Not provided');

  // === VIEW ===
  AddString('view_age_years', '%d Jahre', '%d years old');
  AddString('view_no_image', 'Kein Bild', 'No Image');
  AddString('view_movies_count', '%d Filme', '%d movies');
  AddString('view_series_count', '%d Serien', '%d series');

  // === RELATIONSHIP STATUS ===
  AddString('rel_single', 'Single', 'Single');
  AddString('rel_relationship', 'In einer Beziehung', 'In a Relationship');
  AddString('rel_married', 'Verheiratet', 'Married');
  AddString('rel_complicated', 'Es ist kompliziert', 'It''s Complicated');

  // === EDUCATION ===
  AddString('edu_elementary', 'Grundschule', 'Elementary School');
  AddString('edu_highschool', 'Gymnasium', 'High School');
  AddString('edu_university', 'Universit�t', 'University');
  AddString('edu_apprenticeship', 'Ausbildung', 'Apprenticeship');

  // === NAVIGATION ===
  AddString('nav_previous', '< Zur'#$00FC'ck', '< Back');
  AddString('nav_next', 'Weiter >', 'Next >');
  AddString('nav_of', '%d von %d', '%d of %d');
end;



procedure TLanguageConfigurator.SetLanguage(Language: TLanguage);
begin
  FCurrentLanguage := Language;
end;

function TLanguageConfigurator.GetLanguageName(Language: TLanguage): string;
begin
  case Language of
    lnGerman: Result := 'Deutsch';
    lnEnglish: Result := 'English';
  else
    Result := 'Unknown';
  end;
end;

function TLanguageConfigurator.GetString(const Key: string): string;
begin
  Result := GetString(Key, FCurrentLanguage);
end;

function TLanguageConfigurator.GetString(const Key: string; Language: TLanguage): string;
var
  LanguageDict: TDictionary<TLanguage, string>;
begin
  Result := Key; // Fallback to key if not found

  if FStrings.TryGetValue(Key, LanguageDict) then
  begin
    if not LanguageDict.TryGetValue(Language, Result) then
    begin
      // Try fallback to German if English not found
      if Language = lnEnglish then
        LanguageDict.TryGetValue(lnGerman, Result);
    end;
  end;
end;

function TLanguageConfigurator.GetStringf(const Key: string; const Args: array of const): string;
begin
  Result := Format(GetString(Key), Args);
end;

procedure TLanguageConfigurator.AddString(const Key: string; const GermanText, EnglishText: string);
var
  LanguageDict: TDictionary<TLanguage, string>;
begin
  if not FStrings.TryGetValue(Key, LanguageDict) then
  begin
    LanguageDict := TDictionary<TLanguage, string>.Create;
    FStrings.Add(Key, LanguageDict);
  end;

  LanguageDict.AddOrSetValue(lnGerman, GermanText);
  LanguageDict.AddOrSetValue(lnEnglish, EnglishText);
end;

function TLanguageConfigurator.HasString(const Key: string): Boolean;
begin
  Result := FStrings.ContainsKey(Key);
end;

procedure TLanguageConfigurator.ExportToJSON(const FileName: string);
var
  JSON, LanguagesJSON: TJSONObject;
  Pair: TPair<string, TDictionary<TLanguage, string>>;
  LangPair: TPair<TLanguage, string>;
  KeyJSON: TJSONObject;
begin
  JSON := TJSONObject.Create;
  try
    JSON.AddPair('version', '1.0');
    JSON.AddPair('created', DateTimeToStr(Now));

    LanguagesJSON := TJSONObject.Create;
    JSON.AddPair('strings', LanguagesJSON);

    for Pair in FStrings do
    begin
      KeyJSON := TJSONObject.Create;
      for LangPair in Pair.Value do
      begin
        case LangPair.Key of
          lnGerman: KeyJSON.AddPair('de', LangPair.Value);
          lnEnglish: KeyJSON.AddPair('en', LangPair.Value);
        end;
      end;
      LanguagesJSON.AddPair(Pair.Key, KeyJSON);
    end;

    TFile.WriteAllText(FileName, JSON.ToJSON, TEncoding.UTF8);
  finally
    JSON.Free;
  end;
end;

{procedure TLanguageConfigurator.ImportFromJSON(const FileName: string);
var
  FileContent: string;
  JSON, StringsJSON, KeyJSON: TJSONObject;
  Pair: TJSONPair;
  GermanText, EnglishText: string;
  JSONValue: TJSONValue;
begin
  if not TFile.Exists(FileName) then
    Exit;

  try
    FileContent := TFile.ReadAllText(FileName, TEncoding.UTF8);
    JSON := TJSONObject.ParseJSONValue(FileContent) as TJSONObject;
    try
      if not Assigned(JSON) then
        Exit;

      StringsJSON := JSON.GetValue('strings') as TJSONObject;
      if not Assigned(StringsJSON) then
        Exit;

      for Pair in StringsJSON do
      begin
        KeyJSON := Pair.Value as TJSONObject.;
        if Assigned(KeyJSON) then
        begin
          GermanText := '';
          EnglishText := '';

          JSONValue := KeyJSON.GetValue('de');
          if Assigned(JSONValue) then
            GermanText := JSONValue.Value;

          JSONValue := KeyJSON.GetValue('en');
          if Assigned(JSONValue) then
            EnglishText := JSONValue.Value;

          if (GermanText <> '') or (EnglishText <> '') then
            AddString(Pair.JsonString.Value, GermanText, EnglishText);
        end;
      end;
    finally
      JSON.Free;
    end;
  except
    // Ignore import errors
  end;
end;
   }
initialization
  Lang := TLanguageConfigurator.Create;

finalization
  Lang.Free;

end.
