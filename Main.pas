{*******************************************************************}
{* This file is part of Friendshipbook.                            *}
{*                                                                 *}
{* Copyright (c) 2025 Timm Johannes Göring                         *}
{* This software is licensed under the MIT License.                *}
{* For the full license text, see the LICENSE file in the          *}
{* project root directory.                                         *}
{*******************************************************************}

unit Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Printers, Vcl.ComCtrls,
  Entry, View, PersonData, Winapi.ActiveX, Vcl.OleCtrls,
  SHDocVw, Vcl.WinXCtrls, Vcl.Menus, System.JSON, System.IOUtils, System.Generics.Collections,
  Math, System.Net.HttpClient, System.Net.HttpClientComponent, Registry;

const
  MAX_RECENT_FILES = 5;
  REGISTRY_KEY = 'Software\FreundschaftsbuchApp';

type
  TForm1 = class(TForm)
    ButtonAddAFriend: TButton;
    Panel1: TPanel;
    ButtonLookFriendbook: TButton;
    ButtonEditFriendSite: TButton;
    Panel2: TPanel;
    SearchBox1: TSearchBox;
    MainMenu1: TMainMenu;
    Datei1: TMenuItem;
    DateiOeffnen1: TMenuItem;
    DateiSpeichern1: TMenuItem;
    DateiSpeichernUnter1: TMenuItem;
    // Recent Files Sub-Menu Items (dynamisch erstellt)
    N2: TMenuItem;
    Exit1: TMenuItem;

    Extras1: TMenuItem;
    Pickrandompersonthefriendshipbook1: TMenuItem;
    GivearandomTorontoFunFact1: TMenuItem;
    N3: TMenuItem;
    About1: TMenuItem;

    // Navigation Controls
    ButtonViewNext: TButton;
    ButtonViewPrevious: TButton;
    LabelNavigation: TLabel;
    SaveaProfil1: TMenuItem;
    SpeichernunterdesBuches1: TMenuItem;
    PrintDialog1: TPrintDialog;
    PrinterSetupDialog1: TPrinterSetupDialog;

    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);

    // Datei-Menü Events
    procedure DateiNeu1Click(Sender: TObject);
    procedure DateiOeffnen1Click(Sender: TObject);
    procedure DateiSpeichern1Click(Sender: TObject);
    procedure DateiSpeichernUnter1Click(Sender: TObject);
    procedure Exit1Click(Sender: TObject);

    // Recent Files Event Handler
    procedure RecentFileClick(Sender: TObject);

    // Bestehende Events
    procedure ButtonViewNextClick(Sender: TObject);
    procedure ButtonViewPreviousClick(Sender: TObject);
    procedure ButtonAddAFriendClick(Sender: TObject);
    procedure ButtonLookFriendbookClick(Sender: TObject);
    procedure SearchBox1InvokeSearch(Sender: TObject);
    procedure SearchBox1Change(Sender: TObject);
    procedure Pickrandompersonthefriendshipbook1Click(Sender: TObject);
    procedure ButtonEditFriendSiteClick(Sender: TObject);
    procedure ButtonExportClick(Sender: TObject);
    procedure ButtonPrintClick(Sender: TObject);
    procedure GivARandomFunFactAboutTimmClicked(Sender: TObject);
    procedure GivARandomFunFactAboutTorontoClicked(Sender: TObject);
    procedure About1Click(Sender: TObject);
    procedure GermanDeutsch1Click(Sender: TObject);
    procedure EnglishEnglisch1Click(Sender: TObject);
    
  private
    FCurrentViewIndex: Integer;
    FCurrentFrame: TFrame;
    FFilteredIndices: TArray<Integer>;
    FCurrentFileName: string;
    FFileModified: Boolean;
    FRecentFiles: TStringList;
    FRecentMenuItems: array[0..MAX_RECENT_FILES-1] of TMenuItem;

    // Bestehende Private Methods
    procedure ShowEditDialog;
    procedure ShowPersonView(Index: Integer);
    procedure ClearCurrentFrame;
    procedure UpdateNavigationControls;
    procedure PerformSearch(const SearchText: string);
    procedure ShowRandomPerson;
    procedure CreateNavigationControls;

    // Neue Save/Load Methods
    procedure SaveToFile(const FileName: string);
    procedure LoadFromFile(const FileName: string);
    function PromptForSaveChanges: Integer;
    procedure SetFileModified(Modified: Boolean);
    procedure UpdateWindowCaption;

    // Recent Files Methods
    procedure LoadRecentFiles;
    procedure SaveRecentFiles;
    procedure AddRecentFile(const FileName: string);
    procedure UpdateRecentFilesMenu;
    procedure CreateRecentMenuItems;

    // Helper Methods
    function GetDefaultFileName: string;
    function IsValidFile(const FileName: string): Boolean;

    // Printers
    //procedure PrintCurrentPerson;
    procedure PrintPersonData(Person: TPerson);
    function GetCurrentPerson: TPerson;
    procedure DrawPersonPage(Canvas: TCanvas; Person: TPerson; PageRect: TRect);
    procedure DrawTextBlock(Canvas: TCanvas; const Text: string; var Y: Integer;
      const Rect: TRect; FontStyle: TFontStyles = []; FontSize: Integer = 0);
    procedure ShowPrintPreview;
        procedure PreviewPaintBoxPaint(Sender: TObject); // Neue Methode für Preview
    procedure PrintAllPersons;
  public
    property FileModified: Boolean read FFileModified write SetFileModified;
  end;
var
  Form1: TForm1;

implementation

{$R *.dfm}

uses ABOUT, LanguageConfigurator;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FCurrentViewIndex := 0;
  FCurrentFrame := nil;
  FCurrentFileName := '';
  FFileModified := False;

  // Recent Files initialisieren
  FRecentFiles := TStringList.Create;
  CreateRecentMenuItems;
  LoadRecentFiles;

  // Navigation Controls erstellen
  CreateNavigationControls;

  // Initial state
  UpdateWindowCaption;
  UpdateNavigationControls;

  Lang.CurrentLanguage := lnEnglish;
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
  Result: Integer;
begin
  CanClose := True;

  if FFileModified then
  begin
    Result := PromptForSaveChanges;
    case Result of
      mrYes:
        begin
          DateiSpeichern1Click(nil);
          CanClose := not FFileModified; // Falls Speichern abgebrochen wurde
        end;
      mrNo: CanClose := True;
      mrCancel: CanClose := False;
    end;
  end;

  if CanClose then
  begin
    SaveRecentFiles;
    FRecentFiles.Free;
  end;
end;

// =========================
// DATEI-OPERATIONEN
// =========================


procedure TForm1.DateiNeu1Click(Sender: TObject);
var
  Result: Integer;
begin
  if FFileModified then
  begin
    Result := PromptForSaveChanges;
    case Result of
      mrYes: DateiSpeichern1Click(nil);
      mrCancel: Exit;
    end;
  end;

  // Neue Datei erstellen
  PersonList.Clear;
  ClearCurrentFrame;
  FCurrentFileName := '';
  FFileModified := False;
  SetLength(FFilteredIndices, 0);
  FCurrentViewIndex := 0;

  UpdateWindowCaption;
  UpdateNavigationControls;
end;

procedure TForm1.DateiOeffnen1Click(Sender: TObject);
var
  OpenDialog: TOpenDialog;
  Result: Integer;
begin
  if FFileModified then
  begin
    Result := PromptForSaveChanges;
    case Result of
      mrYes: DateiSpeichern1Click(nil);
      mrCancel: Exit;
    end;
  end;

  OpenDialog := TOpenDialog.Create(Self);
  try
    OpenDialog.Filter := 'friendship book File (*.fb)|*.fb|JSON File (*.json)|*.json';
    OpenDialog.DefaultExt := 'fb';
    OpenDialog.Title := 'Open the friendship book';
    OpenDialog.Options := [ofHideReadOnly, ofFileMustExist, ofEnableSizing];

    if OpenDialog.Execute then
      LoadFromFile(OpenDialog.FileName);
  finally
    OpenDialog.Free;
  end;
end;

procedure TForm1.DateiSpeichern1Click(Sender: TObject);
begin
  if FCurrentFileName = '' then
    DateiSpeichernUnter1Click(Sender)
  else
    SaveToFile(FCurrentFileName);
end;

procedure TForm1.DateiSpeichernUnter1Click(Sender: TObject);
var
  SaveDialog: TSaveDialog;
begin
  SaveDialog := TSaveDialog.Create(Self);
  try
    SaveDialog.Filter := 'friendship book File (*.fb)|*.fb|JSON File (*.json)|*.json';
    SaveDialog.DefaultExt := 'fb';
    SaveDialog.Title := 'Save as the friendship book';
    SaveDialog.Options := [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing];

    if FCurrentFileName <> '' then
      SaveDialog.FileName := FCurrentFileName
    else
      SaveDialog.FileName := GetDefaultFileName;

    if SaveDialog.Execute then
      SaveToFile(SaveDialog.FileName);
  finally
    SaveDialog.Free;
  end;
end;

procedure TForm1.SaveToFile(const FileName: string);
var
  JSON: TJSONObject;
  PersonsArray: TJSONArray;
  I: Integer;
  JSONString: string;
begin
  try
    JSON := TJSONObject.Create;
    try
      PersonsArray := TJSONArray.Create;
      JSON.AddPair('persons', PersonsArray);
      JSON.AddPair('version', '1.2');
      JSON.AddPair('created', DateTimeToStr(Now));
      JSON.AddPair('application', 'friendship book');

      // Alle Personen serialisieren
      for I := 0 to PersonList.Count - 1 do
        PersonsArray.AddElement(PersonList[I].ToJSON(FileName));

      // Als UTF-8 speichern
      JSONString := JSON.ToJSON;
      TFile.WriteAllText(FileName, JSONString, TEncoding.UTF8);

      // Erfolg verarbeiten
      FCurrentFileName := FileName;
      FFileModified := False;
      AddRecentFile(FileName);
      UpdateWindowCaption;

      ShowMessage(Format('Friendship book successfully saved!'#13#10'File: %s'#13#10'People: %d',
        [ExtractFileName(FileName), PersonList.Count]));
    finally
      JSON.Free;
    end;
  except
    on E: Exception do
    begin
      ShowMessage('Error while saving:'#13#10 + E.Message);
    end;
  end;
end;

procedure TForm1.LoadFromFile(const FileName: string);
var
  FileContent: string;
  JSON: TJSONObject;
  PersonsArray: TJSONArray;
  I: Integer;
  Person: TPerson;
begin
  if not IsValidFile(FileName) then
    Exit;

  try
    FileContent := TFile.ReadAllText(FileName, TEncoding.UTF8);
    JSON := TJSONObject.ParseJSONValue(FileContent) as TJSONObject;
    try
      if not Assigned(JSON) then
      begin
        ShowMessage('Invalid file format!'#13#10'File: ' + ExtractFileName(FileName));
        Exit;
      end;

      PersonsArray := JSON.GetValue('persons') as TJSONArray;
      if not Assigned(PersonsArray) then
      begin
        ShowMessage('No personal data found in the file!');
        Exit;
      end;

      // Aktuelle Liste leeren
      PersonList.Clear;

      // Personen laden
      for I := 0 to PersonsArray.Count - 1 do
      begin
        Person := TPerson.CreateFromJSON(PersonsArray.Items[I] as TJSONObject, FileName);
        PersonList.Add(Person);
      end;

      // Erfolgreicher Load
      FCurrentFileName := FileName;
      FFileModified := False;
      AddRecentFile(FileName);

      // UI zurücksetzen
      ClearCurrentFrame;
      SetLength(FFilteredIndices, 0);
      FCurrentViewIndex := 0;

      UpdateWindowCaption;
      UpdateNavigationControls;

      ShowMessage(Format('Friendship book successfully loaded!'#13#10'File: %s'#13#10'People: %d',
        [ExtractFileName(FileName), PersonList.Count]));

    finally
      JSON.Free;
    end;
  except
    on E: Exception do
      ShowMessage('Error loading the file:'#13#10 + E.Message);
  end;
end;

// =========================
// RECENT FILES FUNKTIONALITÄT - not in use
// =========================

procedure TForm1.CreateRecentMenuItems;
var
  I: Integer;
begin
  for I := 0 to MAX_RECENT_FILES - 1 do
  begin
    FRecentMenuItems[I] := TMenuItem.Create(Self);
    FRecentMenuItems[I].OnClick := RecentFileClick;
    FRecentMenuItems[I].Tag := I;
    FRecentMenuItems[I].Visible := False;
  end;
end;

procedure TForm1.LoadRecentFiles;
var
  Reg: TRegistry;
  I: Integer;
  FileName: string;
begin
  FRecentFiles.Clear;

  Reg := TRegistry.Create(KEY_READ);
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKey(REGISTRY_KEY, False) then
    begin
      for I := 0 to MAX_RECENT_FILES - 1 do
      begin
        if Reg.ValueExists('RecentFile' + IntToStr(I)) then
        begin
          FileName := Reg.ReadString('RecentFile' + IntToStr(I));
          if FileExists(FileName) then
            FRecentFiles.Add(FileName);
        end;
      end;
    end;
  finally
    Reg.Free;
  end;

  UpdateRecentFilesMenu;
end;

procedure TForm1.SaveRecentFiles;
var
  Reg: TRegistry;
  I: Integer;
begin
  Reg := TRegistry.Create(KEY_WRITE);
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKey(REGISTRY_KEY, True) then
    begin
      // Alte Einträge löschen
      for I := 0 to MAX_RECENT_FILES - 1 do
      begin
        if Reg.ValueExists('RecentFile' + IntToStr(I)) then
          Reg.DeleteValue('RecentFile' + IntToStr(I));
      end;

      // Neue Einträge speichern
      for I := 0 to Min(FRecentFiles.Count - 1, MAX_RECENT_FILES - 1) do
        Reg.WriteString('RecentFile' + IntToStr(I), FRecentFiles[I]);
    end;
  finally
    Reg.Free;
  end;
end;

procedure TForm1.About1Click(Sender: TObject);
begin
  AboutBox.Show;
end;

procedure TForm1.AddRecentFile(const FileName: string);
var
  Index: Integer;
begin
  // Wenn Datei bereits in Liste, entfernen
  Index := FRecentFiles.IndexOf(FileName);
  if Index >= 0 then
    FRecentFiles.Delete(Index);

  // An erste Stelle einfügen
  FRecentFiles.Insert(0, FileName);

  // Auf Maximum begrenzen
  while FRecentFiles.Count > MAX_RECENT_FILES do
    FRecentFiles.Delete(FRecentFiles.Count - 1);

  UpdateRecentFilesMenu;
end;

procedure TForm1.UpdateRecentFilesMenu;
var
  I: Integer;
begin
  for I := 0 to MAX_RECENT_FILES - 1 do
  begin
    if I < FRecentFiles.Count then
    begin
      FRecentMenuItems[I].Caption := Format('&%d %s', [I + 1, ExtractFileName(FRecentFiles[I])]);
      FRecentMenuItems[I].Hint := FRecentFiles[I];
      FRecentMenuItems[I].Visible := True;
    end
    else
      FRecentMenuItems[I].Visible := False;
  end;

end;

procedure TForm1.RecentFileClick(Sender: TObject);
var
  MenuItem: TMenuItem;
  FileName: string;
  Result: Integer;
begin
  MenuItem := Sender as TMenuItem;
  if (MenuItem.Tag >= 0) and (MenuItem.Tag < FRecentFiles.Count) then
  begin
    FileName := FRecentFiles[MenuItem.Tag];

    if FFileModified then
    begin
      Result := PromptForSaveChanges;
      case Result of
        mrYes: DateiSpeichern1Click(nil);
        mrCancel: Exit;
      end;
    end;

    LoadFromFile(FileName);
  end;
end;

// =========================
// HELPER METHODS
// =========================

function TForm1.PromptForSaveChanges: Integer;
begin
  Result := MessageDlg('The friendship book has been changed.'#13#10'Would you like to save the changes?',
    mtConfirmation, [mbYes, mbNo, mbCancel], 0);
end;

procedure TForm1.SetFileModified(Modified: Boolean);
begin
  if FFileModified <> Modified then
  begin
    FFileModified := Modified;
    UpdateWindowCaption;
  end;
end;

procedure TForm1.UpdateWindowCaption;
var
  Caption: string;
begin
  if FCurrentFileName <> '' then
    Caption := ExtractFileName(FCurrentFileName)
  else
    Caption := 'New friendship book';

  if FFileModified then
    Caption := Caption + ' *';

  Caption := Caption + ' - friendship book';

  if PersonList.Count > 0 then
    Caption := Caption + Format(' (%d people)', [PersonList.Count]);

  Self.Caption := Caption;
end;

function TForm1.GetDefaultFileName: string;
begin
  Result := Format('friendship book_%s.fb', [FormatDateTime('yyyy-mm-dd', Now)]);
end;

procedure TForm1.GivARandomFunFactAboutTimmClicked(Sender: TObject);
const
  Facts: array[1..22] of string = (
    'Timm is 19 and still in high school, but already codes like a college grad.',
    'Timm loves Python, C#, Java, and front-end web dev.',
    'Timm is currently building a super realistic GTA Roleplay server with crazy immersion.',
    'Timm has a DIY project: a train driver’s cab powered by a Raspberry Pi.',
    'Timm dreams of building his own Zusi 3 route from scratch.',
    'Timm finds joy in printing, filing, and keeping everything neatly organized.',
    'Timm contributes to OpenStreetMap to make the world more accurate.',
    'Timm enjoys watching Papaplatte (german twitch streamer), especially Crexpy & Basti-Kevin nights.',
    'Timm is politically left-leaning, supporting climate action and equality.',
    'Timm has a favorite character: Amy Santiago from Brooklyn Nine-Nine (order queen).',
    'Timm loves the tv-shows Brooklyn Nine-Nine, Monk, Psych, The Nanny, Die Anstalt, and Tagesschau.',
    'Timm enjoys films like Turning Red, Elemental, Inside Out, Ratatouille, Brave, and ....',
    'Timm has a fascination for realism in games, from physics to health systems.',
    'Timm spent time in Canada and now wants to speak Canadian English. He ♥️Canada.',
    'Timm sometimes struggles with languages, but sets ambitious goals: English C1, French B2.',
    'Timm enjoys strategic and simulation games like Lotus Simulator, Civilization 6, Transport Fever 2, Zusi 3, and Victoria 3.',
    'Timm balances minimalism with big creative chaos.',
    'Timm is passionate about transport — buses, trams, and trains everywhere.',
    'Timm weighs 128 kg and is on a journey to hit 80 kg.',
    'Timm prefers both physical and digital filing systems for ultimate order.',
    'Timm is inspired by Turning Red because it gave him a sense of independence and critique.',
    'Timm likes mixing tech and creativity, whether it’s code, maps, or roleplay systems.'
  );
var
  i: Integer;
begin
  Randomize; // ensures better randomness
  i := Random(Length(Facts)) + 1; // pick between 1 and 22
  ShowMessage('🎲 Fun Fact: ' + sLineBreak + Facts[i]);
end;

procedure TForm1.GivARandomFunFactAboutTorontoClicked(Sender: TObject);
const
  Facts: array[1..21] of string = (
    'Toronto is the largest city in Canada with over 2.9 million people.',
    'Toronto has the third-largest public transit system in North America.',
    'The CN Tower was once the tallest free-standing structure in the world.',
    'Toronto is one of the most multicultural cities, with over 180 languages spoken.',
    'More than half of Toronto’s residents were born outside of Canada.',
    'Toronto is nicknamed “The Six,” popularized by Drake.',
    'Toronto’s PATH system is the largest underground shopping complex in the world.',
    'Toronto’s Yonge Street was once considered the longest street in the world.',
    'Toronto hosts the Toronto International Film Festival (TIFF), one of the biggest globally.',
    'Toronto has five major sports teams, including the Raptors and Maple Leafs.',
    'Lake Ontario borders Toronto, making the waterfront a huge part of the city.',
    'Toronto Island Park is the largest urban car-free community in North America.',
    'The Toronto Zoo is one of the largest zoos in the world.',
    'Toronto Pearson International Airport is Canada’s busiest airport.',
    'Toronto has more than 1,500 parks and green spaces.',
    'The city’s streetcar system is the largest in the Americas.',
    'Toronto’s Distillery District is a historic area full of art galleries and cafés.',
    'Winter in Toronto can reach -20°C, while summers get hot and humid.',
    'Toronto has over 8,000 restaurants covering almost every cuisine.',
    'Toronto’s skyline is constantly growing with new skyscrapers.',
    'Most of the 1984 movie Police Academy was filmed in Toronto, including the “academy” at Humber College (formerly Lakeshore Psychiatric Hospital).'
  );
var
  i: Integer;
begin
  Randomize;
  i := Random(Length(Facts)) + 1;
  ShowMessage('🍁 Toronto Fun Fact: ' + sLineBreak + Facts[i]);
end;

function TForm1.IsValidFile(const FileName: string): Boolean;
begin
  Result := FileExists(FileName);
  if not Result then
  begin
    ShowMessage('File not found:'#13#10 + FileName);
    // Aus Recent Files entfernen
    if FRecentFiles.IndexOf(FileName) >= 0 then
    begin
      FRecentFiles.Delete(FRecentFiles.IndexOf(FileName));
      UpdateRecentFilesMenu;
    end;
  end;
end;


procedure TForm1.ShowEditDialog;
var
  EditDlg: TForm;
  PersonListBox: TListBox;
  ButtonEdit, ButtonCancel: TButton;
  LabelInstruction: TLabel;
  SelectedIndex: Integer;
  AddFriendFrame: TAddFriendFrame;
begin
  if PersonList.Count = 0 then
  begin
    ShowMessage('No persons available for editing!');
    Exit;
  end;

  // Create selection dialog
  EditDlg := TForm.Create(Self);
  try
    EditDlg.Caption := 'Select person for editing';
    EditDlg.Width := 400;
    EditDlg.Height := 350;
    EditDlg.Position := poMainFormCenter;
    EditDlg.BorderStyle := bsDialog;

    // Instruction label
    LabelInstruction := TLabel.Create(EditDlg);
    LabelInstruction.Parent := EditDlg;
    LabelInstruction.Left := 10;
    LabelInstruction.Top := 10;
    LabelInstruction.Width := 380;
    LabelInstruction.Height := 30;
    LabelInstruction.Caption := 'Select a person to edit:';
    LabelInstruction.WordWrap := True;

    // ListBox with persons
    PersonListBox := TListBox.Create(EditDlg);
    PersonListBox.Parent := EditDlg;
    PersonListBox.Left := 10;
    PersonListBox.Top := 50;
    PersonListBox.Width := 370;
    PersonListBox.Height := 200;

    // Fill ListBox
    for SelectedIndex := 0 to PersonList.Count - 1 do
    begin
      PersonListBox.Items.Add(Format('%d. %s (age: %d)',
        [SelectedIndex + 1,
         PersonList[SelectedIndex].GetFullName,
         PersonList[SelectedIndex].GetAge]));
    end;

    // Edit Button
    ButtonEdit := TButton.Create(EditDlg);
    ButtonEdit.Parent := EditDlg;
    ButtonEdit.Left := 220;
    ButtonEdit.Top := 270;
    ButtonEdit.Width := 75;
    ButtonEdit.Height := 25;
    ButtonEdit.Caption := 'Edit';
    ButtonEdit.Default := True;
    ButtonEdit.ModalResult := mrOK;

    // Cancel Button
    ButtonCancel := TButton.Create(EditDlg);
    ButtonCancel.Parent := EditDlg;
    ButtonCancel.Left := 305;
    ButtonCancel.Top := 270;
    ButtonCancel.Width := 75;
    ButtonCancel.Height := 25;
    ButtonCancel.Caption := 'Cancel';
    ButtonCancel.Cancel := True;
    ButtonCancel.ModalResult := mrCancel;

    // Show dialog
    if EditDlg.ShowModal = mrOK then
    begin
      if PersonListBox.ItemIndex >= 0 then
      begin
        SelectedIndex := PersonListBox.ItemIndex;

        // Clear current frame and show edit form
        ClearCurrentFrame;
        AddFriendFrame := TAddFriendFrame.Create(Self);
        AddFriendFrame.Parent := Panel2;
        AddFriendFrame.Align := alClient;
        FCurrentFrame := AddFriendFrame;

        // Load person data for editing
        AddFriendFrame.LoadPersonForEdit(PersonList[SelectedIndex], SelectedIndex);

        Panel2.Caption := '';
        Self.Caption := Format('friendship book - Edit: %s',
          [PersonList[SelectedIndex].GetFullName]);
        FileModified := true;
      end
      else
      begin
        ShowMessage('Please select a person!');
      end;
    end;
  finally
    EditDlg.Free;
  end;
end;

procedure TForm1.CreateNavigationControls;
begin
  // Previous Button
  ButtonViewPrevious := TButton.Create(Self);
  ButtonViewPrevious.Parent := Panel1;
  ButtonViewPrevious.Left := 10;
  ButtonViewPrevious.Top := 575;
  ButtonViewPrevious.Width := 60;
  ButtonViewPrevious.Height := 25;
  ButtonViewPrevious.Caption := '< Back';
  ButtonViewPrevious.OnClick := ButtonViewPreviousClick;

  // Next Button
  ButtonViewNext := TButton.Create(Self);
  ButtonViewNext.Parent := Panel1;
  ButtonViewNext.Left := 80;
  ButtonViewNext.Top := 575;
  ButtonViewNext.Width := 60;
  ButtonViewNext.Height := 25;
  ButtonViewNext.Caption := 'Next >';
  ButtonViewNext.OnClick := ButtonViewNextClick;

  // Navigation Label
  LabelNavigation := TLabel.Create(Self);
  LabelNavigation.Parent := Panel1;
  LabelNavigation.Left := 10;
  LabelNavigation.Top := 580;
  LabelNavigation.Width := 130;
  LabelNavigation.Height := 15;
  LabelNavigation.Caption := '';
  LabelNavigation.Alignment := taCenter;
end;

procedure TForm1.UpdateNavigationControls;
var
  TotalCount: Integer;
begin
  TotalCount := Length(FFilteredIndices);
  if TotalCount = 0 then
    TotalCount := PersonList.Count;

  ButtonViewPrevious.Enabled := (PersonList.Count > 0) and (FCurrentViewIndex > 0);
  ButtonViewNext.Enabled := (PersonList.Count > 0) and (FCurrentViewIndex < TotalCount - 1);

  if PersonList.Count > 0 then
    LabelNavigation.Caption := Format('%d / %d', [FCurrentViewIndex + 1, TotalCount])
  else
    LabelNavigation.Caption := '0 / 0';
end;

procedure TForm1.ButtonAddAFriendClick(Sender: TObject);
var
  AddFriendFrame: TAddFriendFrame;
begin
  ClearCurrentFrame;
  AddFriendFrame := TAddFriendFrame.Create(Self);
  AddFriendFrame.Parent := Panel2;
  AddFriendFrame.Align := alClient;
  FCurrentFrame := AddFriendFrame;

  Panel2.Caption := '';
  UpdateNavigationControls;
  FileModified := True;
end;

procedure TForm1.ButtonLookFriendbookClick(Sender: TObject);
begin
  if PersonList.Count = 0 then
  begin
    ShowMessage('No people in the friendship book!');
    Exit;
  end;

  FCurrentViewIndex := 0;
  SetLength(FFilteredIndices, 0); // Reset filter
  ShowPersonView(FCurrentViewIndex);
end;

procedure TForm1.ShowPersonView(Index: Integer);
var
  ViewFrame: TFrameView;
  ActualIndex: Integer;
begin
  // Bestimme den tatsächlichen Index
  if Length(FFilteredIndices) > 0 then
  begin
    if (Index < 0) or (Index >= Length(FFilteredIndices)) then
      Exit;
    ActualIndex := FFilteredIndices[Index];
  end
  else
  begin
    if (Index < 0) or (Index >= PersonList.Count) then
      Exit;
    ActualIndex := Index;
  end;

  ClearCurrentFrame;
  ViewFrame := TFrameView.Create(Self);
  ViewFrame.Parent := Panel2;
  ViewFrame.Align := alClient;
  FCurrentFrame := ViewFrame;

  // Load person data into view
  ViewFrame.LoadPersonData(PersonList[ActualIndex]);

  // Update window title
  Self.Caption := Format('friendship book - %s (%d/%d)',
    [PersonList[ActualIndex].GetFullName, Index + 1,
     IfThen(Length(FFilteredIndices) > 0, Length(FFilteredIndices), PersonList.Count)]);

  Panel2.Caption := '';
  UpdateNavigationControls;
end;

procedure TForm1.ClearCurrentFrame;
begin
  if Assigned(FCurrentFrame) then
  begin
    FCurrentFrame.Free;
    FCurrentFrame := nil;
  end;
  Panel2.Caption := 'Select an option on the left.';
end;

procedure TForm1.ButtonViewNextClick(Sender: TObject);
var
  MaxIndex: Integer;
begin
  if PersonList.Count = 0 then
    Exit;

  MaxIndex := IfThen(Length(FFilteredIndices) > 0, Length(FFilteredIndices), PersonList.Count);

  Inc(FCurrentViewIndex);
  if FCurrentViewIndex >= MaxIndex then
    FCurrentViewIndex := 0; // Wrap around

  ShowPersonView(FCurrentViewIndex);
end;

procedure TForm1.ButtonViewPreviousClick(Sender: TObject);
var
  MaxIndex: Integer;
begin
  if PersonList.Count = 0 then
    Exit;

  MaxIndex := IfThen(Length(FFilteredIndices) > 0, Length(FFilteredIndices), PersonList.Count);

  Dec(FCurrentViewIndex);
  if FCurrentViewIndex < 0 then
    FCurrentViewIndex := MaxIndex - 1; // Wrap around

  ShowPersonView(FCurrentViewIndex);
end;

// Such-Funktionalität
procedure TForm1.PerformSearch(const SearchText: string);
var
  I: Integer;
  TempList: TList<Integer>;
  Person: TPerson;
  SearchLower: string;
begin
  if Trim(SearchText) = '' then
  begin
    // Leere Suche - alle anzeigen
    SetLength(FFilteredIndices, 0);
    FCurrentViewIndex := 0;
    if PersonList.Count > 0 then
      ShowPersonView(0);
    UpdateNavigationControls;
    Exit;
  end;

  SearchLower := LowerCase(Trim(SearchText));
  TempList := TList<Integer>.Create;
  try
    for I := 0 to PersonList.Count - 1 do
    begin
      Person := PersonList[I];
      // Suche in verschiedenen Feldern
      if (Pos(SearchLower, LowerCase(Person.GetFullName)) > 0) or
         (Pos(SearchLower, LowerCase(Person.SomethingElse)) > 0) or
         (Person.Nicknames.Text.ToLower.Contains(SearchLower)) then
      begin
        TempList.Add(I);
      end;
    end;

    // Ergebnisse in Array kopieren
    SetLength(FFilteredIndices, TempList.Count);
    for I := 0 to TempList.Count - 1 do
      FFilteredIndices[I] := TempList[I];

    // Zur ersten gefundenen Person wechseln
    FCurrentViewIndex := 0;
    if Length(FFilteredIndices) > 0 then
      ShowPersonView(0)
    else
    begin
      ClearCurrentFrame;
      ShowMessage(Format('No results found for "%s".', [SearchText]));
    end;

    UpdateNavigationControls;
  finally
    TempList.Free;
  end;
end;

procedure TForm1.SearchBox1Change(Sender: TObject);
begin
  // Live-Suche bei Eingabe
  PerformSearch(SearchBox1.Text);
end;

procedure TForm1.SearchBox1InvokeSearch(Sender: TObject);
begin
  PerformSearch(SearchBox1.Text);
end;

// Zusätzliche Features
procedure TForm1.ShowRandomPerson;
var
  RandomIndex: Integer;
begin
  if PersonList.Count = 0 then
  begin
    ShowMessage('No persons found in the friendship book!');
    Exit;
  end;

  Randomize;
  RandomIndex := Random(PersonList.Count);

  SetLength(FFilteredIndices, 0); // Reset filter
  FCurrentViewIndex := RandomIndex;
  ShowPersonView(RandomIndex);
end;

procedure TForm1.Pickrandompersonthefriendshipbook1Click(Sender: TObject);
begin
  ShowRandomPerson;
end;

procedure TForm1.ButtonEditFriendSiteClick(Sender: TObject);
begin
  if PersonList.Count = 0 then
  begin
    ShowMessage('No persons available for editing!');
    Exit;
  end;

  // Hier würdest du einen Dialog öffnen, um eine Person auszuwählen und zu bearbeiten
  ShowEditDialog;

end;

procedure TForm1.ButtonExportClick(Sender: TObject);
var
  SaveDialog: TSaveDialog;
begin
  SaveDialog := TSaveDialog.Create(Self);
  try
    SaveDialog.Filter := 'friendship book File (*.fb)|*.fb|JSON File (*.json)|*.json';
    SaveDialog.DefaultExt := 'fb';
    SaveDialog.Title := 'Save friendship book';

    if SaveDialog.Execute then
      SaveToFile(SaveDialog.FileName);
  finally
    SaveDialog.Free;
  end;
end;

procedure TForm1.GermanDeutsch1Click(Sender: TObject);
begin
  Lang.SetLanguage(lnGerman);
  //UpdateUIStrings;
end;

function TForm1.GetCurrentPerson: TPerson;
var
  ActualIndex: Integer;
begin
  Result := nil;

  if PersonList.Count = 0 then
    Exit;

  // Bestimme aktuellen Index (berücksichtige Filter)
  if Length(FFilteredIndices) > 0 then
  begin
    if (FCurrentViewIndex >= 0) and (FCurrentViewIndex < Length(FFilteredIndices)) then
      ActualIndex := FFilteredIndices[FCurrentViewIndex]
    else
      Exit;
  end
  else
  begin
    if (FCurrentViewIndex >= 0) and (FCurrentViewIndex < PersonList.Count) then
      ActualIndex := FCurrentViewIndex
    else
      Exit;
  end;

  Result := PersonList[ActualIndex];
end;

procedure TForm1.PrintPersonData(Person: TPerson);
var
  PageRect: TRect;
begin
  if not Assigned(Person) then
    Exit;

  try
    Printer.Title := 'Freundschaftsbuch - ' + Person.GetFullName;
    Printer.BeginDoc;
    try
      // Seitenränder definieren (ca. 2cm Rand)
      PageRect.Left := GetDeviceCaps(Printer.Handle, LOGPIXELSX) * 2 div 2; // 2cm
      PageRect.Top := GetDeviceCaps(Printer.Handle, LOGPIXELSY) * 2 div 2;
      PageRect.Right := Printer.PageWidth - PageRect.Left;
      PageRect.Bottom := Printer.PageHeight - PageRect.Top;

      // Person-Seite zeichnen
      DrawPersonPage(Printer.Canvas, Person, PageRect);

    finally
      Printer.EndDoc;
    end;

    ShowMessage(Format('Seite für "%s" wurde erfolgreich gedruckt!',
                      [Person.GetFullName]));

  except
    on E: Exception do
    begin
      Printer.Abort;
      ShowMessage('Fehler beim Drucken:'#13#10 + E.Message);
    end;
  end;
end;

procedure TForm1.DrawPersonPage(Canvas: TCanvas; Person: TPerson; PageRect: TRect);
var
  Y: Integer;
  HeaderRect, ContentRect: TRect;
  LineHeight: Integer;
  TempText: string;
  AddressText: string;
begin
  // Canvas-Einstellungen
  Canvas.Font.Name := 'Arial';
  Canvas.Font.Size := 10;
  Canvas.Brush.Style := bsClear;

  Y := PageRect.Top;
  LineHeight := Canvas.TextHeight('Ag') + 4;

  // === HEADER ===
  HeaderRect := Rect(PageRect.Left, Y, PageRect.Right, Y + LineHeight * 3);

  // Titel
  DrawTextBlock(Canvas, 'FRIENDSHIP BOOK', Y, HeaderRect, [fsBold], 16);
  Y := Y + LineHeight;

  // Datum
  DrawTextBlock(Canvas, 'Printed at: ' + DateToStr(Now), Y, HeaderRect, [], 8);
  Y := Y + LineHeight * 2;

  // Trennlinie
  Canvas.Pen.Width := 2;
  Canvas.MoveTo(PageRect.Left, Y);
  Canvas.LineTo(PageRect.Right, Y);
  Y := Y + LineHeight;

  // === PERSON INFO ===
  ContentRect := Rect(PageRect.Left + 20, Y, PageRect.Right - 20, PageRect.Bottom);

  // Name (groß und fett)
  DrawTextBlock(Canvas, Person.GetFullName, Y, ContentRect, [fsBold], 14);
  Y := Y + LineHeight * 2;

  // Basis-Informationen
  if Person.GetAge > 0 then
  begin
    DrawTextBlock(Canvas, 'Ages: ' + IntToStr(Person.GetAge), Y, ContentRect, [fsBold]);
    Y := Y + LineHeight;
  end;

  // Adresse richtig zusammenbauen
  AddressText := '';
  if Trim(Person.Address1) <> '' then AddressText := AddressText + Person.Address1;
  if Trim(Person.Address2) <> '' then AddressText := AddressText + ', ' + Person.Address2;
  if Trim(Person.Address3) <> '' then AddressText := AddressText + ', ' + Person.Address3;
  if Trim(Person.Address4) <> '' then AddressText := AddressText + ', ' + Person.Address4;
  if Trim(Person.Address5) <> '' then AddressText := AddressText + ', ' + Person.Address5;

  // Falls mit Komma beginnt, entfernen
  if AddressText.StartsWith(', ') then
    AddressText := Copy(AddressText, 3, Length(AddressText) - 2);

  if Trim(AddressText) <> '' then
    DrawTextBlock(Canvas, 'Address: ' + AddressText, Y, ContentRect)
  else
    DrawTextBlock(Canvas, 'Address: No address provided', Y, ContentRect, [fsItalic]);
  Y := Y + LineHeight;

  Y := Y + LineHeight div 2; // Abstand

  // Spitzname/Nicknames
  if Person.Nicknames.Count > 0 then
  begin
    TempText := 'Nicknames: ';
    if Person.Nicknames.Count = 1 then
      TempText := TempText + Person.Nicknames[0]
    else
      TempText := TempText + Person.Nicknames.CommaText;
    DrawTextBlock(Canvas, TempText, Y, ContentRect, [fsItalic]);
    Y := Y + LineHeight * 2;
  end;

  // === BESCHREIBUNG ===
  if Trim(Person.SomethingElse) <> '' then
  begin
    DrawTextBlock(Canvas, 'Something else:', Y, ContentRect, [fsBold]);
    Y := Y + LineHeight;

    // Beschreibungstext mit Zeilenumbruch
    DrawTextBlock(Canvas, Person.SomethingElse, Y, ContentRect);
    // Y wird in DrawTextBlock automatisch erhöht
    Y := Y + LineHeight; // Extra Abstand nach Beschreibung
  end;

  // === PERSÖNLICHE INFORMATIONEN TABELLE ===
  Y := Y + LineHeight; // Abstand
  Canvas.Pen.Width := 1;
  Canvas.MoveTo(ContentRect.Left, Y);
  Canvas.LineTo(ContentRect.Right, Y);
  Y := Y + LineHeight;

  DrawTextBlock(Canvas, 'PERSONAL INFORMATION', Y, ContentRect, [fsBold], 12);
  Y := Y + LineHeight * 2;

  // Tabellen-Layout für persönliche Infos
  var ColWidth := (ContentRect.Right - ContentRect.Left) div 2;
  var Col1X := ContentRect.Left;
  var Col2X := ContentRect.Left + ColWidth + 10;

  // Liebingsfilm(e)
  DrawTextBlock(Canvas, 'Favorite movie(s):', Y, TRect.Create(Col1X, Y, Col1X + ColWidth, Y + LineHeight), [fsBold]);
  if Person.FavoriteMovies.Count > 0 then
  begin
    TempText := '';
    for var i := 0 to Min(Person.FavoriteMovies.Count - 1, 2) do // Max 3 Filme
    begin
      if i > 0 then TempText := TempText + ', ';
      TempText := TempText + Person.FavoriteMovies[i].Title;
      if Trim(Person.FavoriteMovies[i].Year) <> '' then
        TempText := TempText + ' (' + Person.FavoriteMovies[i].Year + ')';
    end;
    if Person.FavoriteMovies.Count > 3 then
      TempText := TempText + Format(' (+%d more)', [Person.FavoriteMovies.Count - 3]);
    DrawTextBlock(Canvas, TempText, Y, TRect.Create(Col2X, Y, ContentRect.Right, Y + LineHeight));
  end
  else
    DrawTextBlock(Canvas, 'Not provided', Y, TRect.Create(Col2X, Y, ContentRect.Right, Y + LineHeight), [fsItalic]);
  Y := Y + LineHeight;

  // Lieblingsserie(n)
  DrawTextBlock(Canvas, 'Favorite tv-show(s):', Y, TRect.Create(Col1X, Y, Col1X + ColWidth, Y + LineHeight), [fsBold]);
  if Person.FavoriteSeries.Count > 0 then
  begin
    TempText := '';
    for var i := 0 to Min(Person.FavoriteSeries.Count - 1, 2) do // Max 3 Serien
    begin
      if i > 0 then TempText := TempText + ', ';
      TempText := TempText + Person.FavoriteSeries[i].Title;
      if Trim(Person.FavoriteSeries[i].Year) <> '' then
        TempText := TempText + ' (' + Person.FavoriteSeries[i].Year + ')';
    end;
    if Person.FavoriteSeries.Count > 3 then
      TempText := TempText + Format(' (+%d more)', [Person.FavoriteSeries.Count - 3]);
    DrawTextBlock(Canvas, TempText, Y, TRect.Create(Col2X, Y, ContentRect.Right, Y + LineHeight));
  end
  else
    DrawTextBlock(Canvas, 'Not provided', Y, TRect.Create(Col2X, Y, ContentRect.Right, Y + LineHeight), [fsItalic]);
  Y := Y + LineHeight;

  // Hobbys
  DrawTextBlock(Canvas, 'Hobbies:', Y, TRect.Create(Col1X, Y, Col1X + ColWidth, Y + LineHeight), [fsBold]);
  if Trim(Person.Hobbies) <> '' then
    DrawTextBlock(Canvas, Person.Hobbies, Y, TRect.Create(Col2X, Y, ContentRect.Right, Y + LineHeight))
  else
    DrawTextBlock(Canvas, 'Not provided', Y, TRect.Create(Col2X, Y, ContentRect.Right, Y + LineHeight), [fsItalic]);
  Y := Y + LineHeight;

  // Ehrenamtliche Tätigkeiten
  DrawTextBlock(Canvas, 'Volunteer activities:', Y, TRect.Create(Col1X, Y, Col1X + ColWidth, Y + LineHeight), [fsBold]);
  if Trim(Person.VolunteerActivities) <> '' then
    DrawTextBlock(Canvas, Person.VolunteerActivities, Y, TRect.Create(Col2X, Y, ContentRect.Right, Y + LineHeight))
  else
    DrawTextBlock(Canvas, 'Not provided', Y, TRect.Create(Col2X, Y, ContentRect.Right, Y + LineHeight), [fsItalic]);
  Y := Y + LineHeight;

  // Fun Fact
  DrawTextBlock(Canvas, 'Fun Fact:', Y, TRect.Create(Col1X, Y, Col1X + ColWidth, Y + LineHeight), [fsBold]);
  if Trim(Person.FunFact) <> '' then
    DrawTextBlock(Canvas, Person.FunFact, Y, TRect.Create(Col2X, Y, ContentRect.Right, Y + LineHeight))
  else
    DrawTextBlock(Canvas, 'Not provided', Y, TRect.Create(Col2X, Y, ContentRect.Right, Y + LineHeight), [fsItalic]);

  // === KONTAKT INFO ===
  Y := Y + LineHeight; // Abstand vor Kontaktinfo
  Canvas.Pen.Width := 1;
  Canvas.MoveTo(ContentRect.Left, Y);
  Canvas.LineTo(ContentRect.Right, Y);
  Y := Y + LineHeight;

  //DrawTextBlock(Canvas, 'KONTAKTINFORMATIONEN', Y, ContentRect, [fsBold], 12);
  //Y := Y + LineHeight;

  // Hier könntest du weitere Felder hinzufügen, falls vorhanden:
  // Beispiel für weitere Eigenschaften der Person-Klasse:
  // if Trim(Person.Email) <> '' then
  // begin
  //   DrawTextBlock(Canvas, 'E-Mail: ' + Person.Email, Y, ContentRect);
  //   Y := Y + LineHeight;
  // end;

  // if Trim(Person.Phone) <> '' then
  // begin
  //   DrawTextBlock(Canvas, 'Telefon: ' + Person.Phone, Y, ContentRect);
  //   Y := Y + LineHeight;
  // end;

  // === FOOTER ===
  Y := PageRect.Bottom - LineHeight * 3;
  Canvas.Pen.Width := 1;
  Canvas.MoveTo(PageRect.Left, Y);
  Canvas.LineTo(PageRect.Right, Y);
  Y := Y + LineHeight div 2;

  DrawTextBlock(Canvas, Format('Page 1 - Created with Friendship Book v1.0 - %s',
                              [FormatDateTime('dd.mm.yyyy hh:nn', Now)]),
                Y, PageRect, [], 8);
end;

// Verbesserte DrawTextBlock Methode
procedure TForm1.DrawTextBlock(Canvas: TCanvas; const Text: string; var Y: Integer;
  const Rect: TRect; FontStyle: TFontStyles = []; FontSize: Integer = 0);
var
  TextRect: TRect;
  Lines: TStringList;
  I: Integer;
  LineHeight: Integer;
  OriginalSize: Integer;
  OriginalStyle: TFontStyles;
  LineText: string;
  MaxWidth: Integer;
begin
  if Trim(Text) = '' then
    Exit;

  // Font-Einstellungen temporär ändern und Original merken
  OriginalSize := Canvas.Font.Size;
  OriginalStyle := Canvas.Font.Style;
  Canvas.Font.Style := FontStyle;
  if FontSize > 0 then
    Canvas.Font.Size := FontSize;

  LineHeight := Canvas.TextHeight('Ag') + 2;
  MaxWidth := Rect.Right - Rect.Left;

  // Text in Zeilen aufteilen für Umbruch
  Lines := TStringList.Create;
  try
    // Zeilenumbrüche normalisieren
    LineText := StringReplace(Text, #13#10, #10, [rfReplaceAll]);
    LineText := StringReplace(LineText, #13, #10, [rfReplaceAll]);
    Lines.Text := LineText;

    for I := 0 to Lines.Count - 1 do
    begin
      if Y + LineHeight > Rect.Bottom - LineHeight then // Etwas Platz zum Seitenrand lassen
        Break; // Seitenende erreicht

      LineText := Lines[I];

      // Lange Zeilen umbrechen
      while (Canvas.TextWidth(LineText) > MaxWidth) and (Length(LineText) > 0) do
      begin
        // Finde einen guten Umbruchpunkt (Leerzeichen)
        var BreakPos := Length(LineText);
        while (BreakPos > 0) and (Canvas.TextWidth(Copy(LineText, 1, BreakPos)) > MaxWidth) do
          Dec(BreakPos);

        // Falls kein Leerzeichen gefunden, bei Maximalbreite umbrechen
        if BreakPos = 0 then
          BreakPos := MaxWidth div Canvas.TextWidth('M'); // Grobe Schätzung

        // Versuche bei Leerzeichen zu trennen
        var SpacePos := BreakPos;
        while (SpacePos > 0) and (LineText[SpacePos] <> ' ') do
          Dec(SpacePos);

        if SpacePos > BreakPos div 2 then // Wenn Leerzeichen nicht zu weit weg
          BreakPos := SpacePos;

        TextRect := TRect.Create(Rect.Left, Y, Rect.Right, Y + LineHeight);
        Canvas.TextRect(TextRect, TextRect.Left, TextRect.Top, Copy(LineText, 1, BreakPos));
        Y := Y + LineHeight;

        LineText := Trim(Copy(LineText, BreakPos + 1, Length(LineText) - BreakPos));

        if Y + LineHeight > Rect.Bottom - LineHeight then
          Break;
      end;

      // Restlichen Text ausgeben
      if (LineText <> '') and (Y + LineHeight <= Rect.Bottom - LineHeight) then
      begin
        TextRect := TRect.Create(Rect.Left, Y, Rect.Right, Y + LineHeight);
        Canvas.TextRect(TextRect, TextRect.Left, TextRect.Top, LineText);
        Y := Y + LineHeight;
      end;
    end;
  finally
    Lines.Free;
    // Font zurücksetzen
    Canvas.Font.Size := OriginalSize;
    Canvas.Font.Style := OriginalStyle;
  end;
end;// === OPTIONAL: PRINT PREVIEW ===
procedure TForm1.PreviewPaintBoxPaint(Sender: TObject);
var
  PaintBox: TPaintBox;
  PreviewRect: TRect;
  Person: TPerson;
begin
  PaintBox := Sender as TPaintBox;
  Person := GetCurrentPerson;

  if Assigned(Person) then
  begin
    PreviewRect := Rect(20, 20, PaintBox.Width - 20, PaintBox.Height - 20);
    DrawPersonPage(PaintBox.Canvas, Person, PreviewRect);
  end;
end;

procedure TForm1.ShowPrintPreview;
var
  PreviewForm: TForm;
  PaintBox: TPaintBox;
  ScrollBox: TScrollBox;
  ButtonPrint, ButtonClose: TButton;
  Person: TPerson;
begin
  Person := GetCurrentPerson;
  if not Assigned(Person) then
  begin
    ShowMessage('No person selected for print preview!');
    Exit;
  end;

  PreviewForm := TForm.Create(Self);
  try
    PreviewForm.Caption := 'print preview - ' + Person.GetFullName;
    PreviewForm.Width := 700;
    PreviewForm.Height := 900;
    PreviewForm.Position := poMainFormCenter;
    PreviewForm.Color := clBtnFace;

    // ScrollBox für große Seiten
    ScrollBox := TScrollBox.Create(PreviewForm);
    ScrollBox.Parent := PreviewForm;
    ScrollBox.Left := 10;
    ScrollBox.Top := 50;
    ScrollBox.Width := PreviewForm.ClientWidth - 20;
    ScrollBox.Height := PreviewForm.ClientHeight - 100;
    ScrollBox.Color := clGray;
    ScrollBox.Anchors := [akLeft, akTop, akRight, akBottom];

    // PaintBox für die Darstellung
    PaintBox := TPaintBox.Create(ScrollBox);
    PaintBox.Parent := ScrollBox;
    PaintBox.Left := 10;
    PaintBox.Top := 10;
    PaintBox.Width := 580;  // A4-ähnlich
    PaintBox.Height := 820; // A4-ähnlich
    PaintBox.Color := clWhite;
    PaintBox.OnPaint := PreviewPaintBoxPaint;

    // Buttons mit direkten ModalResult-Zuweisungen
    ButtonPrint := TButton.Create(PreviewForm);
    ButtonPrint.Parent := PreviewForm;
    ButtonPrint.Left := 10;
    ButtonPrint.Top := 10;
    ButtonPrint.Width := 100;
    ButtonPrint.Height := 30;
    ButtonPrint.Caption := 'Print';
    ButtonPrint.ModalResult := mrOk;  // Direkte Zuweisung

    ButtonClose := TButton.Create(PreviewForm);
    ButtonClose.Parent := PreviewForm;
    ButtonClose.Left := 120;
    ButtonClose.Top := 10;
    ButtonClose.Width := 100;
    ButtonClose.Height := 30;
    ButtonClose.Caption := 'Cancel';
    ButtonClose.ModalResult := mrCancel;  // Direkte Zuweisung

    if PreviewForm.ShowModal = mrOk then
      PrintPersonData(Person);

    if PreviewForm.ShowModal = mrCancel then
      Exit;
  finally
    PreviewForm.Free;
  end;
end;


// === ERWEITERTE PRINT-OPTIONEN ===

// Alle Personen drucken:
procedure TForm1.PrintAllPersons;
var
  I: Integer;
  Person: TPerson;
begin
  if PersonList.Count = 0 then
  begin
    ShowMessage('No people available for printing!');
    Exit;
  end;

  if MessageDlg(Format('Do you want to print all %d people?', [PersonList.Count]),
                mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
    Exit;

  try
    Printer.Title := 'Friendship Book - all people';
    Printer.BeginDoc;
    try
      for I := 0 to PersonList.Count - 1 do
      begin
        Person := PersonList[I];

        if I > 0 then
          Printer.NewPage; // Neue Seite für jede Person

        DrawPersonPage(Printer.Canvas, Person,
          Rect(GetDeviceCaps(Printer.Handle, LOGPIXELSX),
               GetDeviceCaps(Printer.Handle, LOGPIXELSY),
               Printer.PageWidth - GetDeviceCaps(Printer.Handle, LOGPIXELSX),
               Printer.PageHeight - GetDeviceCaps(Printer.Handle, LOGPIXELSY)));
      end;
    finally
      Printer.EndDoc;
    end;

    ShowMessage(Format('All %d people were successfully printed!', [PersonList.Count]));
  except
    on E: Exception do
    begin
      Printer.Abort;
      ShowMessage('Error during printing:'#13#10 + E.Message);
    end;
  end;
end;

procedure TForm1.ButtonPrintClick(Sender: TObject);
var
  Person: TPerson;
begin

  ShowPrintPreview;

  Person := GetCurrentPerson;

  if not Assigned(Person) then
  begin
    ShowMessage('No person selected for printing!'#13#10 +
                'Please first select a person from the friendship book.');
    Exit;
  end;

  // Print-Dialog anzeigen
  if not Assigned(PrintDialog1) then
    PrintDialog1 := TPrintDialog.Create(Self);

  PrintDialog1.Options := [poPrintToFile, poPageNums, poSelection];
  PrintDialog1.PrintRange := prAllPages;
  PrintDialog1.MinPage := 1;
  PrintDialog1.MaxPage := 1;
  PrintDialog1.FromPage := 1;
  PrintDialog1.ToPage := 1;

  if PrintDialog1.Execute then
    PrintPersonData(Person);
end;

procedure TForm1.EnglishEnglisch1Click(Sender: TObject);
begin
  Lang.SetLanguage(lnEnglish);
  //UpdateUIStrings;
end;

{procedure TForm1.UpdateUIStrings;
begin

end;  }

procedure TForm1.Exit1Click(Sender: TObject);
begin
  Close;
end;

end.
