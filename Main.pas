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
    ButtonExport: TButton;
    ButtonLookFriendbook: TButton;
    ButtonEditFriendSite: TButton;
    ButtonPrint: TButton;
    Panel2: TPanel;
    SearchBox1: TSearchBox;
    MainMenu1: TMainMenu;
    Datei1: TMenuItem;
    DateiOeffnen1: TMenuItem;
    DateiSpeichern1: TMenuItem;
    DateiSpeichernUnter1: TMenuItem;
    N1: TMenuItem;
    RecentProjects1: TMenuItem;
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
      JSON.AddPair('version', '1.1');
      JSON.AddPair('created', DateTimeToStr(Now));
      JSON.AddPair('application', 'friendship book');

      // Alle Personen serialisieren
      for I := 0 to PersonList.Count - 1 do
        PersonsArray.AddElement(PersonList[I].ToJSON);

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
        Person := TPerson.CreateFromJSON(PersonsArray.Items[I] as TJSONObject);
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
// RECENT FILES FUNKTIONALITÄT
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
    RecentProjects1.Add(FRecentMenuItems[I]);
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

  RecentProjects1.Enabled := FRecentFiles.Count > 0;
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
    Caption := Caption + Format(' (%d Personen)', [PersonList.Count]);

  Self.Caption := Caption;
end;

function TForm1.GetDefaultFileName: string;
begin
  Result := Format('friendship book_%s.fb', [FormatDateTime('yyyy-mm-dd', Now)]);
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
  ButtonViewPrevious.Top := 300;
  ButtonViewPrevious.Width := 60;
  ButtonViewPrevious.Height := 25;
  ButtonViewPrevious.Caption := '< Back';
  ButtonViewPrevious.OnClick := ButtonViewPreviousClick;

  // Next Button
  ButtonViewNext := TButton.Create(Self);
  ButtonViewNext.Parent := Panel1;
  ButtonViewNext.Left := 80;
  ButtonViewNext.Top := 300;
  ButtonViewNext.Width := 60;
  ButtonViewNext.Height := 25;
  ButtonViewNext.Caption := 'Next >';
  ButtonViewNext.OnClick := ButtonViewNextClick;

  // Navigation Label
  LabelNavigation := TLabel.Create(Self);
  LabelNavigation.Parent := Panel1;
  LabelNavigation.Left := 10;
  LabelNavigation.Top := 335;
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
         (Pos(SearchLower, LowerCase(Person.Description)) > 0) or
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
  DrawTextBlock(Canvas, 'FREUNDSCHAFTSBUCH', Y, HeaderRect, [fsBold], 16);
  Y := Y+ LineHeight;

  // Datum
  DrawTextBlock(Canvas, 'Gedruckt am: ' + DateToStr(Now), Y, HeaderRect, [], 8);
  Y := Y+ LineHeight * 2;

  // Trennlinie
  Canvas.Pen.Width := 2;
  Canvas.MoveTo(PageRect.Left, Y);
  Canvas.LineTo(PageRect.Right, Y);
  Y := Y+ LineHeight;

  // === PERSON INFO ===
  ContentRect := Rect(PageRect.Left + 20, Y, PageRect.Right - 20, PageRect.Bottom);

  // Name (groß und fett)
  DrawTextBlock(Canvas, Person.GetFullName, Y, ContentRect, [fsBold], 14);
  Y := Y+ LineHeight * 2;

  // Basis-Informationen
  if Person.GetAge > 0 then
  begin
    DrawTextBlock(Canvas, 'Alter: ' + IntToStr(Person.GetAge), Y, ContentRect, [fsBold]);
    Y := Y+ LineHeight;
  end;

  if Trim(Person.Address1) <> '' then
  begin
    DrawTextBlock(Canvas, 'Addresse: ' + Person.Address1 + Person.Address2 + Person.Address3 + Person.Address4 + Person.Address5, Y, ContentRect);
   Y := Y+ LineHeight;
  end;

  Y := Y+ LineHeight div 2; // Abstand

  // Spitzname/Nicknames
  if Person.Nicknames.Count > 0 then
  begin
    TempText := 'Spitzname: ';
    if Person.Nicknames.Count = 1 then
      TempText := TempText + Person.Nicknames[0]
    else
      TempText := TempText + Person.Nicknames.CommaText;
    DrawTextBlock(Canvas, TempText, Y, ContentRect, [fsItalic]);
    Y := Y+ LineHeight * 2;
  end;

  // === BESCHREIBUNG ===
  if Trim(Person.Description) <> '' then
  begin
    DrawTextBlock(Canvas, 'Beschreibung:', Y, ContentRect, [fsBold]);
   Y := Y+ LineHeight;

    // Beschreibungstext mit Zeilenumbruch
    DrawTextBlock(Canvas, Person.Description, Y, ContentRect);
    Y := Y+ LineHeight * 3;
  end;

  // === KONTAKT INFO ===
  Canvas.Pen.Width := 1;
  Canvas.MoveTo(ContentRect.Left, Y);
  Canvas.LineTo(ContentRect.Right, Y);
  Y := Y+ LineHeight;

  DrawTextBlock(Canvas, 'KONTAKTINFORMATIONEN', Y, ContentRect, [fsBold], 12);
  Y := Y+ LineHeight;

  // Hier könntest du weitere Felder hinzufügen, falls vorhanden:
  // if Trim(Person.Email) <> '' then
  // begin
  //   DrawTextBlock(Canvas, 'E-Mail: ' + Person.Email, Y, ContentRect);
  //   Y += LineHeight;
  // end;

  // === FOOTER ===
  Y := PageRect.Bottom - LineHeight * 3;
  Canvas.Pen.Width := 1;
  Canvas.MoveTo(PageRect.Left, Y);
  Canvas.LineTo(PageRect.Right, Y);
  Y := Y+ LineHeight div 2;

  DrawTextBlock(Canvas, Format('Seite 1 - Erstellt mit Freundschaftsbuch v1.0 - %s',
                              [FormatDateTime('dd.mm.yyyy hh:nn', Now)]),
                Y, PageRect, [], 8);
end;

procedure TForm1.DrawTextBlock(Canvas: TCanvas; const Text: string; var Y: Integer;
  const Rect: TRect; FontStyle: TFontStyles = []; FontSize: Integer = 0);
var
  TextRect: TRect;
  DrawText: string;
  Lines: TStringList;
  I: Integer;
  LineHeight: Integer;
  OriginalSize: Integer;
begin
  if Trim(Text) = '' then
    Exit;

  // Font-Einstellungen temporär ändern
  OriginalSize := Canvas.Font.Size;
  Canvas.Font.Style := FontStyle;
  if FontSize > 0 then
    Canvas.Font.Size := FontSize;

  LineHeight := Canvas.TextHeight('Ag') + 2;

  // Text in Zeilen aufteilen für Umbruch
  Lines := TStringList.Create;
  try
    DrawText := StringReplace(Text, #13#10, #10, [rfReplaceAll]);
    DrawText := StringReplace(DrawText, #13, #10, [rfReplaceAll]);

    Lines.Text := DrawText;

    for I := 0 to Lines.Count - 1 do
    begin
      if Y + LineHeight > Rect.Bottom then
        Break; // Seitenende erreicht

      TextRect := Rect.Create(Rect.Left, Y, Rect.Right, Y + LineHeight);

      // Text ausgeben mit Umbruch wenn nötig
      if Canvas.TextWidth(Lines[I]) > (Rect.Right - Rect.Left) then
      begin
        // Einfacher Umbruch - könnte erweitert werden
        DrawText := Copy(Lines[I], 1, 80) + '...';
        Canvas.TextRect(TextRect, TextRect.Left, TextRect.Top, DrawText);
      end
      else
        Canvas.TextRect(TextRect, TextRect.Left, TextRect.Top, Lines[I]);

      Y := Y+ LineHeight;
    end;
  finally
    Lines.Free;
    // Font zurücksetzen
    Canvas.Font.Size := OriginalSize;
    Canvas.Font.Style := [];
  end;
end;

// === OPTIONAL: PRINT PREVIEW ===
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

procedure TForm1.Exit1Click(Sender: TObject);
begin
  Close;
end;

end.
