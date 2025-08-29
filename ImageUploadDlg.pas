unit ImageUploadDlg;

interface

uses
  System.SysUtils, System.Classes, System.Math,
  Vcl.Forms, Vcl.Controls, Vcl.Graphics, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Dialogs,
  Vcl.Imaging.pngimage, Vcl.Imaging.jpeg, System.IOUtils, Windows;

type
  TImageUploadDlg = class(TForm)
    OKBtn: TButton;
    CancelBtn: TButton;
    Panel1: TPanel;
    Image1: TImage;
    Button1: TButton;
    Label1: TLabel;
    GroupBox1: TGroupBox;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    LabelFileName: TLabel;
    LabelFileSize: TLabel;
    LabelImageDimensions: TLabel;
    CheckBoxKeepAspectRatio: TCheckBox;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure Image1Click(Sender: TObject);
  private
    FSelectedImagePath: string;
    FImageLoaded: boolean;
    procedure LoadImageFromFile(const FileName: string);
    procedure UpdateImageInfo(const FileName: string);
    procedure ClearImageInfo;
    function GetFileSize(const FileName: string): Int64;
    function FormatFileSize(Size: Int64): string;
  public
    property SelectedImagePath: string read FSelectedImagePath;
    property ImageLoaded: boolean read FImageLoaded;
    function GetSelectedImage: Vcl.Graphics.TBitmap;
  end;

var
  ImageUploadForm: TImageUploadDlg;

implementation

{$R *.dfm}

procedure TImageUploadDlg.FormCreate(Sender: TObject);
begin
  FSelectedImagePath := '';
  FImageLoaded := False;

  // Setup Image component
  Image1.Stretch := True;
  Image1.Proportional := True;
  Image1.Center := True;
  Image1.Picture := nil;

  // Initial state
  OKBtn.Enabled := False;
  ClearImageInfo;

  // Default settings
  CheckBoxKeepAspectRatio.Checked := True;
end;

procedure TImageUploadDlg.Button1Click(Sender: TObject);
var
  OpenDlg: TOpenDialog;
begin
  OpenDlg := TOpenDialog.Create(Self);
  try
    OpenDlg.Title := 'Select Image File';
    OpenDlg.Filter := 'Image Files|*.bmp;*.jpg;*.jpeg;*.png;*.gif;*.tif;*.tiff|' +
                     'JPEG Files (*.jpg;*.jpeg)|*.jpg;*.jpeg|' +
                     'PNG Files (*.png)|*.png|' +
                     'Bitmap Files (*.bmp)|*.bmp|' +
                     'GIF Files (*.gif)|*.gif|' +
                     'TIFF Files (*.tif;*.tiff)|*.tif;*.tiff|' +
                     'All Files (*.*)|*.*';
    OpenDlg.FilterIndex := 1;
    OpenDlg.Options := [ofPathMustExist, ofFileMustExist, ofEnableSizing];

    if OpenDlg.Execute then
    begin
      LoadImageFromFile(OpenDlg.FileName);
    end;
  finally
    OpenDlg.Free;
  end;
end;

procedure TImageUploadDlg.LoadImageFromFile(const FileName: string);
begin
  try
    // Load image into Image component
    Image1.Picture.LoadFromFile(FileName);

    FSelectedImagePath := FileName;
    FImageLoaded := True;
    OKBtn.Enabled := True;

    // Update info labels
    UpdateImageInfo(FileName);

  except
    on E: Exception do
    begin
      ShowMessage('Error loading image: ' + E.Message);
      ClearImageInfo;
      FSelectedImagePath := '';
      FImageLoaded := False;
      OKBtn.Enabled := False;
    end;
  end;
end;

procedure TImageUploadDlg.UpdateImageInfo(const FileName: string);
var
  FileSize: Int64;
  FileSizeStr: string;
  ImageWidth, ImageHeight: Integer;
begin
  // File name
  LabelFileName.Caption := ExtractFileName(FileName);

  // File size
  FileSize := GetFileSize(FileName);
  FileSizeStr := FormatFileSize(FileSize);
  LabelFileSize.Caption := FileSizeStr;

  // Image dimensions
  if Assigned(Image1.Picture.Graphic) then
  begin
    ImageWidth := Image1.Picture.Graphic.Width;
    ImageHeight := Image1.Picture.Graphic.Height;
    LabelImageDimensions.Caption := Format('%d x %d pixels', [ImageWidth, ImageHeight]);
  end
  else
  begin
    LabelImageDimensions.Caption := 'Unknown';
  end;
end;

procedure TImageUploadDlg.ClearImageInfo;
begin
  LabelFileName.Caption := 'No file selected';
  LabelFileSize.Caption := '-';
  LabelImageDimensions.Caption := '-';
end;

function TImageUploadDlg.GetFileSize(const FileName: string): Int64;
var
  FileHandle: THandle;
  FindData: TWin32FindData;
begin
  Result := 0;
  FileHandle := FindFirstFile(PChar(FileName), FindData);
  if FileHandle <> INVALID_HANDLE_VALUE then
  begin
    try
      Result := (Int64(FindData.nFileSizeHigh) shl 32) + FindData.nFileSizeLow;
    finally
      Windows.FindClose(FileHandle);
    end;
  end;
end;

function TImageUploadDlg.FormatFileSize(Size: Int64): string;
begin
  if Size < 1024 then
    Result := Format('%d bytes', [Size])
  else if Size < 1024 * 1024 then
    Result := Format('%.1f KB', [Size / 1024])
  else if Size < 1024 * 1024 * 1024 then
    Result := Format('%.1f MB', [Size / (1024 * 1024)])
  else
    Result := Format('%.1f GB', [Size / (1024 * 1024 * 1024)]);
end;

procedure TImageUploadDlg.Image1Click(Sender: TObject);
begin
  // Allow clicking on image to select new image
  Button1Click(Sender);
end;

procedure TImageUploadDlg.OKBtnClick(Sender: TObject);
begin
  if FImageLoaded and (FSelectedImagePath <> '') then
  begin
    ModalResult := mrOK;
  end
  else
  begin
    ShowMessage('Please select an image first.');
  end;
end;


function TImageUploadDlg.GetSelectedImage: Vcl.Graphics.TBitmap;
var
  Bitmap: Vcl.Graphics.TBitmap;
  W, H: Integer;
  Scale: Double;
begin
  Result := nil;

  if not FImageLoaded or not Assigned(Image1.Picture.Graphic) then
    Exit;

  Bitmap := Vcl.Graphics.TBitmap.Create;
  try
    // Always assign safely, never direct assignment
    Bitmap.Assign(Image1.Picture.Graphic);

    if CheckBoxKeepAspectRatio.Checked then
    begin
      // Calculate scaling factor
      Scale := Min(Image1.Width / Bitmap.Width, Image1.Height / Bitmap.Height);
      W := Round(Bitmap.Width * Scale);
      H := Round(Bitmap.Height * Scale);

      Bitmap.SetSize(W, H);
      Bitmap.Canvas.StretchDraw(Rect(0, 0, W, H), Image1.Picture.Graphic);
    end;

    Result := Bitmap;
  except
    Bitmap.Free;
    raise;
  end;
end;
end.
