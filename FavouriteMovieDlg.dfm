object FavoriteMovieDlg: TFavoriteMovieDlg
  Left = 227
  Top = 108
  BorderStyle = bsDialog
  Caption = 'Favorite Movie  '
  ClientHeight = 370
  ClientWidth = 609
  Color = clBtnFace
  ParentFont = True
  Position = poScreenCenter
  OnCreate = FormCreate
  TextHeight = 15
  object Label1: TLabel
    Left = 32
    Top = 12
    Width = 56
    Height = 15
    Caption = 'movie title'
  end
  object OKBtn: TButton
    Left = 445
    Top = 337
    Width = 75
    Height = 25
    Caption = 'Add'
    Default = True
    Enabled = False
    ModalResult = 1
    TabOrder = 0
    OnClick = OKBtnClick
  end
  object CancelBtn: TButton
    Left = 526
    Top = 337
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Abbrechen'
    ModalResult = 2
    TabOrder = 1
  end
  object Edit1: TEdit
    Left = 94
    Top = 8
    Width = 195
    Height = 23
    TabOrder = 2
  end
  object Button1: TButton
    Left = 301
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Search'
    TabOrder = 3
    OnClick = Button1Click
  end
  object ListView1: TListView
    Left = 40
    Top = 37
    Width = 561
    Height = 276
    Columns = <>
    TabOrder = 4
    OnData = ListView1Data
    OnSelectItem = ListView1SelectItem
  end
  object NetHTTPClientFavMov: TNetHTTPClient
    UserAgent = 'Embarcadero URI Client/1.0'
    Left = 312
    Top = 88
  end
  object ImageList1: TImageList
    Height = 150
    Width = 100
    Left = 344
    Top = 88
  end
end
