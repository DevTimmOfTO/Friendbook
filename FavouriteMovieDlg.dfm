object FavoriteMovieDlg: TFavoriteMovieDlg
  Left = 227
  Top = 108
  BorderStyle = bsDialog
  Caption = 'Favorite Movie  '
  ClientHeight = 193
  ClientWidth = 384
  Color = clBtnFace
  ParentFont = True
  Position = poScreenCenter
  TextHeight = 15
  object Label1: TLabel
    Left = 32
    Top = 12
    Width = 35
    Height = 15
    Caption = 'Name:'
  end
  object Label2: TLabel
    Left = 35
    Top = 67
    Width = 31
    Height = 15
    Caption = 'Gerne'
  end
  object Label3: TLabel
    Left = 8
    Top = 109
    Width = 67
    Height = 15
    Caption = 'Why I like it: '
  end
  object Label4: TLabel
    Left = 32
    Top = 37
    Width = 3
    Height = 15
  end
  object OKBtn: TButton
    Left = 300
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Add'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object CancelBtn: TButton
    Left = 300
    Top = 38
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Abbrechen'
    ModalResult = 2
    TabOrder = 1
  end
  object Edit1: TEdit
    Left = 72
    Top = 8
    Width = 121
    Height = 23
    TabOrder = 2
    Text = 'Edit1'
  end
  object ComboBoxEx1: TComboBoxEx
    Left = 72
    Top = 64
    Width = 145
    Height = 24
    ItemsEx = <>
    TabOrder = 3
    Text = 'ComboBoxEx1'
  end
  object Memo1: TMemo
    Left = 72
    Top = 109
    Width = 136
    Height = 62
    Lines.Strings = (
      'Memo1')
    TabOrder = 4
  end
  object Edit2: TEdit
    Left = 72
    Top = 37
    Width = 121
    Height = 23
    TabOrder = 5
    Text = 'Edit2'
  end
end
