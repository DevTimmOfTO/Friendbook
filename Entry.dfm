object AddFriendFrame: TAddFriendFrame
  Left = 0
  Top = 0
  Width = 818
  Height = 629
  Align = alClient
  TabOrder = 0
  object ScrollBox1: TScrollBox
    Left = 0
    Top = 0
    Width = 818
    Height = 629
    Align = alClient
    PopupMenu = PopupMenuFavMov
    TabOrder = 0
    object Label1: TLabel
      Left = 96
      Top = 59
      Width = 50
      Height = 15
      Caption = 'Vorname:'
    end
    object Label2: TLabel
      Left = 96
      Top = 88
      Width = 61
      Height = 15
      Caption = 'Nachname:'
    end
    object Label3: TLabel
      Left = 96
      Top = 117
      Width = 130
      Height = 15
      Caption = 'Spitzname/Username/...:'
    end
    object Label4: TLabel
      Left = 96
      Top = 209
      Width = 79
      Height = 15
      Caption = 'Geburtsdatum:'
    end
    object Label5: TLabel
      Left = 96
      Top = 242
      Width = 81
      Height = 15
      Caption = 'Favorite Movie:'
    end
    object EditFirstName: TEdit
      Left = 272
      Top = 56
      Width = 193
      Height = 23
      TabOrder = 0
    end
    object EditSurname: TEdit
      Left = 272
      Top = 85
      Width = 193
      Height = 23
      TabOrder = 1
    end
    object MemoNicknames: TMemo
      Left = 272
      Top = 114
      Width = 193
      Height = 89
      TabOrder = 2
    end
    object DateTimePickerBirthday: TDateTimePicker
      Left = 272
      Top = 209
      Width = 193
      Height = 23
      Date = 36526.000000000000000000
      Time = 0.851107280090218400
      TabOrder = 3
    end
    object Button1: TButton
      Left = 544
      Top = 238
      Width = 98
      Height = 25
      Caption = 'Add one!'
      TabOrder = 4
      OnClick = Button1Click
    end
    object ListView1: TListView
      Left = 272
      Top = 238
      Width = 266
      Height = 113
      Columns = <>
      PopupMenu = PopupMenuFavMov
      TabOrder = 5
      ViewStyle = vsReport
    end
    object GroupBox1: TGroupBox
      Left = 544
      Top = 24
      Width = 217
      Height = 153
      Caption = 'Picture'
      TabOrder = 6
      object Image1: TImage
        Left = 11
        Top = 24
        Width = 105
        Height = 105
      end
      object PaintBox1: TPaintBox
        Left = 16
        Top = 24
        Width = 105
        Height = 105
      end
      object Button2: TButton
        Left = 139
        Top = 31
        Width = 75
        Height = 25
        Caption = 'Upload one!'
        TabOrder = 0
        OnClick = Button2Click
      end
    end
  end
  object PopupMenuFavMov: TPopupMenu
    Left = 568
    Top = 456
    object AddEntry1: TMenuItem
      Caption = 'Add Entry'
      OnClick = Button1Click
    end
    object AddEntry2: TMenuItem
      Caption = 'Remove Entry'
      OnClick = AddEntry2Click
    end
  end
  object ImageList1: TImageList
    Left = 632
    Top = 456
  end
end
