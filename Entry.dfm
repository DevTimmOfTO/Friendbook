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
    HorzScrollBar.Range = 794
    VertScrollBar.Range = 1500
    VertScrollBar.Smooth = True
    VertScrollBar.Style = ssFlat
    Align = alClient
    AutoScroll = False
    PopupMenu = PopupMenuFavMov
    TabOrder = 0
    object Label18: TLabel
      Left = 36
      Top = 1318
      Width = 83
      Height = 15
      Caption = 'something else:'
    end
    object MemoNicknames: TMemo
      Left = 200
      Top = 114
      Width = 193
      Height = 89
      TabOrder = 0
    end
    object GroupBox1: TGroupBox
      Left = 432
      Top = 24
      Width = 362
      Height = 415
      Caption = 'picture'
      TabOrder = 1
      object Image1: TImage
        Left = 16
        Top = 35
        Width = 233
        Height = 377
      end
      object PaintBox1: TPaintBox
        Left = 16
        Top = 35
        Width = 233
        Height = 377
      end
      object Button2: TButton
        Left = 275
        Top = 35
        Width = 75
        Height = 25
        Caption = 'Upload one!'
        TabOrder = 0
        OnClick = Button2Click
      end
    end
    object GroupBox2: TGroupBox
      Left = 24
      Top = 445
      Width = 770
      Height = 852
      Caption = 'interests'
      Padding.Bottom = 10
      TabOrder = 2
      object Label5: TLabel
        Left = 12
        Top = 25
        Width = 84
        Height = 15
        Caption = 'favorite movies:'
      end
      object Label6: TLabel
        Left = 9
        Top = 285
        Width = 89
        Height = 15
        Caption = 'favorite tv-show:'
      end
      object Label7: TLabel
        Left = 12
        Top = 544
        Width = 45
        Height = 15
        Caption = 'hobbies:'
      end
      object Label16: TLabel
        Left = 12
        Top = 647
        Width = 50
        Height = 30
        Caption = 'volunteer'#13#10'activities:'
      end
      object Label17: TLabel
        Left = 12
        Top = 753
        Width = 49
        Height = 15
        Caption = 'fun facts:'
      end
      object ListView1: TListView
        Left = 104
        Top = 21
        Width = 537
        Height = 236
        Columns = <>
        PopupMenu = PopupMenuFavMov
        TabOrder = 0
        ViewStyle = vsReport
      end
      object Button1: TButton
        Left = 669
        Top = 21
        Width = 98
        Height = 25
        Caption = 'add movie'
        TabOrder = 1
        OnClick = Button1Click
      end
      object ListView2: TListView
        Left = 104
        Top = 285
        Width = 537
        Height = 236
        Columns = <>
        PopupMenu = PopupMenuFavSeries
        TabOrder = 2
        ViewStyle = vsReport
      end
      object Button3: TButton
        Left = 667
        Top = 281
        Width = 98
        Height = 25
        Caption = 'add tv-show'
        TabOrder = 3
        OnClick = Button3Click
      end
      object Memo2: TMemo
        Left = 104
        Top = 541
        Width = 537
        Height = 89
        TabOrder = 4
      end
      object Memo3: TMemo
        Left = 104
        Top = 644
        Width = 537
        Height = 89
        TabOrder = 5
      end
      object Memo4: TMemo
        Left = 104
        Top = 750
        Width = 537
        Height = 89
        TabOrder = 6
      end
    end
    object GroupBox3: TGroupBox
      Left = 24
      Top = 298
      Width = 402
      Height = 141
      Caption = 'adress'
      TabOrder = 3
      object Label8: TLabel
        Left = 12
        Top = 24
        Width = 136
        Height = 15
        Caption = 'street && housing number:'
      end
      object Label9: TLabel
        Left = 12
        Top = 56
        Width = 64
        Height = 15
        Caption = 'postal code:'
      end
      object Label10: TLabel
        Left = 200
        Top = 56
        Width = 24
        Height = 15
        Caption = 'City:'
      end
      object Label11: TLabel
        Left = 12
        Top = 88
        Width = 98
        Height = 15
        Caption = 'state (sth like this):'
      end
      object Label12: TLabel
        Left = 12
        Top = 117
        Width = 44
        Height = 15
        Caption = 'country:'
      end
      object Edit1: TEdit
        Left = 176
        Top = 24
        Width = 193
        Height = 23
        TabOrder = 0
      end
      object Edit2: TEdit
        Left = 82
        Top = 53
        Width = 96
        Height = 23
        TabOrder = 1
      end
      object Edit3: TEdit
        Left = 232
        Top = 53
        Width = 137
        Height = 23
        TabOrder = 2
      end
      object Edit4: TEdit
        Left = 176
        Top = 84
        Width = 193
        Height = 23
        TabOrder = 3
      end
      object Edit5: TEdit
        Left = 176
        Top = 113
        Width = 193
        Height = 23
        TabOrder = 4
      end
    end
    object GroupBox4: TGroupBox
      Left = 24
      Top = 24
      Width = 402
      Height = 268
      Caption = 'personal information'
      TabOrder = 4
      object Label1: TLabel
        Left = 16
        Top = 27
        Width = 56
        Height = 15
        Caption = 'first name:'
      end
      object Label2: TLabel
        Left = 16
        Top = 56
        Width = 54
        Height = 15
        Caption = 'last name:'
      end
      object Label3: TLabel
        Left = 16
        Top = 85
        Width = 126
        Height = 15
        Caption = 'nickname/username/...:'
      end
      object Label4: TLabel
        Left = 16
        Top = 148
        Width = 68
        Height = 15
        Caption = 'date of birth:'
      end
      object Label13: TLabel
        Left = 16
        Top = 176
        Width = 101
        Height = 15
        Caption = 'religious affiliation:'
      end
      object Label14: TLabel
        Left = 16
        Top = 200
        Width = 117
        Height = 26
        Caption = 'profession/job/career:'
      end
      object Label15: TLabel
        Left = 16
        Top = 232
        Width = 74
        Height = 15
        Caption = 'marital status:'
      end
      object DateTimePickerBirthday: TDateTimePicker
        Left = 176
        Top = 142
        Width = 193
        Height = 23
        Date = 36526.000000000000000000
        Time = 0.851107280090218400
        TabOrder = 0
      end
      object EditSurname: TEdit
        Left = 176
        Top = 53
        Width = 193
        Height = 23
        TabOrder = 1
      end
      object Memo1: TMemo
        Left = 176
        Top = 82
        Width = 193
        Height = 54
        TabOrder = 2
      end
      object EditFirstName: TEdit
        Left = 176
        Top = 24
        Width = 193
        Height = 23
        TabOrder = 3
      end
      object ComboBox1: TComboBox
        Left = 176
        Top = 171
        Width = 193
        Height = 23
        TabOrder = 4
        TextHint = 'Choose one'
        Items.Strings = (
          'Christianity - Catholicism'
          'Christianity - Orthodoxy'
          'Christianity - Protestantism'
          'Christianity - Anglicanism'
          'Christianity - Baptists'
          'Christianity - Methodists'
          'Christianity - Pentecostalism'
          'Christianity - Adventists'
          'Islam - Sunnis'
          'Islam - Shiites'
          'Islam - Sufism'
          'Islam - Ahmadiyya'
          'Hinduism - Vaishnavism'
          'Hinduism - Shaivism'
          'Hinduism - Shaktism'
          'Hinduism - Smartism'
          'Buddhism - Theravada'
          'Buddhism - Mahayana'
          'Buddhism - Vajrayana'
          'Buddhism - Zen'
          'Buddhism - Tibetan Buddhism'
          'Judaism - Orthodox Judaism'
          'Judaism - Conservative Judaism'
          'Judaism - Reform Judaism'
          'Judaism - Reconstructionist Judaism'
          'Judaism - Messianic Judaism'
          'Sikhism - Khalsa'
          'Sikhism - Nanakpanthi'
          'Bahai - Baha'#39'i Faith'
          'Jainism - Digambara'
          'Jainism - Svetambara'
          'Shinto - Koshinto'
          'Shinto - Jinja Shinto'
          'Shinto - Folk Shinto'
          'Taoism - Philosophical Taoism'
          'Taoism - Religious Taoism'
          'Zoroastrianism - Parsis'
          'Zoroastrianism - Iranian Zoroastrianism'
          'Confucianism - Traditional Confucianism'
          'Confucianism - Neo-Confucianism'
          'New Religious Movement - Scientology'
          'New Religious Movement - Falun Gong'
          'New Religious Movement - Raelism'
          
            'New Religious Movement - Church of Jesus Christ of Latter-day Sa' +
            'ints (Mormons)'
          'No religion - Atheism'
          'No religion - Agnosticism'
          'No religion - Humanism'
          'No religion - Deism'
          'No religion - Pantheism'
          'I don'#39't want to answer that.')
      end
      object Edit6: TEdit
        Left = 176
        Top = 200
        Width = 193
        Height = 23
        TabOrder = 5
      end
      object ComboBox2: TComboBox
        Left = 176
        Top = 229
        Width = 193
        Height = 23
        TabOrder = 6
        TextHint = 'Choose one'
        Items.Strings = (
          'Single'
          'Married'
          'Divorced'
          'Widowed'
          'In a relationship'
          'Separated')
      end
    end
    object Button4: TButton
      Left = 24
      Top = 1384
      Width = 770
      Height = 113
      Caption = #55357#56510' Save'
      TabOrder = 5
      OnClick = Button4Click
    end
    object Memo5: TMemo
      Left = 126
      Top = 1315
      Width = 539
      Height = 63
      TabOrder = 6
    end
  end
  object PopupMenuFavMov: TPopupMenu
    BiDiMode = bdLeftToRight
    ParentBiDiMode = False
    Left = 712
    Top = 568
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
    Left = 736
    Top = 240
  end
  object PopupMenuFavSeries: TPopupMenu
    BiDiMode = bdLeftToRight
    ParentBiDiMode = False
    Left = 736
    Top = 528
    object MenuItem1: TMenuItem
      Caption = 'Add Entry'
      OnClick = Button3Click
    end
    object MenuItem2: TMenuItem
      Caption = 'Remove Entry'
      OnClick = AddEntry3Click
    end
  end
end
