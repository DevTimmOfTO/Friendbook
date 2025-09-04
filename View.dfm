object FrameView: TFrameView
  Left = 0
  Top = 0
  Width = 800
  Height = 600
  TabOrder = 0
  object ScrollBox1: TScrollBox
    Left = 0
    Top = 0
    Width = 800
    Height = 600
    HorzScrollBar.Range = 776
    VertScrollBar.Range = 985
    Align = alClient
    AutoScroll = False
    TabOrder = 0
    object Panel1: TPanel
      Left = 0
      Top = 0
      Width = 779
      Height = 200
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 0
      object Image1: TImage
        Left = 16
        Top = 16
        Width = 120
        Height = 160
        Proportional = True
        Stretch = True
      end
      object Label1: TLabel
        Left = 160
        Top = 24
        Width = 34
        Height = 15
        Caption = 'name:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object LabelFullName: TLabel
        Left = 200
        Top = 24
        Width = 79
        Height = 15
        Caption = 'LabelFullName'
      end
      object Label3: TLabel
        Left = 160
        Top = 48
        Width = 23
        Height = 15
        Caption = 'age:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object LabelAge: TLabel
        Left = 200
        Top = 48
        Width = 49
        Height = 15
        Caption = 'LabelAge'
      end
      object GroupBox1: TGroupBox
        Left = 160
        Top = 80
        Width = 300
        Height = 100
        Caption = 'nickname/username/..:'
        TabOrder = 0
        object LabelNicknames: TLabel
          Left = 8
          Top = 20
          Width = 87
          Height = 15
          Caption = 'LabelNicknames'
        end
      end
    end
    object GroupBox2: TGroupBox
      Left = 16
      Top = 220
      Width = 760
      Height = 80
      Caption = 'description'
      TabOrder = 1
      object MemoDescription: TMemo
        Left = 3
        Top = 16
        Width = 754
        Height = 61
        ReadOnly = True
        TabOrder = 0
      end
    end
    object GroupBox3: TGroupBox
      Left = 16
      Top = 320
      Width = 360
      Height = 162
      Caption = 'adress'
      TabOrder = 2
      object LabelAddress: TLabel
        Left = 8
        Top = 20
        Width = 70
        Height = 15
        Caption = 'LabelAddress'
        WordWrap = True
      end
    end
    object GroupBox6: TGroupBox
      Left = 400
      Top = 320
      Width = 376
      Height = 162
      Caption = 'personal information'
      TabOrder = 3
      object LabelPersonalInfo: TLabel
        Left = 8
        Top = 20
        Width = 94
        Height = 15
        Caption = 'LabelPersonalInfo'
        WordWrap = True
      end
    end
    object GroupBox4: TGroupBox
      Left = 16
      Top = 500
      Width = 360
      Height = 281
      Caption = 'favorite movie'
      TabOrder = 4
      object ListView1: TListView
        Left = 3
        Top = 25
        Width = 354
        Height = 253
        Columns = <>
        ReadOnly = True
        RowSelect = True
        TabOrder = 0
        ViewStyle = vsReport
      end
    end
    object GroupBox5: TGroupBox
      Left = 400
      Top = 500
      Width = 376
      Height = 281
      Caption = 'favorite tv-shows'
      TabOrder = 5
      object ListView2: TListView
        Left = 3
        Top = 25
        Width = 370
        Height = 253
        Columns = <>
        ReadOnly = True
        RowSelect = True
        TabOrder = 0
        ViewStyle = vsReport
      end
    end
    object GroupBox7: TGroupBox
      Left = 16
      Top = 796
      Width = 240
      Height = 164
      Caption = 'hobbies'
      TabOrder = 6
      object MemoHobbies: TMemo
        Left = 3
        Top = 16
        Width = 234
        Height = 145
        ReadOnly = True
        TabOrder = 0
      end
    end
    object GroupBox8: TGroupBox
      Left = 280
      Top = 796
      Width = 240
      Height = 164
      Caption = 'volunteer activities'
      TabOrder = 7
      object MemoVolunteerActivities: TMemo
        Left = 3
        Top = 16
        Width = 234
        Height = 145
        Lines.Strings = (
          '')
        ReadOnly = True
        TabOrder = 0
      end
    end
    object GroupBox9: TGroupBox
      Left = 544
      Top = 796
      Width = 232
      Height = 164
      Caption = 'fun fact'
      TabOrder = 8
      object MemoFunFact: TMemo
        Left = 3
        Top = 16
        Width = 226
        Height = 145
        ReadOnly = True
        TabOrder = 0
      end
    end
  end
  object ImageList1: TImageList
    Left = 720
    Top = 16
  end
  object ImageList2: TImageList
    Left = 752
    Top = 16
  end
end
