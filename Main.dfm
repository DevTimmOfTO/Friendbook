object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Friendbook'
  ClientHeight = 629
  ClientWidth = 971
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Menu = MainMenu1
  OnCreate = FormCreate
  TextHeight = 15
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 153
    Height = 629
    Align = alLeft
    TabOrder = 0
    object ButtonExport: TButton
      Left = 10
      Top = 184
      Width = 137
      Height = 25
      Caption = 'Export page'
      TabOrder = 0
    end
    object ButtonLookFriendbook: TButton
      Left = 8
      Top = 24
      Width = 137
      Height = 25
      Caption = 'View friend book'
      TabOrder = 1
    end
    object GroupBox1: TGroupBox
      Left = 10
      Top = 86
      Width = 137
      Height = 84
      Caption = 'Edit'
      TabOrder = 2
      object ButtonEditOwnSite: TButton
        Left = 3
        Top = 16
        Width = 131
        Height = 25
        Caption = 'Your own page'
        TabOrder = 0
      end
      object ButtonEditFriendSite: TButton
        Left = 3
        Top = 47
        Width = 131
        Height = 25
        Caption = 'A friend'#39's page'
        TabOrder = 1
      end
    end
    object ButtonPrint: TButton
      Left = 10
      Top = 215
      Width = 137
      Height = 25
      Caption = 'Print page'
      TabOrder = 3
    end
    object ButtonAddAFriend: TButton
      Left = 10
      Top = 55
      Width = 137
      Height = 25
      Caption = 'Add friend'
      TabOrder = 4
      OnClick = ButtonAddAFriendClick
    end
    object SearchBox1: TSearchBox
      Left = 10
      Top = 263
      Width = 137
      Height = 23
      TabOrder = 5
      TextHint = 'Search for ...'
    end
  end
  object Panel2: TPanel
    Left = 153
    Top = 0
    Width = 818
    Height = 629
    Align = alClient
    Caption = 'W'#228'hlen Sie links eine Option aus.'
    TabOrder = 1
  end
  object MainMenu1: TMainMenu
    Left = 8
    Top = 472
    object Datei1: TMenuItem
      Caption = 'File'
      object Datei2: TMenuItem
        Caption = 'New Profil'
      end
      object Profilladen1: TMenuItem
        Caption = 'Load a Profil'
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object RecentProjects1: TMenuItem
        Caption = 'Recent Projects'
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Caption = 'Exit'
        OnClick = Exit1Click
      end
    end
    object Language1: TMenuItem
      Caption = 'Language'
      object GermanDeutsch1: TMenuItem
        Caption = 'German | Deutsch'
      end
      object EnglishEnglisch1: TMenuItem
        Caption = 'English | Englisch'
      end
    end
    object Extras1: TMenuItem
      Caption = 'Extras'
      object Pickrandompersonthefriendshipbook1: TMenuItem
        Caption = 'Pick random person the friendship book'
      end
      object GivearandomTorontoFunFact1: TMenuItem
        Caption = 'Give a random fun-fact about...'
        object imm1: TMenuItem
          Caption = '... Timm'
        end
        object Toronto1: TMenuItem
          Caption = '... Toronto'
        end
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object About1: TMenuItem
        Caption = 'About'
      end
    end
  end
end
