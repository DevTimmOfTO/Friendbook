object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'friendship book'
  ClientHeight = 648
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
    Height = 648
    Align = alLeft
    TabOrder = 0
    ExplicitHeight = 629
    object ButtonExport: TButton
      Left = 10
      Top = 117
      Width = 137
      Height = 25
      Caption = 'Export page'
      TabOrder = 0
      OnClick = ButtonExportClick
    end
    object ButtonLookFriendbook: TButton
      Left = 10
      Top = 24
      Width = 137
      Height = 25
      Caption = 'View friendship book'
      TabOrder = 1
      OnClick = ButtonLookFriendbookClick
    end
    object ButtonPrint: TButton
      Left = 10
      Top = 148
      Width = 137
      Height = 25
      Caption = 'Print page'
      TabOrder = 2
      OnClick = ButtonPrintClick
    end
    object ButtonAddAFriend: TButton
      Left = 10
      Top = 55
      Width = 137
      Height = 25
      Caption = 'Add friend to book'
      TabOrder = 3
      OnClick = ButtonAddAFriendClick
    end
    object SearchBox1: TSearchBox
      Left = 10
      Top = 263
      Width = 137
      Height = 23
      TabOrder = 4
      TextHint = 'Search for Names'
      OnChange = SearchBox1Change
      OnInvokeSearch = SearchBox1InvokeSearch
    end
    object ButtonEditFriendSite: TButton
      Left = 10
      Top = 86
      Width = 137
      Height = 25
      Caption = 'Edit friendship book site'
      TabOrder = 5
      OnClick = ButtonEditFriendSiteClick
    end
  end
  object Panel2: TPanel
    Left = 153
    Top = 0
    Width = 818
    Height = 648
    Align = alClient
    Caption = 'Choose an option on the left'
    TabOrder = 1
    ExplicitHeight = 629
  end
  object MainMenu1: TMainMenu
    Left = 8
    Top = 472
    object Datei1: TMenuItem
      Caption = 'File'
      object Datei2: TMenuItem
        Caption = 'New friendship book'
        ShortCut = 16462
        OnClick = DateiNeu1Click
      end
      object Profilladen1: TMenuItem
        Caption = 'Load a friendship book'
        ShortCut = 16463
        OnClick = DateiOeffnen1Click
      end
      object SaveaProfil1: TMenuItem
        Caption = 'Save a friendship book'
        ShortCut = 16467
        OnClick = DateiSpeichern1Click
      end
      object SpeichernunterdesBuches1: TMenuItem
        Caption = 'Save as a friendship book'
        OnClick = DateiSpeichernUnter1Click
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
    object Extras1: TMenuItem
      Caption = 'Extras'
      object Pickrandompersonthefriendshipbook1: TMenuItem
        Caption = 'Pick random person the friendship book'
        OnClick = Pickrandompersonthefriendshipbook1Click
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
  object PrintDialog1: TPrintDialog
    Left = 40
    Top = 408
  end
  object PrinterSetupDialog1: TPrinterSetupDialog
    Left = 40
    Top = 368
  end
end
