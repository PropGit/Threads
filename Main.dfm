object Form1: TForm1
  Left = 838
  Top = 337
  Width = 307
  Height = 382
  Caption = 'Thread Test'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object CountToLabel: TLabel
    Left = 85
    Top = 11
    Width = 47
    Height = 13
    Caption = 'Count To:'
  end
  object MainCounterLabel: TLabel
    Left = 192
    Top = 214
    Width = 66
    Height = 13
    Caption = 'Main Counter:'
  end
  object Thread1CounterLabel: TLabel
    Left = 192
    Top = 247
    Width = 86
    Height = 13
    Caption = 'Thread 1 Counter:'
  end
  object Thread2CounterLabel: TLabel
    Left = 192
    Top = 279
    Width = 86
    Height = 13
    Caption = 'Thread 2 Counter:'
  end
  object Thread3CounterLabel: TLabel
    Left = 192
    Top = 311
    Width = 86
    Height = 13
    Caption = 'Thread 3 Counter:'
  end
  object Bevel1: TBevel
    Left = 16
    Top = 136
    Width = 262
    Height = 1
  end
  object ThreadCountLabel: TLabel
    Left = 88
    Top = 158
    Width = 68
    Height = 13
    Caption = 'Thread Count:'
  end
  object MainCounterEdit: TEdit
    Left = 104
    Top = 211
    Width = 81
    Height = 21
    Color = clSkyBlue
    ReadOnly = True
    TabOrder = 0
  end
  object Thread1CounterEdit: TEdit
    Left = 104
    Top = 243
    Width = 81
    Height = 21
    Color = clSkyBlue
    TabOrder = 1
  end
  object Thread1Button: TButton
    Left = 16
    Top = 240
    Width = 75
    Height = 25
    Caption = 'Start Thread'
    TabOrder = 2
    OnClick = ThreadButtonClick
  end
  object MainButton: TButton
    Left = 16
    Top = 208
    Width = 75
    Height = 25
    Caption = 'Start Main'
    TabOrder = 3
    OnClick = MainButtonClick
  end
  object MaxCountEdit: TEdit
    Left = 135
    Top = 8
    Width = 81
    Height = 21
    TabOrder = 4
    Text = '10000'
  end
  object Thread2CounterEdit: TEdit
    Left = 104
    Top = 274
    Width = 81
    Height = 21
    Color = clSkyBlue
    TabOrder = 5
  end
  object Thread2Button: TButton
    Tag = 1
    Left = 16
    Top = 272
    Width = 75
    Height = 25
    Caption = 'Start Thread'
    TabOrder = 6
    OnClick = ThreadButtonClick
  end
  object Thread3CounterEdit: TEdit
    Left = 104
    Top = 306
    Width = 81
    Height = 21
    Color = clSkyBlue
    TabOrder = 7
  end
  object Thread3Button: TButton
    Tag = 2
    Left = 16
    Top = 304
    Width = 75
    Height = 25
    Caption = 'Start Thread'
    TabOrder = 8
    OnClick = ThreadButtonClick
  end
  object CommType: TRadioGroup
    Left = 24
    Top = 40
    Width = 241
    Height = 73
    Caption = 'Thread Communication'
    ItemIndex = 0
    Items.Strings = (
      'Use Synchronize()'
      'Use QueueUserAPC() + SleepEx()')
    TabOrder = 9
  end
  object ThreadCountEdit: TEdit
    Left = 168
    Top = 155
    Width = 25
    Height = 21
    Color = clSkyBlue
    ReadOnly = True
    TabOrder = 10
    Text = '0'
  end
end
