object Form1: TForm1
  Left = 168
  Top = 312
  Width = 744
  Height = 373
  Caption = 'Form1'
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
  object lbl1: TLabel
    Left = 16
    Top = 24
    Width = 42
    Height = 13
    Caption = 'URL MQ'
  end
  object edtURL: TEdit
    Left = 80
    Top = 24
    Width = 121
    Height = 21
    TabOrder = 0
    Text = 'amqp://guest:guest@0.0.0.0:5672/'
  end
  object grpDirect: TGroupBox
    Left = 8
    Top = 56
    Width = 209
    Height = 225
    Caption = 'grpDirect'
    TabOrder = 1
    object lblStatus: TLabel
      Left = 128
      Top = 32
      Width = 40
      Height = 13
      Caption = '<status>'
    end
    object btnPublish: TButton
      Left = 32
      Top = 192
      Width = 89
      Height = 25
      Caption = 'btnPublish'
      TabOrder = 0
      OnClick = btnPublishClick
    end
    object btnConnect: TButton
      Left = 32
      Top = 24
      Width = 84
      Height = 25
      Caption = 'btnConnect'
      TabOrder = 1
      OnClick = btnConnectClick
    end
    object btnDisconn: TButton
      Left = 32
      Top = 56
      Width = 84
      Height = 25
      Caption = 'btnDisconn'
      TabOrder = 2
      OnClick = btnDisconnClick
    end
    object btnNEwChannel: TButton
      Left = 32
      Top = 96
      Width = 84
      Height = 25
      Caption = 'New channel'
      TabOrder = 3
      OnClick = btnNEwChannelClick
    end
    object btnCloseChannel: TButton
      Left = 32
      Top = 128
      Width = 84
      Height = 25
      Caption = 'Close Channel'
      TabOrder = 4
      OnClick = btnCloseChannelClick
    end
    object edtPublish: TEdit
      Left = 32
      Top = 168
      Width = 121
      Height = 21
      TabOrder = 5
      Text = 'edtPublish'
    end
  end
  object grpWrapper: TGroupBox
    Left = 224
    Top = 64
    Width = 361
    Height = 241
    Caption = 'grpWrapper'
    TabOrder = 2
    object lblWStatus: TLabel
      Left = 16
      Top = 16
      Width = 51
      Height = 13
      Caption = 'lblWStatus'
    end
    object lblWCountChannel: TLabel
      Left = 128
      Top = 16
      Width = 67
      Height = 13
      Caption = 'CountChannel'
    end
    object btnWconn: TButton
      Left = 16
      Top = 104
      Width = 89
      Height = 25
      Caption = 'Connect'
      TabOrder = 0
      OnClick = btnWconnClick
    end
    object btnWDiscon: TButton
      Left = 16
      Top = 136
      Width = 89
      Height = 25
      Caption = 'Disconnect'
      TabOrder = 1
      OnClick = btnWDisconClick
    end
    object Button1: TButton
      Left = 16
      Top = 40
      Width = 89
      Height = 25
      Caption = 'Create'
      TabOrder = 2
      OnClick = Button1Click
    end
    object btnFree: TButton
      Left = 16
      Top = 72
      Width = 89
      Height = 25
      Caption = 'Free'
      TabOrder = 3
      OnClick = btnFreeClick
    end
    object btnWCreateChannel: TButton
      Left = 128
      Top = 40
      Width = 97
      Height = 25
      Caption = 'Create Channel'
      TabOrder = 4
      OnClick = btnWCreateChannelClick
    end
    object btnWCloseChannel: TButton
      Left = 128
      Top = 72
      Width = 97
      Height = 25
      Caption = 'Close Channel'
      TabOrder = 5
      OnClick = btnWCloseChannelClick
    end
    object btnWCloseAllChannels: TButton
      Left = 128
      Top = 136
      Width = 97
      Height = 25
      Caption = 'Close All Channels'
      TabOrder = 6
      OnClick = btnWCloseAllChannelsClick
    end
    object btnWPublish: TButton
      Left = 16
      Top = 208
      Width = 75
      Height = 25
      Caption = 'Publish'
      TabOrder = 7
      OnClick = btnWPublishClick
    end
    object edtWPublish: TEdit
      Left = 16
      Top = 176
      Width = 121
      Height = 21
      TabOrder = 8
      Text = 'Message!'
    end
    object btnWFreeChannel: TButton
      Left = 128
      Top = 104
      Width = 97
      Height = 25
      Caption = 'Free Channel'
      TabOrder = 9
      OnClick = btnWFreeChannelClick
    end
    object btnWstart: TButton
      Left = 152
      Top = 176
      Width = 75
      Height = 25
      Caption = 'Start'
      TabOrder = 10
      OnClick = btnWstartClick
    end
  end
  object tmr1: TTimer
    OnTimer = tmr1Timer
    Left = 32
    Top = 40
  end
  object tmr2: TTimer
    Enabled = False
    OnTimer = tmr2Timer
    Left = 456
    Top = 240
  end
end
