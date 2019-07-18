object Form1: TForm1
  Left = 619
  Top = 359
  Width = 1305
  Height = 675
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
  object lblStatus: TLabel
    Left = 360
    Top = 24
    Width = 40
    Height = 13
    Caption = '<status>'
  end
  object edtURL: TEdit
    Left = 80
    Top = 24
    Width = 121
    Height = 21
    TabOrder = 0
    Text = 'amqp://guest:guest@0.0.0.0:5672/'
  end
  object btnConnect: TButton
    Left = 232
    Top = 24
    Width = 84
    Height = 25
    Caption = 'btnConnect'
    TabOrder = 1
    OnClick = btnConnectClick
  end
  object btnDisconn: TButton
    Left = 232
    Top = 56
    Width = 84
    Height = 25
    Caption = 'btnDisconn'
    TabOrder = 2
    OnClick = btnDisconnClick
  end
  object btnNEwChannel: TButton
    Left = 232
    Top = 96
    Width = 84
    Height = 25
    Caption = 'New channel'
    TabOrder = 3
    OnClick = btnNEwChannelClick
  end
  object btnCloseChannel: TButton
    Left = 232
    Top = 128
    Width = 84
    Height = 25
    Caption = 'Close Channel'
    TabOrder = 4
    OnClick = btnCloseChannelClick
  end
  object edtPublish: TEdit
    Left = 56
    Top = 168
    Width = 121
    Height = 21
    TabOrder = 5
    Text = 'edtPublish'
  end
  object btnPublish: TButton
    Left = 224
    Top = 160
    Width = 89
    Height = 25
    Caption = 'btnPublish'
    TabOrder = 6
    OnClick = btnPublishClick
  end
  object btn1: TButton
    Left = 248
    Top = 224
    Width = 75
    Height = 25
    Caption = 'btn1'
    TabOrder = 7
    OnClick = btn1Click
  end
  object tmr1: TTimer
    OnTimer = tmr1Timer
    Left = 520
    Top = 240
  end
end
