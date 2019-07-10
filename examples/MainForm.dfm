object Form1: TForm1
  Left = 157
  Top = 227
  Width = 1109
  Height = 511
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object grp1: TGroupBox
    Left = 16
    Top = 16
    Width = 577
    Height = 129
    Caption = '?????????'
    TabOrder = 0
    object lblURL: TLabel
      Left = 16
      Top = 24
      Width = 42
      Height = 13
      Caption = 'API URL'
    end
    object lblUser: TLabel
      Left = 16
      Top = 56
      Width = 22
      Height = 13
      Caption = 'User'
    end
    object lblPassword: TLabel
      Left = 16
      Top = 88
      Width = 46
      Height = 13
      Caption = 'Password'
    end
    object lblStatus: TLabel
      Left = 368
      Top = 24
      Width = 43
      Height = 13
      Caption = 'CLOSED'
    end
    object edtURL: TEdit
      Left = 80
      Top = 24
      Width = 153
      Height = 21
      TabOrder = 0
      Text = 'http://127.0.0.1:8080'
    end
    object edtUser: TEdit
      Left = 80
      Top = 56
      Width = 153
      Height = 21
      TabOrder = 1
      Text = 'alex'
    end
    object edtPassword: TEdit
      Left = 80
      Top = 88
      Width = 153
      Height = 21
      TabOrder = 2
      Text = '123'
    end
    object btnCreate: TButton
      Left = 280
      Top = 24
      Width = 75
      Height = 25
      Caption = 'Create'
      TabOrder = 3
      OnClick = btnCreateClick
    end
    object btnClose: TButton
      Left = 280
      Top = 56
      Width = 75
      Height = 25
      Caption = 'CLOSE'
      TabOrder = 4
      OnClick = btnCloseClick
    end
  end
  object pcOpers: TPageControl
    Left = 24
    Top = 160
    Width = 569
    Height = 305
    ActivePage = tsAuth
    TabIndex = 0
    TabOrder = 1
    object tsAuth: TTabSheet
      Caption = 'Auth'
      ImageIndex = 1
      object btnLogin: TButton
        Left = 16
        Top = 8
        Width = 99
        Height = 25
        Caption = 'Login'
        TabOrder = 0
        OnClick = btnLoginClick
      end
      object btnRefreshToken: TButton
        Left = 16
        Top = 48
        Width = 97
        Height = 25
        Caption = 'Refresh Tokens'
        TabOrder = 1
        OnClick = btnRefreshTokenClick
      end
      object chkOnlyAcceess: TCheckBox
        Left = 128
        Top = 51
        Width = 97
        Height = 17
        Caption = 'OnlyAcceess'
        TabOrder = 2
      end
    end
  end
  object gpInfo: TGroupBox
    Left = 608
    Top = 24
    Width = 473
    Height = 121
    Caption = 'API Information'
    TabOrder = 2
    object lblInfoToken: TLabel
      Left = 16
      Top = 24
      Width = 69
      Height = 13
      Caption = 'Access Token'
    end
    object lblInfoRefreshToken: TLabel
      Left = 16
      Top = 48
      Width = 71
      Height = 13
      Caption = 'Refresh Token'
    end
    object edtInfoAccessToken: TEdit
      Left = 112
      Top = 16
      Width = 345
      Height = 21
      Color = clBtnFace
      ReadOnly = True
      TabOrder = 0
    end
    object edtInfoRefreshToken: TEdit
      Left = 112
      Top = 40
      Width = 345
      Height = 21
      Color = clBtnFace
      ReadOnly = True
      TabOrder = 1
    end
  end
  object gbInfo: TGroupBox
    Left = 608
    Top = 176
    Width = 481
    Height = 281
    Caption = 'gbInfo'
    TabOrder = 3
    object mmoLog: TMemo
      Left = 2
      Top = 15
      Width = 477
      Height = 264
      Align = alClient
      TabOrder = 0
    end
  end
  object ApplicationEvents1: TApplicationEvents
    OnIdle = ApplicationEvents1Idle
    Left = 520
    Top = 48
  end
end
