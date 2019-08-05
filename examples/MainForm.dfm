object Form1: TForm1
  Left = 538
  Top = 291
  Width = 1025
  Height = 530
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object grp1: TGroupBox
    Left = 16
    Top = 16
    Width = 577
    Height = 129
    Caption = 'Connection parameters'
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
    object lbl1: TLabel
      Left = 368
      Top = 40
      Width = 65
      Height = 13
      Caption = #1053#1072#1078#1084#1080' Login'
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
    ActivePage = tsOthers
    TabIndex = 1
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
    object tsOthers: TTabSheet
      Caption = 'tsOthers'
      ImageIndex = 1
      object btnSession: TButton
        Left = 16
        Top = 8
        Width = 99
        Height = 25
        Caption = 'Session info'
        TabOrder = 0
        OnClick = btnSessionClick
      end
      object btnCheckOnline: TButton
        Left = 16
        Top = 40
        Width = 97
        Height = 25
        Caption = 'Check Online'
        TabOrder = 1
        OnClick = btnCheckOnlineClick
      end
    end
    object tsClient: TTabSheet
      Caption = 'tsClient'
      ImageIndex = 2
      object Label1: TLabel
        Left = 104
        Top = 24
        Width = 31
        Height = 13
        Caption = 'Phone'
      end
      object lblEmail: TLabel
        Left = 104
        Top = 40
        Width = 25
        Height = 13
        Caption = 'Email'
      end
      object Label2: TLabel
        Left = 104
        Top = 64
        Width = 47
        Height = 13
        Caption = 'Birth Date'
      end
      object lblSMS: TLabel
        Left = 112
        Top = 112
        Width = 81
        Height = 13
        Caption = 'Custom text SMS'
      end
      object lblPriority: TLabel
        Left = 328
        Top = 112
        Width = 54
        Height = 13
        Caption = 'Priority [1|2]'
      end
      object lblSMSResponse: TLabel
        Left = 112
        Top = 136
        Width = 68
        Height = 13
        Caption = '<SMS CODE>'
      end
      object lblSessionInfo: TLabel
        Left = 360
        Top = 24
        Width = 62
        Height = 13
        Caption = '<from event>'
      end
      object btnClientInfo: TButton
        Left = 16
        Top = 16
        Width = 75
        Height = 25
        Caption = 'Client Info'
        TabOrder = 0
        OnClick = btnClientInfoClick
      end
      object edtClientInfoPhone: TEdit
        Left = 160
        Top = 16
        Width = 121
        Height = 21
        TabOrder = 1
        Text = '79100000001'
      end
      object edtEmail: TEdit
        Left = 160
        Top = 40
        Width = 121
        Height = 21
        TabOrder = 2
        Text = 'test_1@example.ru'
      end
      object btnClientAdd: TButton
        Left = 16
        Top = 48
        Width = 75
        Height = 25
        Caption = 'Client Add'
        TabOrder = 3
        OnClick = btnClientAddClick
      end
      object dtpBD: TDateTimePicker
        Left = 160
        Top = 64
        Width = 121
        Height = 21
        CalAlignment = dtaLeft
        Date = 29413
        Time = 29413
        DateFormat = dfShort
        DateMode = dmComboBox
        Kind = dtkDate
        ParseInput = False
        TabOrder = 4
      end
      object btnSMS: TButton
        Left = 16
        Top = 112
        Width = 75
        Height = 25
        Caption = 'SendSMS'
        TabOrder = 5
        OnClick = btnSMSClick
      end
      object edtSMSText: TEdit
        Left = 200
        Top = 112
        Width = 121
        Height = 21
        TabOrder = 6
        Text = 'code:  $[sms_code]'
      end
      object edtSMSPriority: TEdit
        Left = 400
        Top = 112
        Width = 73
        Height = 21
        TabOrder = 7
      end
    end
    object tsPurchases: TTabSheet
      Caption = 'tsPurchases'
      ImageIndex = 3
      object lbl2: TLabel
        Left = 128
        Top = 16
        Width = 36
        Height = 13
        Caption = 'Cart ID:'
      end
      object Label3: TLabel
        Left = 128
        Top = 40
        Width = 51
        Height = 13
        Caption = 'Order Num'
      end
      object lbl3: TLabel
        Left = 128
        Top = 136
        Width = 73
        Height = 13
        Caption = 'Move to Phone'
      end
      object btnCalcCart: TButton
        Left = 16
        Top = 8
        Width = 97
        Height = 25
        Caption = 'Calculate Cart'
        TabOrder = 0
        OnClick = btnCalcCartClick
      end
      object edtPurchaseCartID: TEdit
        Left = 192
        Top = 8
        Width = 121
        Height = 21
        TabOrder = 1
      end
      object btnPurchaseNew: TButton
        Left = 16
        Top = 40
        Width = 97
        Height = 25
        Caption = 'Purchase New'
        TabOrder = 2
        OnClick = btnPurchaseNewClick
      end
      object edtOrderNum: TEdit
        Left = 192
        Top = 40
        Width = 121
        Height = 21
        TabOrder = 3
        Text = 'test_order_1'
      end
      object btnPurchaseEdit: TButton
        Left = 16
        Top = 136
        Width = 97
        Height = 25
        Caption = 'Purchase Edit'
        TabOrder = 4
        OnClick = btnPurchaseEditClick
      end
      object btnPurchaseDelete: TButton
        Left = 16
        Top = 168
        Width = 97
        Height = 25
        Caption = 'Purchase Delete'
        TabOrder = 5
        OnClick = btnPurchaseDeleteClick
      end
      object btnPurchaseGet: TButton
        Left = 16
        Top = 72
        Width = 97
        Height = 25
        Caption = 'Purchase Get'
        TabOrder = 6
        OnClick = btnPurchaseGetClick
      end
      object btnPurchaseConfirm: TButton
        Left = 16
        Top = 104
        Width = 97
        Height = 25
        Caption = 'Purchase Confirm'
        TabOrder = 7
        OnClick = btnPurchaseConfirmClick
      end
      object edtToPhone: TEdit
        Left = 208
        Top = 136
        Width = 121
        Height = 21
        TabOrder = 8
      end
      object btnPurchaseToQueue: TButton
        Left = 16
        Top = 232
        Width = 121
        Height = 25
        Caption = 'Purchase To Queue'
        TabOrder = 9
        OnClick = btnPurchaseToQueueClick
      end
      object btnPurchaseReturn: TButton
        Left = 16
        Top = 200
        Width = 97
        Height = 25
        Caption = 'Purchase Return'
        TabOrder = 10
        OnClick = btnPurchaseReturnClick
      end
    end
  end
  object gpInfo: TGroupBox
    Left = 608
    Top = 24
    Width = 369
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
    Width = 369
    Height = 281
    Caption = 'gbInfo'
    TabOrder = 3
    object mmoLog: TMemo
      Left = 2
      Top = 15
      Width = 365
      Height = 264
      Align = alClient
      TabOrder = 0
    end
  end
  object stat1: TStatusBar
    Left = 0
    Top = 484
    Width = 1017
    Height = 19
    Panels = <>
    SimplePanel = True
  end
  object ApplicationEvents1: TApplicationEvents
    OnException = ApplicationEvents1Exception
    OnIdle = ApplicationEvents1Idle
    Left = 520
    Top = 48
  end
  object idhtp1: TIdHTTP
    OnStatus = idhtp1Status
    OnDisconnected = idhtp1Disconnected
    OnConnected = idhtp1Connected
    Request.Accept = 'text/html, */*'
    Request.ContentLength = 0
    Request.ContentRangeEnd = 0
    Request.ContentRangeStart = 0
    Request.ProxyPort = 0
    Request.UserAgent = 'Mozilla/3.0 (compatible; Indy Library)'
    Left = 520
    Top = 96
  end
end
