unit MainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, api_pl, AppEvnts, ComCtrls, NMURL;

type
  TForm1 = class(TForm)
    grp1: TGroupBox;
    edtURL: TEdit;
    lblURL: TLabel;
    lblUser: TLabel;
    edtUser: TEdit;
    edtPassword: TEdit;
    lblPassword: TLabel;
    btnCreate: TButton;
    btnClose: TButton;
    lblStatus: TLabel;
    ApplicationEvents1: TApplicationEvents;
    pcOpers: TPageControl;
    tsAuth: TTabSheet;
    btnLogin: TButton;
    btnRefreshToken: TButton;
    gpInfo: TGroupBox;
    lblInfoToken: TLabel;
    lblInfoRefreshToken: TLabel;
    edtInfoAccessToken: TEdit;
    edtInfoRefreshToken: TEdit;
    gbInfo: TGroupBox;
    mmoLog: TMemo;
    chkOnlyAcceess: TCheckBox;
    tsOthers: TTabSheet;
    btnSession: TButton;
    tsClient: TTabSheet;
    btnClientInfo: TButton;
    edtClientInfoPhone: TEdit;
    NMURL1: TNMURL;
    lbl1: TLabel;
    edtEmail: TEdit;
    Label1: TLabel;
    lblEmail: TLabel;
    btnClientAdd: TButton;
    dtpBD: TDateTimePicker;
    Label2: TLabel;
    procedure btnCreateClick(Sender: TObject);
    procedure ApplicationEvents1Idle(Sender: TObject; var Done: Boolean);
    procedure FormDestroy(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnLoginClick(Sender: TObject);
    procedure btnRefreshTokenClick(Sender: TObject);
    procedure btnSessionClick(Sender: TObject);
    procedure btnClientInfoClick(Sender: TObject);
    procedure btnClientAddClick(Sender: TObject);
    procedure ApplicationEvents1Exception(Sender: TObject; E: Exception);
  private
    { Private declarations }
    FLogID: Integer;
    FAPI: TAPIProgramLoyality;
    function GetIsCreatedAPI: Boolean;
    procedure AddToLog(s: string);

    //Events
    procedure OnLoginAPI(api: TAPIProgramLoyality);
    procedure OnRefreshTokensEvent(api: TAPIProgramLoyality; const OnlyAccess: boolean);
  public
    { Public declarations }
    procedure CreateAPI();
    procedure CloseAPI();
    property IsCreatedAPI: Boolean read GetIsCreatedAPI;
  end;

var
  Form1: TForm1;

implementation

uses
  api_pl_client, api_pl_params, api_template;

{$R *.dfm}

{ TForm1 }

procedure TForm1.CreateAPI;
var
  params: PAPIParameters;
begin
  params := New(PAPIParameters);
  try
    params.URL := edtURL.Text;
    params.User := edtUser.Text;
    params.Password := edtPassword.Text;

    FAPI := TAPIProgramLoyality.Create(params);
  finally
    Dispose(params);
  end;
  FAPI.OnLogin := Self.OnLoginAPI;
  FAPI.OnRefreshTokens := Self.OnRefreshTokensEvent;
end;

procedure TForm1.OnLoginAPI(api: TAPIProgramLoyality);
begin
  AddToLog('ACCESS:  ' + api.AccessToken.AsString);
  AddToLog('REFRESH:  ' + api.RefreshToken.AsString);
end;

procedure TForm1.OnRefreshTokensEvent(api: TAPIProgramLoyality; const OnlyAccess: boolean);
begin
  AddToLog('NEW ACCESS:  ' + api.AccessToken.AsString);
  if not OnlyAccess then
    AddToLog('NEW REFRESH:  ' + api.RefreshToken.AsString);
end;

procedure TForm1.btnCreateClick(Sender: TObject);
begin
  CreateAPI();
end;

function TForm1.GetIsCreatedAPI: Boolean;
begin
  Result := FAPI <> nil;
end;

procedure TForm1.CloseAPI;
begin
  if Assigned(FAPI) then
  begin
    FAPI.Free;
    FAPI := nil;
  end;
end;

procedure TForm1.ApplicationEvents1Idle(Sender: TObject; var Done: Boolean);
begin
  if IsCreatedAPI then
    lblStatus.Caption := 'OK'
  else
    lblStatus.Caption := 'CLOSED';
  btnCreate.Enabled := not IsCreatedAPI;
  btnClose.Enabled := IsCreatedAPI;
  pcOpers.Enabled := IsCreatedAPI;

  if IsCreatedAPI then
  begin
    if FAPI.AccessToken <> nil then
      edtInfoAccessToken.Text := FAPI.AccessToken.AsString;
    if FAPI.RefreshToken <> nil then
      edtInfoRefreshToken.Text := FAPI.RefreshToken.AsString;
    pcOpers.Font.Color := clWindowText;
  end
  else
  begin
    edtInfoAccessToken.Text := '';
    edtInfoRefreshToken.Text := '';
    pcOpers.Font.Color := clInactiveCaption;
  end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  CloseAPI();
end;

procedure TForm1.btnCloseClick(Sender: TObject);
begin
  CloseAPI();
end;

procedure TForm1.btnLoginClick(Sender: TObject);
begin
  FAPI.Login();
end;

procedure TForm1.AddToLog(s: string);
begin
  FLogID := FLogID + 1;
  if mmoLog.Lines.Count = 0 then
    mmoLog.Lines.Add(IntToStr(FLogID) + ': ' + s)
  else
    mmoLog.Lines.Insert(0, IntToStr(FLogID) + ': ' + s);
end;

procedure TForm1.btnRefreshTokenClick(Sender: TObject);
begin
  FAPI.RefreshTokens(chkOnlyAcceess.Checked);
end;

procedure TForm1.btnSessionClick(Sender: TObject);
var
  info: TSessionInfo;
begin
  info := FAPI.GetSessionInfo(nil);
  AddToLog(Format('session info: point enabled:%s', [BoolToStr(info.PointEnabled)]));
end;

procedure TForm1.btnClientInfoClick(Sender: TObject);
var
  info: TClientInfo;
  params: IAPIParams;
begin
  params := TAPIClientInfoParams.Create('', edtClientInfoPhone.Text, '', true, true, '');
  info := FAPI.GetClientInfo(params);
  AddToLog(Format('user info:  %s %s %s phone:%s email:%s ', [info.FirstName, info.MiddleName, info.LastName, info.Phone, info.Email]));
end;

procedure TForm1.btnClientAddClick(Sender: TObject);
var
  params: IAPIParams;
  info : TClientAddResponse;
begin
  params := TAPIClientAddParams.Create('', edtClientInfoPhone.Text, edtEmail.Text,
  AnsiToUtf8('Иванов'), AnsiToUtf8('Иван'), AnsiToUtf8('Иванович'), dtpBD.Date, 1);
  info := FAPI.ClientAdd(params);
  AddToLog(Format('user added:  %s %s %s phone:%s email:%s ', [info.FirstName, info.MiddleName, info.LastName, info.Phone, info.Email]));
end;

procedure TForm1.ApplicationEvents1Exception(Sender: TObject; E: Exception);
begin
  if E.ClassType = ExceptionApiCall then
  begin
    ShowMessage(Format('%s'#13#10'Detail: %s'#13#10'code:%s tag:%s', [
      ExceptionApiCall(E).Response.Message,
      ExceptionApiCall(E).Response.Field['message'],      
      ExceptionApiCall(E).Response.Code,
      ExceptionApiCall(E).Response.Tag]));
    exit;
  end;
  Application.ShowException(E);
end;

end.

