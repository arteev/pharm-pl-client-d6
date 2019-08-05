unit MainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, api_pl, AppEvnts, ComCtrls, NMURL, IdBaseComponent,
  IdComponent, IdTCPConnection, IdTCPClient, IdHTTP, http, IdException,publisher,
  rmq_header;

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
    lbl1: TLabel;
    edtEmail: TEdit;
    Label1: TLabel;
    lblEmail: TLabel;
    btnClientAdd: TButton;
    dtpBD: TDateTimePicker;
    Label2: TLabel;
    btnSMS: TButton;
    edtSMSText: TEdit;
    edtSMSPriority: TEdit;
    lblSMS: TLabel;
    lblPriority: TLabel;
    lblSMSResponse: TLabel;
    tsPurchases: TTabSheet;
    btnCalcCart: TButton;
    edtPurchaseCartID: TEdit;
    btnPurchaseNew: TButton;
    lbl2: TLabel;
    edtOrderNum: TEdit;
    Label3: TLabel;
    btnPurchaseEdit: TButton;
    btnPurchaseDelete: TButton;
    btnPurchaseGet: TButton;
    btnPurchaseConfirm: TButton;
    lbl3: TLabel;
    edtToPhone: TEdit;
    idhtp1: TIdHTTP;
    stat1: TStatusBar;
    lblSessionInfo: TLabel;
    btnPurchaseToQueue: TButton;
    btnPurchaseReturn: TButton;
    btnCheckOnline: TButton;
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
    procedure btnSMSClick(Sender: TObject);
    procedure btnCalcCartClick(Sender: TObject);
    procedure btnPurchaseNewClick(Sender: TObject);
    procedure btnPurchaseGetClick(Sender: TObject);
    procedure btnPurchaseDeleteClick(Sender: TObject);
    procedure btnPurchaseConfirmClick(Sender: TObject);
    procedure btnPurchaseEditClick(Sender: TObject);
    procedure idhtp1Connected(Sender: TObject);
    procedure idhtp1Disconnected(Sender: TObject);
    procedure idhtp1Status(axSender: TObject; const axStatus: TIdStatus;
      const asStatusText: String);
    procedure btnPurchaseToQueueClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnPurchaseReturnClick(Sender: TObject);
    procedure btnCheckOnlineClick(Sender: TObject);
  private
    { Private declarations }
    FLogID: Integer;
    FAPI: TAPIProgramLoyality;
    FHttpClient : IHTTPClient;
    function GetIsCreatedAPI: Boolean;
    procedure AddToLog(s: string);

    //Events
    procedure OnLoginAPI(api: TAPIProgramLoyality);
    procedure OnRefreshTokensEvent(api: TAPIProgramLoyality; const OnlyAccess: boolean);
    procedure OnSMS(api:TAPIProgramLoyality;const SMSCode:string);
    procedure OnCallMethodStart (api:TAPIProgramLoyality; const method:string);
    procedure OnCallMethodEnd(api:TAPIProgramLoyality; const method:string;Data:Pointer);
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

    params.PubURL := 'amqp://guest:guest@localhost:5772/';
    params.PubTimeout:=3;
    params.PubExchange:='logs';
    params.PubQueue := 'my.pl';
    params.PubKey := '';
    params.PubArgsQueue := TStringList.Create;
    params.PubArgsExchange := nil;
    params.PubArgsBind := nil;

    //params.PubArgsQueue.Values['x-dead-letter-exchange']:='my.dead.topic';

    if FHttpClient=nil then
      FHttpClient := CreateHTTPClient(idhtp1);

    FAPI := TAPIProgramLoyality.CreateWithParams(Self,params,FHttpClient);
  finally
    params.PubArgsQueue.Free;
    Dispose(params);
  end;
  FAPI.OnLogin := Self.OnLoginAPI;
  FAPI.OnRefreshTokens := Self.OnRefreshTokensEvent;
  FAPI.OnSMS := Self.OnSMS;
  FAPI.OnCallMethodStart := Self.OnCallMethodStart;
  FAPI.OnCallMethodEnd := Self.OnCallMethodEnd;
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
  FHttpClient := nil;
  if Assigned(FAPI) then
  begin
    FAPI.Free;
    FAPI := nil;
  end;
;
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
var Retry:Integer;
begin
  //example: handle exception
  Retry := 0;
  while(Retry<2) do
  try
    Retry := Retry + 1;
	FAPI.Login();
    exit;
  except
	on E:EIdSocketError do
  	if Retry=2 then
    begin
      // switch offline
      raise;
    end;
    on E:Exception do
    	ShowMessage(E.ClassName);
  end;
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

procedure TForm1.btnSMSClick(Sender: TObject);
var
  params: IAPIParams;
  info : TClientSMSResponse;
begin
  params := TAPIClientSendSMSPArams.Create('', edtClientInfoPhone.Text, '',
  edtSMSText.Text,StrToIntDef(edtSMSPriority.Text,0));
  info:=FAPI.ClientSendSMS(params);
  AddToLog(Format('sms code: %s', [info.Code]))

end;

procedure TForm1.OnSMS(api: TAPIProgramLoyality; const SMSCode: string);
begin
  lblSMSResponse.Caption := SMSCode;
end;

procedure TForm1.btnCalcCartClick(Sender: TObject);
var
  params: IAPIParams;
  cart :TArrayCartItems;
  promocodes, cardNumbers: TArrayStrings;
  info:TMarketingCalcCartResponse;
begin
  SetLength(promocodes,1);
  SetLength(cardNumbers,1);
  SetLength(Cart,2);
  promocodes[0] := '123123';
  cardNumbers[0] := '111222';
  cart[0].Num := '1';
  cart[0].SKU := 'item1';
  cart[0].Price := 1000;
  cart[0].Quantity := 1;
  cart[0].DiscountPoints := 0;
  cart[0].MinPrice := 250;

  cart[1].Num := '2';
  cart[1].SKU := 'item1';
  cart[1].Price := 1500;
  cart[1].Quantity := 2;
  cart[1].DiscountPoints := 0;
  cart[1].MinPrice := 300;


  params := TAPIMarketingCartCalcParams.Create(cart,1,promocodes,cardNumbers);
  info:=FAPI.MarketingCalcCart(params);
  AddToLog(Format('Card Id: %d', [info.Cart.ID]));
  edtPurchaseCartID.Text := IntToStr(info.Cart.ID);
end;

procedure TForm1.btnPurchaseNewClick(Sender: TObject);
var
  params: IAPIParams;
  info:TPurchaseResponse;
begin
  params := TAPIPurcaseNewParams.Create('',edtClientInfoPhone.Text,'',
     edtOrderNum.Text,
     StrToInt(edtPurchaseCartID.Text));
  info := FAPI.PurchaseNew(params);
  AddToLog(Format('Created purchase: %d', [info.Purchase.ID]));
end;

procedure TForm1.btnPurchaseGetClick(Sender: TObject);
var
  params: IAPIParams;
  info:TPurchaseResponse;
begin
  params := TAPIPurchaseGetParams.Create(edtOrderNum.Text);
  info := FAPI.PurchaseGet(params);
  AddToLog(Format('Got purchase: %d', [info.Purchase.ID]));
end;

procedure TForm1.btnPurchaseDeleteClick(Sender: TObject);
var
  params: IAPIParams;
  info:TPurchaseDeleteResponse;
begin
  params := TAPIPurchaseDeleteParams.Create(edtOrderNum.Text);
  info := FAPI.PurchaseDelete(params);
  AddToLog(Format('Deleted purchase: %d', [info.ID]));
end;

procedure TForm1.btnPurchaseConfirmClick(Sender: TObject);
var
  params: IAPIParams;
  info:TPurchaseResponse;
begin
  params := TAPIPurchaseConfirmParams.Create(edtOrderNum.Text);
  info := FAPI.PurchaseConfirm(params);
  AddToLog(Format('Confirm purchase: %d', [info.Purchase.ID]));
end;

procedure TForm1.btnPurchaseEditClick(Sender: TObject);
var
  params : IAPIParams;
  info:TPurchaseEditResponse;
begin
  params := TAPIPurchaseEditParams.Create(edtOrderNum.Text,edtToPhone.Text);
  info := FAPI.PurchaseEdit(params);
  AddToLog(Format('Moved purchase %d to %s', [info.ID,info.User.Phone]));

end;

procedure TForm1.idhtp1Connected(Sender: TObject);
begin
  stat1.SimpleText := 'Connected';
end;

procedure TForm1.idhtp1Disconnected(Sender: TObject);
begin
 stat1.SimpleText := 'Disconnected';
end;

procedure TForm1.idhtp1Status(axSender: TObject; const axStatus: TIdStatus;
  const asStatusText: String);
begin
 stat1.SimpleText := asStatusText;
end;

procedure TForm1.OnCallMethodEnd(api: TAPIProgramLoyality;
  const method: string; Data: Pointer);
var
  pDataClient : ^TClientInfo;
begin
  AddToLog('End '+method);
  if method = MethodClientInfo then
  begin
    pDataClient:=Data;
    lblSessionInfo.Caption:='from event:'+ pDataClient.Phone;
  end;
end;

procedure TForm1.OnCallMethodStart(api: TAPIProgramLoyality;
  const method: string);
begin
  AddToLog('Start '+method);
end;

procedure TForm1.btnPurchaseToQueueClick(Sender: TObject);
var
  params: IAPIParams;
  cart:TArrayCartItems;
  g:TGUID;
const
  userID = 1;
begin
  SetLength(cart,1);
  cart[0].Num := '1';
  cart[0].SKU := 'test1';
  cart[0].Price := 1000.10;
  cart[0].Quantity := 2;
  params := TAPIPurcaseNewQueueParams.Create('',edtClientInfoPhone.Text,'',
     ProviderSailPlay,
     userID,
     edtOrderNum.Text,
     cart,
     );

  Try


    CreateGUID(g);
  	FAPI.PurcaseAddToQueue(params,GUIDToString(g));
  	AddToLog(Format('Added purchase to queue: %s', [edtOrderNum.Text]));
  except
    on E:EPublisherConnFailed do
    begin
    	AddToLog(e.Message);
        FAPI.Publisher.Reconnect();
    end;
  end;

end;

procedure TForm1.FormCreate(Sender: TObject);
var
  name:GoString;
begin
  name := StrToGoString('c:\log1.txt');
  try
    InitLog(name);
  finally
    DisposeGoString(name);
  end;
end;

procedure TForm1.btnPurchaseReturnClick(Sender: TObject);
var
  params: IAPIParams;
  info: TPurchaseReturnsResponse;
  cart: TArrayCartReturnsItems;
begin
  SetLength(cart, 1);
  cart[0].Num := '2';
  cart[0].Quantity := 1;
  cart[0].Reason := 'Брак';
  params := TAPIPurchaseReturnsParams.Create(edtOrderNum.Text, cart);
  info := FAPI.PurchaseReturns(params);

  AddToLog(Format('Purchase returns: return id: %d, purchase: %d', [info.ReturnCart.ID, info.Purchase.ID]));
end;

var
  StatusOnline: array[TCheckOnlineType] of string = ('Unknown', 'Online', 'Offline');

procedure TForm1.btnCheckOnlineClick(Sender: TObject);
var
  info: TCheckOnlineResponse;
  params: TAPICheckOnline;
begin
  params := TAPICheckOnline.Create;
  info := FAPI.CheckOnline(params);
  AddToLog(Format('Check Online:%s', [StatusOnline[info.Online]]));
end;

end.

