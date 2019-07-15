unit api_pl;

interface

uses auth,http,token,IdHTTP, Classes, api_pl_client, api_template, SysUtils;

const MethodLogin='login';
      MethodRefreshTokens = 'refresh_tokens';
      MethodSessionInfo = 'session_info';
      MethodClientInfo = 'client_info';
      MethodClientAdd = 'client_add';
      MethodClientSMS = 'client_sms';
      MethodCartCalc = 'cart_calc';
      MethodPurchaseNew = 'purchase_new';
      MethodPurchaseGet = 'purchase_get';
      MethodPurchaseDelete = 'purchase_delete';
      MethodPurchaseConfirm = 'purchase_confirm';
      MethodPurchaseEdit = 'purchase_edit';

      MethodPurchaseToQueue = 'purchase_to_queue';

type

  PAPIParameters = ^APIParameters;
  APIParameters = record
    URL:string;
    User:string;
    Password:string;
    AuthManager: IAuth;
  end;

  ExceptionEmptyToken=class(Exception);

  TAPIProgramLoyality=class;

  TLoginEvent = procedure(api:TAPIProgramLoyality) of object;
  TRefreshTokensEvent = procedure(api:TAPIProgramLoyality;
  	const OnlyAccess:boolean) of object;
  TSMSEvent = procedure(api:TAPIProgramLoyality;const SMSCode:string) of object;
  TCallMethodStartEvent = procedure(api:TAPIProgramLoyality;
  	const method:string) of object;
  TCallMethodEndEvent = procedure(api:TAPIProgramLoyality;
  	const method:string;Data:Pointer) of object;


  TAPIProgramLoyality=class(TComponent)
  private
    FURL: string;
    FUser: string;
    FPassword: string;
    FAuth: IAuth;
    FAPIClient:IAPIClient;
    FHttpClient:IHTTPClient;
    FidClient : TIdHTTP;
    FOnLogin: TLoginEvent;
    FOnRefreshTokens: TRefreshTokensEvent;
    FOnSMS: TSMSEvent;
    FOnCallMethodStart: TCallMethodStartEvent;
    FOnCallMethodEnd: TCallMethodEndEvent;
  protected
    procedure CheckAccessToken();
    procedure StartMethod(const AMethod:string);
    procedure EndMethod(const AMethod:string;Data:Pointer);
  public
    constructor Create(params: PAPIParameters;AHttpClient:IHTTPClient=nil);
    destructor Destroy();override;

    function GetAuth():IAuth;
    function GetAccessToken():TToken;
    function GetRefreshToken():TToken;

    { API Auth }
    procedure Login();
    procedure RefreshTokens(const OnlyAccess:Boolean=False; const WithToken:string='');
    { API Client }
    function GetSessionInfo(RequestParameters:IAPIParams):TSessionInfo;
    function GetClientInfo(RequestParameters:IAPIParams):TClientInfo;
    function ClientAdd(RequestParameters:IAPIParams):TClientAddResponse;
    function ClientSendSMS(RequestParameters:IAPIParams):TClientSMSResponse;
    function MarketingCalcCart(RequestParameters: IAPIParams):TMarketingCalcCartResponse;
    function PurchaseNew(RequestParameters: IAPIParams):TPurchaseResponse;
    function PurchaseGet(RequestParameters: IAPIParams):TPurchaseResponse;
    function PurchaseDelete(RequestParameters: IAPIParams):TPurchaseDeleteResponse;
    function PurchaseConfirm(RequestParameters: IAPIParams):TPurchaseResponse;
    function PurchaseEdit(RequestParameters: IAPIParams):TPurchaseEditResponse;


    { API Queue }
    procedure PurcaseAddToQueue(RequestParameters: IAPIParams);

    property Auth:IAuth read GetAuth;
 	property AccessToken:TToken read GetAccessToken;
    property RefreshToken:TToken read GetRefreshToken;

    property OnLogin:TLoginEvent read FOnLogin write FOnLogin;
    property OnRefreshTokens:TRefreshTokensEvent  read FOnRefreshTokens
    	write FOnRefreshTokens;
    property OnSMS:TSMSEvent read FOnSMS write FOnSMS;
    property OnCallMethodStart:TCallMethodStartEvent read FOnCallMethodStart
    	write FOnCallMethodStart;
    property OnCallMethodEnd:TCallMethodEndEvent read FOnCallMethodEnd
    	write FOnCallMethodEnd;
  end;

  function CreateHTTPClient(IdHTTP:TIdHTTP=nil):IHTTPClient;

implementation

{ TAPIProgramLoyality }

procedure TAPIProgramLoyality.PurcaseAddToQueue(
  RequestParameters: IAPIParams);
begin
 //TODO:
 raise Exception.Create('not emplemented');
end;

procedure TAPIProgramLoyality.CheckAccessToken;
begin
  if AccessToken = nil then
  	raise ExceptionEmptyToken.Create('empty token');
end;

function TAPIProgramLoyality.ClientAdd(RequestParameters: IAPIParams):TClientAddResponse;
var
  params: IAPIRequiredParams;
begin
  CheckAccessToken();
  StartMethod(MethodClientAdd);
  params := TAPIRequiredParams.Create(ProviderSailPlay,AccessToken.AsString,
	  RequestParameters);
  Result:=FAPIClient.ClientAdd(params);
  EndMethod(MethodClientAdd,@Result);
end;

function TAPIProgramLoyality.ClientSendSMS(
  RequestParameters: IAPIParams): TClientSMSResponse;
var
  params: IAPIRequiredParams;
begin
  CheckAccessToken();
  StartMethod(MethodClientSMS);
  params := TAPIRequiredParams.Create(ProviderSailPlay,AccessToken.AsString,
	  RequestParameters);
  Result:=FAPIClient.ClientSendSMS(params);
  if Assigned(FOnSMS) then
  	FOnSMS(Self,Result.Code);
  EndMethod(MethodClientSMS,@Result);
end;


constructor TAPIProgramLoyality.Create(params:PAPIParameters;
	AHttpClient:IHTTPClient);
begin
  if params<>nil then
  begin
    FURL := params.URL;
    FAuth := params.AuthManager;
    FUser := params.User;
    FPassword := params.Password;
  end;

  FHttpClient:=AHttpClient;

  if FAuth = nil then
  begin
    if FHttpClient=nil then
    begin
      FidClient := CreateDefaultClient(self);
      FHttpClient := CreateHTTPClient(FidClient);
    end;
    FAuth := TAuthManager.Create(FHttpClient,FURL);
  end;

  if FAPIClient = nil then
    FAPIClient := TAPIClient.Create(FHttpClient,FURL);
end;



destructor TAPIProgramLoyality.Destroy;
begin
  //if Assigned(FidClient) then
  //  FidClient.Free;

  FAuth := nil;
  FAPIClient := nil;
  FHttpClient := nil;
  inherited;
end;

procedure TAPIProgramLoyality.EndMethod(const AMethod: string;
  Data: Pointer);
begin
  if Assigned(FOnCallMethodEnd) then
  	FOnCallMethodEnd(self,AMethod,Data);
end;

function TAPIProgramLoyality.GetAccessToken: TToken;
begin
  Result := FAuth.AccessToken
end;

function TAPIProgramLoyality.GetAuth: IAuth;
begin
  Result := FAuth;
end;

function TAPIProgramLoyality.GetClientInfo(
  RequestParameters: IAPIParams): TClientInfo;
var
  params: IAPIRequiredParams;
begin
  CheckAccessToken();
  StartMethod(MethodClientInfo);
  params := TAPIRequiredParams.Create(ProviderSailPlay,AccessToken.AsString,
	  RequestParameters);
  Result:=FAPIClient.GetClientInfo(params);
  EndMethod(MethodClientInfo,@Result);
end;

function TAPIProgramLoyality.GetRefreshToken: TToken;
begin
  Result := FAuth.RefreshToken;
end;

function TAPIProgramLoyality.GetSessionInfo(RequestParameters:IAPIParams): TSessionInfo;
var
  params: IAPIRequiredParams;
begin
  CheckAccessToken();
  StartMethod(MethodSessionInfo);
  params := TAPIRequiredParams.Create(ProviderSailPlay,AccessToken.AsString,
	  RequestParameters);
  Result:=FAPIClient.GetSessionInfo(params);
  EndMethod(MethodSessionInfo,@Result);
end;

procedure TAPIProgramLoyality.Login;
begin
  StartMethod(MethodLogin);
  FAuth.Login(FUser,FPassword);
  if Assigned(FOnLogin) then
    FOnLogin(Self);
  EndMethod(MethodLogin,nil);
end;

function TAPIProgramLoyality.MarketingCalcCart(
  RequestParameters: IAPIParams): TMarketingCalcCartResponse;
var
  params: IAPIRequiredParams;
begin
  CheckAccessToken();
  StartMethod(MethodCartCalc);
  params := TAPIRequiredParams.Create(ProviderSailPlay,AccessToken.AsString,
	  RequestParameters);
  Result:=FAPIClient.MarketingCalcCart(params);
  EndMethod(MethodCartCalc,@Result);
end;

function TAPIProgramLoyality.PurchaseConfirm(
  RequestParameters: IAPIParams): TPurchaseResponse;
var
  params: IAPIRequiredParams;
begin
  CheckAccessToken();
  StartMethod(MethodPurchaseConfirm);
  params := TAPIRequiredParams.Create(ProviderSailPlay,AccessToken.AsString,
	  RequestParameters);
  Result:=FAPIClient.PurchaseConfirm(params);
  EndMethod(MethodPurchaseConfirm,@Result);
end;


function TAPIProgramLoyality.PurchaseDelete(
  RequestParameters: IAPIParams): TPurchaseDeleteResponse;
var
  params: IAPIRequiredParams;
begin
  CheckAccessToken();
  StartMethod(MethodPurchaseDelete);
  params := TAPIRequiredParams.Create(ProviderSailPlay,AccessToken.AsString,
	  RequestParameters);
  Result:=FAPIClient.PurchaseDelete(params);
  EndMethod(MethodPurchaseDelete,@Result);
end;


function TAPIProgramLoyality.PurchaseEdit(
  RequestParameters: IAPIParams): TPurchaseEditResponse;
var
  params: IAPIRequiredParams;
begin
  CheckAccessToken();
  StartMethod(MethodPurchaseEdit);
  params := TAPIRequiredParams.Create(ProviderSailPlay,AccessToken.AsString,
	  RequestParameters);
  Result:=FAPIClient.PurchaseEdit(params);
  EndMethod(MethodPurchaseEdit,@Result);
end;


function TAPIProgramLoyality.PurchaseGet(
  RequestParameters: IAPIParams): TPurchaseResponse;
var
  params: IAPIRequiredParams;
begin
  CheckAccessToken();
  StartMethod(MethodPurchaseGet);
  params := TAPIRequiredParams.Create(ProviderSailPlay,AccessToken.AsString,
	  RequestParameters);
  Result:=FAPIClient.PurchaseGet(params);
  EndMethod(MethodPurchaseGet,@Result);
end;

function TAPIProgramLoyality.PurchaseNew(
  RequestParameters: IAPIParams): TPurchaseResponse;
var
  params: IAPIRequiredParams;
begin
  CheckAccessToken();
  StartMethod(MethodPurchaseNew);
  params := TAPIRequiredParams.Create(ProviderSailPlay,AccessToken.AsString,
	  RequestParameters);
  Result:=FAPIClient.PurchaseNew(params);
  EndMethod(MethodPurchaseNew,@Result);
end;

procedure TAPIProgramLoyality.RefreshTokens(const OnlyAccess: Boolean;
  const WithToken: string);
begin
  StartMethod(MethodRefreshTokens);
  FAuth.RefreshTokens(OnlyAccess,WithToken);
  if Assigned(FOnRefreshTokens) then
    FOnRefreshTokens(Self, OnlyAccess);
  EndMethod(MethodRefreshTokens,nil);
end;


function CreateHTTPClient(IdHTTP: TIdHTTP): IHTTPClient;
begin
   Result:=HTTPClient.Create(IdHTTP);
end;

procedure TAPIProgramLoyality.StartMethod(const AMethod: string);
begin
  if Assigned(FOnCallMethodStart) then
    FOnCallMethodStart(self,AMethod);
end;

end.
