unit api_pl;

interface

uses auth,http,token,IdHTTP, Classes, api_pl_client, api_template, SysUtils;

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
  protected
    function CreateHTTPClient(IdHTTP:TIdHTTP=nil):IHTTPClient;
    procedure CheckAccessToken();
  public
    constructor Create(params: PAPIParameters);
    destructor Destroy();override;

    function GetAuth():IAuth;

    { API Auth }
    procedure Login();
    procedure RefreshTokens(const OnlyAccess:Boolean=False; const WithToken:string='');
    function GetAccessToken():TToken;
    function GetRefreshToken():TToken;

    { API Client }
    function GetSessionInfo(RequestParameters:IAPIParams):TSessionInfo;
    function GetClientInfo(RequestParameters:IAPIParams):TClientInfo;
    function ClientAdd(RequestParameters:IAPIParams):TClientAddResponse;
    function ClientSendSMS(RequestParameters:IAPIParams):TClientSMSResponse;
    function MarketingCalcCart(RequestParameters: IAPIParams):TMarketingCalcCartResponse;

    property Auth:IAuth read GetAuth;
 	property AccessToken:TToken read GetAccessToken;
    property RefreshToken:TToken read GetRefreshToken;

    property OnLogin:TLoginEvent read FOnLogin write FOnLogin;
    property OnRefreshTokens:TRefreshTokensEvent  read FOnRefreshTokens
    	write FOnRefreshTokens;
    property OnSMS:TSMSEvent read FOnSMS write FOnSMS;
  end;

implementation

{ TAPIProgramLoyality }

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
  params := TAPIRequiredParams.Create(ProviderSailPlay,AccessToken.AsString,
	  RequestParameters);
  Result:=FAPIClient.ClientAdd(params);
end;

function TAPIProgramLoyality.ClientSendSMS(
  RequestParameters: IAPIParams): TClientSMSResponse;
var
  params: IAPIRequiredParams;
begin
  CheckAccessToken();
  params := TAPIRequiredParams.Create(ProviderSailPlay,AccessToken.AsString,
	  RequestParameters);
  Result:=FAPIClient.ClientSendSMS(params);
  if Assigned(FOnSMS) then
  	FOnSMS(Self,Result.Code);
end;


constructor TAPIProgramLoyality.Create(params:PAPIParameters);
begin
  if params<>nil then
  begin
    FURL := params.URL;
    FAuth := params.AuthManager;
    FUser := params.User;
    FPassword := params.Password;
  end;

  //TODO: hhtp client from params
  if FAuth = nil then
  begin
    if FHttpClient=nil then
    begin
      FidClient := CreateDefaultClient(self);
      FHttpClient := CreateHTTPClient(FidClient);
    end;
    FAuth := TAuthManager.Create(FHttpClient,FURL);
  end;

  FAPIClient := TAPIClient.Create(FHttpClient,FURL);
end;

function TAPIProgramLoyality.CreateHTTPClient(
  IdHTTP: TIdHTTP): IHTTPClient;
begin
   Result:=HTTPClient.Create(IdHTTP);
end;

destructor TAPIProgramLoyality.Destroy;
begin
  if Assigned(FidClient) then
    FidClient.Free;

  FAuth := nil;
  FAPIClient := nil;
  FHttpClient := nil;
  inherited;
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
  params := TAPIRequiredParams.Create(ProviderSailPlay,AccessToken.AsString,
	  RequestParameters);
  Result:=FAPIClient.GetClientInfo(params);
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
  params := TAPIRequiredParams.Create(ProviderSailPlay,AccessToken.AsString,
	  RequestParameters);
  Result:=FAPIClient.GetSessionInfo(params);
end;

procedure TAPIProgramLoyality.Login;
begin
  FAuth.Login(FUser,FPassword);
  if Assigned(FOnLogin) then
    FOnLogin(Self);
end;

function TAPIProgramLoyality.MarketingCalcCart(
  RequestParameters: IAPIParams): TMarketingCalcCartResponse;
var
  params: IAPIRequiredParams;
begin
  CheckAccessToken();
  params := TAPIRequiredParams.Create(ProviderSailPlay,AccessToken.AsString,
	  RequestParameters);
  Result:=FAPIClient.MarketingCalcCart(params);
end;

procedure TAPIProgramLoyality.RefreshTokens(const OnlyAccess: Boolean;
  const WithToken: string);
begin
  FAuth.RefreshTokens(OnlyAccess,WithToken);
  if Assigned(FOnRefreshTokens) then
    FOnRefreshTokens(Self, OnlyAccess);
end;

end.
