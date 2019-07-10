unit api_pl;

interface

uses auth,http,token,IdHTTP, Classes;

type

  PAPIParameters = ^APIParameters;
  APIParameters = record
    URL:string;
    User:string;
    Password:string;
    AuthManager: IAuth;
  end;

  TAPIProgramLoyality=class;

  TLoginEvent = procedure(api:TAPIProgramLoyality) of object;
  TRefreshTokensEvent = procedure(api:TAPIProgramLoyality;
  	const OnlyAccess:boolean) of object;


  
  TAPIProgramLoyality=class(TComponent)
  private
    FURL: string;
    FUser: string;
    FPassword: string;
    FAuth: IAuth;
    FHttpClient:IHTTPClient;
    FidClient : TIdHTTP;
    FOnLogin: TLoginEvent;
    FOnRefreshTokens: TRefreshTokensEvent;
  protected
    function CreateHTTPClient(IdHTTP:TIdHTTP=nil):IHTTPClient;
  public
    constructor Create(params: PAPIParameters);
    destructor Destroy();override;

    function GetAuth():IAuth;

    procedure Login();
    procedure RefreshTokens(const OnlyAccess:Boolean=False; const WithToken:string='');
    function GetAccessToken():TToken;
    function GetRefreshToken():TToken;

    property Auth:IAuth read GetAuth;
 	property AccessToken:TToken read GetAccessToken;
    property RefreshToken:TToken read GetRefreshToken;

    property OnLogin:TLoginEvent read FOnLogin write FOnLogin;
    property OnRefreshTokens:TRefreshTokensEvent  read FOnRefreshTokens
    	write FOnRefreshTokens;
  end;

implementation

{ TAPIProgramLoyality }

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

function TAPIProgramLoyality.GetRefreshToken: TToken;
begin
  Result := FAuth.RefreshToken;
end;

procedure TAPIProgramLoyality.Login;
begin
  FAuth.Login(FUser,FPassword);
  if Assigned(FOnLogin) then
    FOnLogin(Self);
end;

procedure TAPIProgramLoyality.RefreshTokens(const OnlyAccess: Boolean;
  const WithToken: string);
begin
  self.FAuth.RefreshTokens(OnlyAccess,WithToken);
  if Assigned(FOnRefreshTokens) then
    FOnRefreshTokens(Self, OnlyAccess);
end;

end.
