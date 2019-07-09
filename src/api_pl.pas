unit api_pl;

interface

uses auth,http,token,IdHTTP;

type

  PAPIParameters = ^APIParameters;
  APIParameters = record
    URL:string;
    User:string;
    Password:string;
    AuthManager: IAuth;
  end;

  IAPIProgramLoyality = interface
    function GetAuth():IAuth;
    property Auth:IAuth read GetAuth;

    procedure Login();
    procedure RefreshTokens(const OnlyAccess:Boolean=False; const WithToken:string='');

    function GetAccessToken():TToken;
    function GetRefreshToken():TToken;
    property AccessToken:TToken read GetAccessToken;
    property RefreshToken:TToken read GetRefreshToken;
  end;
  
  TAPIProgramLoyality=class(TInterfacedObject,IAPIProgramLoyality)
  private
    FURL: string;
    FUser: string;
    FPassword: string;
    FAuth: IAuth;
    FHttpClient:IHTTPClient;
    FidClient : TIdHTTP;
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

    //property Auth:IAuth read GetAuth;
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
      FidClient := CreateDefaultClient(nil);
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
  FAuth.Login(self.FUser,self.FPassword);
end;

procedure TAPIProgramLoyality.RefreshTokens(const OnlyAccess: Boolean;
  const WithToken: string);
begin
  self.FAuth.RefreshTokens(OnlyAccess,WithToken);
end;

end.
