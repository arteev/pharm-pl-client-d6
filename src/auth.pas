unit auth;

interface
  uses SysUtils, Classes, http, token, uLkJSON, api_json;

type
  IAuth = interface(IInterface)
    procedure Login(UserName,Password: string);
    procedure RefreshTokens(const OnlyAccess:Boolean=False; const WithToken:string='');
    function GetAccessToken:TToken;
    function GetRefreshToken():TToken;

    property AccessToken:TToken read GetAccessToken;
    property RefreshToken:TToken read GetRefreshToken;
  end;


  ExceptionAuth=class(ExceptionResponse);

  TAuthManager = class(TInterfacedObject,IAuth)
  private
    Client:IHTTPClient;
    URL:string;
    FAccessToken:TToken;
    FRefreshToken:TToken;
  protected
    procedure ParseTokens(js:TlkJSONobject; const OnlyAccess:Boolean=False);
    function ValidToken(Token:TToken):Boolean;
    procedure SetAccessToken(Token:TToken);
    procedure SetRefreshToken(Token:TToken);
  public
    constructor Create(client:IHTTPClient; URL:string);
    destructor Destroy; override;
    procedure Login(UserName, Password: string);
    procedure RefreshTokens(const OnlyAccess:Boolean=False; const WithToken:string='');
    function GetAccessToken():TToken;
    function GetRefreshToken():TToken;

    property AccessToken:TToken read GetAccessToken;
    property RefreshToken:TToken read FRefreshToken;
  end;

implementation


constructor TAuthManager.Create(client:IHTTPClient; URL:string);
begin
  self.Client := client;
  self.URL := URL;
end;

destructor TAuthManager.Destroy;
begin
  Self.Client := nil;
  if Assigned(Self.FAccessToken) then
    Self.FAccessToken.Free;
  if Assigned(Self.FRefreshToken) then
    Self.FRefreshToken.Free;
  inherited Destroy;
end;

procedure TAuthManager.Login(UserName, Password: string);
var
  js  : TlkJsonObject;
  params : TStringStream;
begin
  params := TStringStream.Create('user='+UserName+'&password='+Password);
  try
    js := Client.Post(Self.URL + '/pl/auth/login',params, nil);
  finally
    params.Free;
  end;

  try
    if IsError(js) then
      raise  ExceptionAuth.CreateFromResponse(TErrorResponse.FromJSON(js));
    Self.ParseTokens(js);
  finally
    js.Free;
  end;
end;

procedure TAuthManager.RefreshTokens(const OnlyAccess:Boolean; const WithToken:string);
var
  js  : TlkJsonObject;
  params : TStringStream;
  headers: TStringList;
  RefreshToken: string;
  ParamsStr: string;
begin

  RefreshToken := WithToken;
  if (RefreshToken='') and not Self.ValidToken(Self.RefreshToken) then
  begin
	if Self.RefreshToken<>nil then RefreshToken := Self.RefreshToken.AsString;
  	raise InvalidToken.CreateWithToken(RefreshToken);
  end;

  if RefreshToken='' then
    RefreshToken := Self.RefreshToken.AsString;

  if OnlyAccess then
    ParamsStr := 'only_refresh_token=1';
  params := TStringStream.Create(ParamsStr);
  headers:= TStringList.Create;
  headers.Values['token'] := RefreshToken;
  try
    js := Client.Post(Self.URL + '/pl/auth/refresh_token',params, headers);
  finally
    headers.Free;
    params.Free;
  end;

  try
    if IsError(js) then
      raise  ExceptionAuth.CreateFromResponse(TErrorResponse.FromJSON(js));
    Self.ParseTokens(js, OnlyAccess);
  finally
    js.Free;
  end;
end;

procedure TAuthManager.ParseTokens(js:TlkJSONobject; const OnlyAccess:Boolean);
var
  jsToken:TlkJSONString;
begin
  jsToken:=js.Field['refresh_token'] as TlkJSONString;
  if not Assigned(jsToken) then
    raise ExceptionFieldJSON.CreateFmt('field "%s" not found',['token']);

  Self.SetRefreshToken(TToken.Create(jsToken.Value));


  if OnlyAccess then
    Exit;

  jsToken:=js.Field['token'] as TlkJSONString;
  if not Assigned(jsToken) then
    raise ExceptionFieldJSON.CreateFmt('field "%s" not found',['token']);

  Self.SetAccessToken(TToken.Create(jsToken.Value));

end;

function TAuthManager.ValidToken(Token:TToken):Boolean;
begin
  Result := (Token<>nil) and Assigned(Token) and not Token.Expired()
end;

procedure TAuthManager.SetAccessToken(Token:TToken);
begin
  if Assigned(Self.FAccessToken) then
    Self.FAccessToken.Free;
  Self.FAccessToken := Token;
end;

procedure TAuthManager.SetRefreshToken(Token:TToken);
begin
  if Assigned(Self.FRefreshToken) then
    Self.FRefreshToken.Free;
  Self.FRefreshToken := Token;
end;

function TAuthManager.GetAccessToken: TToken;
begin
  Result := Self.FAccessToken;
end;

function TAuthManager.GetRefreshToken: TToken;
begin
  Result := Self.FRefreshToken;
end;

end.
