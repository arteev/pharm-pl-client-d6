unit auth;

interface
  uses SysUtils, Classes, http, token, uLkJSON, api_json;

type
  IAuth = interface(IInterface)
    procedure Login(UserName,Password: string);
    procedure RefreshTokens(WithToken:string);
  end;


  ExceptionAuth=class(ExceptionResponse);

  TAuthManager = class(TInterfacedObject,IAuth)
  private
    Client:IHTTPClient;
    URL:string;
    FAccessToken:TToken;
    FRefreshToken:TToken;
  protected
    procedure ParseTokens(js:TlkJSONobject);
  public
    constructor Create(client:IHTTPClient; URL:string);
    destructor Destroy; override;
    procedure Login(UserName, Password: string);
    procedure RefreshTokens(WithToken:string='');
    property AccessToken:TToken read FAccessToken;
    property RefreshToken:TToken read FRefreshToken;
  end;

implementation


constructor TAuthManager.Create(client:IHTTPClient; URL:string);
begin
  self.Client := client;
  Self.URL := URL;
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
  req : Request;
  js  : TlkJsonObject;
  params : TStringStream;
begin
  params := TStringStream.Create('user='+UserName+'&password='+Password);
  try

    //Parameters := TStringSTream.create(UTF8String('param1=Value1&param2=????/???&param3=Value3'),TEncoding.UTF8);

    req:=Request.Post(Self.URL + '/pl/auth/login',params);
    js := Client.Post(req);
    if IsError(js) then
      raise  ExceptionAuth.CreateFromResponse(TErrorResponse.FromJSON(js));

    Self.ParseTokens(js);
  finally
    params.Free;
  end;
    {
    1.post

    }

end;

procedure TAuthManager.RefreshTokens(WithToken:string);
begin

end;

procedure TAuthManager.ParseTokens(js:TlkJSONobject);
var
  jsToken:TlkJSONString;
begin
  //TODO: access token optional
  jsToken:=js.Field['token'] as TlkJSONString;
  if not Assigned(jsToken) then
    raise ExceptionFieldJSON.CreateFmt('field "%s" not found',['token']);

  if Assigned(Self.FAccessToken) then
    Self.FAccessToken.Free;
  Self.FAccessToken := TToken.Create(jsToken.Value);

  jsToken:=js.Field['refresh_token'] as TlkJSONString;
  if not Assigned(jsToken) then
    raise ExceptionFieldJSON.CreateFmt('field "%s" not found',['token']);


  if Assigned(Self.FRefreshToken) then
    Self.FRefreshToken.Free;
  Self.FRefreshToken := TToken.Create(jsToken.Value);
end;

end.
