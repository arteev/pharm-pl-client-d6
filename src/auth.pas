unit auth;

interface
  uses http, token;

type
  IAuth = interface(IInterface)
    procedure Login(UserName,Password: string);
    procedure RefreshTokens(WithToken:string);
  end;

  
  TAuthManager = class(TInterfacedObject,IAuth)
  private
    client:IHTTPClient;
    FAccessToken:TToken;
    FRefreshToken:TToken;
  public
    constructor Create(client:IHTTPClient);
    procedure Login(UserName,Password: string);
    procedure RefreshTokens(WithToken:string='');
    property AccessToken:TToken read FAccessToken;
    property RefreshToken:TToken read FRefreshToken;
  end;

implementation


constructor TAuthManager.Create(client:IHTTPClient);
begin
  self.client := client;
end;

procedure TAuthManager.Login(UserName,Password: string);
begin

end;

procedure TAuthManager.RefreshTokens(WithToken:string);
begin

end;

end.
