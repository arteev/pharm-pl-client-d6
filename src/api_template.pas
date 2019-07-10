unit api_template;

interface
uses http,Classes;

type
  TAPIRequiredParams=record
  	Token:string;
    Provider:string;
  end;

  TAPITemplate =class(TInterfacedObject)
  protected
  	Client:IHTTPClient;
  	URL:string;
  public
    constructor Create(client:IHTTPClient; URL:string);
    destructor Destroy; override;
  end;


function CreateRequiredParams(const AProvider,AToken:string):TAPIRequiredParams;
function GetHeadersFromParams(const Params:TAPIRequiredParams):TStrings;

implementation

function CreateRequiredParams(const AProvider,AToken:string):TAPIRequiredParams;
begin
  Result.Token := AToken;
  Result.Provider := AProvider;
end;

function GetHeadersFromParams(const Params:TAPIRequiredParams):TStrings;
begin
  Result := TStringList.Create();
  Result.Values['token'] := Params.Token;
  Result.Values['provider'] := Params.Provider;
end;

{ TAPITemplate }

constructor TAPITemplate.Create(client: IHTTPClient; URL: string);
begin
  self.Client := client;
  self.URL := URL;
end;

destructor TAPITemplate.Destroy;
begin
  Client := nil;
  inherited Destroy;
end;

end.
