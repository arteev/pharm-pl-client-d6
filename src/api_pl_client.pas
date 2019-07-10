unit api_pl_client;

interface
uses api_template,Classes,uLkJSON, api_json;

const
  ProviderSailPlay = 'sailplay';

type

  ExceptionApiCall=class(ExceptionResponse);

  TSessionInfo=record
  	PointEnabled: Boolean
  end;

  IAPIClient = interface
	  function GetSessionInfo(reqParams:TAPIRequiredParams):TSessionInfo;
  end;


  TAPIClient=class(TAPITemplate,IAPIClient)
  public
    function GetSessionInfo(reqParams:TAPIRequiredParams):TSessionInfo;
  end;

implementation

{ TAPIClient }

function TAPIClient.GetSessionInfo(reqParams: TAPIRequiredParams): TSessionInfo;
var
  js  	  : TlkJsonObject;
  payload : TlkJsonObject;
  headers : TStrings;
begin
	try
	  headers:=GetHeadersFromParams(reqParams);
      js := Client.Get(URL+'/pl/session/info',nil,headers);
    finally
      headers.Free;
    end;

    try
      if IsError(js) then
        raise  ExceptionApiCall.CreateFromResponse(TErrorResponse.FromJSON(js));

      payload := MustField(js,'payload') as TlkJSONobject;
	  Result.PointEnabled := GetValueJSON(payload,'point_enabled','0') = '1';
    finally
      js.Free;
    end;
end;

end.
