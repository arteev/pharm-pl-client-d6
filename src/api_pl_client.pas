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

  TClientInfo=record
  	ID: Integer;
    OriginUserID: Integer;
    Phone:string;
    Email:string;

    LastName:string;
    MiddleName:string;
    FirstName:string;
  end;

  IAPIClient = interface
	  // Sessions
	  function GetSessionInfo(reqParams:IAPIRequiredParams):TSessionInfo;
      // Clients
      function GetClientInfo(reqParams:IAPIRequiredParams):TClientInfo;
      // Marketings
      // Purchases
  end;


  TAPIClient=class(TAPITemplate,IAPIClient)
  public
    function GetSessionInfo(reqParams:IAPIRequiredParams):TSessionInfo;
    function GetClientInfo(reqParams:IAPIRequiredParams):TClientInfo;
  end;

implementation

{ TAPIClient }

function TAPIClient.GetSessionInfo(reqParams: IAPIRequiredParams): TSessionInfo;
var
  js  	  : TlkJsonObject;
  payload : TlkJsonObject;
  headers : TStrings;
begin
	headers := TStringList.Create;
	try
      reqParams.ApplyHeaders(headers);
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

function TAPIClient.GetClientInfo(reqParams: IAPIRequiredParams): TClientInfo;
var
  js  	  : TlkJsonObject;
  payload : TlkJsonObject;
  headers : TStrings;
  params  : TStrings;
begin
    headers := TStringList.Create;
    params  := TStringList.Create;
	try
      reqParams.ApplyHeaders(headers);
      reqParams.Extra.ApplyParams(params);
      js := Client.Get(URL+'/pl/clients/info',params,headers);
    finally
      params.Free;
      headers.Free;
    end;

    try
      if IsError(js) then
        raise  ExceptionApiCall.CreateFromResponse(TErrorResponse.FromJSON(js));

      payload := MustField(js,'payload') as TlkJSONobject;
	  Result.Phone := GetValueJSON(payload,'phone','');
	  Result.Email := GetValueJSON(payload,'email','');      
      Result.LastName:=GetValueJSON(payload,'last_name','');
      Result.FirstName:=GetValueJSON(payload,'first_name','');
      Result.MiddleName:=GetValueJSON(payload,'middle_name','');
    finally
      js.Free;
    end;
end;

end.

