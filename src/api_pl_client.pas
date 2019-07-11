unit api_pl_client;

interface
uses api_template,Classes,SysUtils,uLkJSON, api_json, dialogs;

const
  ProviderSailPlay = 'sailplay';

type

  ExceptionApiCall=class(ExceptionResponse);

  TSessionInfo=record
  	PointEnabled: Boolean;
  end;

  THistory = record
    Name:string;
  	Action:string;
    Date:TDateTime;
    IsCompleted:Boolean;
    PointDelta:Integer;
  end;
  TPoints = record
    	Confirmed:Integer;
        Unconfirmed:Integer;
        Spent:Integer;
        Total:Integer;
  end;
  TClientInfo=record
  	ID: Integer;
    OriginUserID: string;
    Phone:string;
    Email:string;
    LastName:string;
    MiddleName:string;
    FirstName:string;
    Sex:Integer;
    BirthDate:TDateTime;
    Points: TPoints;
    History:array of THistory;
    Subscriptions:array of string;
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
  private
    function parseClientInfo(js:TlkJSONObject):TClientInfo;
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
      Result := parseClientInfo(js);
    finally
      js.Free;
    end;
end;

function TAPIClient.parseClientInfo(js: TlkJSONObject): TClientInfo;
var
  payload       : TlkJsonObject;
  points        : TlkJsonObject;
  historyObj    : TlkJSONobject;
  history       : TlkJSONlist;
  subscriptions : TlkJSONlist;
  i             : Integer;
begin
  payload := MustField(js,'payload') as TlkJSONobject;

  Result.ID := GetValueJSON(payload,'id',0);
  Result.OriginUserID := GetValueJSON(payload,'id','');
  Result.Phone := GetValueJSON(payload,'phone','');
  Result.Email := GetValueJSON(payload,'email','');
  Result.LastName:=GetValueJSON(payload,'last_name','');
  Result.FirstName:=GetValueJSON(payload,'first_name','');
  Result.MiddleName:=GetValueJSON(payload,'middle_name','');
  Result.Sex := GetValueJSON(payload,'sex',0);
  Result.BirthDate := JsStrToDate(GetValueJSON(payload,'birth_date',''));

  points := MustField(payload,'points') as TlkJSONobject;
  Result.Points.Confirmed:= GetValueJSON(points,'confirmed',0);
  Result.Points.Unconfirmed:= GetValueJSON(points,'unconfirmed',0);
  Result.Points.Spent:= GetValueJSON(points,'spent',0);
  Result.Points.Total:= GetValueJSON(points,'total',0);

  if not IsNullJSON(payload,'subscriptions') then
  begin
    subscriptions :=  payload.Field['subscriptions'] as TlkJSONlist;
    SetLength(Result.Subscriptions,subscriptions.Count);
    for i:=0 to subscriptions.Count-1 do
    begin
      Result.Subscriptions[i] := subscriptions.getString(i);
    end;
  end;

  if not IsNullJSON(payload,'history') then
  begin
    history := payload.Field['history'] as TlkJSONlist;
    SetLength(Result.History, history.Count);
    for i:=0 to history.Count-1 do
    begin
      historyObj := history.Child[i] as TlkJSONobject;
      Result.History[i].Action :=GetValueJSON(historyObj,'action','');
      Result.History[i].IsCompleted :=GetValueJSON(historyObj,'is_completed',False);
      Result.History[i].Name :=GetValueJSON(historyObj,'name',False);
      Result.History[i].PointDelta :=GetValueJSON(historyObj,'points_delta',0);
      Result.History[i].Date := JsStrToDateTime(GetValueJSON(historyObj,'action_date',''));
    end;
  end;
end;

end.

