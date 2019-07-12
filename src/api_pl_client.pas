unit api_pl_client;

interface

uses
  api_template, Classes, SysUtils, uLkJSON, api_json, dialogs;

const
  ProviderSailPlay = 'sailplay';

type
  ExceptionApiCall = class(ExceptionResponse);

  TSessionInfo = record
    PointEnabled: Boolean;
  end;

  THistory = record
    Name: string;
    Action: string;
    Date: TDateTime;
    IsCompleted: Boolean;
    PointDelta: Integer;
  end;

  TPoints = record
    Confirmed: Integer;
    Unconfirmed: Integer;
    Spent: Integer;
    Total: Integer;
  end;

  TClientInfo = record
    ID: Integer;
    OriginUserID: string;
    Phone: string;
    Email: string;
    LastName: string;
    MiddleName: string;
    FirstName: string;
    Sex: Integer;
    BirthDate: TDateTime;
    Points: TPoints;
    History: array of THistory;
    Subscriptions: array of string;
  end;


  TClientAddResponse = record
    ID: Integer;
    OriginUserID: string;
    Phone: string;
    Email: string;
    LastName: string;
    MiddleName: string;
    FirstName: string;
    Sex: Integer;
    BirthDate: TDateTime;
  end;

  TClientSMSResponse = record
    Code: string;
  end;

  TMarketingAction = record
  	ClientMessage:string;
    ServiceMessage:string;
    Name:string;
    Alias:string;
  end;


  TCartPosition = record
  	Category: record
      SKU:string;
      ID:Integer;
      Name:string;
    end;
    Product:record
      SKU:string;
      ID:Integer;
      Name:string;
    end;
    Num:Integer;
    Price:Double;
    NewPrice:Double;
    Points:Double;
    PointsRate:Double;
    Quantity:Double;
    DiscountPointsMax:integer;
    MinPrice:Double;
    DiscountPoints:Integer;
    MarketingActions:array of string;
    ReversePointsRate:array of string;
  end;

  TCart = record
    ID:Integer;
    TotalPrice:Double;
    TotalDiscountPointMax:Integer;
    TotalPoints:Integer;
    PositionsCount:Integer;
    Positions:array of TCartPosition;
  end;

  TMarketingCalcCartResponse = record
    PossibleActions:array of TMarketingAction;
    Cart:TCart;
  end;

  IAPIClient = interface
	  // Sessions
    function GetSessionInfo(reqParams: IAPIRequiredParams): TSessionInfo;
      // Clients
    function GetClientInfo(reqParams: IAPIRequiredParams): TClientInfo;
    function ClientAdd(reqParams: IAPIRequiredParams):TClientAddResponse;
    function ClientSendSMS(reqParams: IAPIRequiredParams):TClientSMSResponse;
    // Marketings
    function MarketingCalcCart(reqParams: IAPIRequiredParams):TMarketingCalcCartResponse;
      // Purchases
  end;

  TAPIClient = class(TAPITemplate, IAPIClient)
  private
    function parseClientInfo(js: TlkJSONObject): TClientInfo;
    function parseClientAdd(js: TlkJSONObject): TClientAddResponse;
    function parseClientSendSMS(js: TlkJSONobject): TClientSMSResponse;
    function parseMarketingCalcCart(js:TlkJSONobject):TMarketingCalcCartResponse;
  public
    function GetSessionInfo(reqParams: IAPIRequiredParams): TSessionInfo;
    function GetClientInfo(reqParams: IAPIRequiredParams): TClientInfo;
    function ClientAdd(reqParams: IAPIRequiredParams):TClientAddResponse;
    function ClientSendSMS(reqParams: IAPIRequiredParams):TClientSMSResponse;
    // Marketings
    function MarketingCalcCart(reqParams: IAPIRequiredParams):TMarketingCalcCartResponse;
  end;

implementation

{ TAPIClient }

function TAPIClient.GetSessionInfo(reqParams: IAPIRequiredParams): TSessionInfo;
var
  js: TlkJsonObject;
  payload: TlkJsonObject;
  headers: TStrings;
begin
  headers := TStringList.Create;
  try
    reqParams.ApplyHeaders(headers);
    js := Client.Get(URL + '/pl/session/info', nil, headers);
  finally
    headers.Free;
  end;

  try
    if IsError(js) then
      raise ExceptionApiCall.CreateFromResponse(TErrorResponse.FromJSON(js));

    payload := MustField(js, 'payload') as TlkJSONobject;
    Result.PointEnabled := GetValueJSON(payload, 'point_enabled', '0') = '1';
  finally
    js.Free;
  end;
end;

function TAPIClient.GetClientInfo(reqParams: IAPIRequiredParams): TClientInfo;
var
  js: TlkJsonObject;
  headers: TStrings;
  params: TStrings;
begin
  headers := TStringList.Create;
  params := TStringList.Create;
  try
    reqParams.ApplyHeaders(headers);
    reqParams.Extra.ApplyParams(params);
    js := Client.Get(URL + '/pl/clients/info', params, headers);
  finally
    params.Free;
    headers.Free;
  end;
  try
    if IsError(js) then
      raise ExceptionApiCall.CreateFromResponse(TErrorResponse.FromJSON(js));
    Result := parseClientInfo(js);
  finally
    js.Free;
  end;
end;

function TAPIClient.parseClientInfo(js: TlkJSONObject): TClientInfo;
var
  payload: TlkJsonObject;
  points: TlkJsonObject;
  historyObj: TlkJSONobject;
  history: TlkJSONlist;
  subscriptions: TlkJSONlist;
  i: Integer;
begin
  payload := MustField(js, 'payload') as TlkJSONobject;

  Result.ID := GetValueJSON(payload, 'id', 0);
  Result.OriginUserID := GetValueJSON(payload, 'id', '');
  Result.Phone := GetValueJSON(payload, 'phone', '');
  Result.Email := GetValueJSON(payload, 'email', '');
  Result.LastName := GetValueJSON(payload, 'last_name', '');
  Result.FirstName := GetValueJSON(payload, 'first_name', '');
  Result.MiddleName := GetValueJSON(payload, 'middle_name', '');
  Result.Sex := GetValueJSON(payload, 'sex', 0);
  Result.BirthDate := JsStrToDate(GetValueJSON(payload, 'birth_date', ''));

  points := MustField(payload, 'points') as TlkJSONobject;
  Result.Points.Confirmed := GetValueJSON(points, 'confirmed', 0);
  Result.Points.Unconfirmed := GetValueJSON(points, 'unconfirmed', 0);
  Result.Points.Spent := GetValueJSON(points, 'spent', 0);
  Result.Points.Total := GetValueJSON(points, 'total', 0);

  if not IsNullJSON(payload, 'subscriptions') then
  begin
    subscriptions := payload.Field['subscriptions'] as TlkJSONlist;
    SetLength(Result.Subscriptions, subscriptions.Count);
    for i := 0 to subscriptions.Count - 1 do
    begin
      Result.Subscriptions[i] := subscriptions.getString(i);
    end;
  end;

  if not IsNullJSON(payload, 'history') then
  begin
    history := payload.Field['history'] as TlkJSONlist;
    SetLength(Result.History, history.Count);
    for i := 0 to history.Count - 1 do
    begin
      historyObj := history.Child[i] as TlkJSONobject;
      Result.History[i].Action := GetValueJSON(historyObj, 'action', '');
      Result.History[i].IsCompleted := GetValueJSON(historyObj, 'is_completed', False);
      Result.History[i].Name := GetValueJSON(historyObj, 'name', False);
      Result.History[i].PointDelta := GetValueJSON(historyObj, 'points_delta', 0);
      Result.History[i].Date := JsStrToDateTime(GetValueJSON(historyObj, 'action_date', ''));
    end;
  end;
end;

function TAPIClient.ClientAdd(reqParams: IAPIRequiredParams):TClientAddResponse;
var
  js: TlkJsonObject;
  headers: TStrings;
  params: TStrings;
  paramsStr: string;
  i: integer;
begin
  headers := TStringList.Create;
  params := TStringList.Create;
  try
    reqParams.ApplyHeaders(headers);
    reqParams.Extra.ApplyParams(params);
    js := Client.Post(URL + '/pl/clients/add', params, headers);
  finally
    params.Free;
    headers.Free;
  end;
  try
    if IsError(js) then
      raise ExceptionApiCall.CreateFromResponse(TErrorResponse.FromJSON(js));
    Result := parseClientAdd(js);
  finally
    js.Free;
  end;
end;

function TAPIClient.parseClientAdd(js: TlkJSONObject): TClientAddResponse;
var
  payload: TlkJsonObject;
  points: TlkJsonObject;
  historyObj: TlkJSONobject;
  history: TlkJSONlist;
  subscriptions: TlkJSONlist;
  i: Integer;
begin
  payload := MustField(js, 'payload') as TlkJSONobject;

  Result.ID := GetValueJSON(payload, 'id', 0);
  Result.OriginUserID := GetValueJSON(payload, 'id', '');
  Result.Phone := GetValueJSON(payload, 'phone', '');
  Result.Email := GetValueJSON(payload, 'email', '');
  Result.LastName := GetValueJSON(payload, 'last_name', '');
  Result.FirstName := GetValueJSON(payload, 'first_name', '');
  Result.MiddleName := GetValueJSON(payload, 'middle_name', '');
  Result.Sex := GetValueJSON(payload, 'sex', 0);
  Result.BirthDate := JsStrToDate(GetValueJSON(payload, 'birth_date', ''));
end;


function TAPIClient.ClientSendSMS(
  reqParams: IAPIRequiredParams): TClientSMSResponse;
var
  js: TlkJsonObject;
  headers: TStrings;
  params: TStrings;
  paramsStr: string;
  i: integer;
begin
  headers := TStringList.Create;
  params := TStringList.Create;
  try
    reqParams.ApplyHeaders(headers);
    reqParams.Extra.ApplyParams(params);
    js := Client.Post(URL + '/pl/clients/sms-code', params, headers);
  finally
    params.Free;
    headers.Free;
  end;
  try
    if IsError(js) then
      raise ExceptionApiCall.CreateFromResponse(TErrorResponse.FromJSON(js));
    Result := parseClientSendSMS(js);
  finally
    js.Free;
  end;
end;

function TAPIClient.parseClientSendSMS(
  js: TlkJSONobject): TClientSMSResponse;
var
  payload: TlkJsonObject;

begin
  payload := MustField(js, 'payload') as TlkJSONobject;
  Result.Code := GetValueJSON(payload, 'sms_code', 0);
end;

function TAPIClient.MarketingCalcCart(
  reqParams: IAPIRequiredParams): TMarketingCalcCartResponse;
var
  js: TlkJsonObject;
  headers: TStrings;
  params: TStrings;
  paramsStr: string;
  i: integer;
begin
  headers := TStringList.Create;
  params := TStringList.Create;
  try
    reqParams.ApplyHeaders(headers);
    reqParams.Extra.ApplyParams(params);
    js := Client.Post(URL + '/pl/marketing/calc-cart', params, headers);
  finally
    params.Free;
    headers.Free;
  end;
  try
    if IsError(js) then
      raise ExceptionApiCall.CreateFromResponse(TErrorResponse.FromJSON(js));
    Result := parseMarketingCalcCart(js);
  finally
    js.Free;
  end;
end;

function TAPIClient.parseMarketingCalcCart(
  js: TlkJSONobject): TMarketingCalcCartResponse;
var
  payload: TlkJsonObject;
  actions: TlkJSONlist;
  action: TlkJSONobject;
  cart: TlkJSONobject;
  positions: TlkJSONList;
  position: TlkJSONobject;
  obj:TlkJSONobject;
  list: TlkJSONlist;
  i,j: Integer;
begin
  payload := MustField(js, 'payload') as TlkJSONobject;

  //Result.ID := GetValueJSON(payload, 'id', 0);

  if not IsNullJSON(payload, 'possible_marketing_actions') then
  begin
    actions := payload.Field['possible_marketing_actions'] as TlkJSONlist;
    SetLength(Result.PossibleActions, actions.Count);
    for i := 0 to actions.Count - 1 do
    begin
      action := actions.Child[i] as TlkJSONobject;
      Result.PossibleActions[i].Name := GetValueJSON(action, 'name', '');
      Result.PossibleActions[i].Alias := GetValueJSON(action, 'alias', '');
      Result.PossibleActions[i].ClientMessage := GetValueJSON(action, 'client_msg', '');
      Result.PossibleActions[i].ServiceMessage := GetValueJSON(action, 'service_msg', '');
    end;
  end;

  if IsNullJSON(payload, 'cart') then exit;
  cart := payload['cart'] as TlkJSONobject;
  Result.Cart.ID := GetValueJSON(cart, 'id', '');
  Result.Cart.TotalPrice := JsStrToFloatDef(GetValueJSON(cart, 'total_price', '0'),0);
  Result.Cart.TotalDiscountPointMax := StrToIntDef(GetValueJSON(cart, 'total_discount_points_max', '0'),0);
  Result.Cart.TotalPoints := StrToIntDef(GetValueJSON(cart, 'total_points', '0'),0);
  Result.Cart.PositionsCount := StrToIntDef(GetValueJSON(cart, 'total_points', '0'),0);

  if not IsNullJSON(cart, 'positions') then
  begin
  	positions := cart.Field['positions'] as TlkJSONlist;
    SetLength(Result.Cart.Positions,positions.Count);
    for i := 0 to positions.Count - 1 do
    begin
      position := positions.Child[i] as TlkJSONobject;

      with Result.Cart.Positions[i] do
      begin
        Num := StrToIntDef(GetValueJSON(position, 'num', '0'), 0);
        Price := JsStrToFloatDef(GetValueJSON(position, 'price', '0'), 0);
        NewPrice := JsStrToFloatDef(GetValueJSON(position, 'new_price', '0'), 0);
        Points:= JsStrToFloatDef(GetValueJSON(position, 'points', '0'), 0);
    	PointsRate:= JsStrToFloatDef(GetValueJSON(position, 'points_rate', '0'), 0);
        Quantity:= JsStrToFloatDef(GetValueJSON(position, 'quantity', '0'), 0);
        DiscountPointsMax:=StrToIntDef(GetValueJSON(position, 'discount_points_max', '0'), 0);
    	MinPrice:=JsStrToFloatDef(GetValueJSON(position, 'min_price', '0'), 0);
    	DiscountPoints:=StrToIntDef(GetValueJSON(position, 'discount_points', '0'), 0);
      end;

      if not IsNullJSON(position, 'marketing_actions') then
      begin
        list := MustField(position, 'marketing_actions') as TlkJSONlist;
        SetLength(Result.Cart.Positions[i].MarketingActions, list.Count);
        for j := 0 to list.Count - 1 do
          Result.Cart.Positions[i].MarketingActions[j] := list.getString(j);
      end;

      if not IsNullJSON(position, 'reverse_points_rate') then
      begin
        list := MustField(position, 'reverse_points_rate') as TlkJSONlist;
        SetLength(Result.Cart.Positions[i].ReversePointsRate, list.Count);
        for j := 0 to list.Count - 1 do
          Result.Cart.Positions[i].ReversePointsRate[j] := list.getString(j);
      end;

      if not IsNullJSON(position, 'category') then
      begin
        obj := MustField(position, 'category') as TlkJSONobject;
        with Result.Cart.Positions[i].Category do
        begin
          SKU := GetValueJSON(obj, 'sku', '');
          ID := StrToIntDef(GetValueJSON(obj, 'id', ''), 0);
          Name := GetValueJSON(obj, 'name', '');
        end;
      end;

      if not IsNullJSON(position, 'product') then
      begin
        obj := MustField(position, 'product') as TlkJSONobject;
        with Result.Cart.Positions[i].Product do
        begin
          SKU := GetValueJSON(obj, 'sku', '');
          ID := StrToIntDef(GetValueJSON(obj, 'id', ''), 0);
          Name := GetValueJSON(obj, 'name', '');
        end;
      end;




    end;
  end;

end;

end.

