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


  TCartMarketingActions=record
    Alias:string;
    PriceDelta:double;
    Name:string;
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
    MarketingActions:array of TCartMarketingActions;
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

  TMarketingActions = array of TMarketingAction;
  TMarketingCalcCartResponse = record
    PossibleActions:TMarketingActions;
    Cart:TCart;
  end;

  TPurchase = record
  	IsCompleted: Boolean;
    StoreDepartmentID: Integer;
    CompletedDate:TDateTime;
    OrderNum:string;
    ID:Integer;
    PointDelta:Integer;
    Key:string;
    UserID:Integer;
    Date:TDateTime;
    Pin:string;
    Price:Double;
  end;

  TPurchaseResponse = record
    Purchase:TPurchase;
    PossibleActions:TMarketingActions;
    MarketingActionsApplied:TMarketingActions;
    Cart:TCart;
  end;

  TPurchaseDeleteResponse = record
    OrderNum:string;
    ID:Integer;
  end;

  TPurchaseEditResponse = record
    OrderNum: string;
    ID: Integer;
    User: record
      FullPhone: string;
      Phone: string;
      OriginUserID: Integer;
      LastName: string;
      FirstName: string;
      Points: Integer;
      ID: Integer;
      Email: string;
    end;
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
    function PurchaseNew(reqParams: IAPIRequiredParams):TPurchaseResponse;
    function PurchaseGet(reqParams: IAPIRequiredParams):TPurchaseResponse;
    function PurchaseDelete(reqParams: IAPIRequiredParams):TPurchaseDeleteResponse;
    function PurchaseConfirm(reqParams: IAPIRequiredParams):TPurchaseResponse;
    function PurchaseEdit(reqParams: IAPIRequiredParams):TPurchaseEditResponse;
  end;

  TAPIClient = class(TAPITemplate, IAPIClient)
  private
    function parseClientInfo(js: TlkJSONObject): TClientInfo;
    function parseClientAdd(js: TlkJSONObject): TClientAddResponse;
    function parseClientSendSMS(js: TlkJSONobject): TClientSMSResponse;
    function parseMarketingCalcCart(js:TlkJSONobject):TMarketingCalcCartResponse;
    function parsePurchaseResponse(js:TlkJSONobject):TPurchaseResponse;
    function parsePurchaseEditResponse(js:TlkJSONobject):TPurchaseEditResponse;

  public

    function GetSessionInfo(reqParams: IAPIRequiredParams): TSessionInfo;
    function GetClientInfo(reqParams: IAPIRequiredParams): TClientInfo;
    function ClientAdd(reqParams: IAPIRequiredParams):TClientAddResponse;
    function ClientSendSMS(reqParams: IAPIRequiredParams):TClientSMSResponse;
    // Marketings
    function MarketingCalcCart(reqParams: IAPIRequiredParams):TMarketingCalcCartResponse;
    // Purchases
    function PurchaseNew(reqParams: IAPIRequiredParams):TPurchaseResponse;
    function PurchaseGet(reqParams: IAPIRequiredParams):TPurchaseResponse;
    function PurchaseDelete(reqParams: IAPIRequiredParams):TPurchaseDeleteResponse;
    function PurchaseConfirm(reqParams: IAPIRequiredParams):TPurchaseResponse;
    function PurchaseEdit(reqParams: IAPIRequiredParams):TPurchaseEditResponse;
  end;

implementation

function parsePurchase(js: TlkJSONobject): TPurchase;
begin
  Result.IsCompleted := GetValueJSON(js, 'is_completed', False);
  Result.StoreDepartmentID := StrToInt(GetValueJSON(js, 'store_department_id', '0'));
  Result.CompletedDate := JsStrToDateTimeDef(GetValueJSON(js, 'completed_date', ''),0);
  Result.OrderNum := GetValueJSON(js, 'order_num', '');
  Result.ID := StrToInt(GetValueJSON(js, 'id', 0));
  Result.PointDelta := StrToInt(GetValueJSON(js, 'points_delta', 0));
  Result.Key := GetValueJSON(js, 'public_key', '');
  Result.UserID := StrToInt(GetValueJSON(js, 'user_id', 0));
  Result.Date := JsStrToDateTimeDef(GetValueJSON(js, 'purchase_date', ''),0);
  Result.Pin := GetValueJSON(js, 'clerk_pin', '');
  Result.Price := JsStrToFloatDef(GetValueJSON(js, 'price', '0'), 0);
end;

function parseMarketingActions(actions:TlkJSONlist):TMarketingActions;
var
  action: TlkJSONobject;
  i:Integer;
begin
    SetLength(Result, actions.Count);
    for i := 0 to actions.Count - 1 do
    begin
      action := actions.Child[i] as TlkJSONobject;
      Result[i].Name := GetValueJSON(action, 'name', '');
      Result[i].Alias := GetValueJSON(action, 'alias', '');
      Result[i].ClientMessage := GetValueJSON(action, 'client_msg', '');
      Result[i].ServiceMessage := GetValueJSON(action, 'service_msg', '');
    end;
end;

function parseCart(cart:TlkJSONobject): TCart;
var
  positions: TlkJSONList;
  position: TlkJSONobject;
  obj:TlkJSONobject;
  list: TlkJSONlist;
  i,j: Integer;
begin

  Result.ID := GetValueJSON(cart, 'id', 0);
  Result.TotalPrice := JsStrToFloatDef(GetValueJSON(cart, 'total_price', '0'),0);
  Result.TotalDiscountPointMax := StrToIntDef(GetValueJSON(cart, 'total_discount_points_max', '0'),0);
  Result.TotalPoints := StrToIntDef(GetValueJSON(cart, 'total_points', '0'),0);
  Result.PositionsCount := StrToIntDef(GetValueJSON(cart, 'total_points', '0'),0);

  if not IsNullJSON(cart, 'positions') then
  begin
  	positions := cart.Field['positions'] as TlkJSONlist;
    SetLength(Result.Positions,positions.Count);
    for i := 0 to positions.Count - 1 do
    begin
      position := positions.Child[i] as TlkJSONobject;
      with Result.Positions[i] do
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
        SetLength(Result.Positions[i].MarketingActions, list.Count);
        for j := 0 to list.Count - 1 do
        begin
          if list.Child[j].SelfType<>jsNull then
          with Result.Positions[i].MarketingActions[j] do
          begin
            Alias:= GetValueJSON(list.Child[j], 'alias', '');
            Name:=GetValueJSON(list.Child[j],'name','');
            PriceDelta := JsStrToFloatDef(GetValueJSON(list.Child[j], 'points_delta', '0'), 0);
          end;
        end;
      end;

      if not IsNullJSON(position, 'reverse_points_rate') then
      begin
        list := MustField(position, 'reverse_points_rate') as TlkJSONlist;
        SetLength(Result.Positions[i].ReversePointsRate, list.Count);
        for j := 0 to list.Count - 1 do
          Result.Positions[i].ReversePointsRate[j] := list.getString(j);
      end;

      if not IsNullJSON(position, 'category') then
      begin
        obj := MustField(position, 'category') as TlkJSONobject;
        with Result.Positions[i].Category do
        begin
          SKU := GetValueJSON(obj, 'sku', '');
          ID := StrToIntDef(GetValueJSON(obj, 'id', ''), 0);
          Name := GetValueJSON(obj, 'name', '');
        end;
      end;

      if not IsNullJSON(position, 'product') then
      begin
        obj := MustField(position, 'product') as TlkJSONobject;
        with Result.Positions[i].Product do
        begin
          SKU := GetValueJSON(obj, 'sku', '');
          ID := StrToIntDef(GetValueJSON(obj, 'id', ''), 0);
          Name := GetValueJSON(obj, 'name', '');
        end;
      end;
    end;
  end;

end;


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
begin
  payload := MustField(js, 'payload') as TlkJSONobject;

  if not IsNullJSON(payload, 'possible_marketing_actions') then
    Result.PossibleActions:=parseMarketingActions(payload.Field['possible_marketing_actions'] as TlkJSONlist);

  Result.Cart := parseCart(MustField(payload, 'cart') as TlkJSONobject);
end;

function TAPIClient.PurchaseNew(
  reqParams: IAPIRequiredParams): TPurchaseResponse;
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
    js := Client.Post(URL + '/pl/purchases/new', params, headers);
  finally
    params.Free;
    headers.Free;
  end;
  try
    if IsError(js) then
      raise ExceptionApiCall.CreateFromResponse(TErrorResponse.FromJSON(js));
    Result := parsePurchaseResponse(js);
  finally
    js.Free;
  end;
end;

function TAPIClient.parsePurchaseResponse(
  js: TlkJSONobject): TPurchaseResponse;
var
  payload: TlkJSONobject;
  cart: TlkJSONobject;
begin
  payload:= MustField(js,'payload') as TlkJSONobject;
  Result.Purchase := parsePurchase(MustField(payload,'purchase') as TlkJSONobject);

  cart:= MustField(payload,'cart') as TlkJSONobject;

    if not IsNullJSON(cart,'possible_marketing_actions') then
      Result.PossibleActions := parseMarketingActions(
        MustField(cart,'possible_marketing_actions') as TlkJSONlist);

    if not IsNullJSON(cart,'marketing_actions_applied') then
      Result.MarketingActionsApplied := parseMarketingActions(
        MustField(cart,'marketing_actions_applied') as TlkJSONlist);

	if not IsNullJSON(cart,'cart') then
    	Result.Cart := parseCart(MustField(cart,'cart') as TlkJSONobject);

end;

function TAPIClient.PurchaseGet(
  reqParams: IAPIRequiredParams): TPurchaseResponse;
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
    js := Client.Get(URL + '/pl/purchases/get', params, headers);
  finally
    params.Free;
    headers.Free;
  end;
  try
    if IsError(js) then
      raise ExceptionApiCall.CreateFromResponse(TErrorResponse.FromJSON(js));
    Result := parsePurchaseResponse(js);
  finally
    js.Free;
  end;
end;


function TAPIClient.PurchaseDelete(
  reqParams: IAPIRequiredParams): TPurchaseDeleteResponse;
var
  js: TlkJsonObject;
  headers: TStrings;
  params: TStrings;
  payload: TlkJSONobject;
begin
  headers := TStringList.Create;
  params := TStringList.Create;
  try
    reqParams.ApplyHeaders(headers);
    reqParams.Extra.ApplyParams(params);
    js := Client.Post(URL + '/pl/purchases/delete', params, headers);
  finally
    params.Free;
    headers.Free;
  end;
  try
    if IsError(js) then
      raise ExceptionApiCall.CreateFromResponse(TErrorResponse.FromJSON(js));
	  payload:= MustField(js,'payload') as TlkJSONobject;
      Result.OrderNum := GetValueJSON(payload,'order_num','');
      Result.ID := StrToIntDef(GetValueJSON(payload,'id',0),0);
  finally
    js.Free;
  end;
end;

function TAPIClient.PurchaseConfirm(
  reqParams: IAPIRequiredParams): TPurchaseResponse;
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
    js := Client.Post(URL + '/pl/purchases/confirm', params, headers);
  finally
    params.Free;
    headers.Free;
  end;
  try
    if IsError(js) then
      raise ExceptionApiCall.CreateFromResponse(TErrorResponse.FromJSON(js));
    Result := parsePurchaseResponse(js);
  finally
    js.Free;
  end;
end;

function TAPIClient.PurchaseEdit(
  reqParams: IAPIRequiredParams): TPurchaseEditResponse;
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
    js := Client.Post(URL + '/pl/purchases/edit', params, headers);
  finally
    params.Free;
    headers.Free;
  end;
  try
    if IsError(js) then
      raise ExceptionApiCall.CreateFromResponse(TErrorResponse.FromJSON(js));
    Result := parsePurchaseEditResponse(js);
  finally
    js.Free;
  end;
end;

function TAPIClient.parsePurchaseEditResponse(
  js: TlkJSONobject): TPurchaseEditResponse;
var
  payload: TlkJSONobject;
  user: TlkJSONobject;
begin
  payload:= MustField(js,'payload') as TlkJSONobject;
  Result.ID := GetValueJSON(payload,'id',0);
  Result.OrderNum := GetValueJSON(payload,'order_num','');

  if not IsNullJSON(payload,'user') then
  begin
    user := MustField(payload,'user') as TlkJSONobject;
  	Result.User.FullPhone:= GetValueJSON(user,'full_phone','');
    Result.User.Phone:= GetValueJSON(user,'phone','');
    Result.User.OriginUserID:= GetValueJSON(user,'origin_user_id','');
    Result.User.LastName:= GetValueJSON(user,'last_name','');
    Result.User.FirstName:= GetValueJSON(user,'first_name','');
    Result.User.Points:= GetValueJSON(user,'points',0);
    Result.User.ID:= GetValueJSON(user,'id',0);
    Result.User.Email:= GetValueJSON(user,'id','');
  end;
end;



end.

