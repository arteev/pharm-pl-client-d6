unit api_pl_params;

interface

uses
  api_template, Classes, SysUtils, uLkJSON;

type
  TAPIBaseParams = class(TInterfacedObject, IAPIParams)
  protected
    procedure AddValue(strings: TStrings; const name, value: string);
  public
    procedure ApplyHeaders(strings: TStrings); virtual;
    procedure ApplyParams(strings: TStrings); overload;virtual;
    procedure ApplyParams(stream:TStream);overload;virtual;
  end;

  TAPIClientInfoParams = class(TAPIBaseParams)
  private
    FOriginID: string;
    FPhone: string;
    FEmail: string;
    FHistory: Boolean;
    FSubscriptions: Boolean;
    FExtraFields: string;
  public
    constructor Create(const AOriginID, APhone, AEmail: string; const AHistory, ASubscriptions: Boolean; const AExtraFields: string);
    procedure ApplyParams(strings: TStrings); override;
  end;

  TAPIClientAddParams = class(TAPIBaseParams)
  private
    FOriginID: string;
    FPhone: string;
    FEmail: string;
    FFirstName: string;
    FLastName: string;
    FMiddleName: string;
    FBirthDate: TDateTime;
    FSex: Integer;
    FUserCategoryName:string;
    FExtraFields: string;
    FReferrerOriginUserID: string;
    FReferrerPhone: string;
    FReferrerEmail: string;
    FReferrerPromocode: string;
  public
    constructor Create(const AOriginID, APhone, AEmail, AFirstName, ALastName,
    	AMiddleName: string; ABirthDate: TDateTime; ASex: Integer;
        const AUserCategoryName: string = '';
        const AExtraFields: string = '';
        const AReferrerOriginUserID: string = '';
        const AReferrerPhone: string = '';
        const AReferrerEmail: string = '';
        const AReferrerPromocode: string = '');
    procedure ApplyParams(strings: TStrings); override;
  end;

  TAPIClientSendSMSParams = class(TAPIBaseParams)
  private
    FOriginID: string;
    FPhone: string;
    FEmail: string;
    FSMSText: string;
    FPriority: Integer;
  public
    constructor Create(const AOriginID, APhone, AEmail:string;
    	const ASMSText:string=''; const APriority:Integer=1);
    procedure ApplyParams(strings: TStrings); override;
  end;

  TArrayStrings = array of string;
  TCartItem = record
    Num:string;
    SKU:string;
    Price:Double;
    Quantity:Double;
    DiscountPoints:Integer;
    MinPrice:Double;
  end;
  TArrayCartItems = array of TCartItem;

  TAPIMarketingCartCalcParams = class(TAPIBaseParams)
  private
    FPromocodes:TArrayStrings;
    FVerbose:Integer;
    FCardNumbers:TArrayStrings;
    FCart:TArrayCartItems;
  public
    constructor Create(
        const ACart:TArrayCartItems;
	    AVerbose:Integer=0;
    	APromocodes:TArrayStrings=nil;
    	ACardNumbers:TArrayStrings=nil);
    procedure ApplyParams(strings: TStrings); override;
  end;

  TAPIPurcaseNewParams = class(TAPIBaseParams)
  private
    FOriginID: string;
    FPhone: string;
    FEmail: string;
    FOrderNum: string;
    FCardID: integer;
    FCart: TArrayCartItems;
    FPurchaseDepID: Integer;
    FPurchaseDepOriginID: Integer;
    FForceComplete: Boolean;
  public
    constructor Create(const AOriginID, APhone, AEmail: string;
      const AOrderNum: string;
      const ACardID: integer;
      const ACart: TArrayCartItems = nil;
      const APurchaseDepID: Integer = 0;
      const APurchaseDepOriginID: Integer = 0;
      const AForceComplete: Boolean = False);
    procedure ApplyParams(strings: TStrings); override;
  end;

  TAPIPurchaseGetParams = class(TAPIBaseParams)
  private
    ForderNum:string;
  public
    constructor Create(const AOrderNum:string);
    procedure ApplyParams(strings: TStrings); override;
  end;

  TAPIPurchaseDeleteParams = class(TAPIBaseParams)
  private
    ForderNum:string;
    FPurchaseID:Integer;
  public
    constructor Create(const AOrderNum:string;APurchaseID:Integer=0);
    procedure ApplyParams(strings: TStrings); override;
  end;

  TAPIPurchaseConfirmParams = class (TAPIBaseParams)
  private
    ForderNum:string;
    FPurchaseID:Integer;
    FNewPrice:Double;
    FPositons:String;
  public
    constructor Create(const AOrderNum:string;APurchaseID:Integer=0;
    	const ANewPrice:double=0;
        const APositons:String='');
    procedure ApplyParams(strings: TStrings); override;
  end;

  TAPIPurchaseEditParams = class(TAPIBaseParams)
  private
    ForderNum: string;
    FPurchaseID: Integer;
    FEmail:string;
    FOriginUserID:string;
    FUserPhone:string;
  public
    constructor Create(const AOrderNum: string; const AUserPhone: string;
    	const AEmail: string = '';
        const AOriginUserID: string = '';
        const APurchaseID: Integer = 0);
    procedure ApplyParams(strings: TStrings); override;
  end;

  TAPIPurcaseNewQueueParams = class(TAPIBaseParams)
  private
    FOriginID:string;
    FPhone:string;
    FEmail:string;
    FProviderName:string;
    FUserID:Integer;
    FOrderNum:string;
    FCart:TArrayCartItems;
    FPurchaseDepID:Integer;
    FPurchaseDepOriginID:Integer;
  public
    constructor Create(const AOriginID, APhone, AEmail: string;
      const AProviderName:string;
      const AUserID:Integer;
      const AOrderNum: string;
      const ACart: TArrayCartItems;
      const APurchaseDepID: Integer = 0;
      const APurchaseDepOriginID: Integer = 0);
    procedure ApplyParams(stream:TStream);override;
  end;

  TCartReturnsItem = record
    Num:string;
    Quantity: Integer;
    Reason: string;
  end;
  TArrayCartReturnsItems = array of TCartReturnsItem;
  TAPIPurchaseReturnsParams = class (TAPIBaseParams)
  private
    ForderNum:string;
    FCart:TArrayCartReturnsItems;
  public
    constructor Create(const AOrderNum:string;
    	ACart:TArrayCartReturnsItems);
    procedure ApplyParams(strings: TStrings); override;
  end;
  TAPICheckOnline = class(TAPIBaseParams)
  end;



implementation

var
  BoolToStr: array[Boolean] of string = ('0', '1');
  
{ TAPIBaseParams }

procedure TAPIBaseParams.AddValue(strings: TStrings; const name, value: string);
begin
  if value <> '' then
    strings.Values[name] := value;
end;

procedure TAPIBaseParams.ApplyHeaders(strings: TStrings);
begin

end;

procedure TAPIBaseParams.ApplyParams(strings: TStrings);
begin

end;

procedure TAPIBaseParams.ApplyParams(stream: TStream);
begin
//
end;

{ TAPIClientInfoParams }

procedure TAPIClientInfoParams.ApplyParams(strings: TStrings);
begin
  inherited ApplyParams(strings);
  AddValue(strings, 'user_phone', FPhone);
  AddValue(strings, 'origin_user_id', FOriginID);
  AddValue(strings, 'email', FOriginID);
  AddValue(strings, 'extra_fields', FExtraFields);
  AddValue(strings, 'history', BoolToStr[FHistory]);
  AddValue(strings, 'subscriptions', BoolToStr[FSubscriptions]);
end;

constructor TAPIClientInfoParams.Create(const AOriginID, APhone, AEmail: string; const AHistory, ASubscriptions: Boolean; const AExtraFields: string);
begin
  FOriginID := AOriginID;
  FPhone := APhone;
  FEmail := AEmail;
  FHistory := AHistory;
  FSubscriptions := ASubscriptions;
  FExtraFields := AExtraFields;
end;

{ TAPIClientAddParams }

procedure TAPIClientAddParams.ApplyParams(strings: TStrings);
begin
  inherited ApplyParams(strings);
  AddValue(strings, 'user_phone', FPhone);
  AddValue(strings, 'origin_user_id', FOriginID);
  AddValue(strings, 'email', FEmail);
  AddValue(strings, 'user_category_name', FUserCategoryName);
  AddValue(strings, 'first_name', FFirstName);
  AddValue(strings, 'last_name', FLastName);
  AddValue(strings, 'middle_name', FMiddleName);
  if FBirthDate<>0 then
    AddValue(strings, 'birth_date', FormatDateTime('yyyy-mm-dd',FBirthDate));
  AddValue(strings, 'sex', IntToStr(FSex));
  AddValue(strings, 'extra_fields', FExtraFields);
  AddValue(strings, 'referrer_origin_user_id', FReferrerOriginUserID);
  AddValue(strings, 'referrer_phone', FReferrerPhone);
  AddValue(strings, 'referrer_email', FReferrerEmail);
  AddValue(strings, 'referrer_promocode', FReferrerPromocode);
end;

constructor TAPIClientAddParams.Create(const AOriginID, APhone, AEmail,
        AFirstName, ALastName,AMiddleName: string;
        ABirthDate: TDateTime; ASex: Integer;
        const AUserCategoryName: string = '';
        const AExtraFields: string = '';
        const AReferrerOriginUserID: string = '';
        const AReferrerPhone: string = '';
        const AReferrerEmail: string = '';
        const AReferrerPromocode: string = '');
begin
  FOriginID := AOriginID;
  FPhone := APhone;
  FEmail := AEmail;
  FFirstName := AFirstName;
  FLastName := ALastName;
  FMiddleName := AMiddleName;
  FBirthDate := ABirthDate;
  FSex := ASex;
  FUserCategoryName:=AUserCategoryName;
  FExtraFields:=AExtraFields;
  FReferrerOriginUserID:=AReferrerOriginUserID;
  FReferrerPhone:=AReferrerPhone;
  FReferrerEmail:=AReferrerEmail;
  FReferrerPromocode:=AReferrerPromocode;
end;

{ TAPIClientSendSMSPArams }
procedure TAPIClientSendSMSPArams.ApplyParams(strings: TStrings);
begin
  inherited ApplyParams(strings);
  AddValue(strings, 'user_phone', FPhone);
  AddValue(strings, 'origin_user_id', FOriginID);
  AddValue(strings, 'email', FEmail);
  AddValue(strings, 'text', FSMSText);
  if FPriority<>0 then
	AddValue(strings, 'priority', IntToStr(FPriority));
end;

constructor TAPIClientSendSMSPArams.Create(const AOriginID, APhone, AEmail,
	ASMSText: string; const APriority: Integer);
begin
  FOriginID := AOriginID;
  FPhone := APhone;
  FEmail := AEmail;
  FSMSText := ASMSText;
  FPriority := APriority;
end;

{ TAPIMarketingCartCalcParams }

procedure TAPIMarketingCartCalcParams.ApplyParams(strings: TStrings);
var js:TlkJSONlist;
	i:Integer;
    jsCart:TlkJSONobject;
    jsCartItem:TlkJSONobject;
begin
  inherited ApplyParams(strings);
  AddValue(strings,'verbose',IntToStr(FVerbose));
  js := TlkJSONlist.Create();
  try
    for i:=0 to Length(FPromocodes)-1 do
      js.Add(TlkJSONstring.Generate(FPromocodes[i]));

    AddValue(strings,'promocodes',TlkJSON.GenerateText(js));
  finally
    js.Free;
  end;

  js := TlkJSONlist.Create();
  try
    for i:=0 to Length(FCardNumbers)-1 do
      js.Add(TlkJSONstring.Generate(FCardNumbers[i]));

    AddValue(strings,'card_numbers',TlkJSON.GenerateText(js));
  finally
    js.Free;
  end;

  jsCart := TlkJSONobject.Create();
  try
    for i:=0 to Length(FCart)-1 do
    begin
      jsCartItem := TlkJSONobject.Create();
      jsCartItem.Add('sku', TlkJSONstring.Generate(FCart[i].SKU));
      jsCartItem.Add('price', TlkJSONnumber.Generate(FCart[i].Price));
      jsCartItem.Add('quantity', TlkJSONnumber.Generate(FCart[i].Quantity));
      jsCartItem.Add('discount_points', TlkJSONnumber.Generate(FCart[i].DiscountPoints));
      jsCartItem.Add('min_price', TlkJSONnumber.Generate(FCart[i].MinPrice));
      jsCart.Add(FCart[i].Num, jsCartItem);
    end;

    AddValue(strings,'cart',TlkJSON.GenerateText(jsCart));
  finally
    jsCart.Free;
  end;
end;

constructor TAPIMarketingCartCalcParams.Create(const ACart: TArrayCartItems;
  AVerbose: Integer; APromocodes, ACardNumbers: TArrayStrings);
begin
  FCart := ACart;
  FVerbose:=AVerbose;
  FPromocodes:=APromocodes;
  FCardNumbers:=ACardNumbers;
end;

{ TAPIPurcaseNewParams }

procedure TAPIPurcaseNewParams.ApplyParams(strings: TStrings);
var 
	i:Integer;
    jsCart:TlkJSONobject;
    jsCartItem:TlkJSONobject;
    OldDecimalSeparator:Char;
begin
  inherited ApplyParams(strings);
  OldDecimalSeparator:=DecimalSeparator;
  DecimalSeparator := '.';
  try
    AddValue(strings, 'user_phone', FPhone);
    AddValue(strings, 'origin_user_id', FOriginID);
    AddValue(strings, 'email', FEmail);
    AddValue(strings, 'order_num', FOrderNum);
    if FCardID<>0 then
      AddValue(strings, 'cart_id', IntToStr(FCardID));
    if FPurchaseDepID<>0 then
      AddValue(strings, 'purchase_dep_id', IntToStr(FPurchaseDepID));
    if FPurchaseDepOriginID<>0 then
      AddValue(strings, 'purchase_dep_origin_id', IntToStr(FPurchaseDepOriginID));
    if FForceComplete then
      AddValue(strings,'force_complete',BoolToStr[FForceComplete]);

    if Length(FCart)>0 then
    begin
      jsCart := TlkJSONobject.Create();
      try
        for i:=0 to Length(FCart)-1 do
        begin
          jsCartItem := TlkJSONobject.Create();
          jsCartItem.Add('sku', TlkJSONstring.Generate(FCart[i].SKU));
          jsCartItem.Add('price', TlkJSONnumber.Generate(FCart[i].Price));
          jsCartItem.Add('quantity', TlkJSONnumber.Generate(FCart[i].Quantity));
          jsCart.Add(FCart[i].Num, jsCartItem);
        end;
        AddValue(strings,'cart',TlkJSON.GenerateText(jsCart));
      finally
        jsCart.Free;
      end;
    end;
  finally
    DecimalSeparator:=OldDecimalSeparator;
  end;
end;


constructor TAPIPurcaseNewParams.Create(const AOriginID, APhone, AEmail,
  AOrderNum: string; const ACardID: integer; const ACart: TArrayCartItems;
  const APurchaseDepID, APurchaseDepOriginID: Integer;
  const AForceComplete: Boolean);
begin
  FOriginID := AOriginID;
  FPhone := APhone;
  FEmail := AEmail;
  FOrderNum := AOrderNum;
  FCardID := ACardID;
  FCart := ACart;
  FPurchaseDepID := APurchaseDepID;
  FPurchaseDepOriginID := APurchaseDepOriginID;
  FForceComplete := AForceComplete;
end;

{ TAPIPurchaseGetParams }

procedure TAPIPurchaseGetParams.ApplyParams(strings: TStrings);
begin
  inherited ApplyParams(strings);
  AddValue(strings, 'order_num', ForderNum);
end;

constructor TAPIPurchaseGetParams.Create(const AOrderNum: string);
begin
  ForderNum:=AOrderNum;
end;

{ TAPIPurchaseDeleteParams }

procedure TAPIPurchaseDeleteParams.ApplyParams(strings: TStrings);
begin
  inherited ApplyParams(strings);
  AddValue(strings, 'order_num', ForderNum);
  if FPurchaseID<>0 then
    AddValue(strings, 'purchase_id', IntToStr(FPurchaseID));
end;

constructor TAPIPurchaseDeleteParams.Create(const AOrderNum: string;
  APurchaseID: Integer);
begin
  ForderNum:=AOrderNum;
  FPurchaseID := APurchaseID;
end;

{ TAPIPurchaseConfirmParams }

procedure TAPIPurchaseConfirmParams.ApplyParams(strings: TStrings);
begin
  inherited ApplyParams(strings);
  AddValue(strings, 'order_num', ForderNum);
  if FPurchaseID<>0 then
    AddValue(strings, 'purchase_id', IntToStr(FPurchaseID));
  if FNewPrice<>0 then
    AddValue(strings,'new_price',FloatToStr(FNewPrice));
  AddValue(strings,'positions',FPositons);

end;

constructor TAPIPurchaseConfirmParams.Create(const AOrderNum: string;
  APurchaseID: Integer; const ANewPrice: double; const APositons: String);
begin
  FOrderNum := AOrderNum;
  FPurchaseID := APurchaseID;
  FNewPrice := ANewPrice;
  FPositons := APositons;
end;

{ TAPIPurchaseEditParams }

procedure TAPIPurchaseEditParams.ApplyParams(strings: TStrings);
begin
  inherited ApplyParams(strings);
  AddValue(strings,'order_num',ForderNum);
  if FPurchaseID>0 then
    AddValue(strings, 'purchase_id', IntToStr(FPurchaseID));
  AddValue(strings,'user_phone',FUserPhone);
  AddValue(strings,'email',FEmail);
  AddValue(strings,'origin_user_id',FOriginUserID);
end;

constructor TAPIPurchaseEditParams.Create(const AOrderNum, AUserPhone,
  AEmail, AOriginUserID: string; const APurchaseID: Integer);
begin
  ForderNum := AOrderNum;
  FUserPhone := AUserPhone;
  FEmail := AEmail;
  FOriginUserID := AOriginUserID;
  FPurchaseID := APurchaseID;
end;

{ TAPIPurcaseNewQueueParams }

procedure TAPIPurcaseNewQueueParams.ApplyParams(stream: TStream);
var
	i:Integer;
    js: TlkJSONobject;
    jsCart:TlkJSONobject;
    jsCartItem:TlkJSONobject;
    OldDecimalSeparator:Char;
begin
  OldDecimalSeparator:=DecimalSeparator;
  DecimalSeparator := '.';
  js := TlkJSONobject.Create();
  try
    if FPhone<>'' then
    	js.Add('user_phone',TlkJSONstring.Generate(FPhone));
    if FOriginID<>'' then
    	js.Add('origin_user_id',TlkJSONstring.Generate(FOriginID));
    if FEmail<>'' then
    	js.Add('email',TlkJSONstring.Generate(FEmail));

    js.Add('provider',TlkJSONstring.Generate(FProviderName));
    js.Add('order_num',TlkJSONstring.Generate(FOrderNum));
    js.Add('user_id',TlkJSONnumber.Generate(FUserID));

    if FPurchaseDepID<>0 then
        js.Add('purchase_dep_id',TlkJSONnumber.Generate(FPurchaseDepID));
	if FPurchaseDepOriginID<>0 then
    	js.Add('purchase_dep_origin_id',TlkJSONnumber.Generate(FPurchaseDepOriginID));


    if Length(FCart)>0 then
    begin
      jsCart := TlkJSONobject.Create();
      for i:=0 to Length(FCart)-1 do
      begin
        jsCartItem := TlkJSONobject.Create();
        jsCartItem.Add('sku', TlkJSONstring.Generate(FCart[i].SKU));
        jsCartItem.Add('price', TlkJSONnumber.Generate(FCart[i].Price));
        jsCartItem.Add('quantity', TlkJSONnumber.Generate(FCart[i].Quantity));

        jsCart.Add(FCart[i].Num, jsCartItem);
      end;
      js.Add('cart',jsCart);
    end;

    TlkJSONstreamed.SaveToStream(js,stream);

  finally
    DecimalSeparator:=OldDecimalSeparator;
    js.Free;
  end;
end;

constructor TAPIPurcaseNewQueueParams.Create(const AOriginID, APhone,
  AEmail,AProviderName:string;
  const AUserID:Integer;
  const AOrderNum: string; const ACart: TArrayCartItems;
  const APurchaseDepID, APurchaseDepOriginID: Integer);
begin
  FOriginID := AOriginID;
  FPhone := APhone;
  FEmail := AEmail;
  FProviderName:=AProviderName;
  FUserID := AUserID;
  FOrderNum := AOrderNum;
  FCart := ACart;
  FPurchaseDepID:=APurchaseDepID;
  FPurchaseDepOriginID := APurchaseDepOriginID;
end;




{ TAPIPurchaseReturnsParams }

procedure TAPIPurchaseReturnsParams.ApplyParams(strings: TStrings);
var
  i: Integer;
  jsCart: TlkJSONobject;
  jsCartItem: TlkJSONobject;
  OldDecimalSeparator: Char;
begin
  inherited ApplyParams(strings);
  OldDecimalSeparator := DecimalSeparator;
  DecimalSeparator := '.';
  try
    AddValue(strings, 'order_num', ForderNum);
    if Length(FCart) > 0 then
    begin
      jsCart := TlkJSONobject.Create();
      try
        for i := 0 to Length(FCart) - 1 do
        begin
          jsCartItem := TlkJSONobject.Create();
          jsCartItem.Add('quantity', TlkJSONnumber.Generate(FCart[i].Quantity));
          jsCartItem.Add('reason', TlkJSONstring.Generate(FCart[i].Reason));
          jsCart.Add(FCart[i].Num, jsCartItem);
        end;
        AddValue(strings, 'return_cart', TlkJSON.GenerateText(jsCart));
      finally
        jsCart.Free;
      end;
    end;
  finally
    DecimalSeparator := OldDecimalSeparator;
  end;
end;

constructor TAPIPurchaseReturnsParams.Create(const AOrderNum: string;
  ACart: TArrayCartReturnsItems);
begin
  Self.ForderNum := AOrderNum;
  Self.FCart := ACart;
end;

end.

