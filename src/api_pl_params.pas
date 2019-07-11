unit api_pl_params;

interface

uses
  api_template, Classes, SysUtils;

type
  TAPIBaseParams = class(TInterfacedObject, IAPIParams)
  protected
    procedure AddValue(strings: TStrings; const name, value: string);
  public
    procedure ApplyHeaders(strings: TStrings); virtual;
    procedure ApplyParams(strings: TStrings); virtual;
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

  TAPIClientSendSMSPArams = class(TAPIBaseParams)
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

end.

