unit api_pl_params;

interface

uses api_template, Classes;


type
  TAPIBaseParams=class(TInterfacedObject,IAPIParams)
  protected
    procedure AddValue(strings:TStrings;const name,value:string);
  public
    procedure ApplyHeaders(strings:TStrings);virtual;
    procedure ApplyParams(strings:TStrings);virtual;
  end;

  TAPIClientInfoParams=class(TAPIBaseParams)
  private
    FOriginID:string;
    FPhone:string;
    FEmail:string;
    FHistory:Boolean;
    FSubscriptions:Boolean;
    FExtraFields:string;
  public
    constructor Create(const AOriginID,APhone,AEmail:string;
    	const AHistory,ASubscriptions:Boolean;
	    const AExtraFields:string);
    procedure ApplyParams(strings:TStrings);override;
  end;

implementation

var
  BoolToStr: array[Boolean] of string = ('0','1');
  
{ TAPIBaseParams }

procedure TAPIBaseParams.AddValue(strings: TStrings; const name,
  value: string);
begin
  if value<>'' then
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
  AddValue(strings,'user_phone',FPhone);
  AddValue(strings,'origin_user_id',FOriginID);
  AddValue(strings,'email',FOriginID);
  AddValue(strings,'extra_fields',FExtraFields);
  AddValue(strings,'history',BoolToStr[FHistory]);
  AddValue(strings,'subscriptions',BoolToStr[FSubscriptions]);
end;

constructor TAPIClientInfoParams.Create(const AOriginID, APhone,
  AEmail: string; const AHistory, ASubscriptions: Boolean;
  const AExtraFields: string);
begin
  FOriginID:=AOriginID;
  FPhone:=APhone;
  FEmail:=AEmail;
  FHistory:=AHistory;
  FSubscriptions:=ASubscriptions;
  FExtraFields:=AExtraFields;
end;

end.
