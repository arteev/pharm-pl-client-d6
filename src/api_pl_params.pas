unit api_pl_params;

interface

uses auth;

type

implementation

{ TAPIParameters }

constructor TAPIParameters.Create(URL: string);
begin
 FURL := URL;
end;

end.
