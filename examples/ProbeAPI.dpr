program ProbeAPI;

{%File 'http:\127.0.0.1:8080'}

uses
  Forms,
  MainForm in 'MainForm.pas' {Form1},
  auth in '..\src\auth.pas',
  api_json in '..\src\api_json.pas',
  api_pl in '..\src\api_pl.pas',
  http in '..\src\http.pas',
  token in '..\src\token.pas',
  unix_utils in '..\src\unix_utils.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.