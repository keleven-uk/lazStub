program stub;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, UStub, UAbout, Uhelp, UOptions
  { you can add units after this };

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmAbout, frmAbout);
  Application.CreateForm(TfrmHelp, frmHelp);
  Application.CreateForm(TfrmOptions, frmOptions);
  Application.Run;
end.

