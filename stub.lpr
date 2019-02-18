program stub;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, formStub, formAbout, formhelp, formOptions, formLicence, stubUntils,
  ULogging
  { you can add units after this };
  //, SysUtils;

{$R *.res}

begin
  //if FileExists('heap.trc') then
  //  DeleteFile('heap.trc');
  //SetHeapTraceOutput('heap.trc');

  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmOptions, frmOptions);
  Application.Run;
end.

