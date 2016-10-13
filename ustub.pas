unit UStub;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ComCtrls, Menus, UAbout, Uhelp, UOptions, uLicence;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    mnuLicence: TMenuItem;
    mnuItmOptions: TMenuItem;
    mnuItmHelp: TMenuItem;
    mnuItmAbout: TMenuItem;
    mnuItmExit: TMenuItem;
    mnuhelp: TMenuItem;
    mnuFile: TMenuItem;
    mnuMain: TMainMenu;
    stsBrInfo: TStatusBar;
    Timer1: TTimer;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure mnuItmAboutClick(Sender: TObject);
    procedure mnuItmExitClick(Sender: TObject);
    procedure mnuItmHelpClick(Sender: TObject);
    procedure mnuItmOptionsClick(Sender: TObject);
    procedure mnuLicenceClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
  public
    noOfTicks : integer ;
  end; 

var
  frmMain  : TfrmMain;
  debugFle : text;
  debug    : Boolean;
implementation

{$R *.lfm}

{ TfrmMain }


procedure TfrmMain.FormCreate(Sender: TObject);
VAR
       DebufFleName : String;
begin

  debug := true ;

  if debug then begin
    DebufFleName := 'stub.log';
    assignfile(debugFle, DebufFleName);
    rewrite(debugFle);
    writeLn(debugFle, format ('%s : %s Created', [timeToStr(now), DebufFleName]));
  end;
end;


procedure TfrmMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if debug then begin
    writeLn(debugFle, format ('%s : log file Closed', [timeToStr(now)]));
    CloseFile(debugFle);
  end;
end;

procedure TfrmMain.mnuItmAboutClick(Sender: TObject);
begin
  frmAbout.ShowModal;
end;

procedure TfrmMain.mnuItmExitClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.mnuItmHelpClick(Sender: TObject);
begin
  frmHelp.ShowModal;
end;

procedure TfrmMain.mnuItmOptionsClick(Sender: TObject);
begin
  frmOptions.ShowModal;
end;

procedure TfrmMain.mnuLicenceClick(Sender: TObject);
begin
  frmLicence.Show;
end;

procedure TfrmMain.Timer1Timer(Sender: TObject);
begin
  stsBrInfo.Panels.Items[0].Text := TimeToStr(Time) ;
  stsBrInfo.Panels.Items[1].Text := FormatDateTime('DD MMM YYYY', Now);
end;

end.

