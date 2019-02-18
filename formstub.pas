unit formStub;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ComCtrls, Menus, formAbout, formhelp, formOptions, formLicence, uOptions,
  ULogging, stubUntils;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    mnuItmLicense : TMenuItem;
    mnuItmOptions : TMenuItem;
    mnuItmHelp    : TMenuItem;
    mnuItmAbout   : TMenuItem;
    mnuItmExit    : TMenuItem;
    mnuhelp       : TMenuItem;
    mnuFile       : TMenuItem;
    mnuMain       : TMainMenu;
    stsBrInfo     : TStatusBar;
    Timer1        : TTimer;

    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure mnuItmClick(Sender: TObject);
    procedure mnuItmOptionsClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
  public
    noOfTicks : integer ;
  end; 

var
  frmMain     : TfrmMain;
  userOptions : Options;    //  used to hold all the user options.
  kLog        : Logger;     //  used to log errors, debug statements etc.
  appStartTime: int64;      //  used by formAbout to determine how long the app has been running.
implementation

{$R *.lfm}

{ TfrmMain }


procedure TfrmMain.FormCreate(Sender: TObject);
begin
  appStartTime := GetTickCount64;  //  tick count when application starts.
  userOptions  := Options.Create;  // create options file as c:\Users\<user>\AppData\Local\Stub\Options.xml

  kLog := Logger.Create;
  logHeader;

  frmMain.Top  := UserOptions.formTop;
  frmmain.Left := UserOptions.formLeft;

  //if userOptions.cullLogs then     //  Removed old log files, if instructed.
  //  kLog.cullLogFile(userOptions.CullLogsDays);
  kLog.cullLogFile(14);
end;

procedure TfrmMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  UserOptions.formTop  := frmMain.Top;
  UserOptions.formLeft := frmmain.Left;

  logFooter;
  kLog.Free;                        //  Release the logger object.

  userOptions.writeCurrentOptions;  // write out options file.
  userOptions.Free;

  CloseAction:= caFree;
end;
//
// ********************************************************* Menu Items *********
//
procedure TfrmMain.mnuItmClick(Sender: TObject);
{  A generic click routine called by each menu item.

   The action of the menu is determined from the item name.
}
VAR
  itemName   : string;
begin
  itemName := '';

  //  set the appropiate name.
  if (Sender is TMenuItem) then
    itemName := TMenuItem(Sender).Name;

  if itemName = '' then exit;    //  not called by a TMenuItem

  case itemName of
  // ********************************************************* File Menu *********
  'mnuItmExit': close;
  // ********************************************************* Help Menu *********
  'mnuItmHelp':
  begin
    frmhelp := TfrmHelp.Create(Nil);
    frmhelp.ShowModal;
    FreeAndNil(frmHelp);
  end;
  'mnuItmAbout':                                                      //  Calls the About screen.
  begin
    frmAbout := TfrmAbout.Create(Nil);  //frmAbout is created
    frmAbout.ShowModal;                 //frmAbout is displayed
    FreeAndNil(frmAbout);               //frmAbout is released
  end;
  'mnuItmLicense':                                                      //  Calls the License screen.
  begin
    frmLicence := TfrmLicence.Create(Nil);
    frmLicence.ShowModal;
    FreeAndNil(frmLicence);
  end;
  end;
end;

procedure TfrmMain.mnuItmOptionsClick(Sender: TObject);
begin
  frmOptions.ShowModal;
end;

procedure TfrmMain.Timer1Timer(Sender: TObject);
begin
  stsBrInfo.Panels.Items[0].Text := TimeToStr(Time) ;
  stsBrInfo.Panels.Items[1].Text := FormatDateTime('DD MMM YYYY', Now);
end;

end.

