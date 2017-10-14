unit formAbout;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  LCLVersion, ExtCtrls;

type

  { TfrmAbout }

  TfrmAbout = class(TForm)
    btnAboutExit: TButton;
    Image1: TImage;
    lblFileVersion: TLabel;
    lblCompanyName: TLabel;
    lblContact: TLabel;
    lblDiskSize: TLabel;
    lblStubversion: TLabel;
    lblProgrammer: TLabel;
    lblProgramDescription: TLabel;
    lblProgramName: TLabel;
    lblLazarusVersion: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    procedure btnAboutExitClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  frmAbout: TfrmAbout;

implementation

uses
  formStub;

{$R *.lfm}

{ TfrmAbout }


procedure TfrmAbout.btnAboutExitClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmAbout.FormCreate(Sender: TObject);
VAR
  dskSize : String;
  dskFree : String;
begin
  dskFree := FloatToStrF(DiskFree(0) / 1073741824, ffFixed, 3, 2);
  dskSize := FloatToStrF(DiskSize(0) / 1073741824, ffFixed, 3, 2);

  lblProgramName.Caption := userOptions.productName;
  lblProgramDescription.Caption := userOptions.fileDescription;
  lblProgrammer.Caption := userOptions.legalCopyright;

  {$ifdef WIN32}
    lblLazarusVersion.Caption := format('Built with 32 bit Lazarus Version :: %s', [lcl_version]);
  {$else}
    lblLazarusVersion.Caption := format('Built with 64 bit Lazarus Version :: %s', [lcl_version]);
  {$endif}

  lblStubversion.Caption := format('stub Build   :: %s', [userOptions.productVersion]);
  lblFileVersion.Caption := format('stub Version :: %s', [userOptions.fileVersion]);
  lblCompanyName.Caption := UserOptions.CompanyName;
  lblContact.Caption := UserOptions.Comments;

  lblDiskSize.Caption := ' Disk Free / Size :: ' + dskFree + ' / ' +  dskSize + ' Gbytes'
end;

end.

