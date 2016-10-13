unit UAbout;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  LCLVersion, ExtCtrls, UOptions;

type

  { TfrmAbout }

  TfrmAbout = class(TForm)
    btnAboutExit: TButton;
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

  lblLazarusVersion.Caption := format('Built with Lazarus Version :: %s', [lcl_version]);
  lblStubversion.Caption := format('stub Version :: %s', [OptionsRec.Version]);
  lblDiskSize.Caption := ' Disk Free / Size :: ' + dskFree + ' / ' +  dskSize + ' Gbytes'
end;

end.

