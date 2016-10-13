unit uLicence;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls;

type

  { TfrmLicence }

  TfrmLicence = class(TForm)
    btnLicenceExit: TButton;
    mmoLicence: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    procedure btnLicenceExitClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  frmLicence: TfrmLicence;

implementation

{$R *.lfm}

{ TfrmLicence }

procedure TfrmLicence.FormCreate(Sender: TObject);
begin
  mmoLicence.Append('Stub.');
  mmoLicence.Append('');
  try
    mmoLicence.Lines.LoadFromFile('GNU GENERAL PUBLIC LICENSE.txt');
  except
    on Exception do begin
      mmoLicence.Append(' help License not found.');
      mmoLicence.Append('');
      mmoLicence.Append(' The application is issued under the GNU GENERAL PUBLIC LICENSE.');
    end;
  end;

  mmoLicence.Append('');
  mmoLicence.Append('Kevin Scott (c) - 2012.');
  mmoLicence.Append('Stub Build Version :: 6');
end;

procedure TfrmLicence.btnLicenceExitClick(Sender: TObject);
begin
  Close;
end;

end.

