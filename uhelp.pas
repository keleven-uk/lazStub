unit Uhelp;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls;

type

  { TfrmHelp }

  TfrmHelp = class(TForm)
    btnhelpExit: TButton;
    mmoHelp: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    procedure btnhelpExitClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  frmHelp: TfrmHelp;

implementation

{$R *.lfm}

{ TfrmHelp }

procedure TfrmHelp.FormCreate(Sender: TObject);
begin
  mmoHelp.Append('Stub.');
  mmoHelp.Append('');
  mmoHelp.Append('Kevin Scott (c) - 2012.');
  mmoHelp.Append('Stub Version :: 0.0.0.1.');
end;

procedure TfrmHelp.btnhelpExitClick(Sender: TObject);
begin
  Close;
end;

end.

