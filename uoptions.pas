unit UOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, INIFiles;

type

  { TfrmOptions }

  TfrmOptions = class(TForm)
    btnExit: TButton;
    Panel1: TPanel;
    Panel2: TPanel;
    procedure btnExitClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure checkIniFile;
    procedure writeIniFile;
    procedure writeIniValues;
  public
    { public declarations }
  end; 

{                                                      ** Options Class  **                        }
  OptionsRecord = class

  Private

  Public
    Version : string;                 //  application version

    Constructor init;
  end;


var
  frmOptions : TfrmOptions;
  OptionsRec : OptionsRecord;                 //  Options record
  IniFile    : TIniFile ;
  iniName    : String;

implementation

{$R *.lfm}

{ TfrmOptions }

{                      ********************************** Options Class methods  **                }

Constructor OptionsRecord.init;
begin
  self.Version := '8';
end;

procedure TfrmOptions.FormCreate(Sender: TObject);
VAR
       DebufFleName : String;
begin
  iniName := 'stub.ini';

  OptionsRec := OptionsRecord.Create;    //  create options record, can then be used in main form.
  OptionsRec.init;                       //  Does not seem to be called automaticaly.

  checkIniFile;                        //  check for ini file, if not there - create.

end;

procedure TfrmOptions.btnExitClick(Sender: TObject);
begin
  Close;
end;


{  ********************************************************************************** ini file **  }

procedure TfrmOptions.checkIniFile;
begin
  IniFile := TINIFile.Create(iniName);

  if (FileExists(iniName)) then begin  // read ini files and populate options record.

  end
  else begin  //  ini file does not exist, create it.
      writeIniValues
  end;

  iniFile.Free;
end;

procedure TfrmOptions.writeIniFile;
{  write optione record to ini file.                                                               }
begin
  IniFile := TINIFile.Create(iniName);

  writeIniValues;

  iniFile.Free;
end;

procedure TfrmOptions.writeIniValues;
begin
  IniFile.WriteString('Stub', 'Version', OptionsRec.Version);
end;

















end.

