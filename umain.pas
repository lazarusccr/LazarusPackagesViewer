unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  BCPanel, BCLabel, BCComboBox, fphttpclient, fpjson;

resourcestring
  rsCantLoadJSON = 'Can''''t load JSON from server.';

type

  { TfrmMain }

  TfrmMain = class(TForm)
    bccCategory: TBCComboBox;
    bclTitle: TBCLabel;
    bclPackageDescription: TBCLabel;
    bclPackageDescriptionText: TBCLabel;
    bclPackageLicense: TBCLabel;
    bclPackageLicenseText: TBCLabel;
    bclPackageVersion: TBCLabel;
    bclPackageVersionText: TBCLabel;
    bclPackageTitle: TBCLabel;
    bclPackageCategory: TBCLabel;
    bclDownload: TBCLabel;
    bclHomePage: TBCLabel;
    bclPackageName: TBCLabel;
    bclPackageNameText: TBCLabel;
    bclPackageAuthor: TBCLabel;
    bclPackageAuthorText: TBCLabel;
    bcpHeader: TBCPanel;
    bcpPackage: TBCPanel;
    bcpSearch: TBCPanel;
    edtSearch: TEdit;
    bcpPackageLPK: TPanel;
    bcpLinks: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    JSON: TJSONArray;
    procedure LoadJSON;
  public

  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  LoadJSON;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  if Assigned(JSON) then
    JSON.Free;
end;

procedure TfrmMain.LoadJSON;
var
  data: string;
begin
  try
    data := TFPHTTPClient.SimpleGet('http://packages.lazarus-ide.org/packagelist.json');
    JSON := TJSONArray(GetJSON(data));
  except
    on e: Exception do
    begin
      ShowMessage(rsCantLoadJSON + LineEnding + e.Message);
      Application.Terminate;
    end;
  end;
end;

end.

