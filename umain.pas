unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  BCPanel, BCLabel, BCComboBox, fphttpclient, fpjson, uCloneControls, LCLIntF;

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
    sbMain: TScrollBox;
    procedure bclLinkMouseEnter(Sender: TObject);
    procedure bclLinkMouseLeave(Sender: TObject);
    procedure bclLinkClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    JSON: TJSONObject;
    procedure LoadJSON;
    procedure DisplayJSON;
  public

  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.bclLinkMouseEnter(Sender: TObject);
begin
  TBCLabel(Sender).FontEx.Style := [fsUnderline];
end;

procedure TfrmMain.bclLinkMouseLeave(Sender: TObject);
begin
  TBCLabel(Sender).FontEx.Style := [];
end;

procedure TfrmMain.bclLinkClick(Sender: TObject);
begin
  OpenURL(TControl(Sender).Hint);
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  LoadJSON;
  DisplayJSON;
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
    JSON := TJSONObject(GetJSON(data));
  except
    on e: Exception do
    begin
      ShowMessage(rsCantLoadJSON + LineEnding + e.Message);
      Application.Terminate;
    end;
  end;
end;

procedure TfrmMain.DisplayJSON;
var
  i: integer;
  data: TJSONObject;
  files: TJSONArray;
  ctrl, bcp: TControl;
  bcl: TBCLabel;
begin
  i := 0;
  while (JSON.FindPath('PackageData' + IntToStr(i)) <> nil) do
  begin
    data := TJSONObject(JSON.FindPath('PackageData' + IntToStr(i)));
    files := TJSONArray(JSON.FindPath('PackageFiles' + IntToStr(i)));
    CloneControls(bcpPackage, sbMain, i);
    ctrl := sbMain.Controls[i];
    if Assigned(ctrl) then
    begin

      bcl := TBCLabel(ctrl.FindComponent('CloneOf_bclPackageTitle0'));
      bcl.Caption := data.FindPath('DisplayName').AsString;
      bcp := TControl(ctrl.FindComponent('CloneOf_bcpLinks2'));
      bcp.Caption := '';
      bcp := TControl(ctrl.FindComponent('CloneOf_bcpPackageLPK3'));
      bcp.Caption := '';
      ctrl.Caption := '';
      ctrl.Visible := True;
    end;
    Inc(i);
    // ToDo: limit of scrollbox height
    if (i >= 10) then
      break;
  end;
end;

end.

