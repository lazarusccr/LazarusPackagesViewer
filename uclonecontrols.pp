unit uCloneControls;

{
  Author: Lainz and howardpc
  Original From Lainz: https://forum.lazarus.freepascal.org/index.php/topic,24239.msg287928.html#msg287928
  Latest Source From howardpc: https://forum.lazarus.freepascal.org/index.php/topic,24239.msg288051.html#msg288051
}

{$mode objfpc}{$H+}

interface

uses
  Classes, TypInfo, Controls;

procedure CloneControls(const aControl: TControl; const aNewOwnerParent: TWinControl; anIndex: Integer);

procedure CloneEvents(aSourceControl, aClonedControl: TControl);

procedure CloneProperties(aSourceControl: TControl; aClonedControl: TControl);


implementation

procedure CloneControls(const aControl: TControl; const aNewOwnerParent: TWinControl; anIndex: Integer);
var
  i: Integer;
  currentControl, clonedControl: TControl;
  winControl: TWinControl absolute currentControl;
  s: String;
begin
  currentControl := aControl;
  clonedControl := TControlClass(currentControl.ClassType).Create(aNewOwnerParent);
  clonedControl.Parent := aNewOwnerParent;
  CloneProperties(currentControl, clonedControl);
  WriteStr(s, anIndex);
  clonedControl.Name := 'CloneOf_' + currentControl.Name + s;
  // Debug
  // clonedControl.Hint := clonedControl.Name;
  CloneEvents(currentControl, clonedControl);
  if clonedControl is TWinControl then
    // Lainz: changed order to preserve positioning
    for i := 0 to winControl.ControlCount - 1 do
      CloneControls(winControl.Controls[i], TWinControl(clonedControl), i);
end;

procedure CloneEvents(aSourceControl, aClonedControl: TControl);
var
  i: integer;
  propList: TPropList;
begin
  Assert(aSourceControl.ClassNameIs(aClonedControl.ClassName),'CloneEvents: incompatible controls');
  for i := 0 to GetPropList(aSourceControl.ClassInfo, [tkMethod], @propList) - 1 do
    SetMethodProp(aClonedControl, propList[i], GetMethodProp(aSourceControl, propList[i]));
end;

procedure CloneProperties(aSourceControl: TControl; aClonedControl: TControl);
var
  ms: TMemoryStream;
  oldName: String;
begin
  Assert(aSourceControl.ClassNameIs(aClonedControl.ClassName),'CloneProperties: incompatible controls');
  oldName := aSourceControl.Name;
  aSourceControl.Name := '';
  try
    ms := TMemoryStream.Create;
    try
      ms.WriteComponent(aSourceControl);
      ms.Position := 0;
      ms.ReadComponent(aClonedControl);
    finally
      ms.Free;
    end;
  finally
    aSourceControl.Name := oldName;
  end;
end;

end.

