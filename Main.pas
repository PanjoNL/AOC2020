unit Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, AOCBase, AOCSolutions, Vcl.ExtCtrls,
  System.Generics.Collections, uAOCUtils;

type
  TForm1 = class(TForm)
    btnSolve: TButton;
    cbb1: TComboBox;
    btnTest: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnSolveClick(Sender: TObject);
    procedure btnTestClick(Sender: TObject);
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
var AdventOfCodeClasses: TList<TAdventOfCodeRef>;
    AdventOfCode: TAdventOfCodeRef;
begin
  AllocConsole;
  AOCUtils.Config.LoadConfig;
  AdventOfCodeClasses := AOCUtils.GetAdventOfCode;

  try
    for AdventOfCode in AdventOfCodeClasses Do
      cbb1.Items.AddObject('Day ' + AOCUtils.DayIndexFromClassName(AdventOfCode.ClassName), TObject(AdventOfCode));
  finally
    AdventOfCodeClasses.Free;
  end;
  cbb1.ItemIndex := cbb1.Items.Count - 1;

//  btnSolveClick(nil);
//  Application.Terminate;
  ReportMemoryLeaksOnShutdown := True;
end;

procedure TForm1.btnSolveClick(Sender: TObject);
begin
  AOCUtils.DoAdventOfCode(TAdventOfCodeRef(Cbb1.Items.Objects[cbb1.ItemIndex]));
end;

procedure TForm1.btnTestClick(Sender: TObject);
begin
//  AOCTests.RunTests;
end;

end.
