unit AOCBase;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, system.Diagnostics, ClipBrd, system.UITypes;

type TProcedureToRun = procedure of object;
type TFunctionToRun = function: Variant of object;

type TAdventOfCode = class(TPersistent)
  constructor Create;
  destructor Destroy; override;
  protected
    FInput: TStrings;
    function SolveA: Variant; virtual;
    function SolveB: Variant; virtual;
    procedure BeforeSolve; virtual;
    procedure AfterSolve; virtual;
    function SaveFilePath: String;
  private
    function InputFilePath: string;
    function MakeFilePath(const aFileName: String): string;
    function DayIndex: String;
    procedure DoProcedure(ProcedureToRun: TProcedureToRun; const aDisplayName: String);
    function DoFunction(FuntionToRun: TFunctionToRun; const aDisplayName: string; Out TicksTaken: Int64): String;
    procedure LoadInput;
    procedure WriteTicksToDebug(Const aFunctionName: string; Const aStartTick: Int64);
    procedure InternalSolve(Out SolutionA, SolutionB: string; out TimeA, TimeB: Int64);
  public
  { Public declarations }
    procedure Solve;
    procedure Test(Out SolutionA, SolutionB: String; Const OverRidenTestInput: String);
  end;

implementation

uses
  uAOCUtils;

function TAdventOfCode.DayIndex: String;
begin
  Result := AOCUtils.DayIndexFromClassName(Self.ClassName);
end;

constructor TAdventOfCode.Create;
begin
  Assert(Self.ClassName.StartsWith('TAdventOfCodeDay'), 'Classname should begin with TAdventOfCodeDay, followd by the dayindex');

  FInput := TStringList.Create;
  DoProcedure(LoadInput, 'LoadInput');
end;

destructor TAdventOfCode.Destroy;
begin
  FInput.Free;
  inherited;
end;

function TAdventOfCode.SaveFilePath: String;
begin
  Result := MakeFilePath('Solution');
end;

function TAdventOfCode.InputFilePath: string;
begin
  Result := MakeFilePath('Input')
end;

function TAdventOfCode.MakeFilePath(const aFileName: String): string;
begin
  result := Format('%s\%s%s.txt', [AOCUtils.Config.BaseFilePath, aFileName, DayIndex])
end;

function TAdventOfCode.SolveA: Variant;
begin
  Result := 'Not implemented'
end;

function TAdventOfCode.SolveB: Variant;
begin
  Result := 'Not implemented'
end;

procedure TAdventOfCode.BeforeSolve;
begin
  // To be overriden
end;

procedure TAdventOfCode.AfterSolve;
begin
  // To be overriden
end;

procedure TAdventOfCode.WriteTicksToDebug(Const aFunctionName: string; Const aStartTick: Int64);
begin
  Writeln(Format('%s -> TickCount: %d', [aFunctionName, GetTickCount-aStartTick] ));
end;

procedure TAdventOfCode.DoProcedure(ProcedureToRun: TProcedureToRun; const aDisplayName: String);
var StartTick: Int64;
begin
  StartTick := GetTickCount;
  ProcedureToRun;
  WriteTicksToDebug(aDisplayName, StartTick);
end;

function TAdventOfCode.DoFunction(FuntionToRun: TFunctionToRun; const aDisplayName: string; Out TicksTaken: Int64): String;
var StartTick: Int64;
begin
  StartTick := GetTickCount;
  Result := VarToStr(FuntionToRun);
  WriteTicksToDebug(aDisplayName, StartTick);
  TicksTaken := GetTickCount - StartTick;
end;

procedure TAdventOfCode.LoadInput;
var FilePath: string;

  procedure _DownLoadInput;
  begin
    AOCUtils.DownLoadPuzzleInput(FInput, DayIndex);
    FInput.SaveToFile(FilePath);
  end;

begin
  FilePath := InputFilePath;
  if FileExists(FilePath) then
  begin
    FInput.LoadFromFile(FilePath);
    if (FInput.Count > 0) And (FInput[0].StartsWith('Please don')) then
      _DownLoadInput //File exists, but downloaded to early, let's try again
  end
  else
    _DownLoadInput;
end;

procedure TAdventOfCode.Solve;
var TimeA, TimeB, StartTime, TotalTime: Int64;
    SolutionA, SolutionB: String;
begin
  StartTime := GetTickCount;
  InternalSolve(SolutionA, SolutionB, TimeA, TimeB);

  TotalTime := GetTickCount - StartTime;

  if (MessageDlg(Format('Solution A: %s Solved in %d  ms.' +#10#13 +
                 'Copy to clipboard?', [SolutionA, TimeA]), mtInformation, [mbYes, mbNo], 0) <> Ord(mbNo)) then
    Clipboard.AsText := SolutionA;

  if (MessageDlg(Format('Solution B: %s Solved in %d ms.' + #10#13 +
                 'Total execution time: %d ms.' + #10#13 +
                 'Copy to clipboard?', [SolutionB, TimeB, TotalTime]), mtInformation, [mbYes, mbNo], 0) <> Ord(mbNo)) then
    Clipboard.AsText := SolutionB;
end;

procedure TAdventOfCode.InternalSolve(Out SolutionA, SolutionB: String; out TimeA, TimeB: Int64);
begin
  DoProcedure(BeforeSolve, 'BeforeSolve');
  SolutionA := DoFunction(SolveA, 'SolveA', TimeA);
  SolutionB := DoFunction(SolveB, 'SolveB', TimeB);
  DoProcedure(AfterSolve, 'AfterSolve');
end;

procedure TAdventOfCode.Test(Out SolutionA, SolutionB: String; Const OverRidenTestInput: String);
var Dummy: Int64;
begin
  if OverRidenTestInput <> '' then
  begin
    FInput.Clear;
    FInput.Add(OverRidenTestInput);
  end;

  InternalSolve(SolutionA, SolutionB, Dummy, Dummy);
end;

end.
