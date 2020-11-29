object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 400
  ClientWidth = 728
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object btnSolve: TButton
    Left = 8
    Top = 35
    Width = 145
    Height = 25
    Caption = 'Solve'
    TabOrder = 0
    OnClick = btnSolveClick
  end
  object cbb1: TComboBox
    Left = 8
    Top = 8
    Width = 145
    Height = 21
    TabOrder = 1
  end
  object btnTest: TButton
    Left = 8
    Top = 66
    Width = 145
    Height = 25
    Caption = 'Test all'
    TabOrder = 2
    OnClick = btnTestClick
  end
end
