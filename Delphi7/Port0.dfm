object FPort0: TFPort0
  Left = 658
  Top = 282
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = #1055#1086#1088#1090' 0'
  ClientHeight = 48
  ClientWidth = 294
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 120
  TextHeight = 16
  object Label1: TLabel
    Left = 148
    Top = 20
    Width = 41
    Height = 16
    Caption = 'Label1'
  end
  object ComboBox1: TComboBox
    Left = 10
    Top = 10
    Width = 119
    Height = 24
    ItemHeight = 16
    TabOrder = 0
    OnChange = ComboBox1Change
    OnDropDown = ComboBox1DropDown
  end
  object Timer1: TTimer
    Interval = 50
    OnTimer = Timer1Timer
    Left = 192
    Top = 8
  end
end
