' RunTDS.vbs — Shinyアプリを非表示で起動（スプラッシュ機能は凍結中）
Option Explicit
Dim sh, base, cmd
Set sh = CreateObject("WScript.Shell")

' アプリフォルダ
base = "C:\Apps\TDS_GUi"
sh.CurrentDirectory = base

' ----------------------------------------------
' ★ スプラッシュ機能（現在は凍結中）★
'Dim iconPath, splash
'iconPath = base & "\default.png"
'splash = "powershell -NoProfile -ExecutionPolicy Bypass -File """ & base & "\Splash.ps1""" _
'         & " -ImagePath """ & iconPath & """"
'sh.Run splash, 0, False
' ----------------------------------------------

' VBS経由で pause を無効化（バッチ残留を防止）
sh.Environment("PROCESS")("NO_PAUSE") = "1"

' Rscript探索つきのバッチを非表示で実行
cmd = """" & base & "\run_app.bat"""
sh.Run cmd, 0, False   ' 0=非表示, False=非同期
