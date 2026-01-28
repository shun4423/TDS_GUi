@echo off
setlocal EnableExtensions EnableDelayedExpansion

REM --- 固定の英数字のみ出力（文字化け回避） ---
REM chcp 65001 >nul  & rem ← UTF-8が必要なら有効化

REM --- move to script folder ---
PUSHD "%~dp0"

REM --- pick target R script (launch_app.R -> lauch_app.R -> app.R) ---
set "SCRIPT="
if exist "%~dp0launch_app.R" set "SCRIPT=%~dp0launch_app.R"
if not defined SCRIPT if exist "%~dp0lauch_app.R" set "SCRIPT=%~dp0lauch_app.R"
if not defined SCRIPT if exist "%~dp0app.R"       set "SCRIPT=%~dp0app.R"

echo [INFO] cwd: %CD%
echo [INFO] script chosen: %SCRIPT%
if not defined SCRIPT (
  echo [ERROR] No R script found: launch_app.R / lauch_app.R / app.R
  goto :END
)

REM --- locate Rscript.exe ---
set "RS="
where Rscript.exe >nul 2>&1 && for /f "delims=" %%P in ('where Rscript.exe') do set "RS=%%~fP"
if not defined RS (
  for %%B in ("C:\Program Files\R" "C:\Program Files (x86)\R") do (
    if exist %%~B for /f "delims=" %%D in ('dir /b /ad "%%~B" ^| sort /r') do (
      if exist "%%~B\%%~D\bin\Rscript.exe" set "RS=%%~B\%%~D\bin\Rscript.exe" & goto :FOUND
    )
  )
)
:FOUND
if not defined RS (
  echo [ERROR] Rscript.exe not found. Please install R.
  goto :END
)

echo [INFO] using: %RS%
"%RS%" --version || (echo [ERROR] Rscript not runnable. & goto :END)

REM --- run (absolute path; no WD dependency) ---
echo [INFO] running: "%RS%" --vanilla "%SCRIPT%"
"%RS%" --vanilla "%SCRIPT%"
echo [INFO] exit code: %ERRORLEVEL%

:END
POPD

REM ▼ NO_PAUSE=1 のときは待たない（VBS 経由）
if /i "%NO_PAUSE%"=="1" goto :EOF

REM それ以外（手動ダブルクリック等）のときだけ待つ
pause
endlocal
