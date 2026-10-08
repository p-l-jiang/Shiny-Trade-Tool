@echo off
rem ============================================================
rem  Canadian Trade Explorer - Windows launcher
rem  Double-click this file to start the app in your web browser.
rem ============================================================
setlocal EnableExtensions EnableDelayedExpansion
title Canadian Trade Explorer
cd /d "%~dp0"

set "RSCRIPT="

rem 1. Rscript on the PATH
for /f "delims=" %%i in ('where Rscript 2^>nul') do (
  if not defined RSCRIPT set "RSCRIPT=%%i"
)

rem 2. Install location recorded in the registry (per-user, then machine-wide)
if not defined RSCRIPT (
  for %%k in ("HKCU\Software\R-core\R" "HKLM\Software\R-core\R" "HKLM\Software\WOW6432Node\R-core\R") do (
    if not defined RSCRIPT (
      for /f "tokens=2,*" %%a in ('reg query %%k /v InstallPath 2^>nul ^| find "InstallPath"') do (
        if exist "%%b\bin\Rscript.exe" set "RSCRIPT=%%b\bin\Rscript.exe"
      )
    )
  )
)

rem 3. Usual install folders (newest version first)
if not defined RSCRIPT (
  for %%d in ("%LOCALAPPDATA%\Programs\R" "%ProgramFiles%\R" "%USERPROFILE%\Documents\R" "%USERPROFILE%\R") do (
    if not defined RSCRIPT if exist "%%~d" (
      for /f "delims=" %%v in ('dir /b /ad /o-n "%%~d\R-*" 2^>nul') do (
        if not defined RSCRIPT if exist "%%~d\%%v\bin\Rscript.exe" set "RSCRIPT=%%~d\%%v\bin\Rscript.exe"
      )
    )
  )
)

if not defined RSCRIPT (
  echo.
  echo  R is not installed on this computer ^(or could not be found^).
  echo.
  echo  1. Your browser will now open the R download page.
  echo  2. Click "Download R for Windows", run the installer and accept the defaults.
  echo     ^(If you are asked for an administrator password, choose to install
  echo      "just for me" instead.^)
  echo  3. Then double-click "Start Trade Explorer" again.
  echo.
  start "" "https://cran.r-project.org/bin/windows/base/"
  pause
  exit /b 1
)

echo Using R at: !RSCRIPT!
"!RSCRIPT!" "%~dp0launch.R"

if errorlevel 1 (
  echo.
  echo  Something went wrong - see the message above.
  echo  If you need help, take a screenshot of this window.
  echo.
  pause
)
endlocal
