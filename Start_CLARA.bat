@echo off
title CLARA
cd /d "%~dp0"

docker info >nul 2>&1
if errorlevel 1 (
  echo Docker Desktop is not running. Please open Docker Desktop, wait until it says "running", then double-click this file again.
  pause
  exit /b 1
)

echo Getting CLARA ready (the first time can take a few minutes)...
docker compose pull >nul 2>&1 || docker compose build

echo.
echo CLARA will open in your browser at http://localhost:3838
echo Close this window (or press Ctrl+C) to stop CLARA.
start "" /min cmd /c "timeout /t 6 >nul & start http://localhost:3838"
docker compose up
pause
