@rem BATCH FILE
echo "Updating application. Do not close this window.
@echo off
ping -n 3 127.0.0.1 >nul 

@rem * Copy from source to destination including subdirs and hidden
@rem * File
xcopy installer . /S /E /H /Y
rd /s /q installer
start /d "." NoiseIde.exe