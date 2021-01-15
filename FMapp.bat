@echo off

:: User should only modify r_path1 and replace it with the actual path for Rscript.exe
:: /!\ DO NOT INCLUDE "\Rscript.exe" in r_path1 /!\

:: Rscript partial paths
echo Setting Rscript.exe's path...
echo.
set "r_path1=C:\InstalledProg\R\R-4.0.3\bin"
set "r_path2=\Rscript.exe"

:: R application partial paths
echo Setting application's path...
echo.
set "app_path1=%cd%"
set "app_path2=\app.R"

:: Run application using Rscript.exe full path from command prompt
echo Running application...
echo.
%r_path1%%r_path2% %app_path1%%app_path2%
