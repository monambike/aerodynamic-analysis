:: /***************************************************************************
:: Copyright(c) 2026 The Authors. Built at UFV (Universidade Federal de Viçosa).
:: Project authors: Vinícius Gabriel, Luiz Lopes, Davi Atayde, Pedro Balduino.
:: For license information, please see the LICENSE file in the root directory.
:: ***************************************************************************/


@echo off

color 9
prompt [:)] 


set "TITLE_PREFIX=host: (aerodynamic-analysis)"

set "DASHBOARD_PORT=3838"
set "SLIDESHOW_PORT=4200"

set "SOURCE_FILE_PATH=..\src"

set "LOCALHOST=127.0.0.1"


:: Show title message
call :message_title

:: Start dashboard hosting
call :message_server_start dashboard %DASHBOARD_PORT%
start "%TITLE_PREFIX% dashboard - " /min cmd /k "quarto.exe preview %SOURCE_FILE_PATH%\dashboard.qmd --port %DASHBOARD_PORT% --no-browser"
call :wait_for_port %DASHBOARD_PORT%
call :message_server_host_success dashboard %DASHBOARD_PORT%

:: Print empty line terminal
echo(

:: Start slideshow hosting after dashboard is online
call :message_server_start slideshow %SLIDESHOW_PORT%
start "%TITLE_PREFIX% slideshow - " /min cmd /k "quarto.exe preview %SOURCE_FILE_PATH%\slideshow.qmd --port %SLIDESHOW_PORT%"
call :wait_for_port %SLIDESHOW_PORT%
call :message_server_host_success slideshow %SLIDESHOW_PORT%


:: ------------------------------------------------------------
:: Show message about process start in application host.
::
:: Arguments:
::   %1 - Application name.
::   %2 - Port number.
:: ------------------------------------------------------------
:message_server_start %1 %2
set "MESSAGE=[%DATE% %TIME%] starting server for "%1" at localhost (%LOCALHOST%) port: %2..."
echo %MESSAGE%
goto :eof

:: ------------------------------------------------------------
:: Show message about success in application host status.
::
:: Arguments:
::   %1 - Application name.
::   %2 - Port number.
:: ------------------------------------------------------------
:message_server_host_success %1 %2
set "MESSAGE=[%DATE% %TIME%] server successfully started on localhost (%LOCALHOST%:%2)"
echo %MESSAGE%
goto :eof

:: ------------------------------------------------------------
:: Show title message about application.
:: ------------------------------------------------------------
:message_title
echo Aerodynamic Analysis (Slideshow and Dashboard)
echo Copyright(c) 2026 The Authors. Built at UFV (Universidade Federal de Vicosa).
echo For license information, please see the LICENSE file in the root directory.
echo - - - - - - - - - - - - - - - 
echo(
goto :eof

:: ------------------------------------------------------------
:: Wait until a TCP port becomes available.
::
:: Arguments:
::   %1 - Port Number
::
:: Returns:
::   0 when the port is listening
:: ------------------------------------------------------------
:wait_for_port %1
:wait_for_port_loop
netstat -an | find ":%1" | find "LISTENING" >nul

if errorlevel 1 (
  timeout /t 1 >nul
  goto :wait_for_port_loop
)
goto :eof
