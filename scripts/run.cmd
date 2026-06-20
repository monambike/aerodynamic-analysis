@echo off

start "dashboard" cmd /k "quarto.exe preview ..\src\dashboard.qmd --port 3838 --no-browser"

start "slideshow" cmd /k "quarto.exe preview ..\src\slideshow.qmd --port 4200"

exit
