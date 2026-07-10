$Rscript = "C:\Program Files\R\R-4.5.1\bin\Rscript.exe"
Set-Location $PSScriptRoot
& $Rscript -e "shiny::runApp('.')"

