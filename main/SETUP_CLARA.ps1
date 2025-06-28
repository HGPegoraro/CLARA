Write-Host "======================================================"
Write-Host "Searching for the latest R installation..." -ForegroundColor Yellow
$rPath = Get-ChildItem -Path "C:\Program Files\R" -Filter "R-*" -Directory |
         Sort-Object Name -Descending |
         Select-Object -First 1

if ($rPath) {
    $rBinPath = Join-Path -Path $rPath.FullName -ChildPath "bin"
    Write-Host "Found R at: $rBinPath"

    Write-Host "Checking the permanent user PATH..."

    $userPath = [System.Environment]::GetEnvironmentVariable("Path", "User")

   if ($userPath -like "*$rBinPath*") {
        Write-Host "R is already in the permanent user PATH." -ForegroundColor Green
    } else {
        Write-Host "Adding R to the permanent user PATH..." -ForegroundColor Yellow
        
        $newPath = ($userPath, $rBinPath) -join ';'
        
        [System.Environment]::SetEnvironmentVariable("Path", $newPath, "User")
        
        Write-Host "R has been added to the permanent user PATH." -ForegroundColor Green
        Write-Host "You may need to restart PowerShell/VSCode/Windows Terminal for the change to take effect." -ForegroundColor Cyan
    }

    if (-not ($env:Path -like "*$rBinPath*")) {
        $env:Path += ";$rBinPath"
    }
    
    Write-Host "======================================================"
    Write-Host "Verifying Rscript version from the updated session PATH..."
    try {
        Rscript --version
    } catch {
        Write-Host "Could not execute Rscript. Please restart your terminal and try again." -ForegroundColor Red
        # You might want to exit here if Rscript is critical for the next step
        # exit 1 
    }
    Write-Host ""

    Write-Host "Executing package installation script..."
    Rscript install_lib_CLARA.R
    
    Write-Host "R setup finished successfully!" -ForegroundColor Cyan

} else {
    Write-Host "No R installation found in C:\Program Files\R" -ForegroundColor Red
}