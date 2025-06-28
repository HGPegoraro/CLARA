Write-Host "======================================================"
Write-Host " Procurando a instalação mais recente do R..." -ForegroundColor Yellow
$rPath = Get-ChildItem -Path "C:\Program Files\R" -Filter "R-*" -Directory |
         Sort-Object Name -Descending |
         Select-Object -First 1

if ($rPath) {
    $rBinPath = Join-Path -Path $rPath.FullName -ChildPath "bin"
    Write-Host "Encontrado R em: $rBinPath"

    # Verifica se o caminho já está no PATH da sessão atual
    if ($env:Path -like "*$rBinPath*") {
        Write-Host "O R já está no PATH desta sessão." -ForegroundColor Green
    } else {
        # Adiciona o caminho do R ao PATH APENAS PARA ESTA SESSÃO
        $env:Path += ";$rBinPath"
        Write-Host "R adicionado ao PATH *desta sessão*." -ForegroundColor Green
        Write-Host "O PATH permanente do sistema não foi alterado."
    }

    Write-Host "======================================================"
    Write-Host "Verificando a versão do Rscript..."
    Rscript --version
    Write-Host ""

    Write-Host "Executando script de instalação de pacotes..."
    Rscript install_lib_CLARA.R
    
    Write-Host "Configuração do R finalizada com sucesso!" -ForegroundColor Cyan

} else {
    Write-Host "Nenhuma instalação do R encontrada em C:\Program Files\R" -ForegroundColor Red
}