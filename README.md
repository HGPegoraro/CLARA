# CLARA (Centralized Logic for Assay Reading and Analysis)
CLARA é um programa com objetivo de auxiliar na análise e visualização de gráficos de ELISA de Resposta Humoral e Curvas de BCA. O Script conta com diversas funcionalidades, separado por seções para facilitar a compreensão do usuário. A `Wiki` possui explicações para cada aba dentro do programa.
#
### Usando o Script

1. Abra o `Windows Powershell` como `Administrador` para instalar os aplicativos de `.R`, `RStudio` e `Rtools`.
```
winget install --id=RProject.R --version 4.5.1 -e; winget install --id=RStudio --version 2025.05.1+513  -e; winget install --id=RProject.Rtools --version 4.5.6608 -e; 
```

>[!NOTE]
>Se já estiver com os aplicativos de `.R` instalados pode pular essa etapa

2. Cria um diretório, instala o `.zip` e abre a pasta contendo o aplicativo.
```
$DesktopPATH = [System.Environment]::GetFolderPath('Desktop'); $ProjectPath = Join-Path -Path $DesktopPATH -ChildPath "Analise_R"; mkdir $ProjectPath -ErrorAction SilentlyContinue; $ZipFilePath = Join-Path -Path $ProjectPath -ChildPath "CLARA.zip"; Invoke-WebRequest -Uri https://raw.githubusercontent.com/HGPegoraro/CLARA/main/CLARA.zip -OutFile $ZipFilePath; Expand-Archive -Path $ZipFilePath -DestinationPath $ProjectPath -Force; cd (Join-Path -Path $ProjectPath -ChildPath "CLARA")
```

3. Após extrair o arquivo `.zip`, instale as bibliotecas necessárias.
```
Rscript install_lib_CLARA.R
```

4. Inicialize o programa.
```
Rscript app.R
```

>[!NOTE]
>Caso seja a primeira vez abrindo o `RStudio` será necessário algumas configurações adicionais dentro do aplicativo

### Lista de Items Gerais

- `Selecionar Formato do Arquivo Excel`: Permite selecionar entre três métodos de leitura de placa, que deve ser escolhida pelo usuário de acordo com qual aparelho foi efetuado a obtenção dos dados da placa.

- `Resposta Humoral / BCA`: Seleção entre as análises de `Resposta Humoral` e `BCA`.

Rscript -e "shiny::runApp('./Analise_R/CLARA/app.R', launch.browser = TRUE)"
