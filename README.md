![CLARA_LOGO_V2](https://github.com/user-attachments/assets/03f2881b-700e-430e-9701-a61838e97c06)
#
CLARA é um programa com objetivo de auxiliar na análise e visualização de gráficos de ELISA de Resposta Humoral e Curvas de BCA. O Script conta com diversas funcionalidades, separado por seções para facilitar a compreensão do usuário. A `Wiki` possui explicações para cada aba dentro do programa.
#
### Usando o Script

1. Abra o `Windows Powershell` como `Administrador` para instalar os aplicativos de `.R`, `RStudio` e `Rtools`.
```
winget install --id=RProject.R --version 4.5.1 -e; winget install --id=RStudio --version 2025.05.1+513  -e; winget install --id=RProject.Rtools --version 4.5.6608 -e; 

```

>[!NOTE]
>Se já estiver com os aplicativos de `.R` instalados pode pular essa etapa

2. Cria um diretório, instala o `.zip`, abre a pasta contendo o aplicativo e instala as bibliotecas necessárias.
```
$DesktopPATH = [System.Environment]::GetFolderPath('Desktop'); $ProjectPath = Join-Path -Path $DesktopPATH -ChildPath "Analise_R"; mkdir $ProjectPath -ErrorAction SilentlyContinue; $ZipFilePath = Join-Path -Path $ProjectPath -ChildPath "CLARA.zip"; Invoke-WebRequest -Uri https://raw.githubusercontent.com/HGPegoraro/CLARA/main/CLARA.zip -OutFile $ZipFilePath; Expand-Archive -Path $ZipFilePath -DestinationPath $ProjectPath -Force; cd (Join-Path -Path $ProjectPath -ChildPath "CLARA"); .\PATHfind.bat; Rscript install_lib_CLARA.R
```

3. Inicialize o programa.
```
Rscript -e "shiny::runApp('app.R', launch.browser = TRUE)"
```
#
Para abrir o programa novamente após reiniciar o `Windows Powershell`
```
$DesktopPATH = [System.Environment]::GetFolderPath('Desktop'); $ProjectPath = Join-Path -Path $DesktopPATH -ChildPath "Analise_R"; cd (Join-Path -Path $ProjectPath -ChildPath "CLARA"); Rscript -e "shiny::runApp('app.R', launch.browser = TRUE)"
```
