# CLARA (Centralized Logic for Assay Reading and Analysis)
CLARA é um programa com objetivo de auxiliar na análise e visualização de gráficos de ELISA de Resposta Humoral e Curvas de BCA. O Script conta com diversas funcionalidades, separado por seções para facilitar a compreensão do usuário. A `Wiki` possui explicações para cada aba dentro do programa.
#
### Usando o Script

Crie um diretório onde sera instalado os aplicativos e efetue as próximas etapas dentro dele.
```
$DesktopPATH = [System.Environment]::GetFolderPath([System.Environment+SpecialFolder]::Desktop); $RPATH= Join-Path -Path $DesktopPATH -ChildPath Analise_R; mkdir $RPATH; cd ..\$DesktopPATH\Analise_R
```

1. Abra o WindowsPowershell para instalar os aplicativos de `.R`, `RStudio` e `Rtools`.
```
winget install --id=RProject.R --version 4.5.1 -e; winget install --id=RStudio --version 2025.05.1+513  -e; winget install --id=RProject.Rtools --version 4.5.6608 -e
```

>[!NOTE]
>Se já estiver com os aplicativos de `.R` instalados pode pular essa etapa

2. Ainda no PowerShell faça download do arquivo `.zip`.
```
Invoke-WebRequest -Uri https://raw.githubusercontent.com/HGPegoraro/CLARA/main/CLARA.zip -OutFile ..\Analise_R\CLARA.zip; Expand-Archive -Path ..\Analise_R\CLARA.zip -DestinationPath ..\Analise_R
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
