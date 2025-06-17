# CLARA (Centralized Logic for Assay Reading and Analysis)
CLARA é um programa com objetivo de auxiliar na análise e visualização de gráficos de ELISA de Resposta Humoral e Curvas de BCA. O Script conta com diversas funcionalidades, separado por seções para facilitar a compreensão do usuário. A `Wiki` possui explicações para cada aba dentro do programa.
#
### Usando o Script

Caso ainda não tenha o `RStudio` prossiga com passo 1, caso ja tenha instalado siga para o passo 3

1. Abra o WindowsPowershell para instalar os aplicativos de .R e RStudio.
```
winget install --id=RProject.R --version 4.5.1 -e && winget install --id=RStudio --version 2025.05.1+513  -e
```

2. Ainda no PowerShell faça download do arquivo `.zip`
```
git 
```

3. Após instalar o arquivo `.zip`, abra o arquivo `app.R` em RStudio e coloque no console:
```
install.packages("shiny", "shinyjs", "readxl", "DT", "ggplot2", "dplyr", "jsonlite", "ggtext", "shinyWidgets",
                 "digest", "tibble", "tidyr", "ggpattern", "emmeans", "multcomp", "multcompView", "jsonlite",
                 "sortable", "ggtext", "commonmark", "shinyWidgets", "digest", "fBasics") 
```
para posibilitar o funcionamento correto do script. Então para selecionar o aplicativo selecione a opção `Run App`, ou pressione `Ctrl + Shift + Enter`. Prossiga com as abas de `WIKI` para entender a funcionalidade das abas.
### Lista de Items Gerais

- `Selecionar Formato do Arquivo Excel`: Permite selecionar entre três métodos de leitura de placa, que deve ser escolhida pelo usuário de acordo com qual aparelho foi efetuado a obtenção dos dados da placa.

- `Resposta Humoral / BCA`: Seleção entre as análises de `Resposta Humoral` e `BCA`.
