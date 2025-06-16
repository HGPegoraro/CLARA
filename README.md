# CLARA (Centralized Logic for Assay Reading and Analysis)
CLARA é um programa com objetivo de auxiliar na análise e visualização de gráficos de ELISA de Resposta Humoral e Curvas de BCA. O Script conta com diversas funcionalidades, separado por seções para facilitar a compreensão do usuário. Os arquivos denominados com prefixo `WIKI_` são correspondentes as funcionalidades de cada aba.
# Lista de Items Gerais

- `Selecionar Formato do Arquivo Excel`: Permite selecionar entre três métodos de leitura de placa, que deve ser escolhida pelo usuário de acordo com qual aparelho foi efetuado a obtenção dos dados da placa.

- `Resposta Humoral / BCA`: Seleção entre as análises de `Resposta Humoral` e `BCA`.
#
### Usando o Script
Após instalar o arquivo `.zip`, abra o arquivo `app.R` em RStudio, no console insira:
```
install.packages("shiny", "shinyjs", "readxl", "DT", "ggplot2", "dplyr", "jsonlite", "ggtext", "shinyWidgets",
                 "digest", "tibble", "tidyr", "ggpattern", "emmeans", "multcomp", "multcompView", "jsonlite",
                 "sortable", "ggtext", "commonmark", "shinyWidgets", "digest", "fBasics") 
```
para posibilitar o funcionamento correto do script.
