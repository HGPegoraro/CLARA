# CLARA (Centralized Logic for Assay Reading and Analysis)
CLARA é um programa com objetivo de auxiliar na análise e visualização de gráficos de ELISA de Resposta Humoral e Curvas de BCA. O Script conta com diversas funcionalidades, separado por seções para facilitar a compreensão do usuário.
#
### Usando o Script
Após instalar o arquivo `.zip`, abra o arquivo `app.R` em RStudio, no console insira:
```
install.packages("shiny", "shinyjs", "readxl", "DT", "ggplot2", "dplyr", "jsonlite", "ggtext", "shinyWidgets",
                 "digest", "tibble", "tidyr", "ggpattern", "emmeans", "multcomp", "multcompView", "jsonlite",
                 "sortable", "ggtext", "commonmark", "shinyWidgets", "digest", "fBasics") 
```
para posibilitar o funcionamento correto do script.
#
### Dentro da aba `BCA`:

- `Importar Excel`: Selecione as placas para serem analizadas, após isso os dados serão espelhados em uma placa contendo a identificação dos poços e suas concentrações.

- `Placa de 96 poços`: Interface que permite usuario fazer seleção de poços.

- `Selecionar Placa`: Seleção entre as placas importadas pelo usuário.

- `Definição da Curva Padrão`: Menu drop-down que permite que o usuário selecione quais poços serão considerados parte da curva. Possui as funções de Definir o valor de um Ponto na Curva, `Atribuir Poços`, `Apagar Poços`, `Adicionar Ponto`, `Restaurar Ignorados`.

>[!NOTE]
> O usuário pode selecionar pontos na placa para que eles sejam ignorados na `Curva`.

- `Nome da Amostra`: Onde o usuario define o nome da Amostra selecionada na placa, que é adicionado a placa após selecionar `Atribuir Amostra aos Selecionados`.

- `Remover Atribuição`: 
