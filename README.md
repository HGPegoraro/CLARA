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
Após aberto, o script apresentará as seguintes alternativas:

- `Selecionar Formato do Arquivo Excel`: Permite selecionar entre três métodos de leitura de placa, que deve ser escolhida pelo usuário de acordo com qual aparelho foi efetuado a obtenção dos dados da placa.

- `Resposta Humoral / BCA`: Seleção entre as análises de `Resposta Humoral` e `BCA`.

### Dentro da aba `Resposta Humoral`:

- `Importar Excel`: Selecione as placas para serem analizadas, após isso os dados serão espelhados em uma placa contendo a identificação dos poços e suas concentrações.

- `Placa de 96 poços`: Interface que permite usuario fazer seleção de poços.

- `Selecionar Placa`: Seleção entre as placas importadas pelo usuário.

- `Modo de Réplica`: Seleção de quantas replicatas foram estabelecidas pelo usuário.

- `Configurações dos Dias`: Permite que o usuário defina o número de dias usados no experimento.

- `Selecione o Dia para Atribuição`: Seleciona o dia correspondente a cada grupo selecionado na placa.

> [!Tip]
> Atribua os dias como `Dia X` para que apareça corretamente no grafico 

- `Atualizar Lista de Dias`: Atualiza os dias que podem ser selecionados caso o usuario altere esses valores na atribuição anterior.

- `Nome do Teste`: Permite com que o usuário faça a seleção de mais de uma placa que possa conter grupos iguais, mas não pertencem ao mesmo teste (Ex.: IgG1 e IgG2a).

- `Grupo`: Define o nome do grupo selecionado na placa, que é adicionado a placa após selecionar `Adicionar Grupo`.

> [!NOTE]
> Os `Grupos` e os `Dias` também apareceram listados em cada poço selecionado e logo abaixo da tabela para facilitar identificação.

- `Marcar como Branco`: Define os poços selecionados como Branco.

- `Remover do Grupo Selecionado`: Retira o grupo selecionado na placa da fórmula que realiza a montagem do gráfico.

- `Salvar Projeto`: Salvar atribuições efetuadas até aquele momento, permitindo resumir o trabalho com `Carregar Projeto Salvo`.

- `Gráfico Dinâmico`: Gráfico que atualiza em tempo real a seleção de `Filtrar por Tipo de Teste`, `Selecionar Grupos para Gráfico` e `Arraste os grupos para definir a ordem no gráfico`, ao lado há a alternativa `Escala`, que permite o usuário escolher o valor máximo do eixo Y apresentado no gráfico.

- `Gráfico Final`: Visualização do gráfico com os parâmetros mencionados anteriormente para download em formato `PNG`,`PDF` ou `TIFF`.

- `Selecionar Gráfico para Exibição`: Mostra qual gráfico aparecerá no `Gráfico Final`.

- `Remover Gráfico Selecionado`: Permite remoção do gráfico apresentado no `Selecionar Gráfico para Exibição`.

- `Filtrar por tipo de Teste`: Permite seleção de qual teste será usado na montagem do gráfico.

- `Nome do Gráfico`: Define o nome que aparecerá no `Gráfico Final` após o usuário selecionar `Adicionar Gráfico`.

- `Selecionar Grupos para Gráfico`: Onde o usuário define quais grupos serão apresentados no gráfico.

- `Arraste os grupos para definir a ordem no gráfico`: Permite que o usuário defina a ordem na qual os dados serão apresentados no gráfico.

> [!NOTE]
> O `Diagnóstico de Normalidade` é uma função opcional que mostra se os grupos definidos em qualquer dia específico estão dentro da normalidade.

- `Largura x Altura`: Permite definir um tamanho específico, em pixels, do gráfico que será baixado, seus valores padrões são `1000` x `500`.

### Dentro da aba `BCA`:

- `Importar Excel`: Selecione as placas para serem analizadas, após isso os dados serão espelhados em uma placa contendo a identificação dos poços e suas concentrações.

- `Placa de 96 poços`: Interface que permite usuario fazer seleção de poços.

- `Selecionar Placa`: Seleção entre as placas importadas pelo usuário.

- `Definição da Curva Padrão`: Menu drop-down que permite que o usuário selecione quais poços serão considerados parte da curva. Possui as funções de Definir o valor de um Ponto na Curva, `Atribuir Poços`, `Apagar Poços`, `Adicionar Ponto`, `Restaurar Ignorados`.

>[!NOTE]
> O usuário pode selecionar pontos na placa para que eles sejam ignorados na `Curva`.

- `Nome da Amostra`: Onde o usuario define o nome da Amostra selecionada na placa, que é adicionado a placa após selecionar `Atribuir Amostra aos Selecionados`.

- `Remover Atribuição`: 
