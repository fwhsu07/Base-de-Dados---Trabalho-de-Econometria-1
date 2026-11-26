# Trabalho de Econometria I — Agências bancárias, serviços e produtividade

Este repositório reúne a base de dados e o código R usados no trabalho de Econometria I. O objetivo é estimar, em corte transversal de países (ano de 2019), a associação entre a **produtividade do trabalho** (PIB por pessoa empregada) e dois fatores de interesse: a **densidade de agências bancárias** e a **participação do emprego em serviços**, controlando por infraestrutura, capital humano e características demográficas.

## Arquivos

| Arquivo | Descrição |
|---|---|
| `Base de Dados - Econo1.xlsx` | Base de dados extraída dos *World Development Indicators* (Banco Mundial), ano de 2019 |
| `Trabalho_Econometria_1.R` | Script R com limpeza dos dados, estimação dos modelos, testes e gráficos |

## Base de dados

A planilha tem duas abas:

- **Data**: 266 linhas (países e agregados regionais/de renda) para o ano de 2019. Valores ausentes aparecem como `..`.
- **Series - Metadata**: definições, fontes e metodologia de cada indicador.

Variáveis utilizadas (nome no script → indicador do Banco Mundial):

| Variável no R | Indicador | Código |
|---|---|---|
| `gdp_pc_emp` | PIB por pessoa empregada (PPC, US$ constantes de 2021) — **variável dependente** | SL.GDP.PCAP.EM.KD |
| `bank_branches` | Agências de bancos comerciais (por 100 mil adultos) | FB.CBK.BRCH.P5 |
| `emp_services` | Emprego em serviços (% do emprego total) | SL.SRV.EMPL.ZS |
| `elec_access` | Acesso à eletricidade (% da população) | EG.ELC.ACCS.ZS |
| `internet_use` | Usuários de internet (% da população) | IT.NET.USER.ZS |
| `school_ter` | Matrícula no ensino superior (% bruta) | SE.TER.ENRR |
| `pop_total` | População total | SP.POP.TOTL |
| `labor_force` | Força de trabalho total | SL.TLF.TOTL.IN |
| `pop_urban` | População urbana (% do total) | SP.URB.TOTL.IN.ZS |

## O que o script faz

1. **Importação e limpeza** — lê a planilha (`file.choose()`), renomeia as colunas, remove observações com valores ausentes e exclui os agregados regionais/de renda (ex.: `WLD`, `HIC`, `LAC`), mantendo apenas países.
2. **Transformações** — população e força de trabalho em milhões, `log_pop`, taxa de participação (`lfp_rate`), `log_gdp` e emprego em serviços centrado na média (com termo quadrático) para reduzir colinearidade.
3. **Modelos MQO** — do modelo simples ao modelo com controles; especificação final em nível (`modelo_transf`) e em log (`modelo_log_y`).
4. **Inferência robusta** — erros-padrão HC1 e HC3, testes de Wald conjuntos, teste de Breusch-Pagan e VIF.
5. **Robustez e heterogeneidade** — comparação com/sem agregados, interação bancos × internet, regressões por quartil de internet, análise de influência (Cook, leverage, resíduos studentizados) e reestimação sem observações influentes e sem Luxemburgo.
6. **Descritivas e gráficos** — estatísticas, correlações de Pearson e Spearman e gráficos de diagnóstico (`ggplot2`).
7. **Magnitudes econômicas** — efeitos de um desvio-padrão, intervalos de confiança e efeito parcial dos serviços pelo método delta.

> Os coeficientes devem ser lidos como **associações condicionais**, não como efeitos causais: erros robustos corrigem a inferência sob heterocedasticidade, mas não variáveis omitidas, causalidade reversa, erro de medida ou seleção amostral.

## Como rodar

1. Instale os pacotes necessários no R:

   ```r
   install.packages(c("AER", "tidyverse", "readxl", "lmtest", "sandwich", "car"))
   ```

2. Abra e execute `Trabalho_Econometria_1.R`.
3. Quando a janela de seleção abrir, escolha o arquivo `Base de Dados - Econo1.xlsx`.

## Fonte

Banco Mundial, *World Development Indicators* (licença CC BY 4.0).
