# LIMPEZA DO AMBIENTE E PACOTES
rm(list = ls())

library(AER)
library(tidyverse)
library(readxl)
library(lmtest)
library(sandwich)
library(car)

# 1. IMPORTAÇÃO
dados_brutos <- read_xlsx(file.choose(), na = c("", "NA", ".."))
tail(dados_brutos)

# 2. RENOMEAÇÃO, DIAGNÓSTICO E LIMPEZA
dados_renomeados <- dados_brutos %>%
  rename(
    gdp_pc_emp    = `GDP per person employed (constant 2021 PPP $) [SL.GDP.PCAP.EM.KD]`,
    bank_branches = `Commercial bank branches (per 100,000 adults) [FB.CBK.BRCH.P5]`,
    emp_services  = `Employment in services (% of total employment) (modeled ILO estimate) [SL.SRV.EMPL.ZS]`,
    elec_access   = `Access to electricity (% of population) [EG.ELC.ACCS.ZS]`,
    internet_use  = `Individuals using the Internet (% of population) [IT.NET.USER.ZS]`,
    school_ter    = `School enrollment, tertiary (% gross) [SE.TER.ENRR]`,
    pop_total     = `Population, total [SP.POP.TOTL]`,
    labor_force   = `Labor force, total [SL.TLF.TOTL.IN]`,
    pop_urban     = `Urban population (% of total population) [SP.URB.TOTL.IN.ZS]`
  )

vars <- c("gdp_pc_emp", "bank_branches", "emp_services", "elec_access",
          "internet_use", "school_ter", "pop_total", "labor_force", "pop_urban")

cat("Linhas no arquivo bruto:", nrow(dados_renomeados), "\n\n")
cat("Valores ausentes por variável:\n")
print(colSums(is.na(dados_renomeados[, vars])))

dados_sem_na <- dados_renomeados %>% drop_na(all_of(vars))
cat("\nApós drop_na:", nrow(dados_sem_na), "\n")

agregados <- c("AFE","AFW","ARB","CSS","CEB","EAR","EAS","EAP","TEA","EMU",
               "ECS","ECA","TEC","EUU","FCV","HPC","HIC","IBD","IBT","IDB",
               "IDX","IDA","LTE","LCN","LAC","TLA","LDC","LMY","LIC","LMC",
               "MEA","MNA","TMN","MIC","NAC","INX","OED","OSS","PSS","PST",
               "PRE","SST","SAS","TSA","SSF","SSA","TSS","UMC","WLD")

dados_limpos <- dados_sem_na %>%
  filter(!`Country Code` %in% agregados) %>%
  mutate(
    pop_total       = pop_total / 1e6,
    labor_force     = labor_force / 1e6,
    emp_services_sq = emp_services^2
  )

# 5.3 TRANSFORMAÇÕES
dados_limpos <- dados_limpos %>%
  mutate(
    log_pop  = log(pop_total),
    lfp_rate = (labor_force / pop_total) * 100,
    log_gdp  = log(gdp_pc_emp)
  )

media_serv <- mean(dados_limpos$emp_services, na.rm = TRUE)

dados_limpos <- dados_limpos %>%
  mutate(
    emp_services_c    = emp_services - media_serv,
    emp_services_c_sq = emp_services_c^2
  )

cat("Cor(serv, serv^2)       =",
    round(cor(dados_limpos$emp_services,   dados_limpos$emp_services_sq),   4), "\n")
cat("Cor(serv_c, serv_c^2)   =",
    round(cor(dados_limpos$emp_services_c, dados_limpos$emp_services_c_sq), 4), "\n")

# Amostra 2 (comparação)
dados_com_agregados <- dados_sem_na %>%
  mutate(
    pop_total       = pop_total / 1e6,
    labor_force     = labor_force / 1e6,
    emp_services_sq = emp_services^2
  )

cat("\nAmostra SÓ PAÍSES:      ", nrow(dados_limpos), "observações\n")
cat("Amostra COM AGREGADOS:  ", nrow(dados_com_agregados), "observações\n")
cat("Agregados removidos:    ", nrow(dados_com_agregados) - nrow(dados_limpos), "\n")

summary(dados_limpos$pop_total)
summary(dados_limpos$labor_force)

agregados_removidos <- dados_sem_na %>%
  filter(`Country Code` %in% agregados) %>%
  select(`Country Name`, `Country Code`) %>%
  arrange(`Country Code`)

print(agregados_removidos, n = 50)
cat("\nTotal de agregados removidos:", nrow(agregados_removidos), "\n")

dados_com_agregados <- dados_com_agregados %>%
  mutate(
    log_pop          = log(pop_total),
    lfp_rate         = (labor_force / pop_total) * 100,
    log_gdp          = log(gdp_pc_emp),
    emp_services_c   = emp_services - media_serv,
    emp_services_c_sq = emp_services_c^2
  )
# Tabela de dados ausentes (Corrigida)
vars_faltantes <- c("gdp_pc_emp", "bank_branches", "emp_services", "elec_access",
                    "internet_use", "school_ter", "pop_total", "labor_force", "pop_urban")

tabela_faltantes <- dados_renomeados %>%
  summarise(across(all_of(vars_faltantes), ~sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variavel", values_to = "faltantes") %>%
  mutate(
    n_original = nrow(dados_renomeados),
    percentual_perda = round((faltantes / n_original) * 100, 1)
  ) %>%
  arrange(desc(faltantes))

print(tabela_faltantes)
# 3. INSPEÇÃO
str(dados_limpos)
summary(dados_limpos)

# 4. MODELOS
modelo_simples <- lm(
  gdp_pc_emp ~ bank_branches + emp_services,
  data = dados_limpos
)
summary(modelo_simples)

modelo_de_verificaçãob <- lm(
  gdp_pc_emp ~ bank_branches + emp_services + I(bank_branches^2),
  data = dados_limpos
)
summary(modelo_de_verificaçãob)

modelo_de_verificaçãos <- lm(
  gdp_pc_emp ~ bank_branches + emp_services + emp_services_sq,
  data = dados_limpos
)
summary(modelo_de_verificaçãos)

modelo_controle <- lm(
  gdp_pc_emp ~ bank_branches + emp_services + emp_services_sq +
    elec_access + internet_use + school_ter +
    pop_total + labor_force + pop_urban,
  data = dados_limpos
)
summary(modelo_controle)

# Comparação correta usando a especificação final
formula_final <- gdp_pc_emp ~ bank_branches + emp_services_c + emp_services_c_sq +
  elec_access + internet_use + school_ter +
  log_pop + lfp_rate + pop_urban

modelo_paises_final <- lm(formula_final, data = dados_limpos)
modelo_todos_final  <- lm(formula_final, data = dados_com_agregados)

vars_interesse <- c("bank_branches", "emp_services_c", "emp_services_c_sq", "internet_use")

comparacao_final <- data.frame(
  variavel  = vars_interesse,
  paises    = coef(modelo_paises_final)[vars_interesse],
  todos     = coef(modelo_todos_final)[vars_interesse]
)
comparacao_final$diferenca <- comparacao_final$paises - comparacao_final$todos
print(comparacao_final)

# Modelo final (transformações)
modelo_transf <- lm(
  gdp_pc_emp ~ bank_branches + emp_services_c + emp_services_c_sq +
    elec_access + internet_use + school_ter +
    log_pop + lfp_rate + pop_urban,
  data = dados_limpos
)
coeftest(modelo_transf, vcov. = vcovHC(modelo_transf, type = "HC1"))
vif(modelo_transf)

# Modelo em log
modelo_log_y <- lm(
  log_gdp ~ bank_branches + emp_services_c + emp_services_c_sq +
    elec_access + internet_use + school_ter +
    log_pop + lfp_rate + pop_urban,
  data = dados_limpos
)
coeftest(modelo_log_y, vcov. = vcovHC(modelo_log_y, type = "HC1"))

# Verificação da centralização
modelo_transf_sem_centrar <- lm(
  gdp_pc_emp ~ bank_branches + emp_services + emp_services_sq +
    elec_access + internet_use + school_ter +
    log_pop + lfp_rate + pop_urban,
  data = dados_limpos
)
cat("vertice (transf sem centrar): ",
    -coef(modelo_transf_sem_centrar)["emp_services"] /
      (2 * coef(modelo_transf_sem_centrar)["emp_services_sq"]), "\n")
cat("vertice (transf centrado):    ",
    media_serv - coef(modelo_transf)["emp_services_c"] /
      (2 * coef(modelo_transf)["emp_services_c_sq"]), "\n")

# 5. AMOSTRA E ERROS ROBUSTOS (HC1)
nobs(modelo_simples)
nobs(modelo_controle)

coeftest(modelo_simples,            vcov. = vcovHC(modelo_simples,            type = "HC1"))
coeftest(modelo_de_verificaçãob,    vcov. = vcovHC(modelo_de_verificaçãob,    type = "HC1"))
coeftest(modelo_de_verificaçãos,    vcov. = vcovHC(modelo_de_verificaçãos,    type = "HC1"))
coeftest(modelo_controle,           vcov. = vcovHC(modelo_controle,           type = "HC1"))

# 6. TESTES F, HETEROCEDASTICIDADE E WALD (HC1)
linearHypothesis(
  modelo_controle,
  c("bank_branches = 0", "emp_services = 0", "emp_services_sq = 0",
    "elec_access = 0", "internet_use = 0", "school_ter = 0",
    "pop_total = 0", "labor_force = 0", "pop_urban = 0"),
  vcov. = vcovHC(modelo_controle, type = "HC1")
)

linearHypothesis(
  modelo_controle,
  c("elec_access = 0", "internet_use = 0", "school_ter = 0",
    "pop_total = 0", "labor_force = 0", "pop_urban = 0"),
  vcov. = vcovHC(modelo_controle, type = "HC1")
)

bptest(modelo_controle)

waldtest(
  modelo_simples, modelo_controle,
  vcov = vcovHC(modelo_controle, type = "HC1")
)

# Modelos restritos (usados nos testes de Wald — definidos UMA vez)
modelo_restrito_transf <- lm(
  gdp_pc_emp ~ elec_access + internet_use + school_ter +
    log_pop + lfp_rate + pop_urban,
  data = dados_limpos
)

modelo_restrito_serv <- lm(
  gdp_pc_emp ~ bank_branches + elec_access + internet_use + school_ter +
    log_pop + lfp_rate + pop_urban,
  data = dados_limpos
)

# Wald HC1 — 3 termos de interesse e bloco de serviços
cat("\n--- Wald HC1: 3 termos de interesse ---\n")
print(waldtest(modelo_restrito_transf, modelo_transf,
               vcov = vcovHC(modelo_transf, type = "HC1")))

cat("\n--- Wald HC1: serviços ---\n")
print(waldtest(modelo_restrito_serv, modelo_transf,
               vcov = vcovHC(modelo_transf, type = "HC1")))

# bptest dos modelos finais
bptest(modelo_transf)
bptest(modelo_log_y)

# Correlação banco × internet (suporte à interpretação)
cor(dados_limpos$bank_branches, dados_limpos$internet_use)
waldtest(modelo_restrito_transf, modelo_transf,
         vcov = vcovHC(modelo_transf, type = "HC1"))

waldtest(modelo_restrito_serv, modelo_transf,
         vcov = vcovHC(modelo_transf, type = "HC1"))

# 5.5 VERIFICAÇÃO HC1 vs HC3

cat("\n--- modelo_transf | HC1 ---\n")
print(coeftest(modelo_transf, vcov. = vcovHC(modelo_transf, type = "HC1")))

cat("\n--- modelo_transf | HC3 ---\n")
print(coeftest(modelo_transf, vcov. = vcovHC(modelo_transf, type = "HC3")))

cat("\n--- modelo_log_y | HC1 ---\n")
print(coeftest(modelo_log_y, vcov. = vcovHC(modelo_log_y, type = "HC1")))

cat("\n--- modelo_log_y | HC3 ---\n")
print(coeftest(modelo_log_y, vcov. = vcovHC(modelo_log_y, type = "HC3")))

cat("\n--- Wald HC3: 3 termos de interesse ---\n")
print(waldtest(modelo_restrito_transf, modelo_transf,
               vcov = vcovHC(modelo_transf, type = "HC3")))

cat("\n--- Wald HC3: serviços ---\n")
print(waldtest(modelo_restrito_serv, modelo_transf,
               vcov = vcovHC(modelo_transf, type = "HC3")))


# 5.6 NOTA METODOLÓGICA (texto para a tese, não é código)

# Erros robustos (HC1/HC3) corrigem APENAS a inferência diante de
# heterocedasticidade. NÃO corrigem:
#   - variáveis omitidas
#   - causalidade reversa
#   - erro de mensuração
#   - viés de seleção amostral
# Portanto, os coeficientes devem ser interpretados como associações
# condicionais, não como efeitos causais.
# Teste Wald conjunto de serviços no modelo em LOG
modelo_restrito_log_serv <- lm(
  log_gdp ~ bank_branches + elec_access + internet_use + school_ter +
    log_pop + lfp_rate + pop_urban,
  data = dados_limpos
)

cat("\n--- Wald HC1: serviços no modelo em log ---\n")
print(waldtest(modelo_restrito_log_serv, modelo_log_y, 
               vcov = vcovHC(modelo_log_y, type = "HC1")))

cat("\n--- Wald HC3: serviços no modelo em log ---\n")
print(waldtest(modelo_restrito_log_serv, modelo_log_y, 
               vcov = vcovHC(modelo_log_y, type = "HC3")))
# 7. ANÁLISE DESCRITIVA
vars <- c("gdp_pc_emp", "bank_branches", "emp_services",
          "emp_services_sq", "elec_access", "internet_use",
          "school_ter", "pop_total", "labor_force", "pop_urban")

nrow(dados_limpos)
summary(dados_limpos[, vars])
sapply(dados_limpos[, vars], var)
round(cor(dados_limpos[, vars], use = "complete.obs"), 2)

ggplot(dados_limpos, aes(gdp_pc_emp)) + geom_histogram(bins = 30)
ggplot(dados_limpos, aes(bank_branches, gdp_pc_emp)) +
  geom_point() + geom_smooth(method = "lm")

# 8. MULTICOLINEARIDADE
regressores <- c("bank_branches", "emp_services", "emp_services_sq",
                 "elec_access", "internet_use", "school_ter",
                 "pop_total", "labor_force", "pop_urban")

round(cor(dados_limpos[, regressores], use = "complete.obs"), 2)
vif(modelo_controle)


# 5.7 DIAGNÓSTICO DE COLINEARIDADE ENTRE BLOCOS

cat("\n--- Correlações entre internet, urbanização e serviços ---\n")
print(round(cor(dados_limpos[, c("internet_use", "pop_urban", "emp_services",
                                 "emp_services_c", "emp_services_c_sq")],
                use = "complete.obs"), 2))

cat("\n--- VIF modelo_transf ---\n")
print(vif(modelo_transf))

cat("\n--- VIF modelo_log_y ---\n")
print(vif(modelo_log_y))

modelo_sem_bloco <- lm(
  gdp_pc_emp ~ bank_branches + elec_access + school_ter +
    log_pop + lfp_rate,
  data = dados_limpos
)

modelo_com_bloco <- lm(
  gdp_pc_emp ~ bank_branches + elec_access + school_ter +
    log_pop + lfp_rate +
    internet_use + pop_urban + emp_services_c + emp_services_c_sq,
  data = dados_limpos
)

cat("\n--- Wald HC1: bloco internet + urbanização + serviços ---\n")
print(waldtest(modelo_sem_bloco, modelo_com_bloco,
               vcov = vcovHC(modelo_com_bloco, type = "HC1")))

cat("\n--- Correlação parcial serviços × produtividade | internet, urbanização ---\n")
res_y <- resid(lm(gdp_pc_emp ~ internet_use + pop_urban, data = dados_limpos))
res_x <- resid(lm(emp_services ~ internet_use + pop_urban, data = dados_limpos))
print(cor(res_y, res_x))


# 9. HETEROGENEIDADE — interação bancos × internet

# Modelo com interação (mesma especificação do modelo_transf + interação)
modelo_interacao <- lm(
  gdp_pc_emp ~ bank_branches * internet_use +
    emp_services_c + emp_services_c_sq +
    elec_access + school_ter +
    log_pop + lfp_rate + pop_urban,
  data = dados_limpos
)
cat("\n--- modelo_interacao | HC1 ---\n")
print(coeftest(modelo_interacao, vcov. = vcovHC(modelo_interacao, type = "HC1")))

# Teste da interação via Wald (modelo sem interação vs. com interação)
modelo_sem_interacao <- lm(
  gdp_pc_emp ~ bank_branches + internet_use +
    emp_services_c + emp_services_c_sq +
    elec_access + school_ter +
    log_pop + lfp_rate + pop_urban,
  data = dados_limpos
)
cat("\n--- Wald HC1: interação bancos × internet ---\n")
print(waldtest(modelo_sem_interacao, modelo_interacao,
               vcov = vcovHC(modelo_interacao, type = "HC1")))

# Efeito marginal de bancos em diferentes níveis de internet
# (interpretação da interação)
cat("\n--- Efeito marginal de bancos em níveis de internet ---\n")
b1  <- coef(modelo_interacao)["bank_branches"]
b12 <- coef(modelo_interacao)["bank_branches:internet_use"]
for (x in c(20, 50, 80, 95)) {
  cat("Internet =", x, "% → efeito marginal bancos =",
      round(b1 + b12 * x, 2), "\n")
}

# 9b. (EXPLORATÓRIO) Bancos: com vs. sem internet

modelo_sem_internet <- lm(
  gdp_pc_emp ~ bank_branches + emp_services_c + emp_services_c_sq +
    elec_access + school_ter +
    log_pop + lfp_rate + pop_urban,
  data = dados_limpos
)

cat("\n--- Coef. de bancos: com vs. sem internet ---\n")
cat("Com internet (modelo_transf): ",
    round(coef(modelo_transf)["bank_branches"], 2),
    " | SE HC1: ",
    round(sqrt(vcovHC(modelo_transf, type = "HC1")["bank_branches","bank_branches"]), 2),
    "\n")
cat("Sem internet:                  ",
    round(coef(modelo_sem_internet)["bank_branches"], 2),
    " | SE HC1: ",
    round(sqrt(vcovHC(modelo_sem_internet, type = "HC1")["bank_branches","bank_branches"]), 2),
    "\n")


# 9c. (EXPLORATÓRIO) Quartis de internet — mantido como complementar
dados_limpos <- dados_limpos %>%
  mutate(grupo_net = ntile(internet_use, 4))

for (g in 1:4) {
  m <- lm(
    gdp_pc_emp ~ bank_branches + emp_services_c + emp_services_c_sq,
    data = filter(dados_limpos, grupo_net == g)
  )
  cat("\n--- Quartil internet", g, "| N =", nobs(m), "---\n")
  print(coeftest(m, vcov. = vcovHC(m, type = "HC1")))
}


# 10. ANÁLISE DE INFLUÊNCIA

cooksd    <- cooks.distance(modelo_transf)
leverage  <- hatvalues(modelo_transf)
resid_st  <- rstudent(modelo_transf)

n <- nrow(dados_limpos)
p <- length(coef(modelo_transf))

limite_cook  <- 4 / n
limite_lev   <- 2 * p / n

cat("Limite Cook (4/n):",       round(limite_cook, 4), "\n")
cat("Limite leverage (2p/n):",  round(limite_lev, 4),  "\n")
cat("Limite |resíduo studentizado|: 2\n\n")

cat("Obs com Cook > limite:      ", sum(cooksd > limite_cook), "\n")
cat("Obs com leverage > limite:  ", sum(leverage > limite_lev), "\n")
cat("Obs com |resid stud| > 2:   ", sum(abs(resid_st) > 2), "\n\n")

# Top 10 por Cook
df_infl <- data.frame(
  pais      = dados_limpos$`Country Name`,
  cooks     = cooksd,
  leverage  = leverage,
  resid_st  = resid_st
)
print(head(df_infl[order(-df_infl$cooks), ], 10))

# Refit sem as observações mais influentes (Cook > 1; se nenhuma, remover top 3)
if (sum(cooksd > 1) > 0) {
  dados_sem_infl <- dados_limpos[-which(cooksd > 1), ]
} else {
  dados_sem_infl <- dados_limpos[-order(-cooksd)[1:3], ]
}

modelo_sem_influentes <- lm(
  gdp_pc_emp ~ bank_branches + emp_services_c + emp_services_c_sq +
    elec_access + internet_use + school_ter +
    log_pop + lfp_rate + pop_urban,
  data = dados_sem_infl
)

cat("\n--- Coeficientes: com vs. sem influentes ---\n")
vars_comp <- c("bank_branches", "emp_services_c",
               "emp_services_c_sq", "internet_use")
print(data.frame(
  variavel       = vars_comp,
  com_todos      = coef(modelo_transf)[vars_comp],
  sem_influentes = coef(modelo_sem_influentes)[vars_comp]
))

# 11. SPEARMAN + GRÁFICOS DIAGNÓSTICOS

# Spearman como complemento a Pearson
cat("\n--- Spearman ---\n")
print(round(cor(dados_limpos[, vars], method = "spearman",
                use = "complete.obs"), 2))

# (1) Serviços × produtividade — com curva quadrática BIVARIADA
# Sem a linha vertical do modelo múltiplo, e mudando o termo para "vértice"
p1 <- ggplot(dados_limpos, aes(emp_services, gdp_pc_emp)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2),
              color = "red", se = TRUE) +
  labs(x = "Emprego em serviços (%)",
       y = "Produtividade (PPC 2021)",
       title = "Relação quadrática bivariada: serviços e produtividade") +
  theme_minimal()
print(p1)

# (2) Bancos × log da produtividade
p2 <- ggplot(dados_limpos, aes(bank_branches, log_gdp)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  labs(x = "Agências por 100 mil adultos",
       y = "log(Produtividade)",
       title = "Agências bancárias × log(produtividade)") +
  theme_minimal()
print(p2)

# (3) Resíduos × ajustados
df_res <- data.frame(
  fitted = fitted(modelo_transf),
  resid  = resid(modelo_transf)
)
p3 <- ggplot(df_res, aes(fitted, resid)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  labs(x = "Valores ajustados", y = "Resíduos",
       title = "Resíduos × valores ajustados") +
  theme_minimal()
print(p3)

# (4) Distância de Cook
df_cook <- data.frame(
  obs   = 1:nrow(dados_limpos),
  cooks = cooksd,
  pais  = dados_limpos$`Country Name`
)
limite_cook <- 4 / nrow(dados_limpos)

p4 <- ggplot(df_cook, aes(obs, cooks)) +
  geom_segment(aes(xend = obs, yend = 0)) +
  geom_hline(yintercept = limite_cook, linetype = "dashed", color = "red") +
  geom_text(data = subset(df_cook, cooks > limite_cook),
            aes(label = pais), hjust = -0.1, size = 2.5, check_overlap = TRUE) +
  labs(x = "Observação", y = "Distância de Cook",
       title = "Influência das observações (Cook's D)",
       subtitle = paste("Linha vermelha: 4/n =", round(limite_cook, 4))) +
  theme_minimal()
print(p4)
# 12. COMPARAÇÃO DESCRITIVA: PAÍSES vs. PAÍSES + AGREGADOS

vars_desc <- c("gdp_pc_emp", "bank_branches", "emp_services",
               "elec_access", "internet_use", "school_ter", "pop_urban")

stats_paises <- dados_limpos %>%
  summarise(across(all_of(vars_desc),
                   list(mean = ~mean(., na.rm = TRUE),
                        sd   = ~sd(.,   na.rm = TRUE),
                        min  = ~min(.,  na.rm = TRUE),
                        max  = ~max(.,  na.rm = TRUE)))) %>%
  mutate(grupo = "Países", n = nrow(dados_limpos), .before = 1)

stats_agregados <- dados_com_agregados %>%
  summarise(across(all_of(vars_desc),
                   list(mean = ~mean(., na.rm = TRUE),
                        sd   = ~sd(.,   na.rm = TRUE),
                        min  = ~min(.,  na.rm = TRUE),
                        max  = ~max(.,  na.rm = TRUE)))) %>%
  mutate(grupo = "Países + agregados", n = nrow(dados_com_agregados), .before = 1)

print(rbind(stats_paises, stats_agregados))

# 12. MAGNITUDES ECONÔMICAS (seções 4.2, 4.3 e 5.1 do texto)
V      <- vcovHC(modelo_transf, type = "HC1")
b      <- coef(modelo_transf)
tc     <- qt(0.975, df.residual(modelo_transf))
media_y <- mean(dados_limpos$gdp_pc_emp)
dp_y    <- sd(dados_limpos$gdp_pc_emp)
dp_b    <- sd(dados_limpos$bank_branches)
dp_net  <- sd(dados_limpos$internet_use)
# 12.1 Agências: uma unidade, um desvio-padrão, IC de 95%
cat("\n--- Agências ---\n")
cat("1 agência / média:", b["bank_branches"] / media_y, "\n")
cat("1 d.p. (US$):", b["bank_branches"] * dp_b,
    "| % da média:", b["bank_branches"] * dp_b / media_y,
    "| em d.p. de Y:", b["bank_branches"] * dp_b / dp_y, "\n")
ic <- b["bank_branches"] + c(-1, 1) * tc * sqrt(V["bank_branches", "bank_branches"])
cat("IC 95% (por agência):", ic, "\n")
cat("IC 95% (1 d.p., % da média):", ic * dp_b / media_y, "\n")
cat("Internet, 1 d.p. (US$):", b["internet_use"] * dp_net, "\n")
modelo_sem_lux <- update(modelo_transf,
                         data = subset(dados_limpos, `Country Code` != "LUX"))
cat("Sem LUX: coef =", coef(modelo_sem_lux)["bank_branches"],
    "| 1 d.p. =", coef(modelo_sem_lux)["bank_branches"] * dp_b,
    "| % da média =", coef(modelo_sem_lux)["bank_branches"] * dp_b / media_y, "\n")
# 12.2 Serviços: efeito parcial com IC (método delta, HC1)
cat("\n--- Efeito parcial dos serviços ---\n")
for (S in c(20, 40, 60, 80)) {
  c0  <- S - media_serv
  w   <- c(emp_services_c = 1, emp_services_c_sq = 2 * c0)
  me  <- sum(w * b[names(w)])
  se  <- sqrt(as.numeric(t(w) %*% V[names(w), names(w)] %*% w))
  cat(S, "% | efeito:", round(me), "| % da média:", round(me / media_y * 100, 1),
      "| IC 95%:", round(me - tc * se), round(me + tc * se), "\n")
}
cat("Países com serviços < 30%:", sum(dados_limpos$emp_services < 30),
    "| >= 80%:", sum(dados_limpos$emp_services >= 80), "\n")
# 12.3 Modelo em log: efeito parcial (semielasticidade) por nível
bl <- coef(modelo_log_y)
for (S in c(20, 40, 60, 80)) {
  c0 <- S - media_serv
  cat("log |", S, "% :", bl["emp_services_c"] + 2 * bl["emp_services_c_sq"] * c0, "\n")
}

