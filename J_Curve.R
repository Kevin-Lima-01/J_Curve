# Estabelecendo os pacotes
library(readr)
library(readxl)
library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(scales)
library(writexl)
library(stargazer)
library(haven)
library(plm)
library(lmtest)
library(fastDummies)
library(fixest)
library(car)
library(gridExtra)
library(patchwork) 
library(broom)
library(corrplot)
library(ggcorrplot)

# Estelecendo a base
df <- read_excel("C:/Users/KEVIN/Desktop/Lições FGV/5 Semestre/Trabalho Econo/Base Final/dados_finais.xlsx")

# Removemos entradas que indicam apenas regiões geograficas, ou territorios de outros países
df_filtered <- df %>%
  filter(!(Country_Code %in% c("AFE", "AFW", "ARB", "ASM", "CEB", "CHI", "CSS", "CYM", "CZE",
                               "EAP", "EAR", "EAS", "ECA", "ECS", "EMU", "EUU", "FCS", "FRO",
                               "GIB", "GRL", "GUM", "HIC", "HPC", "IBD", "IBT", "IDA", "IDB",
                               "IDX", "IMN", "INX", "LAC", "LCN", "LDC", "LIC", "LMC", "LMY",
                               "LTE", "MAF", "MEA", "MIC", "MNA", "MNP", "NAC", "OED", "OSS",
                               "PRE", "PSE", "PSS", "PST", "PYF", "SAS", "SSA", "SSF", "SST",
                               "SXM", "TCA", "TEA", "TEC", "TLA", "TMN", "TSA", "TSS", "UMC",
                               "VGB", "VIR", "WLD"))) %>%
  # Cortamos para uma data em que passamos a ter concistentemente dados de cambio real. 
  filter(Year >= 1960)

# Separamos paises desenvolvidos de subdesenvolvidos
df_pib <- df_filtered %>%
  group_by(Country_Code) %>%  
  filter(!is.na(PIB_Capita)) %>%  
  filter(Year == max(Year)) %>%  
  select(Country_Code, PIB_recente = PIB_Capita) %>%  
  distinct()

df_classified <- df_filtered %>%
  left_join(df_pib, by = "Country_Code") %>%  
  mutate(
    desenvolvido_pib = ifelse(PIB_recente >= 20000, 1, 0) # Consideramos desenvolvidos para acima de 20.000
  ) %>%
  select(-PIB_recente)

df_classified <- df_classified[, -1]

df_panel <- pdata.frame(df_classified, index = c("Country_Code", "Year"))

df_des <- df_panel[df_panel$desenvolvido_pib == 1, ]
df_subdes <- df_panel[df_panel$desenvolvido_pib == 0, ]

summary(df_panel)
summary(df_des)
summary(df_subdes)

###
###
# Iniciamos a nossa analise econometrica para países subdesenvolvidos
# Pooled Simples
mod1_polssub <- plm(RBalanca ~ RRCambio,
                    data = df_subdes,
                    cluster = c("Country_Code"),
                    model = "pooling")
summary(mod1_polssub)

# Adicionar controles
mod2_polssub <- plm(RBalanca ~ RRCambio + RPIBC + RJuros + RConsumo + RReservas + RInflação + RGasto,
            data = df_subdes,
            cluster = c("Country_Code"),
            model = "pooling")
summary(mod2_polssub)

# Incluir lags para o cambio
mod3_polssub <- plm(RBalanca ~ RRCambio + RRCambio_lag1 + RRCambio_lag2 + RRCambio_lag3,
            data = df_subdes,
            cluster = c("Country_Code"),
            model = "pooling")
summary(mod3_polssub)

# Modelo com lags e controles
mod4_polssub <- plm(RBalanca ~ RRCambio + RRCambio_lag1 + RRCambio_lag2 + RRCambio_lag3 + RPIBC + RJuros + RConsumo + RReservas + RInflação + RGasto,
            data = df_subdes,
            cluster = c("Country_Code"),
            model = "pooling")
summary(mod4_polssub)

# Considerando matriz robusta para heterocedasticidade e outros testes
bptest(mod1_polssub)
bptest(mod2_polssub)
bptest(mod3_polssub)
bptest(mod4_polssub)

summary(mod1_polssub, vcov = vcovHC(mod1_polssub, type = "HC1", cluster = "group"))
summary(mod2_polssub, vcov = vcovHC(mod2_polssub, type = "HC1", cluster = "group"))
summary(mod3_polssub, vcov = vcovHC(mod3_polssub, type = "HC1", cluster = "group"))
summary(mod4_polssub, vcov = vcovHC(mod4_polssub, type = "HC1", cluster = "group"))

linearHypothesis(mod4_polssub, c("RRCambio=0", "RRCambio_lag1=0", "RRCambio_lag2=0", "RRCambio_lag3=0"),
                 vcov = vcovHC(mod4_polssub, cluster = "group"))
linearHypothesis(mod4_polssub, c("RRCambio=0", "RRCambio_lag1=0", "RRCambio_lag2=0", "RRCambio_lag3=0"))

linearHypothesis(mod4_polssub, c("RRCambio_lag1=0", "RRCambio_lag2=0", "RRCambio_lag3=0"),
                 vcov = vcovHC(mod4_polssub, cluster = "group"))
linearHypothesis(mod4_polssub, c("RRCambio_lag1=0", "RRCambio_lag2=0", "RRCambio_lag3=0"))

stargazer(mod1_polssub, mod2_polssub, mod3_polssub, mod4_polssub, type = "text")

# Testando correlação serial dos resudios, averigunado a necessidade de usar RE
pbgtest(mod1_polssub, order = 1)
pbgtest(mod2_polssub, order = 1)  
pbgtest(mod3_polssub, order = 1)  
pbgtest(mod4_polssub, order = 1)  

# Testando Random walk para os residous, averiguando a necessidade de usar RE.
rmod1_polssub <- residuals(mod1_polssub)
rmod1_polssub <- data.frame(
  Observacao = 1:length(rmod1_polssub),
  Residuos = rmod1_polssub
)
rmod2_polssub <- residuals(mod2_polssub)
rmod2_polssub <- data.frame(
  Observacao = 1:length(rmod2_polssub),
  Residuos = rmod2_polssub
)
rmod3_polssub <- residuals(mod3_polssub)
rmod3_polssub <- data.frame(
  Observacao = 1:length(rmod3_polssub),
  Residuos = rmod3_polssub
)
rmod4_polssub <- residuals(mod4_polssub)
rmod4_polssub <- data.frame(
  Observacao = 1:length(rmod4_polssub),
  Residuos = rmod4_polssub
)

ggplot(rmod1_polssub, aes(x = Observacao, y = Residuos)) +
  geom_line(color = "blue", linewidth = 0.7) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", linewidth = 0.8) +
  labs(
    title = "Resíduos do Modelo 1 (POLS - SUBDES)",
    x = "Ordem das Observações",
    y = "Valores dos Resíduos"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
ggplot(rmod2_polssub, aes(x = Observacao, y = Residuos)) +
  geom_line(color = "blue", linewidth = 0.7) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", linewidth = 0.8) +
  labs(
    title = "Resíduos do Modelo 2 (POLS - SUBDES)",
    x = "Ordem das Observações",
    y = "Valores dos Resíduos"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
ggplot(rmod3_polssub, aes(x = Observacao, y = Residuos)) +
  geom_line(color = "blue", linewidth = 0.7) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", linewidth = 0.8) +
  labs(
    title = "Resíduos do Modelo 3 (POLS - SUBDES)",
    x = "Ordem das Observações",
    y = "Valores dos Resíduos"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
ggplot(rmod4_polssub, aes(x = Observacao, y = Residuos)) +
  geom_line(color = "blue", linewidth = 0.7) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", linewidth = 0.8) +
  labs(
    title = "Resíduos do Modelo 4 (POLS - SUBDES)",
    x = "Ordem das Observações",
    y = "Valores dos Resíduos"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Modelo FE
mod1_fesub <- plm(RBalanca ~ RRCambio,
                  data = df_subdes,
                  cluster = c("Country_Code"),
                  model = "within")
summary(mod1_fesub)

# Adicionar controles
mod2_fesub <- plm(RBalanca ~ RRCambio + RInflação + RJuros + RPIBC + RJuros + RConsumo + RReservas + RInflação + RGasto,
                  data = df_subdes,
                  cluster = c("Country_Code"),
                  model = "within")
summary(mod2_fesub)

# Incluir lags para o cambio
mod3_fesub <- plm(RBalanca ~ RRCambio + RRCambio_lag1 + RRCambio_lag2 + RRCambio_lag3,
                  data = df_subdes,
                  cluster = c("Country_Code"),
                  model = "within")
summary(mod3_fesub)


# Modelo com lags e controles
mod4_fesub <- plm(RBalanca ~ RRCambio + RRCambio_lag1 + RRCambio_lag2 + RRCambio_lag3 + RPIBC + RJuros + RConsumo + RReservas + RInflação + RGasto,
                  data = df_subdes,
                  cluster = c("Country_Code"),
                  model = "within")
summary(mod4_fesub)

# Considerando matriz robusta para heterocedasticidade e outros testes
bptest(mod1_fesub)
bptest(mod2_fesub)
bptest(mod3_fesub)
bptest(mod4_fesub)

summary(mod1_fesub, vcov = vcovHC(mod1_fesub, type = "HC1", cluster = "group"))
summary(mod2_fesub, vcov = vcovHC(mod2_fesub, type = "HC1", cluster = "group"))
summary(mod3_fesub, vcov = vcovHC(mod3_fesub, type = "HC1", cluster = "group"))
summary(mod4_fesub, vcov = vcovHC(mod4_fesub, type = "HC1", cluster = "group"))

linearHypothesis(mod4_fesub, c("RRCambio=0", "RRCambio_lag1=0", "RRCambio_lag2=0", "RRCambio_lag3=0"),
                 vcov = vcovHC(mod4_fesub, cluster = "group"))
linearHypothesis(mod4_fesub, c("RRCambio=0", "RRCambio_lag1=0", "RRCambio_lag2=0", "RRCambio_lag3=0"))

linearHypothesis(mod4_fesub, c("RRCambio_lag1=0", "RRCambio_lag2=0", "RRCambio_lag3=0"),
                 vcov = vcovHC(mod4_fesub, cluster = "group"))
linearHypothesis(mod4_fesub, c("RRCambio_lag1=0", "RRCambio_lag2=0", "RRCambio_lag3=0"))


stargazer(mod1_fesub, mod2_fesub, mod3_fesub, mod4_fesub, type = "text")

# Testando correlação serial dos resudios, averigunado a necessidade de usar RE. 
pbgtest(mod1_fesub, order = 1)
pbgtest(mod2_fesub, order = 1)  
pbgtest(mod3_fesub, order = 1)  
pbgtest(mod4_fesub, order = 1) 

pwartest(mod1_fesub)
pwartest(mod2_fesub)
pwartest(mod3_fesub)
pwartest(mod4_fesub)

# Testando Random walk para os residous, averiguando a necessidade de usar RE.
rmod1_fesub <- residuals(mod1_fesub)
rmod1_fesub <- data.frame(
  Observacao = 1:length(rmod1_fesub),
  Residuos = rmod1_fesub
)
rmod2_fesub <- residuals(mod2_fesub)
rmod2_fesub <- data.frame(
  Observacao = 1:length(rmod2_fesub),
  Residuos = rmod2_fesub
)
rmod3_fesub <- residuals(mod3_fesub)
rmod3_fesub <- data.frame(
  Observacao = 1:length(rmod3_fesub),
  Residuos = rmod3_fesub
)
rmod4_fesub <- residuals(mod4_fesub)
rmod4_fesub <- data.frame(
  Observacao = 1:length(rmod4_fesub),
  Residuos = rmod4_fesub
)

ggplot(rmod1_fesub, aes(x = Observacao, y = Residuos)) +
  geom_line(color = "blue", linewidth = 0.7) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", linewidth = 0.8) +
  labs(
    title = "Resíduos do Modelo 1 (FE - SUBDES)",
    x = "Ordem das Observações",
    y = "Valores dos Resíduos"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
ggplot(rmod2_fesub, aes(x = Observacao, y = Residuos)) +
  geom_line(color = "blue", linewidth = 0.7) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", linewidth = 0.8) +
  labs(
    title = "Resíduos do Modelo 2 (FE - SUBDES)",
    x = "Ordem das Observações",
    y = "Valores dos Resíduos"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
ggplot(rmod3_fesub, aes(x = Observacao, y = Residuos)) +
  geom_line(color = "blue", linewidth = 0.7) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", linewidth = 0.8) +
  labs(
    title = "Resíduos do Modelo 3 (FE - SUBDES)",
    x = "Ordem das Observações",
    y = "Valores dos Resíduos"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
ggplot(rmod4_fesub, aes(x = Observacao, y = Residuos)) +
  geom_line(color = "blue", linewidth = 0.7) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", linewidth = 0.8) +
  labs(
    title = "Resíduos do Modelo 4 (FE - SUBDES)",
    x = "Ordem das Observações",
    y = "Valores dos Resíduos"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Modelo RE
mod1_resub <- plm(RBalanca ~ RRCambio,
                data = df_subdes,
                cluster = c("Country_Code"),
                model = "random")
summary(mod1_resub)

# Adicionar controles
mod2_resub <- plm(RBalanca ~ RRCambio + RPIBC + RJuros + RConsumo + RReservas + RInflação + RGasto,
                data = df_subdes,
                cluster = c("Country_Code"),
                model = "random")
summary(mod2_resub)

# Incluir lags para o cambio
mod3_resub <- plm(RBalanca ~ RRCambio + RRCambio_lag1 + RRCambio_lag2 + RRCambio_lag3,
                data = df_subdes,
                cluster = c("Country_Code"),
                model = "random")
summary(mod3_resub)


# Modelo com lags e controles
mod4_resub <- plm(RBalanca ~ RRCambio + RRCambio_lag1 + RRCambio_lag2 + RRCambio_lag3 + RPIBC + RJuros + RConsumo + RReservas + RInflação + RGasto,
                data = df_subdes,
                cluster = c("Country_Code"),
                model = "random")
summary(mod4_resub)

# Considerando matriz robusta para heterocedasticidade e outros testes
bptest(mod1_resub)
bptest(mod2_resub)
bptest(mod3_resub)
bptest(mod4_resub)

summary(mod1_resub, vcov = vcovHC(mod1_resub, type = "HC1", cluster = "group"))
summary(mod2_resub, vcov = vcovHC(mod2_resub, type = "HC1", cluster = "group"))
summary(mod3_resub, vcov = vcovHC(mod3_resub, type = "HC1", cluster = "group"))
summary(mod4_resub, vcov = vcovHC(mod4_resub, type = "HC1", cluster = "group"))

linearHypothesis(mod4_resub, c("RRCambio=0", "RRCambio_lag1=0", "RRCambio_lag2=0", "RRCambio_lag3=0"),
                 vcov = vcovHC(mod4_resub, cluster = "group"))
linearHypothesis(mod4_resub, c("RRCambio=0", "RRCambio_lag1=0", "RRCambio_lag2=0", "RRCambio_lag3=0"))

linearHypothesis(mod4_fesub, c("RRCambio_lag1=0", "RRCambio_lag2=0", "RRCambio_lag3=0"),
                 vcov = vcovHC(mod4_fesub, cluster = "group"))
linearHypothesis(mod4_fesub, c("RRCambio_lag1=0", "RRCambio_lag2=0", "RRCambio_lag3=0"))


stargazer(mod1_resub, mod2_resub, mod3_resub, mod4_resub, type = "text")

# Testando correlação serial dos resudios, averigunado a necessidade de usar RE. 
pbgtest(mod1_resub, order = 1)
pbgtest(mod2_resub, order = 1)  
pbgtest(mod3_resub, order = 1)  
pbgtest(mod4_resub, order = 1)  

# Testando Random walk para os residous, averiguando a necessidade de usar RE.
rmod1_resub <- residuals(mod1_resub)
rmod1_resub <- data.frame(
  Observacao = 1:length(rmod1_resub),
  Residuos = rmod1_resub
)
rmod2_resub <- residuals(mod2_resub)
rmod2_resub <- data.frame(
  Observacao = 1:length(rmod2_resub),
  Residuos = rmod2_resub
)
rmod3_resub <- residuals(mod3_resub)
rmod3_resub <- data.frame(
  Observacao = 1:length(rmod3_resub),
  Residuos = rmod3_resub
)
rmod4_resub <- residuals(mod4_resub)
rmod4_resub <- data.frame(
  Observacao = 1:length(rmod4_resub),
  Residuos = rmod4_resub
)

ggplot(rmod1_resub, aes(x = Observacao, y = Residuos)) +
  geom_line(color = "blue", linewidth = 0.7) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", linewidth = 0.8) +
  labs(
    title = "Resíduos do Modelo 1 (RE - SUBDES)",
    x = "Ordem das Observações",
    y = "Valores dos Resíduos"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
ggplot(rmod2_resub, aes(x = Observacao, y = Residuos)) +
  geom_line(color = "blue", linewidth = 0.7) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", linewidth = 0.8) +
  labs(
    title = "Resíduos do Modelo 2 (RE - SUBDES)",
    x = "Ordem das Observações",
    y = "Valores dos Resíduos"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
ggplot(rmod3_resub, aes(x = Observacao, y = Residuos)) +
  geom_line(color = "blue", linewidth = 0.7) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", linewidth = 0.8) +
  labs(
    title = "Resíduos do Modelo 3 (RE - SUBDES)",
    x = "Ordem das Observações",
    y = "Valores dos Resíduos"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
ggplot(rmod4_resub, aes(x = Observacao, y = Residuos)) +
  geom_line(color = "blue", linewidth = 0.7) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", linewidth = 0.8) +
  labs(
    title = "Resíduos do Modelo 4 (RE - SUBDES)",
    x = "Ordem das Observações",
    y = "Valores dos Resíduos"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Conclusão comparativa
stargazer(mod1_polssub, mod1_fesub, mod1_resub, type = "text")
stargazer(mod2_polssub, mod2_fesub, mod2_resub, type = "text")
stargazer(mod3_polssub, mod3_fesub, mod3_resub, type = "text")
stargazer(mod4_polssub, mod4_fesub, mod4_resub, type = "text")

phtest(mod1_fesub, mod1_resub)
phtest(mod3_fesub, mod3_resub)
phtest(mod4_fesub, mod4_resub)

###
###
# Iniciamos a nossa analise econometrica para países desenvolvidos
# Pooled Simples
mod1_polsdes <- plm(RBalanca ~ RRCambio,
                    data = df_des,
                    cluster = c("Country_Code"),
                    model = "pooling")
summary(mod1_polsdes)

# Adicionar controles
mod2_polsdes <- plm(RBalanca ~ RRCambio + RPIBC + RJuros + RConsumo + RReservas + RInflação + RGasto,
                    data = df_des,
                    cluster = c("Country_Code"),
                    model = "pooling")
summary(mod2_polsdes)

# Incluir lags para o cambio
mod3_polsdes <- plm(RBalanca ~ RRCambio + RRCambio_lag1 + RRCambio_lag2 + RRCambio_lag3,
                    data = df_des,
                    cluster = c("Country_Code"),
                    model = "pooling")
summary(mod3_polsdes)

# Modelo com lags e controles
mod4_polsdes <- plm(RBalanca ~ RRCambio + RRCambio_lag1 + RRCambio_lag2 + RRCambio_lag3 + RPIBC + RJuros + RConsumo + RReservas + RInflação + RGasto,
                    data = df_des,
                    cluster = c("Country_Code"),
                    model = "pooling")
summary(mod4_polsdes)

# Considerando matriz robusta para heterocedasticidade e outros testes
bptest(mod1_polsdes)
bptest(mod2_polsdes)
bptest(mod3_polsdes)
bptest(mod4_polsdes)

summary(mod1_polsdes, vcov = vcovHC(mod1_polsdes, type = "HC1", cluster = "group"))
summary(mod2_polsdes, vcov = vcovHC(mod2_polsdes, type = "HC1", cluster = "group"))
summary(mod3_polsdes, vcov = vcovHC(mod3_polsdes, type = "HC1", cluster = "group"))
summary(mod4_polsdes, vcov = vcovHC(mod4_polsdes, type = "HC1", cluster = "group"))

linearHypothesis(mod4_polsdes, c("RRCambio=0", "RRCambio_lag1=0", "RRCambio_lag2=0", "RRCambio_lag3=0"),
                 vcov = vcovHC(mod4_polsdes, cluster = "group"))
linearHypothesis(mod4_polsdes, c("RRCambio=0", "RRCambio_lag1=0", "RRCambio_lag2=0", "RRCambio_lag3=0"))

linearHypothesis(mod4_polsdes, c("RRCambio_lag1=0", "RRCambio_lag2=0", "RRCambio_lag3=0"),
                 vcov = vcovHC(mod4_polsdes, cluster = "group"))
linearHypothesis(mod4_polsdes, c("RRCambio_lag1=0", "RRCambio_lag2=0", "RRCambio_lag3=0"))

stargazer(mod1_polsdes, mod2_polsdes, mod3_polsdes, mod4_polsdes, type = "text")

# Testando correlação serial dos resudios, averigunado a necessidade de usar RE
pbgtest(mod1_polsdes, order = 1)
pbgtest(mod2_polsdes, order = 1)  
pbgtest(mod3_polsdes, order = 1)  
pbgtest(mod4_polsdes, order = 1)  

# Testando Random walk para os residous, averiguando a necessidade de usar RE.
rmod1_polsdes <- residuals(mod1_polsdes)
rmod1_polsdes <- data.frame(
  Observacao = 1:length(rmod1_polsdes),
  Residuos = rmod1_polsdes
)
rmod2_polsdes <- residuals(mod2_polsdes)
rmod2_polsdes <- data.frame(
  Observacao = 1:length(rmod2_polsdes),
  Residuos = rmod2_polsdes
)
rmod3_polsdes <- residuals(mod3_polsdes)
rmod3_polsdes <- data.frame(
  Observacao = 1:length(rmod3_polsdes),
  Residuos = rmod3_polsdes
)
rmod4_polsdes <- residuals(mod4_polsdes)
rmod4_polsdes <- data.frame(
  Observacao = 1:length(rmod4_polsdes),
  Residuos = rmod4_polsdes
)

ggplot(rmod1_polsdes, aes(x = Observacao, y = Residuos)) +
  geom_line(color = "blue", linewidth = 0.7) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", linewidth = 0.8) +
  labs(
    title = "Resíduos do Modelo 1 (POLS - DES)",
    x = "Ordem das Observações",
    y = "Valores dos Resíduos"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
ggplot(rmod2_polsdes, aes(x = Observacao, y = Residuos)) +
  geom_line(color = "blue", linewidth = 0.7) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", linewidth = 0.8) +
  labs(
    title = "Resíduos do Modelo 2 (POLS - DES)",
    x = "Ordem das Observações",
    y = "Valores dos Resíduos"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
ggplot(rmod3_polsdes, aes(x = Observacao, y = Residuos)) +
  geom_line(color = "blue", linewidth = 0.7) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", linewidth = 0.8) +
  labs(
    title = "Resíduos do Modelo 3 (POLS - DES)",
    x = "Ordem das Observações",
    y = "Valores dos Resíduos"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
ggplot(rmod4_polsdes, aes(x = Observacao, y = Residuos)) +
  geom_line(color = "blue", linewidth = 0.7) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", linewidth = 0.8) +
  labs(
    title = "Resíduos do Modelo 4 (POLS - DES)",
    x = "Ordem das Observações",
    y = "Valores dos Resíduos"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Modelo FE
mod1_fedes <- plm(RBalanca ~ RRCambio,
                  data = df_des,
                  cluster = c("Country_Code"),
                  model = "within")
summary(mod1_fedes)

# Adicionar controles
mod2_fedes <- plm(RBalanca ~ RRCambio + RInflação + RJuros + RPIBC + RJuros + RConsumo + RReservas + RInflação + RGasto,
                  data = df_des,
                  cluster = c("Country_Code"),
                  model = "within")
summary(mod2_fedes)

# Incluir lags para o cambio
mod3_fedes <- plm(RBalanca ~ RRCambio + RRCambio_lag1 + RRCambio_lag2 + RRCambio_lag3,
                  data = df_des,
                  cluster = c("Country_Code"),
                  model = "within")
summary(mod3_fedes)


# Modelo com lags e controles
mod4_fedes <- plm(RBalanca ~ RRCambio + RRCambio_lag1 + RRCambio_lag2 + RRCambio_lag3 + RPIBC + RJuros + RConsumo + RReservas + RInflação + RGasto,
                  data = df_des,
                  cluster = c("Country_Code"),
                  model = "within")
summary(mod4_fedes)

# Considerando matriz robusta para heterocedasticidade e outros testes
bptest(mod1_fedes)
bptest(mod2_fedes)
bptest(mod3_fedes)
bptest(mod4_fedes)

summary(mod1_fedes, vcov = vcovHC(mod1_fedes, type = "HC1", cluster = "group"))
summary(mod2_fedes, vcov = vcovHC(mod2_fedes, type = "HC1", cluster = "group"))
summary(mod3_fedes, vcov = vcovHC(mod3_fedes, type = "HC1", cluster = "group"))
summary(mod4_fedes, vcov = vcovHC(mod4_fedes, type = "HC1", cluster = "group"))

linearHypothesis(mod4_fedes, c("RRCambio=0", "RRCambio_lag1=0", "RRCambio_lag2=0", "RRCambio_lag3=0"),
                 vcov = vcovHC(mod4_fedes, cluster = "group"))
linearHypothesis(mod4_fedes, c("RRCambio=0", "RRCambio_lag1=0", "RRCambio_lag2=0", "RRCambio_lag3=0"))

linearHypothesis(mod4_fedes, c("RRCambio_lag1=0", "RRCambio_lag2=0", "RRCambio_lag3=0"),
                 vcov = vcovHC(mod4_fedes, cluster = "group"))
linearHypothesis(mod4_fedes, c("RRCambio_lag1=0", "RRCambio_lag2=0", "RRCambio_lag3=0"))


stargazer(mod1_fedes, mod2_fedes, mod3_fedes, mod4_fedes, type = "text")

# Testando correlação serial dos resudios, averigunado a necessidade de usar RE. 
pbgtest(mod1_fedes, order = 1)
pbgtest(mod2_fedes, order = 1)  
pbgtest(mod3_fedes, order = 1)  
pbgtest(mod4_fedes, order = 1) 

pwartest(mod1_fedes)
pwartest(mod2_fedes)
pwartest(mod3_fedes)
pwartest(mod4_fedes)

# Testando Random walk para os residous, averiguando a necessidade de usar RE.
rmod1_fedes <- residuals(mod1_fedes)
rmod1_fedes <- data.frame(
  Observacao = 1:length(rmod1_fedes),
  Residuos = rmod1_fedes
)
rmod2_fedes <- residuals(mod2_fedes)
rmod2_fedes <- data.frame(
  Observacao = 1:length(rmod2_fedes),
  Residuos = rmod2_fedes
)
rmod3_fedes <- residuals(mod3_fedes)
rmod3_fedes <- data.frame(
  Observacao = 1:length(rmod3_fedes),
  Residuos = rmod3_fedes
)
rmod4_fedes <- residuals(mod4_fedes)
rmod4_fedes <- data.frame(
  Observacao = 1:length(rmod4_fedes),
  Residuos = rmod4_fedes
)

ggplot(rmod1_fedes, aes(x = Observacao, y = Residuos)) +
  geom_line(color = "blue", linewidth = 0.7) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", linewidth = 0.8) +
  labs(
    title = "Resíduos do Modelo 1 (FE - DES)",
    x = "Ordem das Observações",
    y = "Valores dos Resíduos"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
ggplot(rmod2_fedes, aes(x = Observacao, y = Residuos)) +
  geom_line(color = "blue", linewidth = 0.7) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", linewidth = 0.8) +
  labs(
    title = "Resíduos do Modelo 2 (FE - DES)",
    x = "Ordem das Observações",
    y = "Valores dos Resíduos"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
ggplot(rmod3_fedes, aes(x = Observacao, y = Residuos)) +
  geom_line(color = "blue", linewidth = 0.7) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", linewidth = 0.8) +
  labs(
    title = "Resíduos do Modelo 3 (FE - DES)",
    x = "Ordem das Observações",
    y = "Valores dos Resíduos"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
ggplot(rmod4_fedes, aes(x = Observacao, y = Residuos)) +
  geom_line(color = "blue", linewidth = 0.7) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", linewidth = 0.8) +
  labs(
    title = "Resíduos do Modelo 4 (FE - DES)",
    x = "Ordem das Observações",
    y = "Valores dos Resíduos"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Modelo RE
mod1_redes <- plm(RBalanca ~ RRCambio,
                  data = df_des,
                  cluster = c("Country_Code"),
                  model = "random")
summary(mod1_redes)

# Adicionar controles
mod2_redes <- plm(RBalanca ~ RRCambio + RPIBC + RJuros + RConsumo + RReservas + RInflação + RGasto,
                  data = df_des,
                  cluster = c("Country_Code"),
                  model = "random")
summary(mod2_redes)

# Incluir lags para o cambio
mod3_redes <- plm(RBalanca ~ RRCambio + RRCambio_lag1 + RRCambio_lag2 + RRCambio_lag3,
                  data = df_des,
                  cluster = c("Country_Code"),
                  model = "random")
summary(mod3_redes)


# Modelo com lags e controles
mod4_redes <- plm(RBalanca ~ RRCambio + RRCambio_lag1 + RRCambio_lag2 + RRCambio_lag3 + RPIBC + RJuros + RConsumo + RReservas + RInflação + RGasto,
                  data = df_des,
                  cluster = c("Country_Code"),
                  model = "random")
summary(mod4_redes)

# Considerando matriz robusta para heterocedasticidade e outros testes
bptest(mod1_redes)
bptest(mod2_redes)
bptest(mod3_redes)
bptest(mod4_redes)

summary(mod1_redes, vcov = vcovHC(mod1_redes, type = "HC1", cluster = "group"))
summary(mod2_redes, vcov = vcovHC(mod2_redes, type = "HC1", cluster = "group"))
summary(mod3_redes, vcov = vcovHC(mod3_redes, type = "HC1", cluster = "group"))
summary(mod4_redes, vcov = vcovHC(mod4_redes, type = "HC1", cluster = "group"))

linearHypothesis(mod4_redes, c("RRCambio=0", "RRCambio_lag1=0", "RRCambio_lag2=0", "RRCambio_lag3=0"),
                 vcov = vcovHC(mod4_redes, cluster = "group"))
linearHypothesis(mod4_redes, c("RRCambio=0", "RRCambio_lag1=0", "RRCambio_lag2=0", "RRCambio_lag3=0"))

linearHypothesis(mod4_fedes, c("RRCambio_lag1=0", "RRCambio_lag2=0", "RRCambio_lag3=0"),
                 vcov = vcovHC(mod4_fedes, cluster = "group"))
linearHypothesis(mod4_fedes, c("RRCambio_lag1=0", "RRCambio_lag2=0", "RRCambio_lag3=0"))


stargazer(mod1_redes, mod2_redes, mod3_redes, mod4_redes, type = "text")

# Testando correlação serial dos resudios, averigunado a necessidade de usar RE. 
pbgtest(mod1_redes, order = 1)
pbgtest(mod2_redes, order = 1)  
pbgtest(mod3_redes, order = 1)  
pbgtest(mod4_redes, order = 1)  

# Testando Random walk para os residous, averiguando a necessidade de usar RE.
rmod1_redes <- residuals(mod1_redes)
rmod1_redes <- data.frame(
  Observacao = 1:length(rmod1_redes),
  Residuos = rmod1_redes
)
rmod2_redes <- residuals(mod2_redes)
rmod2_redes <- data.frame(
  Observacao = 1:length(rmod2_redes),
  Residuos = rmod2_redes
)
rmod3_redes <- residuals(mod3_redes)
rmod3_redes <- data.frame(
  Observacao = 1:length(rmod3_redes),
  Residuos = rmod3_redes
)
rmod4_redes <- residuals(mod4_redes)
rmod4_redes <- data.frame(
  Observacao = 1:length(rmod4_redes),
  Residuos = rmod4_redes
)

ggplot(rmod1_redes, aes(x = Observacao, y = Residuos)) +
  geom_line(color = "blue", linewidth = 0.7) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", linewidth = 0.8) +
  labs(
    title = "Resíduos do Modelo 1 (RE - DES)",
    x = "Ordem das Observações",
    y = "Valores dos Resíduos"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
ggplot(rmod2_redes, aes(x = Observacao, y = Residuos)) +
  geom_line(color = "blue", linewidth = 0.7) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", linewidth = 0.8) +
  labs(
    title = "Resíduos do Modelo 2 (RE - DES)",
    x = "Ordem das Observações",
    y = "Valores dos Resíduos"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
ggplot(rmod3_redes, aes(x = Observacao, y = Residuos)) +
  geom_line(color = "blue", linewidth = 0.7) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", linewidth = 0.8) +
  labs(
    title = "Resíduos do Modelo 3 (RE - DES)",
    x = "Ordem das Observações",
    y = "Valores dos Resíduos"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
ggplot(rmod4_redes, aes(x = Observacao, y = Residuos)) +
  geom_line(color = "blue", linewidth = 0.7) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", linewidth = 0.8) +
  labs(
    title = "Resíduos do Modelo 4 (RE - DES)",
    x = "Ordem das Observações",
    y = "Valores dos Resíduos"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Conclusão comparativa
stargazer(mod1_polsdes, mod1_fedes, mod1_redes, type = "text")
stargazer(mod2_polsdes, mod2_fedes, mod2_redes, type = "text")
stargazer(mod3_polsdes, mod3_fedes, mod3_redes, type = "text")
stargazer(mod4_polsdes, mod4_fedes, mod4_redes, type = "text")

phtest(mod1_fedes, mod1_redes)
phtest(mod3_fedes, mod3_redes)
phtest(mod4_fedes, mod4_redes)

###
###
# Comparação ente des e subdes
stargazer(mod1_polsdes, mod1_polssub, type = "text")
stargazer(mod2_polsdes, mod2_polssub, type = "text")
stargazer(mod3_polsdes, mod3_polssub, type = "text")
stargazer(mod4_polsdes, mod4_polssub, type = "text")

###
###
# Graficos Finais
# Grafico de dispersão
df_filtrado <- df_panel %>%
  filter(
    RBalanca >= -400, 
    RRCambio < 4    
  )

df_filtrado$desenvolvido_pib <- factor(df_filtrado$desenvolvido_pib)
df_panel$desenvolvido_pib <- factor(df_panel$desenvolvido_pib)

ggplot(df_filtrado, aes(x = RRCambio, y = RBalanca, color = desenvolvido_pib)) +
  geom_point(alpha = 0.7, size = 3) +  
  scale_color_manual(
    values = c("0" = "#FF6B6B", "1" = "#4ECDC4"),  
    labels = c("0" = "Subdesenvolvidos", "1" = "Desenvolvidos"),
    name = "Status de Desenvolvimento"
  ) +
  labs(
    title = "Relação entre Taxa de Câmbio Real e Balança Comercial",
    x = "Retorno do Câmbio Real (RRCambio)",
    y = "Retorno da Balança Comercial (RBalanca)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    panel.grid.minor = element_blank()  
  ) 

df_BRA <- df_panel %>%
  filter(Country_Code == "BRA")  
ggplot(df_BRA, aes(x = RRCambio, y = RBalanca, color = desenvolvido_pib)) +
  geom_point(alpha = 0.7, size = 3) +
  scale_color_manual(
    values = c("0" = "#FF6B6B", "1" = "#4ECDC4"),
    labels = c("0" = "Subdesenvolvidos", "1" = "Desenvolvidos"),
    name = "Status de Desenvolvimento"
  ) +
  labs(
    title = paste("Relação entre Câmbio Real e Balança Comercial: Brasil"),
    x = "Retorno do Câmbio Real (RRCambio)",
    y = "Retorno da Balança Comercial (RBalanca)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

df_CHN <- df_panel %>%
  filter(Country_Code == "CHN")  
ggplot(df_CHN, aes(x = RRCambio, y = RBalanca, color = desenvolvido_pib)) +
  geom_point(alpha = 0.7, size = 3) +
  scale_color_manual(
    values = c("0" = "#FF6B6B", "1" = "#4ECDC4"),
    labels = c("0" = "Subdesenvolvidos", "1" = "Desenvolvidos"),
    name = "Status de Desenvolvimento"
  ) +
  labs(
    title = paste("Relação entre Câmbio Real e Balança Comercial: China"),
    x = "Retorno do Câmbio Real (RRCambio)",
    y = "Retorno da Balança Comercial (RBalanca)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

df_SAF <- df_panel %>%
  filter(Country_Code == "ZAF")  
ggplot(df_SAF, aes(x = RRCambio, y = RBalanca, color = desenvolvido_pib)) +
  geom_point(alpha = 0.7, size = 3) +
  scale_color_manual(
    values = c("0" = "#FF6B6B", "1" = "#4ECDC4"),
    labels = c("0" = "Subdesenvolvidos", "1" = "Desenvolvidos"),
    name = "Status de Desenvolvimento"
  ) +
  labs(
    title = paste("Relação entre Câmbio Real e Balança Comercial: Africa do Sul"),
    x = "Retorno do Câmbio Real (RRCambio)",
    y = "Retorno da Balança Comercial (RBalanca)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

df_IRN <- df_panel %>%
  filter(Country_Code == "IRN")  
ggplot(df_IRN, aes(x = RRCambio, y = RBalanca, color = desenvolvido_pib)) +
  geom_point(alpha = 0.7, size = 3) +
  scale_color_manual(
    values = c("0" = "#FF6B6B", "1" = "#4ECDC4"),
    labels = c("0" = "Subdesenvolvidos", "1" = "Desenvolvidos"),
    name = "Status de Desenvolvimento"
  ) +
  labs(
    title = paste("Relação entre Câmbio Real e Balança Comercial: Iran"),
    x = "Retorno do Câmbio Real (RRCambio)",
    y = "Retorno da Balança Comercial (RBalanca)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

df_CAN <- df_panel %>%
  filter(Country_Code == "CAN")  
ggplot(df_CAN, aes(x = RRCambio, y = RBalanca, color = desenvolvido_pib)) +
  geom_point(alpha = 0.7, size = 3) +
  scale_color_manual(
    values = c("0" = "#FF6B6B", "1" = "#4ECDC4"),
    labels = c("0" = "Subdesenvolvidos", "1" = "Desenvolvidos"),
    name = "Status de Desenvolvimento"
  ) +
  labs(
    title = paste("Relação entre Câmbio Real e Balança Comercial: Canadá"),
    x = "Retorno do Câmbio Real (RRCambio)",
    y = "Retorno da Balança Comercial (RBalanca)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

df_GBR <- df_panel %>%
  filter(Country_Code == "GBR")  
ggplot(df_GBR, aes(x = RRCambio, y = RBalanca, color = desenvolvido_pib)) +
  geom_point(alpha = 0.7, size = 3) +
  scale_color_manual(
    values = c("0" = "#FF6B6B", "1" = "#4ECDC4"),
    labels = c("0" = "Subdesenvolvidos", "1" = "Desenvolvidos"),
    name = "Status de Desenvolvimento"
  ) +
  labs(
    title = paste("Relação entre Câmbio Real e Balança Comercial: Reino Unido"),
    x = "Retorno do Câmbio Real (RRCambio)",
    y = "Retorno da Balança Comercial (RBalanca)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

df_DEU <- df_panel %>%
  filter(Country_Code == "DEU")  
ggplot(df_DEU, aes(x = RRCambio, y = RBalanca, color = desenvolvido_pib)) +
  geom_point(alpha = 0.7, size = 3) +
  scale_color_manual(
    values = c("0" = "#FF6B6B", "1" = "#4ECDC4"),
    labels = c("0" = "Subdesenvolvidos", "1" = "Desenvolvidos"),
    name = "Status de Desenvolvimento"
  ) +
  labs(
    title = paste("Relação entre Câmbio Real e Balança Comercial: Alemanha"),
    x = "Retorno do Câmbio Real (RRCambio)",
    y = "Retorno da Balança Comercial (RBalanca)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

df_JPN <- df_panel %>%
  filter(Country_Code == "JPN")  
ggplot(df_JPN, aes(x = RRCambio, y = RBalanca, color = desenvolvido_pib)) +
  geom_point(alpha = 0.7, size = 3) +
  scale_color_manual(
    values = c("0" = "#FF6B6B", "1" = "#4ECDC4"),
    labels = c("0" = "Subdesenvolvidos", "1" = "Desenvolvidos"),
    name = "Status de Desenvolvimento"
  ) +
  labs(
    title = paste("Relação entre Câmbio Real e Balança Comercial: Japão"),
    x = "Retorno do Câmbio Real (RRCambio)",
    y = "Retorno da Balança Comercial (RBalanca)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

lista_des <- list(
  Canadá = df_CAN,  
  Japão = df_JPN,
  Alemanha = df_DEU,
  Inglaterra = df_GBR
)
df_grad <- bind_rows(lista_des, .id = "Pais")
ggplot(df_grad, aes(x = Year, y = Balanca_PIB, color = Pais, group = Pais)) +
  geom_line(linewidth = 1.2, alpha = 0.8) +
  geom_point(size = 2) +  
  scale_color_manual(
    values = c(
      "Canadá" = "#E41A1C",    # Vermelho
      "Japão" = "#377EB8",      # Azul
      "Alemanha" = "#4DAF4A",   # Verde
      "Inglaterra" = "#984EA3"  # Roxo
    ),
    
  ) +
  labs(
    title = "Evolução da Balança Comercial",
    x = "Ano",
    y = "Balança Comercial",
    color = "País"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank()
  )
df_grad <- df_grad %>%
  mutate(Year = as.numeric(as.character(Year)))
ggplot(df_grad %>% filter(Year > (min(Year) + 20)), 
       aes(x = Year, y = Real_Exchange, color = Pais)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(
    values = c("Canadá" = "#E41A1C", "Japão" = "#377EB8",
               "Alemanha" = "#4DAF4A", "Inglaterra" = "#984EA3")
  ) +
  labs(
    title = "Evolução do Câmbio Real",
    x = "Ano",
    y = "Câmbio Real",
    color = "País"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.grid.minor = element_blank()
  )



lista_sub <- list(
  Brasil = df_BRA,  
  Africa_do_Sul = df_SAF,
  China = df_CHN,
  Iran = df_IRN
)
df_gras <- bind_rows(lista_sub, .id = "Pais")
ggplot(df_gras, aes(x = Year, y = Balanca_PIB, color = Pais, group = Pais)) +
  geom_line(linewidth = 1.2, alpha = 0.8) +
  geom_point(size = 2) +  
  scale_color_manual(
    values = c(
      "Brasil" = "#1F77B4",        # Azul
      "Africa_do_Sul" = "#FF7F0E", # Laranja
      "China" = "#2CA02C",         # Verde
      "Iran" = "#D62728"            # Vermelho
    ),
    
  ) +
  labs(
    title = "Evolução da Balança Comercial",
    x = "Ano",
    y = "Balança Comercial",
    color = "País"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank()
  )
df_gras <- df_gras %>%
  mutate(Year = as.numeric(as.character(Year)))
ggplot(df_gras %>% filter(Year > (min(Year) + 20)), 
       aes(x = Year, y = Real_Exchange, color = Pais)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(
    values = c(
      "Brasil" = "#1F77B4",        # Azul
      "Africa_do_Sul" = "#FF7F0E", # Laranja
      "China" = "#2CA02C",         # Verde
      "Iran" = "#D62728"            # Vermelho
  ) )+
  labs(
    title = "Evolução do Câmbio Real",
    x = "Ano",
    y = "Câmbio Real",
    color = "País"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.grid.minor = element_blank()
  )

# Grafico efeitos
model_tidy <- tidy(mod4_polssub, conf.int = TRUE, conf.level = 0.90)
cambio_vars <- c("RRCambio", "RRCambio_lag1", "RRCambio_lag2", "RRCambio_lag3")
model_cambio <- model_tidy %>% 
  filter(term %in% cambio_vars)
ggplot(model_cambio, aes(x = estimate, y = term, color = term)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2, size = 1) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  scale_y_discrete(labels = c("Câmbio (t)", "Câmbio (t-1)", "Câmbio (t-2)", "Câmbio (t-3)")) +
  labs(title = "Impacto do Câmbio e Seus Lags na Balança Comercial - POLS (SUBDES)",
       x = "Coeficiente Estimado",
       y = "Variável",
       caption = "Intervalos de confiança de 90%") +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid.major.y = element_blank())

model_tidy <- tidy(mod4_fesub, conf.int = TRUE, conf.level = 0.90)
cambio_vars <- c("RRCambio", "RRCambio_lag1", "RRCambio_lag2", "RRCambio_lag3")
model_cambio <- model_tidy %>% 
  filter(term %in% cambio_vars)
ggplot(model_cambio, aes(x = estimate, y = term, color = term)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2, size = 1) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  scale_y_discrete(labels = c("Câmbio (t)", "Câmbio (t-1)", "Câmbio (t-2)", "Câmbio (t-3)")) +
  labs(title = "Impacto do Câmbio e Seus Lags na Balança Comercial - POLS e RE (SUBDES)",
       x = "Coeficiente Estimado",
       y = "Variável",
       caption = "Intervalos de confiança de 90%") +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid.major.y = element_blank())

model_tidy <- tidy(mod4_resub, conf.int = TRUE, conf.level = 0.90)
cambio_vars <- c("RRCambio", "RRCambio_lag1", "RRCambio_lag2", "RRCambio_lag3")
model_cambio <- model_tidy %>% 
  filter(term %in% cambio_vars)
ggplot(model_cambio, aes(x = estimate, y = term, color = term)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2, size = 1) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  scale_y_discrete(labels = c("Câmbio (t)", "Câmbio (t-1)", "Câmbio (t-2)", "Câmbio (t-3)")) +
  labs(title = "Impacto do Câmbio e Seus Lags na Balança Comercial - RE (SUBDES)",
       x = "Coeficiente Estimado",
       y = "Variável",
       caption = "Intervalos de confiança de 90%") +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid.major.y = element_blank())

model_tidy <- tidy(mod4_polsdes, conf.int = TRUE, conf.level = 0.90)
cambio_vars <- c("RRCambio", "RRCambio_lag1", "RRCambio_lag2", "RRCambio_lag3")
model_cambio <- model_tidy %>% 
  filter(term %in% cambio_vars)
ggplot(model_cambio, aes(x = estimate, y = term, color = term)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2, size = 1) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  scale_y_discrete(labels = c("Câmbio (t)", "Câmbio (t-1)", "Câmbio (t-2)", "Câmbio (t-3)")) +
  labs(title = "Impacto do Câmbio e Seus Lags na Balança Comercial - POLS (DES)",
       x = "Coeficiente Estimado",
       y = "Variável",
       caption = "Intervalos de confiança de 90%") +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid.major.y = element_blank())

model_tidy <- tidy(mod4_fedes, conf.int = TRUE, conf.level = 0.90)
cambio_vars <- c("RRCambio", "RRCambio_lag1", "RRCambio_lag2", "RRCambio_lag3")
model_cambio <- model_tidy %>% 
  filter(term %in% cambio_vars)
ggplot(model_cambio, aes(x = estimate, y = term, color = term)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2, size = 1) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  scale_y_discrete(labels = c("Câmbio (t)", "Câmbio (t-1)", "Câmbio (t-2)", "Câmbio (t-3)")) +
  labs(title = "Impacto do Câmbio e Seus Lags na Balança Comercial - FE (DES)",
       x = "Coeficiente Estimado",
       y = "Variável",
       caption = "Intervalos de confiança de 90%") +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid.major.y = element_blank())

model_tidy <- tidy(mod4_redes, conf.int = TRUE, conf.level = 0.90)
cambio_vars <- c("RRCambio", "RRCambio_lag1", "RRCambio_lag2", "RRCambio_lag3")
model_cambio <- model_tidy %>% 
  filter(term %in% cambio_vars)
ggplot(model_cambio, aes(x = estimate, y = term, color = term)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2, size = 1) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  scale_y_discrete(labels = c("Câmbio (t)", "Câmbio (t-1)", "Câmbio (t-2)", "Câmbio (t-3)")) +
  labs(title = "Impacto do Câmbio e Seus Lags na Balança Comercial - RE (DES)",
       x = "Coeficiente Estimado",
       y = "Variável",
       caption = "Intervalos de confiança de 90%") +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid.major.y = element_blank())


coef_data <- data.frame(
  Lag = 0:3,
  Efeito = c(coef(mod4_polssub)["RRCambio"],
             coef(mod4_polssub)["RRCambio_lag1"],
             coef(mod4_polssub)["RRCambio_lag2"],
             coef(mod4_polssub)["RRCambio_lag3"]),
  SE = c(sqrt(diag(vcov(mod4_polssub)))["RRCambio"],
         sqrt(diag(vcov(mod4_polssub)))["RRCambio_lag1"],
         sqrt(diag(vcov(mod4_polssub)))["RRCambio_lag2"],
         sqrt(diag(vcov(mod4_polssub)))["RRCambio_lag3"])
)
coef_data <- coef_data %>%
  mutate(
    CI_lower = Efeito - 1.645 * SE,
    CI_upper = Efeito + 1.645 * SE
  )
ggplot(coef_data, aes(x = Lag, y = Efeito)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_line(color = "#1f77b4", linewidth = 1.2) +
  geom_point(size = 3, color = "#1f77b4") +
  geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper), 
                width = 0.1, 
                color = "#1f77b4",
                linewidth = 0.8) +
  geom_label(aes(label = round(Efeito, 3)), 
             vjust = -1.2, 
             color = "#1f77b4",
             size = 3.5) +
  scale_x_continuous(breaks = 0:3, 
                     labels = c("t (atual)", "t-1", "t-2", "t-3")) +
  labs(title = "Efeito Dinâmico do Câmbio na Balança Comercial - POLS (SUB)",
       subtitle = "Coeficientes e intervalos de confiança de 90%",
       x = "Defasagem Cambial",
       y = "Impacto na Balança Comercial",
       caption = "Linha vermelha tracejada indica efeito nulo") +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold"),
    axis.text = element_text(color = "black")
  )

coef_data <- data.frame(
  Lag = 0:3,
  Efeito = c(coef(mod4_fesub)["RRCambio"],
             coef(mod4_fesub)["RRCambio_lag1"],
             coef(mod4_fesub)["RRCambio_lag2"],
             coef(mod4_fesub)["RRCambio_lag3"]),
  SE = c(sqrt(diag(vcov(mod4_fesub)))["RRCambio"],
         sqrt(diag(vcov(mod4_fesub)))["RRCambio_lag1"],
         sqrt(diag(vcov(mod4_fesub)))["RRCambio_lag2"],
         sqrt(diag(vcov(mod4_fesub)))["RRCambio_lag3"])
)
coef_data <- coef_data %>%
  mutate(
    CI_lower = Efeito - 1.645 * SE,
    CI_upper = Efeito + 1.645 * SE
  )
ggplot(coef_data, aes(x = Lag, y = Efeito)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_line(color = "#1f77b4", linewidth = 1.2) +
  geom_point(size = 3, color = "#1f77b4") +
  geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper), 
                width = 0.1, 
                color = "#1f77b4",
                linewidth = 0.8) +
  geom_label(aes(label = round(Efeito, 3)), 
             vjust = -1.2, 
             color = "#1f77b4",
             size = 3.5) +
  scale_x_continuous(breaks = 0:3, 
                     labels = c("t (atual)", "t-1", "t-2", "t-3")) +
  labs(title = "Efeito Dinâmico do Câmbio na Balança Comercial - FE (SUB)",
       subtitle = "Coeficientes e intervalos de confiança de 90%",
       x = "Defasagem Cambial",
       y = "Impacto na Balança Comercial",
       caption = "Linha vermelha tracejada indica efeito nulo") +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold"),
    axis.text = element_text(color = "black")
  )

coef_data <- data.frame(
  Lag = 0:3,
  Efeito = c(coef(mod4_resub)["RRCambio"],
             coef(mod4_resub)["RRCambio_lag1"],
             coef(mod4_resub)["RRCambio_lag2"],
             coef(mod4_resub)["RRCambio_lag3"]),
  SE = c(sqrt(diag(vcov(mod4_resub)))["RRCambio"],
         sqrt(diag(vcov(mod4_resub)))["RRCambio_lag1"],
         sqrt(diag(vcov(mod4_resub)))["RRCambio_lag2"],
         sqrt(diag(vcov(mod4_resub)))["RRCambio_lag3"])
)
coef_data <- coef_data %>%
  mutate(
    CI_lower = Efeito - 1.645 * SE,
    CI_upper = Efeito + 1.645 * SE
  )
ggplot(coef_data, aes(x = Lag, y = Efeito)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_line(color = "#1f77b4", linewidth = 1.2) +
  geom_point(size = 3, color = "#1f77b4") +
  geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper), 
                width = 0.1, 
                color = "#1f77b4",
                linewidth = 0.8) +
  geom_label(aes(label = round(Efeito, 3)), 
             vjust = -1.2, 
             color = "#1f77b4",
             size = 3.5) +
  scale_x_continuous(breaks = 0:3, 
                     labels = c("t (atual)", "t-1", "t-2", "t-3")) +
  labs(title = "Efeito Dinâmico do Câmbio na Balança Comercial - RE (SUB)",
       subtitle = "Coeficientes e intervalos de confiança de 90%",
       x = "Defasagem Cambial",
       y = "Impacto na Balança Comercial",
       caption = "Linha vermelha tracejada indica efeito nulo") +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold"),
    axis.text = element_text(color = "black")
  )

coef_data <- data.frame(
  Lag = 0:3,
  Efeito = c(coef(mod4_polsdes)["RRCambio"],
             coef(mod4_polsdes)["RRCambio_lag1"],
             coef(mod4_polsdes)["RRCambio_lag2"],
             coef(mod4_polsdes)["RRCambio_lag3"]),
  SE = c(sqrt(diag(vcov(mod4_polsdes)))["RRCambio"],
         sqrt(diag(vcov(mod4_polsdes)))["RRCambio_lag1"],
         sqrt(diag(vcov(mod4_polsdes)))["RRCambio_lag2"],
         sqrt(diag(vcov(mod4_polsdes)))["RRCambio_lag3"])
)
coef_data <- coef_data %>%
  mutate(
    CI_lower = Efeito - 1.645 * SE,
    CI_upper = Efeito + 1.645 * SE
  )
ggplot(coef_data, aes(x = Lag, y = Efeito)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_line(color = "#1f77b4", linewidth = 1.2) +
  geom_point(size = 3, color = "#1f77b4") +
  geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper), 
                width = 0.1, 
                color = "#1f77b4",
                linewidth = 0.8) +
  geom_label(aes(label = round(Efeito, 3)), 
             vjust = -1.2, 
             color = "#1f77b4",
             size = 3.5) +
  scale_x_continuous(breaks = 0:3, 
                     labels = c("t (atual)", "t-1", "t-2", "t-3")) +
  labs(title = "Efeito Dinâmico do Câmbio na Balança Comercial - POLS (DES)",
       subtitle = "Coeficientes e intervalos de confiança de 90%",
       x = "Defasagem Cambial",
       y = "Impacto na Balança Comercial",
       caption = "Linha vermelha tracejada indica efeito nulo") +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold"),
    axis.text = element_text(color = "black")
  )

coef_data <- data.frame(
  Lag = 0:3,
  Efeito = c(coef(mod4_fedes)["RRCambio"],
             coef(mod4_fedes)["RRCambio_lag1"],
             coef(mod4_fedes)["RRCambio_lag2"],
             coef(mod4_fedes)["RRCambio_lag3"]),
  SE = c(sqrt(diag(vcov(mod4_fedes)))["RRCambio"],
         sqrt(diag(vcov(mod4_fedes)))["RRCambio_lag1"],
         sqrt(diag(vcov(mod4_fedes)))["RRCambio_lag2"],
         sqrt(diag(vcov(mod4_fedes)))["RRCambio_lag3"])
)
coef_data <- coef_data %>%
  mutate(
    CI_lower = Efeito - 1.645 * SE,
    CI_upper = Efeito + 1.645 * SE
  )
ggplot(coef_data, aes(x = Lag, y = Efeito)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_line(color = "#1f77b4", linewidth = 1.2) +
  geom_point(size = 3, color = "#1f77b4") +
  geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper), 
                width = 0.1, 
                color = "#1f77b4",
                linewidth = 0.8) +
  geom_label(aes(label = round(Efeito, 3)), 
             vjust = -1.2, 
             color = "#1f77b4",
             size = 3.5) +
  scale_x_continuous(breaks = 0:3, 
                     labels = c("t (atual)", "t-1", "t-2", "t-3")) +
  labs(title = "Efeito Dinâmico do Câmbio na Balança Comercial - FE (DES)",
       subtitle = "Coeficientes e intervalos de confiança de 90%",
       x = "Defasagem Cambial",
       y = "Impacto na Balança Comercial",
       caption = "Linha vermelha tracejada indica efeito nulo") +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold"),
    axis.text = element_text(color = "black")
  )

coef_data <- data.frame(
  Lag = 0:3,
  Efeito = c(coef(mod4_redes)["RRCambio"],
             coef(mod4_redes)["RRCambio_lag1"],
             coef(mod4_redes)["RRCambio_lag2"],
             coef(mod4_redes)["RRCambio_lag3"]),
  SE = c(sqrt(diag(vcov(mod4_redes)))["RRCambio"],
         sqrt(diag(vcov(mod4_redes)))["RRCambio_lag1"],
         sqrt(diag(vcov(mod4_redes)))["RRCambio_lag2"],
         sqrt(diag(vcov(mod4_redes)))["RRCambio_lag3"])
)
coef_data <- coef_data %>%
  mutate(
    CI_lower = Efeito - 1.645 * SE,
    CI_upper = Efeito + 1.645 * SE
  )
ggplot(coef_data, aes(x = Lag, y = Efeito)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_line(color = "#1f77b4", linewidth = 1.2) +
  geom_point(size = 3, color = "#1f77b4") +
  geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper), 
                width = 0.1, 
                color = "#1f77b4",
                linewidth = 0.8) +
  geom_label(aes(label = round(Efeito, 3)), 
             vjust = -1.2, 
             color = "#1f77b4",
             size = 3.5) +
  scale_x_continuous(breaks = 0:3, 
                     labels = c("t (atual)", "t-1", "t-2", "t-3")) +
  labs(title = "Efeito Dinâmico do Câmbio na Balança Comercial - RE (DES)",
       subtitle = "Coeficientes e intervalos de confiança de 90%",
       x = "Defasagem Cambial",
       y = "Impacto na Balança Comercial",
       caption = "Linha vermelha tracejada indica efeito nulo") +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold"),
    axis.text = element_text(color = "black")
  )


coef_cambio <- data.frame(
  Lag = 0:3,
  Efeito = c(coef(mod4_polssub)["RRCambio"],
             coef(mod4_polssub)["RRCambio_lag1"],
             coef(mod4_polssub)["RRCambio_lag2"],
             coef(mod4_polssub)["RRCambio_lag3"]),
  stringsAsFactors = FALSE
)
ggplot(coef_cambio, aes(x = Lag, y = Efeito)) +
  geom_line(color = "#2C3E50", linewidth = 1.2) +
  geom_point(color = "#E41A1C", size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  scale_x_continuous(
    breaks = 0:3,
    labels = c("Atual (t)", "Lag 1 (t-1)", "Lag 2 (t-2)", "Lag 3 (t-3)")
  ) +
  labs(
    title = "Efeito Dinâmico do Câmbio na Balança Comercial - POLS (SUB)",
    x = "Defasagem Temporal",
    y = "Coeficiente Estimado"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.x = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5, size = 13),
    axis.title = element_text(size = 12),
    axis.text = element_text(color = "black", size = 11)
  )

coef_cambio <- data.frame(
  Lag = 0:3,
  Efeito = c(coef(mod4_fesub)["RRCambio"],
             coef(mod4_fesub)["RRCambio_lag1"],
             coef(mod4_fesub)["RRCambio_lag2"],
             coef(mod4_fesub)["RRCambio_lag3"]),
  stringsAsFactors = FALSE
)
ggplot(coef_cambio, aes(x = Lag, y = Efeito)) +
  geom_line(color = "#2C3E50", linewidth = 1.2) +
  geom_point(color = "#E41A1C", size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  scale_x_continuous(
    breaks = 0:3,
    labels = c("Atual (t)", "Lag 1 (t-1)", "Lag 2 (t-2)", "Lag 3 (t-3)")
  ) +
  labs(
    title = "Efeito Dinâmico do Câmbio na Balança Comercial - FE (SUB)",
    x = "Defasagem Temporal",
    y = "Coeficiente Estimado"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.x = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5, size = 13),
    axis.title = element_text(size = 12),
    axis.text = element_text(color = "black", size = 11)
  )

coef_cambio <- data.frame(
  Lag = 0:3,
  Efeito = c(coef(mod4_resub)["RRCambio"],
             coef(mod4_resub)["RRCambio_lag1"],
             coef(mod4_resub)["RRCambio_lag2"],
             coef(mod4_resub)["RRCambio_lag3"]),
  stringsAsFactors = FALSE
)
ggplot(coef_cambio, aes(x = Lag, y = Efeito)) +
  geom_line(color = "#2C3E50", linewidth = 1.2) +
  geom_point(color = "#E41A1C", size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  scale_x_continuous(
    breaks = 0:3,
    labels = c("Atual (t)", "Lag 1 (t-1)", "Lag 2 (t-2)", "Lag 3 (t-3)")
  ) +
  labs(
    title = "Efeito Dinâmico do Câmbio na Balança Comercial - RE (SUB)",
    x = "Defasagem Temporal",
    y = "Coeficiente Estimado"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.x = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5, size = 13),
    axis.title = element_text(size = 12),
    axis.text = element_text(color = "black", size = 11)
  )

coef_cambio <- data.frame(
  Lag = 0:3,
  Efeito = c(coef(mod4_polsdes)["RRCambio"],
             coef(mod4_polsdes)["RRCambio_lag1"],
             coef(mod4_polsdes)["RRCambio_lag2"],
             coef(mod4_polsdes)["RRCambio_lag3"]),
  stringsAsFactors = FALSE
)
ggplot(coef_cambio, aes(x = Lag, y = Efeito)) +
  geom_line(color = "#2C3E50", linewidth = 1.2) +
  geom_point(color = "#E41A1C", size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  scale_x_continuous(
    breaks = 0:3,
    labels = c("Atual (t)", "Lag 1 (t-1)", "Lag 2 (t-2)", "Lag 3 (t-3)")
  ) +
  labs(
    title = "Efeito Dinâmico do Câmbio na Balança Comercial - POLS (DES)",
    x = "Defasagem Temporal",
    y = "Coeficiente Estimado"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.x = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5, size = 13),
    axis.title = element_text(size = 12),
    axis.text = element_text(color = "black", size = 11)
  )

coef_cambio <- data.frame(
  Lag = 0:3,
  Efeito = c(coef(mod4_fedes)["RRCambio"],
             coef(mod4_fedes)["RRCambio_lag1"],
             coef(mod4_fedes)["RRCambio_lag2"],
             coef(mod4_fedes)["RRCambio_lag3"]),
  stringsAsFactors = FALSE
)
ggplot(coef_cambio, aes(x = Lag, y = Efeito)) +
  geom_line(color = "#2C3E50", linewidth = 1.2) +
  geom_point(color = "#E41A1C", size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  scale_x_continuous(
    breaks = 0:3,
    labels = c("Atual (t)", "Lag 1 (t-1)", "Lag 2 (t-2)", "Lag 3 (t-3)")
  ) +
  labs(
    title = "Efeito Dinâmico do Câmbio na Balança Comercial - FE (DES)",
    x = "Defasagem Temporal",
    y = "Coeficiente Estimado"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.x = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5, size = 13),
    axis.title = element_text(size = 12),
    axis.text = element_text(color = "black", size = 11)
  )

coef_cambio <- data.frame(
  Lag = 0:3,
  Efeito = c(coef(mod4_redes)["RRCambio"],
             coef(mod4_redes)["RRCambio_lag1"],
             coef(mod4_redes)["RRCambio_lag2"],
             coef(mod4_redes)["RRCambio_lag3"]),
  stringsAsFactors = FALSE
)
ggplot(coef_cambio, aes(x = Lag, y = Efeito)) +
  geom_line(color = "#2C3E50", linewidth = 1.2) +
  geom_point(color = "#E41A1C", size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  scale_x_continuous(
    breaks = 0:3,
    labels = c("Atual (t)", "Lag 1 (t-1)", "Lag 2 (t-2)", "Lag 3 (t-3)")
  ) +
  labs(
    title = "Efeito Dinâmico do Câmbio na Balança Comercial - RE (DES)",
    x = "Defasagem Temporal",
    y = "Coeficiente Estimado"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.x = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5, size = 13),
    axis.title = element_text(size = 12),
    axis.text = element_text(color = "black", size = 11)
  )

###
###
# Latex
stargazer(mod1_polssub, mod2_polssub, mod3_polssub, mod4_polssub, type = "latex")
stargazer(mod1_fesub, mod2_fesub, mod3_fesub, mod4_fesub, type = "latex")
stargazer(mod1_resub, mod2_resub, mod3_resub, mod4_resub, type = "latex")
stargazer(mod1_polssub, mod1_fesub, mod1_resub, type = "latex")
stargazer(mod2_polssub, mod2_fesub, mod2_resub, type = "latex")
stargazer(mod3_polssub, mod3_fesub, mod3_resub, type = "latex")
stargazer(mod4_polssub, mod4_fesub, mod4_resub, type = "latex")

stargazer(mod1_polsdes, mod2_polsdes, mod3_polsdes, mod4_polsdes, type = "latex")
stargazer(mod1_fedes, mod2_fedes, mod3_fedes, mod4_fedes, type = "latex")
stargazer(mod1_redes, mod2_redes, mod3_redes, mod4_redes, type = "latex")
stargazer(mod1_polsdes, mod1_fedes, mod1_redes, type = "latex")
stargazer(mod2_polsdes, mod2_fedes, mod2_redes, type = "latex")
stargazer(mod3_polsdes, mod3_fedes, mod3_redes, type = "latex")
stargazer(mod4_polsdes, mod4_fedes, mod4_redes, type = "latex")

###
###
# Tabela correlação
dados_cor <- df_panel %>% 
  select(RBalanca, RRCambio, RRCambio_lag1, RRCambio_lag2, RRCambio_lag3,
         RPIBC, RJuros, RConsumo, RReservas, RInflação, RGasto)
cor_matrix <- cor(dados_cor, use = "complete.obs")
ggcorrplot(
  cor_matrix,
  hc.order = TRUE,
  type = "lower",
  lab = TRUE,
  lab_size = 3,
  colors = c("#6D9EC1", "white", "#E46726"),
  title = "Matriz de Correlação"
) +
  theme_minimal() +
  theme(
    # Mantém os nomes das variáveis, mas remove os títulos dos eixos (VAR1/VAR2)
    axis.title.x = element_blank(),  # Remove "VAR1" (eixo X)
    axis.title.y = element_blank(),  # Remove "VAR2" (eixo Y)
    
    # Ajustes opcionais para os nomes das variáveis:
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10, color = "black"),
    axis.text.y = element_text(size = 10, color = "black"),
    
    # Configurações do tema:
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    panel.grid = element_blank(),
    panel.background = element_blank()
  )
