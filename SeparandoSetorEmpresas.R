library(tidyr)
library(dplyr)
library(tidyverse)
library(GetDFPData2)
library(BatchGetSymbols)

# Informações das Empresas

df_info <- get_info_companies()
names(df_info)
print(df_info)

#setores
df_comercio <- df_info %>%
  filter(SETOR_ATIV == "Comércio (Atacado e Varejo)" & SIT_REG == "ATIVO")

df_farmacia <- df_info %>%
  filter(SETOR_ATIV == "Farmacêutico e Higiene" & SIT_REG == "ATIVO")

#csv
write.csv2(df_comercio, file = "SetorComercioATIVO.csv", row.names = FALSE)
write.csv2(df_farmacia, file = "SetorFarmaceuticoATIVO.csv", row.names = FALSE)


# Qtd Empresas Ativas, Canceladas e Suspensas

df_info %>%
  count(SIT_REG)

# Qtd Empresas por Setor e Segmentos de Governança Corporativas

df_info %>%
  count(SETOR_ATIV) %>%
  arrange(desc(n))
  
# Selecionar Empresas Ativas de um Determinado Setor

df_info %>%
  group_by(SETOR_ATIV) %>%
  filter(SIT_REG == "ATIVO" & SETOR_ATIV == "Bancos")

# Pesquisando Informações Básicas de uma Empresa

df_search <- search_company('MAGAZINE LUIZA SA')
print(df_search)

# Baixando Dados DFP com Base no Código CVM da Empresa - CD_CVM
## Set opções

id_companies <- 24783
first_year <- 2022
last_year <- 2023

# Download Dados

## AENTçÂO
# Símbolo do tipo de documento financeiro a ser retornado. 
# Definições: ’*’ = retorna todos documentos, ‘BPA’ = Ativo, 
# ‘BPP’ = passivo, ‘DRE’ = demonstrativo de resultados do exercício, 
# ‘DFC_MD’ = fluxo de caixa pelo metodo direto, 
# ‘DFC_MI’ = fluxo de caixa pelo metodo indireto, 
# ‘DMPL’ = mutacoes do patrimonio liquido, 
# ‘DVA’ = demonstrativo de valor agregado.

l_dfp <- get_dfp_data(companies_cvm_codes = id_companies,
                      type_docs = '*', # pegar todos
                      type_format = 'con', # consolidade
                      first_year = first_year,
                      last_year = last_year)

l_dfp <- purrr::map(l_dfp, function(df) {
  if ("DT_REFER" %in% colnames(df)) {
    df$DT_REFER <- as.Date(df$DT_REFER, format = "%Y-%m-%d")
  }
  return(df)
})


str(l_dfp)

# Salvar BPA em df
BPA_Natura <- l_dfp$`DF Consolidado - Balanço Patrimonial Ativo`
write.csv2(BPA_Natura, file = "BalancoPatrimonialAtivoNatura.csv", row.names = FALSE)
# Verificando
print(fr_assests)
glimpse(fr_assests)
str(fr_assests)
View(fr_assests)

l_dfp_grandene <- get_dfp_data(companies_cvm_codes = 22470,
                      type_docs = c('BPA'),
                      type_format = 'con',
                      first_year = 2023,
                      last_year = 2024)


l_dfp_grandene <- as.data.frame(l_dfp_grandene)
View(l_dfp_grandene)

# Salvar BPP em df

BPP_magalu <- l_dfp$`DF Consolidado - Balanço Patrimonial Passivo`
write.csv2(BPP_magalu, file = "BalancoPatrimonialPassivoagalu.csv", row.names = FALSE)
# Verificando

print(fr_passiv)
glimpse(fr_passiv)
str(fr_passiv)
View(fr_passiv)

# Ajustando df do Ativo para Cálculo de Índice
fr_assests_1 <- BPA_magalu %>%
  select(DT_REFER, CD_CONTA, DS_CONTA, VL_CONTA)

At_total <- fr_assests_1[1,4]
At_circulante <- fr_assests_1[2,4]

# Ajustando df do Passivo para Cálculo de Índice
fr_passiv_1 <- fr_passiv %>%
  select(DT_REFER, CD_CONTA, DS_CONTA, VL_CONTA)

Ps_toal <- fr_passiv_1[1,4]
Ps_circulante <- fr_passiv_1[2,4]

# Índice de Liquez
At_circulante/Ps_circulante
