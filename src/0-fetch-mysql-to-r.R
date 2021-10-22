library(here)
library(tidyverse)
library(dbplyr)

# conecta com banco de dados AeP
source(here("local-files/conecta-mydb.R"), encoding = "utf-8")

# `mysql2tibble()` envia uma query para o BD AeP (string) e retorna tabela como uma tibble
mysql2tibble <- function(mysql_query)  DBI::dbGetQuery(mydb, mysql_query) %>% as_tibble()

# `cod_pedidos_ativos` é uma lista de códigos de pedidos ativos
cod_pedidos_ativos <- tbl(mydb, "pedidos") %>%
  filter(Ativo == 1) %>%
  pull(Codigo)

# strings com as queries
source(here("src/mysql-queries/pedidos.R"), encoding = "utf-8")
source(here("src/mysql-queries/pedidos_interacoes.R"), encoding = "utf-8")

# Tabela `pedidos` do banco de dados do AeP
pedidos <- mysql2tibble(query_pedidos)

# Tabela `pedidos_interacoes` do banco de dados do AeP
interac <- mysql2tibble(query_pedidos_interacoes)

# Tabela `tipo_pedido_resposta` do banco de dados AeP
tp_resp <- tbl(mydb, "tipo_pedido_resposta") %>%
  select(-Criacao) %>% 
  collect()

# Tabela `agentes` do banco de dados AeP  
agentes <- tbl(mydb, "agentes") %>% 
  select(-Criacao, -Alteracao) %>% 
  collect()

# **`tblai` é uma lista de `tibbles` extraídas do banco de dados do AeP**
# - Tabela `pedidos` do banco de dados do AeP
# - Tabela `pedidos_interacoes` do banco de dados do AeP
# - Tabela `agentes` do banco de dados AeP
tblai <- list(
  pedidos = pedidos,
  interac = interac,
  tp_resp = tp_resp,
  agentes = agentes
)

save(tblai, file = here("local-files/tblai.RData"))
