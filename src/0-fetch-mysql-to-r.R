library(here)
library(tidyverse)
library(dbplyr)

# conecta ----------------------------------------------------------------------
source(here("local-files/conecta-mydb.R"))

# carrega tabelas do banco de dados --------------------------------------------
tblai_tb <- DBI::dbListTables(mydb)

pedidos <- tbl(mydb, "pedidos")
interac <- tbl(mydb, "pedidos_interacoes")
tp_resp <- tbl(mydb, "tipo_pedido_resposta")
agentes <- tbl(mydb, "agentes")

# código pedidos ativos
cod_pedidos_ativos <- pedidos %>% filter(Ativo == 1) %>% pull(Codigo)

# fetch no R -------------------------------------------------------------------
pedidos <- pedidos %>%
  filter(Codigo %in% cod_pedidos_ativos) %>% 
  collect() %>%
  select(-Criacao, -Alteracao)

interac <- interac %>%
  filter(CodigoPedido %in% cod_pedidos_ativos) %>% 
  collect() %>% 
  select(-Criacao, -Alteracao)

tp_resp <- tp_resp %>%
  collect() %>%
  select(-Criacao)

agentes <- agentes %>%
  collect() %>% 
  select(-Criacao, -Alteracao)

# mini-tblai -------------------------------------------------------------------
tblai <- list(
  tblai_tb = tblai_tb,
  pedidos = pedidos,
  interac = interac,
  tp_resp = tp_resp,
  agentes = agentes
)

save(tblai, file = here("local-files/tblai.RData"))
