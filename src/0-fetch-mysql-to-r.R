library(here)
library(tidyverse)
library(dbplyr)
`%notin%` <- function(x, y) !(x %in% y)

# conecta ----------------------------------------------------------------------
source(here("local-files/conecta-mydb.R"))

# carrega tabelas do banco de dados --------------------------------------------
tblai <- DBI::dbListTables(mydb)

pedidos <- tbl(mydb, "pedidos")
interac <- tbl(mydb, "pedidos_interacoes")
tp_resp <- tbl(mydb, "tipo_pedido_resposta")
agentes <- tbl(mydb, "agentes")

# código pedidos ativos
cod_pedidos_ativos <- pedidos %>% filter(Ativo == 1) %>% pull(Codigo)

# fetch no R -------------------------------------------------------------------
pedidos <- pedidos %>% filter(Codigo %in% cod_pedidos_ativos) %>% collect()
interac <- interac %>% filter(CodigoPedido %in% cod_pedidos_ativos) %>% collect()
tp_resp <- tp_resp %>% collect()
agentes <- agentes %>% collect()

saveRDS(pedidos, here("local-files/pedidos.rds"))  
saveRDS(interac, here("local-files/interac.rds"))  
saveRDS(tp_resp, here("local-files/tp_resp.rds"))
saveRDS(agentes, here("local-files/agentes.rds"))
