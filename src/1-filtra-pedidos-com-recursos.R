library(here)
library(tidyverse)
library(lubridate)
library(glue)

# data from tblai --------------------------------------------------------------
load(here("local-files/tblai.RData"))

# escolhe pedidos que entraram com recurso -------------------------------------

# Uma lista dos códigos de pedidos onde o usuário entrou com recurso
codigo_pedidos_com_recurso <- tblai$interac %>% 
  filter(CodigoTipoPedidoResposta == 4) %>%
  select(Codigo, CodigoPedido) %>% 
  distinct(CodigoPedido) %>% 
  pull()

# filtra na tabela de pedidos, somente aqueles com recurso ---------------------

# `pedidos_com_recurso` é um dataframe dos pedidos que entraram com recurso
pedidos_com_recurso <- tblai$pedidos %>%
  filter(Codigo %in% codigo_pedidos_com_recurso) %>% 
  transmute(CodigoPedido = Codigo, DataEnvio, Nome = "Pedido")

# interações dos pedidos que tiveram recurso -----------------------------------

key <- c("CodigoTipoPedidoResposta" = "Codigo")

# `interacoes` é um dataframe das interações dos pedidos que entraram com recurso
interacoes <- tblai$interac %>%
  filter(CodigoPedido %in% codigo_pedidos_com_recurso) %>% 
  left_join(tblai$tp_resp, by = key)

# enfileira as interações dos pedidos ------------------------------------------

# `instancias` é um dataframe onde os pedidos e as interações estão sequênciadas
instancias <- interacoes %>%
  select(
    CodigoPedidoInteracao = Codigo, 
    CodigoPedido,
    CodigoTipoPedidoResposta,
    Nome,
    DataEnvio = DataResposta
  ) %>%
  bind_rows(pedidos_com_recurso) %>%
  mutate(DataEnvio = as_date(DataEnvio)) %>%
  filter(!is.na(DataEnvio) | year(DataEnvio) > 2012 | year(DataEnvio) < 2021) %>%
  arrange(CodigoPedido, CodigoTipoPedidoResposta)

glimpse(instancias)

# **`tblai_load` é uma lista de `data.frames`:**
# - `pedidos_com_recurso` pedidos que entraram com recurso
# - `interacoes` interações dos pedidos que entraram com recurso
# - `instancias` pedidos e interações sequênciadas por orderm de respostas e recursos
tblai_load <- list(
  pedidos_com_recursos = pedidos_com_recurso,
  interacoes = interacoes,
  instancias = instancias
)

save(tblai_load, file = here("data/filtra-pedidos-com-recursos.RData"))
