library(tidyverse)
library(here)

# datasets que vou usar
load(here("local-files/tblai.RData"))
load(here("data/tblai-load.RData"))
load(here("data/fluxo-e-contagem-de-interacoes.RData"))

# helper
`%notin%` <- function(x, y) !(x %in% y)

# vou remover pedidos inconsistentes
inconsistentes <- tblai$interac %>% 
  count(CodigoPedido, CodigoTipoPedidoResposta, sort = T) %>% 
  filter(n > 1) %>% 
  pull(CodigoPedido)

# remove pedidos inconsistentes
tblai$pedidos <- tblai$pedidos %>% filter(Codigo %notin% inconsistentes) %>% rename(CodigoPedido = Codigo)
tblai$interac <- tblai$interac %>% filter(CodigoPedido %notin% inconsistentes)

# vou enfileirar os pedidos na sequência das interações.
# seleciono pedidos validados
filter_pedido <- function(df,
                          cd_pedido,
                          cd_inconsistentes) {
  df %>% 
    filter(CodigoPedido %in% cd_pedido & 
             CodigoPedido %notin% cd_inconsistentes) %>% 
    select(CodigoPedido, CodigoAgente, Slug, pedido = Descricao)
}

# seleciono as interações validadas
filter_interacao <- function(df,
                             cd_pedido_resposta,
                             cd_inconsistentes, 
                             cd_pedido,
                             nm_interacao)  {
  
  df %>% 
    filter(CodigoTipoPedidoResposta == cd_pedido_resposta &
             CodigoPedido %notin% cd_inconsistentes &
             CodigoPedido %in% cd_pedido) %>% 
    select(CodigoPedido, Descricao) %>% 
    `names<-`(c("CodigoPedido", nm_interacao))
  
}

# vou enfileirar as interações conforme as combinações
cmb <- combinacoes %>% 
  mutate(
    cd_pedidos = map(fluxo_interacoes, pull, CodigoPedido),
    pedidos = map(cd_pedidos, ~ filter_pedido(tblai$pedidos, .x, inconsistentes)),
    respostas = map(cd_pedidos, ~ filter_interacao(tblai$interac, 1, inconsistentes, .x, "resposta_pedido")),
    reclamacao = map(cd_pedidos, ~ filter_interacao(tblai$interac, 2, inconsistentes, .x, "reclamacao")),
    resposta_reclamacao = map(cd_pedidos, ~ filter_interacao(tblai$interac, 3, inconsistentes, .x, "resposta_reclamacao")),
    recurso_1 = map(cd_pedidos, ~ filter_interacao(tblai$interac, 4, inconsistentes, .x, "recurso_1")),
    resposta_recurso_1 = map(cd_pedidos, ~ filter_interacao(tblai$interac, 5, inconsistentes, .x, "resposta_recurso_1")),
    recurso_2 = map(cd_pedidos, ~ filter_interacao(tblai$interac, 6, inconsistentes, .x, "recurso_2")),
    resposta_recurso_2 = map(cd_pedidos, ~ filter_interacao(tblai$interac, 7, inconsistentes, .x, "resposta_recurso_2")),
    recurso_3 = map(cd_pedidos, ~ filter_interacao(tblai$interac, 8, inconsistentes, .x, "recurso_3")),
    resposta_recurso_3 = map(cd_pedidos, ~ filter_interacao(tblai$interac, 9, inconsistentes, .x, "resposta_recurso_3")),
    recurso_4 = map(cd_pedidos, ~ filter_interacao(tblai$interac, 8, inconsistentes, .x, "recurso_4")),
    resposta_recurso_4 = map(cd_pedidos, ~ filter_interacao(tblai$interac, 9, inconsistentes, .x, "resposta_recurso_4")),
    n = map_int(pedidos, nrow)
  ) %>% 
  filter(n > 0)

# junta todas as interações
finaliza_pedidos <- function(comb) {
  cmb %>% 
    filter(combinacao == comb) %>% 
    select(pedidos:resposta_recurso_4) %>% 
    pivot_longer(everything(), names_to = "interacao", values_to = "df") %>% 
    deframe() %>% 
    reduce(left_join) 
}

# Entrega o data frame final
pedidos_respostas_recurso <- cmb %>% 
  transmute(
    combinacao,
    final = map(combinacao, finaliza_pedidos)
  ) %>% 
  unnest(final)

saveRDS(pedidos_respostas_recurso, here("data/pedidos-respostas-recursos.rds"))
