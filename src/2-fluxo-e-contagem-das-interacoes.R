library(here)
library(tidyverse)
library(lubridate)
library(glue)

#' Onde vou guardar os resultados ----------------------------------------------

#' URL com o destino da planilha onde os resultados serão salvos
google_sheet <- "https://docs.google.com/spreadsheets/d/1oyA3Pnf382uwHE1Ovg4WMGA4hdzBTiju9M5uiRguW-0"

#' data from tblai -------------------------------------------------------------
load(here("data/filtra-pedidos-com-recursos.RData"))

#' checando por quais interações os pedidos passaram ---------------------------

#' É uma função recebe o dataframe `instancias` e atribui `TRUE`/`FALSE` quando uma interação é detectada ou não
checa_fluxo_interacoes <- function(df) {
  df %>%
    group_by(CodigoPedido) %>% 
    mutate(
      
      # ordenação das interações
      rank = replace_na(CodigoTipoPedidoResposta, 0) + 1,
      Nome = fct_reorder(Nome, rank),
      
      # checa qual interação está presente no pedido
      possui_pedido = if_else(rank == 1, TRUE, NA),
      possui_resposta_pedido = if_else(rank == 2, TRUE, NA),
      possui_reclamacao = if_else(rank == 3, TRUE, NA),
      possui_resposta_reclamacao = if_else(rank == 4, TRUE, NA),
      possui_recurso_1a = if_else(rank == 5, TRUE, NA),
      possui_resposta_recurso_1a = if_else(rank == 6, TRUE, NA),
      possui_recurso_2a = if_else(rank == 7, TRUE, NA),
      possui_resposta_recurso_2a = if_else(rank == 8, TRUE, NA),
      possui_recurso_3a = if_else(rank == 9, TRUE, NA),
      possui_resposta_recurso_3a = if_else(rank == 10, TRUE, NA),
      possui_recurso_4a = if_else(rank == 11, TRUE, NA),
      possui_resposta_recurso_4a = if_else(rank == 12, TRUE, NA)
      
    ) %>% 
    fill(possui_pedido:possui_resposta_recurso_4a, .direction = "downup") %>% 
    mutate(across(where(is.logical), ~ if_else(is.na(.), FALSE, .))) %>% 
    ungroup() %>% 
    filter(is.na(CodigoPedidoInteracao) & is.na(CodigoTipoPedidoResposta)) %>% 
    select(CodigoPedido, possui_pedido:possui_resposta_recurso_4a)
}

#' inspeciona
tblai_load$instancias %>%
  checa_fluxo_interacoes() %>%
  glimpse()

#' contagem dos fluxos ---------------------------------------------------------

#' `contagens` é um dataframe que que conta as linhas de `instancias` conforme o fluxo de interações auferido em `checa_fluxo_interacoes(df)`
contagens <- tblai_load$instancias %>% 
  checa_fluxo_interacoes() %>% 
  count(
    possui_pedido,
    possui_resposta_pedido,
    possui_reclamacao,
    possui_resposta_reclamacao,
    possui_recurso_1a,
    possui_resposta_recurso_1a,
    possui_recurso_2a,
    possui_resposta_recurso_2a,
    possui_recurso_3a,
    possui_resposta_recurso_3a,
    possui_recurso_4a,
    possui_resposta_recurso_4a,
    sort = T
  ) %>% 
  mutate(
    across(where(is.logical), ~ if_else(., "POSSUI", "NÃO")),
    combinacao = glue("combinacão {1:n()}")
  )

#' plot fluxo do pedido --------------------------------------------------------

#' Essa função faz um waffle chart com as interações e as respostas
plot_combinacoes_de_fluxo_interacoes <- function() {
  
  ord <- c("pedido", "resposta_pedido", "reclamacao", "resposta_reclamacao",
           "recurso_1a", "resposta_recurso_1a", "recurso_2a",
           "resposta_recurso_2a", "recurso_3a", "resposta_recurso_3a", 
           "recurso_4a", "resposta_recurso_4a")
  
  lbl_x <- c("Pedido", "Resposta", "Reclamação", "Resposta\nreclamação",
             "Recurso 1ª", "Resposta\nrecurso 1ª", "Recurso 2ª", 
             "Resposta\nrecurso 2ª", "Recurso 3ª", "Resposta\nrecurso 3ª",
             "Recurso 4ª", "Resposta\nrecurso 4ª")
  
  contagens %>% 
    pivot_longer(-c(combinacao, n), names_to = "possui", values_to = "qt") %>%
    mutate(
      possui = ordered(str_remove(possui, "possui_"), levels = ord),
      possui_class = case_when(
        possui %in% ord[1:2] ~ "Pedido",
        possui %in% ord[3:4] ~ "Reclamação",
        possui %in% ord[5:6] ~ "1ª instância",
        possui %in% ord[7:8] ~ "2ª instância",
        possui %in% ord[9:10] ~ "3ª instância",
        possui %in% ord[11:12] ~ "4ª instância"
      )
    ) %>% 
    ggplot(aes(y = fct_reorder(combinacao, n), x = possui, fill = qt)) +
    geom_tile(color = "gray80") +
    geom_text(data = . %>% filter(possui == "pedido"), aes(label = n), size = 3) +
    scale_x_discrete(labels = lbl_x, position = "top", expand = c(0, 0)) +
    scale_y_discrete(labels = 39:1) +
    labs(
      x = NULL,
      y = "Combinação",
      fill = NULL,
      title = "Combinações - sequências possíveis de interações"
    ) +
    theme(
      legend.position = "top",
      legend.direction = "horizontal",
      legend.justification = "left",
      axis.ticks.x = element_blank()
    )
    
}

plot_combinacoes_de_fluxo_interacoes()

#' organiza combinações --------------------------------------------------------

#' **o dataframe`combinacao` é o resultado do join entre `instancias` e `contagens`:**
#' - `instancia`: pedidos e interações sequênciadas por orderm de respostas e recursos
#' - `contagens`: conta as linhas de `instancias` conforme o fluxo de interações auferido em `checa_fluxo_interacoes(df)`
combinacoes <- tblai_load$instancias %>% 
  checa_fluxo_interacoes() %>% 
  mutate(across(where(is.logical), ~ if_else(., "POSSUI", "NÃO"))) %>%
  left_join(contagens) %>% 
  mutate(combinacao = fct_reorder(combinacao, -n)) %>% 
  arrange(combinacao) %>% 
  group_by(combinacao) %>% 
  nest() %>% 
  ungroup() %>% 
  rename(fluxo_interacoes = data)

save(combinacoes,
     contagens,
     checa_fluxo_interacoes,
     plot_combinacoes_de_fluxo_interacoes,
     file = here("data/fluxo-e-contagem-de-interacoes.RData"))
