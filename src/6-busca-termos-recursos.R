library(tidyverse)
library(here)
library(glue)
# Analisar recursos em primeira instância: ver frequência de palavras e verificar os termos:
#   

source(here("src/4-lista-de-regex.R"), encoding = "utf-8")

#' url da planilha onde salvarei os resultados
google_sheet <- "https://docs.google.com/spreadsheets/d/1oyA3Pnf382uwHE1Ovg4WMGA4hdzBTiju9M5uiRguW-0"

#' **`respostas_concatenadas` é um dataframe que concatena as respostas do pedido e dos recursos, com as colunas:**
#' - `link_aep`: url para consultar o pedido no site AeP,
#' - `combinacao`: combinação de respostas e recursos,
#' - `codigo_pedido`: id pedido,
#' - `resposta_pedido` respostas do pedido e dos recursos concatenadas
#' - `resposta_pedido_clean` respostas do pedido e dos recursos concatenadas (texto normalizado)

respostas_concatenadas  <- "data/pedidos-respostas-recursos.rds" %>%
  here() %>%
  readRDS() %>%
  janitor::clean_names() %>%
  transmute(
    link_aep = glue("https://achadosepedidos.org.br/pedidos/{slug}"),
    combinacao = combinacao,
    codigo_pedido = codigo_pedido,
    recursos = glue("{recurso_1} {recurso_2} {recurso_3} {recurso_4} {reclamacao}"),
    recursos = recursos %>%
      str_replace_all(rgx_url, "urltagged") %>%
      str_replace_all(rgx_data, "daymonthyear") %>%
      tolower() %>%
      stringi::stri_trans_general("Latin-ASCII") %>%
      str_replace_all("\\r\\n\\r\\n", " ") %>%
      str_replace_all("\\r\\n", " ") %>%
      str_replace_all("\\r\\r", " ") %>%
      str_replace_all("\\n\\r", " ") %>%
      str_replace_all("\\n", " ") %>%
      str_replace_all("\\r", " ") %>%
      str_remove_all("[:punct:]") %>%
      str_replace_all("°|º|ª", " ") %>%
      str_squish()
  ) %>% filter(!is.na(recursos))

#' `detect_termos` é um dataframe que indica em quais pedidos os termos foram
#'  detectados as regex são aplicadas na função `str_detect()`
#'  

rgx_para_recursos <- c(
  "nao consta",
  "esta faltando",
  "nao contempla", 
  "nao satisfaz",
  "insatisfatoria",
  "nao corresponde",
  "nao atende",
  "incompleta",
  "nao esta completa"
)

respostas_concatenadas_termos <- respostas_concatenadas %>%
  mutate(
    `Não consta` = str_detect(recursos, "nao consta"),
    `Está faltando` = str_detect(recursos, "estao? faltando|faltaram (as|os)?"),
    `Não contempla` = str_detect(recursos, "nao contempla"),
    `Insatisfatória` = str_detect(recursos, "insatisfatori(a|o)|nao satisfaz"),
    `Não corresponde` = str_detect(recursos, "nao corresponde"),
    `Não atende` = str_detect(recursos, "nao atende"),
    `Incompleta` = str_detect(recursos, "incomplet(a|o)|nao esta completo"),
    `Insuficiente` = str_detect(recursos, "insuficiente")
  )


# aplica identidade visual da TB/AeP:
cores_aep <- c(
  laranja = "#F9A521",
  rosa = "#D81755",
  cinza = "#969696",
  marrom = "#B27D5C"
)

cores_tb <- c(
  laranja = "#F6A323",
  cinza_escuro = "#1d1d1b",
  cinza_claro = "#6f7171",
  cinza_quase_branco = "#ececec",
  azul = "#41ACBD"
)

respostas_concatenadas_termos %>% 
  select(
    codigo_pedido,
    `Não consta`,
    `Está faltando`,
    `Não contempla`,
    `Insatisfatória`,
    `Não corresponde`,
    `Não atende`,
    `Incompleta`,
    `Insuficiente`
  ) %>% 
  pivot_longer(-codigo_pedido, names_to = "termo", values_to = "prevalencia") %>% 
  filter(prevalencia) %>% 
  count(termo, prevalencia) %>%
  mutate(termo = reorder(termo, n)) %>% 
  ggplot(aes(x = termo, y = n)) +
  geom_col(fill = cores_aep[["laranja"]]) +
  geom_text(aes(label = n)) +
  coord_flip() +
  scale_y_continuous(expand = c(0.008, 0), limits = c(0, 105)) +
  labs(
    x = NULL,
    y = "Quantidade de pedidos",
    title = "Detecção de termos no conteúdo dos recursos"
  )

