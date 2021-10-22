library(tidyverse)
library(here)
library(glue)

# url da planilha onde salvarei os resultados
google_sheet <- "https://docs.google.com/spreadsheets/d/1oyA3Pnf382uwHE1Ovg4WMGA4hdzBTiju9M5uiRguW-0"

# regex que detecta URL
rgx_url <- "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"

# regex que detecta data (ex: dd/mm/aaaa)
rgx_data <- "\\d{2}.\\d{2}.\\d{4}"

# **`respostas_concatenadas` é um dataframe que concatena as respostas do pedido e dos recursos, com as colunas:**
# - `link_aep`: url para consultar o pedido no site AeP,
# - `combinacao`: combinação de respostas e recursos,
# - `codigo_pedido`: id pedido,
# - `resposta_pedido` respostas do pedido e dos recursos concatenadas
# - `resposta_pedido_clean` respostas do pedido e dos recursos concatenadas (texto normalizado)
respostas_concatenadas  <- "data/pedidos-respostas-recursos.rds" %>%
  here() %>%
  readRDS() %>%
  janitor::clean_names() %>%
  transmute(

    link_aep = glue("https://achadosepedidos.org.br/pedidos/{slug}"),
    combinacao = combinacao,
    codigo_pedido = codigo_pedido,

    resposta_pedido = glue(
      "{resposta_pedido} {resposta_recurso_1} {resposta_recurso_2} {resposta_recurso_3} {resposta_recurso_4} {resposta_reclamacao}"
    ),
    resposta_pedido = resposta_pedido %>%
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
      str_replace_all("[:punct:]", " ") %>%
      str_replace_all("°|º|ª", " ") %>%
      str_squish(),

    recursos = glue(
      "{recurso_1} {recurso_2} {recurso_3} {recurso_4} {reclamacao}"
    ),
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

  )

respostas_concatenadas

# strings com regex ------------------------------------------------------------

# regex para quando a informação é inexistente:
rgx_inexistentes <- c(
  "informac...? inexistente",
  "informac...? nao existe",
  "dados? inexistente",
  "dados? nao existe",
  "documentos? inexistente",
  "documentos? nao existe",
  "nao detem (a |as )informac",
  "nao detem (o |os )dado"
) %>% paste(collapse = "|")

# regex quando o pedido é dezarrazoado ou desproporcional
rgx_desarrazoado <- c(
  "desarrazoado",
  "desproporcional"
) %>% paste(collapse = "|")

# regex quando o pedido é fishing expedition
rgx_fishing <- c(
  "fishing"
) %>% paste(collapse = "|")

# regex quando o pedido é segurança nacional
rgx_seguranca_nac <- c(
  "seguranca nacional",
  "seguranca do estado"
) %>% paste(collapse = "|")

# regex quando o pedido é sigiloso
rgx_sigilo <- c(
  "sigilo"
) %>% paste(collapse = "|")

# regex quando o pedido é processo decisório em curso
rgx_decisao <- c(
  "processo decisorio em curso",
  "documentos? preparatorio"
) %>% paste(collapse = "|")

# regex quando o pedido exige trabalho adicional
rgx_trabalho_adic  <- c(
  "trabalhos? adiciona",
  "tratamentos? adiciona"
) %>% paste(collapse = "|")

# regex quando o pedido é genérico
rgx_generico <- c(
  "generico"
) %>% paste(collapse = "|")

# regex quando pedido alega dados pessoais
rgx_dados_pessoais <- c(
  "dados? pessoa(l|is)"
) %>% paste(collapse = "|")

# regex quando pedido menciona LGPD
rgx_lgpd <- c(
  "lei geral de protecao de dados",
  "lei de protecao de dados pessoais",
  "lgpd",
  "13709"
) %>% paste(collapse = "|")

# regex quando órgão não tem competência para responder
rgx_nao_competencia <- c(
  "nao( sao de| e de| sao| e| tem) competencia"
) %>% paste(collapse = "|")

# regex quando o anexo está corrompido ou ilegível
regex_anexo_corrompido <- c(
  "anexo ilegivel",
  "anexo corrompido",
  "anexo nao consta",
  "nao( foi)? anex",
  "anexo faltante",
  "faltou anexar",
  "faltou o anexo",
  "sem( arquivo)? anexo",
  "arquivo nao encontrado",
  "nao e possivel acessar o anexo"
) %>% paste(collapse = "|")

# regex resposta incompleta
rgx_resposta_incompleta <- c(
  "resposta incompleta",
  "parcialmente respondido",
  "faltaram informacoes",
  "faltam informacoes",
  "faltou informar",
  "pedido parcialmente atendido",
  "resposta apresentada nao contempla",
  "nao encaminhou( copia d..?)?( arquivo| documento| normativo)"
) %>% paste(collapse = "|")


# `detect_termos` é um dataframe que indica em quais pedidos os termos foram detectados as regex são aplicadas na função `str_detect()`
respostas_concatenadas_termos <- respostas_concatenadas %>%
  mutate(
    controversos_inexistente = str_detect(resposta_pedido, rgx_inexistentes),
    controversos_desarrazoado = str_detect(resposta_pedido, rgx_desarrazoado),
    controversos_fishing = str_detect(resposta_pedido, rgx_fishing),
    controversos_seguranca_nacional = str_detect(resposta_pedido, rgx_seguranca_nac),
    controversos_sigilo = str_detect(resposta_pedido, rgx_sigilo),
    controversos_decisao = str_detect(resposta_pedido, rgx_decisao),
    controversos_trabalho_adicional = str_detect(resposta_pedido, rgx_trabalho_adic),
    controversos_generico = str_detect(resposta_pedido, rgx_generico),
    controversos_dados_pessoais = str_detect(resposta_pedido, rgx_dados_pessoais),
    controversos_lgpd = str_detect(resposta_pedido, rgx_lgpd),
    controversos_nao_competencia = str_detect(resposta_pedido, rgx_nao_competencia),
    controversos_anexo_corrompido = str_detect(recursos, regex_anexo_corrompido),
    controversos_resposta_incompleta = str_detect(recursos, rgx_resposta_incompleta),
    envoiu_link = str_detect(resposta_pedido, "urltagged")
  )

# aba Respostas com termos detectados
# matriz de identificação dos termos
respostas_concatenadas_termos %>%
  filter(
    !(!controversos_inexistente &
      !controversos_desarrazoado &
      !controversos_fishing &
      !controversos_seguranca_nacional &
      !controversos_sigilo &
      !controversos_decisao &
      !controversos_trabalho_adicional &
      !controversos_generico &
      !controversos_dados_pessoais &
      !controversos_lgpd &
      !controversos_nao_competencia &
      !controversos_anexo_corrompido &
      !controversos_resposta_incompleta)
  ) %>%
  mutate(across(where(is.logical), ~ if_else(., "X", "-"))) %>%
  rename(
    desarrazoado = controversos_desarrazoado,
    fishing = controversos_fishing,
    seguranca = controversos_seguranca,
    sigilo = controversos_sigilo,
    decisao = controversos_decisao,
    trabalho_adic = controversos_trabalho_adic,
    generico = controversos_generico,
    dados_pessoais = controversos_dados_pessoais,
    lgpd = controversos_lgpd,
    inexistentes = controversos_inexistentes,
    nao_competencia = controverso_nao_competencia
  ) %>%
  googlesheets4::write_sheet(
    ss = google_sheet,
    sheet = "Respostas com termos detectaos"
  )

# aba Respostas com termos **NÃO** detectados
# matriz de identificação dos termos

# `non_detected_termos` é um dataframe que indica em quais pedidos os termos **não** foram detectados
non_detected_termos <- respostas_concatenadas_termos %>%
  filter(
    !controversos_inexistente &
      !controversos_desarrazoado &
      !controversos_fishing &
      !controversos_seguranca_nacional &
      !controversos_sigilo &
      !controversos_decisao &
      !controversos_trabalho_adicional &
      !controversos_generico &
      !controversos_dados_pessoais &
      !controversos_lgpd &
      !controversos_nao_competencia &
      !controversos_anexo_corrompido &
      !controversos_resposta_incompleta
  )

"data/pedidos-respostas-recursos.rds" %>%
  here() %>%
  readRDS() %>%
  janitor::clean_names() %>%
  filter(codigo_pedido %in% unique(non_detected_termos$codigo_pedido)) %>%
  mutate(link_aep = "https://achadosepedidos.org.br/pedidos/{slug}") %>% 
  left_join(tblai$agentes, by = c("codigo_agente" = "Codigo")) %>%
  select(
    Link = link_aep,
    Combinação = combinacao,
    CodigoPedido = codigo_pedido,
    Orgao = Nome,
    pedido:resposta_recurso_4
  ) %>%
  mutate(
    across(pedido:resposta_recurso_4, ~ str_remove_all(., "\n") %>%
      str_remove_all("\r") %>%
      str_squish() %>%
      replace_na("-"))
  ) %>%
  googlesheets4::write_sheet(
    ss = google_sheet,
    sheet = "Pedidos sem termos detectados (todos)"
  )

# plot com resultados da detecção dos termos
respostas_concatenadas_termos %>%
  filter(
    !(!controversos_inexistente &
      !controversos_desarrazoado &
      !controversos_fishing &
      !controversos_seguranca_nacional &
      !controversos_sigilo &
      !controversos_decisao &
      !controversos_trabalho_adicional &
      !controversos_generico &
      !controversos_dados_pessoais &
      !controversos_lgpd &
      !controversos_nao_competencia &
      !controversos_anexo_corrompido &
      !controversos_resposta_incompleta)
  ) %>% 
  transmute(
    codigo_pedido,
    `Dado/info inexistente` = controversos_inexistente,
    Desarrazoado = controversos_desarrazoado,
    `Fishing expedition` = controversos_fishing,
    `Seguranca nacional` = controversos_seguranca_nacional,
    Sigilo = controversos_sigilo,
    `Processo decisório em curso` = controversos_decisao,
    `Trabalho adicional` = controversos_trabalho_adicional,
    `Pedido genérico` = controversos_generico,
    `Dados pessoais` = controversos_dados_pessoais,
    `LGPD` = controversos_lgpd,
    `Órgão incompetente` = controversos_nao_competencia,
    `Recurso por anexo corrompido` = controversos_anexo_corrompido,
    `Recurso por resposta incompleta` = controversos_resposta_incompleta
  ) %>%
  pivot_longer(-codigo_pedido, names_to = "termo_detectado", values_to = "prevalencia") %>%
  group_by(termo_detectado, prevalencia) %>%
  summarise(qt = n(), .groups = "drop") %>%
  filter(prevalencia) %>%
  mutate(termo_detectado = fct_reorder(termo_detectado, qt)) %>%
  ggplot(aes(x = termo_detectado, y = qt, fill = termo_detectado)) +
  geom_col(show.legend = F) +
  geom_text(aes(label = qt)) +
  coord_flip() +
  labs(
    x = NULL, y = "Quantidade de pedidos", title = "Número de pedidos por termo buscado",
    subtitle = glue::glue("395 pedidos retornaram pelo menos 1 dos termos buscados\n(alguns pedidos tiveram mais de 1 termo encontrado)")
  )
