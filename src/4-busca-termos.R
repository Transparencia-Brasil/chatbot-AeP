library(tidyverse)
library(here)

# Onde vou guardar os resultados -----------------------------------------------
google_sheet <- "https://docs.google.com/spreadsheets/d/1oyA3Pnf382uwHE1Ovg4WMGA4hdzBTiju9M5uiRguW-0"

# abre uma tabela concatenando respostas
respostas <- readRDS(here("data/pedidos-respostas-recursos.rds")) %>% 
  janitor::clean_names() %>% 
  transmute(
    combinacao,
    codigo_pedido,
    resposta_pedido = glue::glue(
      "{resposta_pedido} {resposta_recurso_1} {resposta_recurso_2} {resposta_recurso_3} {resposta_recurso_4}"
    )
  )

# faz a detectção dos termos
detect_termos <- respostas %>% 
  
  mutate(
    resposta_pedido = tolower(resposta_pedido),
    resposta_pedido = str_squish(resposta_pedido),
    resposta_pedido = stringi::stri_trans_general(resposta_pedido, "Latin-ASCII"),
    
    controversos_desarrazoado = str_detect(resposta_pedido,'desarrazoado|desproporciona'), 
    controversos_fishing  = str_detect(resposta_pedido,'fishing'),
    controversos_seguranca =  str_detect(resposta_pedido,'segurança [nacional|do estado]'),
    controversos_sigilo = str_detect(resposta_pedido,'sigilo'),
    controversos_decisao =  str_detect(resposta_pedido,'processo decisorio em curso|documento preparatorio'),
    controversos_trabalho_adic = str_detect(resposta_pedido,'trabalho[s]? adiciona'),
    controversos_generico =  str_detect(resposta_pedido,'generico'),
    controversos_lgpd = str_detect(resposta_pedido, 'lei geral de proteção de dados|lei de protecao de dados pessoais|13\\.?709|13\\.?853|lgpd')
    
  )

# filtra pedidos que não possui nenhum termo
detect_termos %>% 
  filter(
    !controversos_desarrazoado &
      !controversos_fishing &
      !controversos_seguranca &
      !controversos_sigilo &
      !controversos_decisao &
      !controversos_trabalho_adic &
      !controversos_generico &
      !controversos_lgpd
  )

# salva os pedidos em planilha
load(here("local-files/tblai.RData"))

readRDS(here("data/pedidos-respostas-recursos.rds")) %>% 
  janitor::clean_names() %>%
  mutate(link_aep = glue::glue('https://achadosepedidos.org.br/pedidos/{slug}')) %>% 
  left_join(tblai$agentes, by = c("codigo_agente" = "Codigo")) %>% 
  select(link_aep, combinacao, codigo_pedido, orgao = Nome, pedido:resposta_recurso_4) %>% 
  googlesheets4::write_sheet(
    ss = google_sheet,
    sheet = "Pedidos, respostas e recursos"
  )

# filtra pedidos que não possui nenhum termo
detect_termos %>% 
  filter(
    !(!controversos_desarrazoado &
        !controversos_fishing &
        !controversos_seguranca &
        !controversos_sigilo &
        !controversos_decisao &
        !controversos_trabalho_adic &
        !controversos_generico &
        !controversos_lgpd)
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
    lgpd = controversos_lgpd
  ) %>% 
  googlesheets4::write_sheet(
    ss = google_sheet,
    sheet = "Respostas com termos detectaos"
  )
  
