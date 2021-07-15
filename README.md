Chatbot - AeP
================

## Código

-   local: `/src`

> -   [`0-fetch-mysql-to-r.R`](src/0-fetch-mysql-to-r.R): *Exporta a
>     base de dados do MySQL, armazenando em um arquivo `tblai.RData`*
> -   [`1-filtra-pedidos-com-recursos.R`](src/1-filtra-pedidos-com-recursos.R):
>     *Extrai da base pedidos com recurso em 1ª instância, entrega nova
>     base chamada `filtra-pedidos-com-recursos.RData`*
> -   [`2-fluxo-e-contagem-das-interacoes.R`](src/2-fluxo-e-contagem-das-interacoes.R):
>     *Faz o mapeamento dos pedidos que tiveram ou não resposta,
>     recurso, resposta do recurso até a 4ª instãncia, entrega o
>     mapeamento na base `fluxo-e-contagem-das-interacoes.RData`*
> -   [`3-pedidos-respostas-recursos.R`](src/3-pedidos-respostas-recursos.R):
>     *Realiza o enfileiramento das interações, fazendo que o fluxo
>     completo do pedido possa ser lido horizontalmente em uma tabela
>     (isso permite a leitura humana e facilita o processamento por
>     máquinha, seguindo o mesmo padrão das bases entregues para estudo
>     NLP). O resultado é entregue em uma tabela chamada
>     `pedidos-respostas-recursos.rds`*
> -   [`4-busca-termos.R`](src/4-busca-termos.R): *Realiza busca por
>     termos-chave e exporta resultados para uma planilha no Google
>     Sheet*

## Bases de dados produzidas

local: `/data` ou `local/files`

> -   [`tblai.RData`](https://drive.google.com/file/d/1MZcRj2hZGmdToccZoMOiRH9eR78AU7Gb/view?usp=sharing):
>     bases extraídas do MySQL (tblai)
> -   [`filtra-pedidos-com-recursos.RData`](data/filtra-pedidos-com-recursos.RData):
>     base somente com pedidos que entraram com recursos em 1ª instância
> -   [`fluxo-e-contagem-das-interacoes.RData`](data/fluxo-e-contagem-das-interacoes.RData):
>     Combinações de respostas e recursos + contagem
> -   [`pedidos-respostas-recursos.rds`](data/pedidos-respostas-recursos.rds):
>     Base final pronta para analise de termos-chave

## Planilha com resultados no Google Drive

> -   [Clique
>     aqui](https://docs.google.com/spreadsheets/d/1oyA3Pnf382uwHE1Ovg4WMGA4hdzBTiju9M5uiRguW-0)

## Combinações

O gráfico a seguir mostra as combinações possíveis de interações
verificadas no pedido:

![](README_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

## Termos chave utilizados

-   Termo (`<regex>`)

> -   Pedido desarrazoado ou desproporcional
>     (`desarrazoado|desproporciona`)
> -   Fishing expedition (`fishing`)
> -   Segurança Nacional (`segurança [nacional|do estado]`)
> -   Sigilo (`sigilo`)
> -   Decisão em curso
>     (`processo decisorio em curso|documento preparatorio`)
> -   Trabalho adicional (`trabalho[s]? adiciona`)
> -   Pedido Genérico (`generico`)
> -   LGPD
>     (`lei geral de proteção de dados|lei de protecao de dados pessoais|13\\.?709|13\\.?853|lgpd`)
