Chatbot - AeP
================

## Código

-   local: `/src`

> -   [`0-fetch-mysql-to-r.R`](src/0-fetch-mysql-to-r.R): *Exporta a
>     base de dados do MySQL*
> -   [`1-filtra-pedidos-com-recursos.R`](src/1-filtra-pedidos-com-recursos.R):
>     *Extrai da base pedidos com recurso em 1ª instância*
> -   [`2-fluxo-e-contagem-das-interacoes.R`](src/2-fluxo-e-contagem-das-interacoes.R):
>     *Faz o mapeamento dos pedidos que tiveram ou não resposta,
>     recurso, resposta do recurso até a 4ª instãncia*
> -   [`3-pedidos-respostas-recursos.R`](src/3-pedidos-respostas-recursos.R):
>     *Realiza o enfileiramento das interações, fazendo com que o fluxo
>     completo do pedido possa ser lido horizontalmente em uma tabela*
> -   [`4-lista-de-regex.R`](src/5-lista-de-regex.R): *Lista de
>     expressões regulares*
> -   [`5-busca-termos.R`](src/4-busca-termos.R): *Realiza busca por
>     termos-chave e exporta resultados para uma planilha no Google
>     Sheet*

## Bases de dados produzidas

-   local: `/data` ou `local/files`

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

| Termo (grupo)                   | Variações                                                                                                           | Expressão regular                                                                                                                                                                                                                                                                       |
|:--------------------------------|:--------------------------------------------------------------------------------------------------------------------|:----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| Trabalho adicional              | trabalho adicional, tratamento adicional                                                                            | <code>trabalhos? adiciona\|tratamentos? adiciona</code>                                                                                                                                                                                                                                 |
| Sigilo                          | sigilo                                                                                                              | <code>sigilo</code>                                                                                                                                                                                                                                                                     |
| Pedido genérico                 | genérico                                                                                                            | <code>generico</code>                                                                                                                                                                                                                                                                   |
| Desarrazoado                    | desarrazoado, desproporcional                                                                                       | <code>desarrazoado\|desproporcional</code>                                                                                                                                                                                                                                              |
| Dados pessoais                  | dados pessoais                                                                                                      | <code>dados? pessoa(l\|is)</code>                                                                                                                                                                                                                                                       |
| Órgão incompetente              | \[órgão\] não tem competência, \[assunto\] não é de competência \[do órgão\]                                        | <code>nao( sao de\| e de\| sao\| e\| tem) competencia</code>                                                                                                                                                                                                                            |
| Recurso por resposta incompleta | resposta incompleta, parcialmente respondida(o), parcialmente atendida(o), faltam incormações, faltou informar      | <code>resposta incompleta\|parcialmente respondid(a\|o)\|parcialmente atendid(a\|o)\|faltaram informacoes\|faltam informacoes\|faltou informar\|pedido parcialmente atendido\|resposta apresentada nao contempla\|nao encaminhou( copia d..?)?( arquivo\| documento\| normativo)</code> |
| Recurso por anexo corrompido    | anexo ilegível, anexo comrrompido, anexo faltante, faltou anexar, sem (arquivo) anexo, não é possível acessar anexo | <code>anexo ilegivel\|anexo corrompido\|anexo nao consta\|nao( foi)? anex\|anexo faltante\|faltou anexar\|faltou o anexo\|sem( arquivo)? anexo\|arquivo nao encontrado\|nao e possivel acessar o anexo</code>                                                                           |
| Processo decisório em curso     | processo decisório em curso, documentos preparatórios                                                               | <code>processo decisorio em curso\|documentos? preparatorio</code>                                                                                                                                                                                                                      |
| Segurança nacional              | segurança nacional, segurança de/do estado                                                                          | <code>seguranca nacional\|seguranca do estado</code>                                                                                                                                                                                                                                    |
| Dado/informação inexistente     | informação/dado/documento inexistente                                                                               | <code>informac…? inexistente\|informac…? nao existe\|dados? inexistente\|dados? nao existe\|documentos? inexistente\|documentos? nao existe\|nao detem (a \|as )informac\|nao detem (o \|os )dado</code>                                                                                |

![](README_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

## Recursos

### Detecção de termos em recursos

| Termo (grupo)   | Expressão regular                                |
|:----------------|:-------------------------------------------------|
| Não consta      | <code>nao consta</code>                          |
| Está faltando   | <code>estao? faltando\|faltaram (as\|os)?</code> |
| Não contempla   | <code>nao contempla</code>                       |
| Insatisfatória  | <code>insatisfatori(a\|o)\|nao satisfaz</code>   |
| Não corresponde | <code>nao corresponde</code>                     |
| Não atende      | <code>nao atende</code>                          |
| Incompleta      | <code>incomplet(a\|o)\|nao esta completo</code>  |
| Insuficiente    | <code>insuficiente</code>                        |

![](README_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->
