# Query da tabela `pedidos` no MySQL do AeP:
query_pedidos_interacoes <- glue::glue(
  "
    SELECT `Codigo`,
           `CodigoPedido`,
           `CodigoTipoPedidoResposta`, 
           `DataResposta`,
           REPLACE(`Descricao`, CHAR(0), ' ') AS `Descricao`
    FROM `pedidos_interacoes`
    WHERE `CodigoPedido` IN ({paste(cod_pedidos_ativos, collapse = ', ')})
    ;
  "
)

message("Query de `pedidos_interacoes` foi carregada no global environment")