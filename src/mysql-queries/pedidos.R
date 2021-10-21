# Query da tabela `pedidos` no MySQL do AeP:
query_pedidos <- glue::glue(
  "
    SELECT `Codigo`,
           `CodigoUsuario`,
           `CodigoAgente`,
           `CodigoTipoOrigem`,
           `CodigoTipoPedidoSituacao`,
           `CodigoStatusPedido`,
           `CodigoStatusPedidoInterno`,
           `IdentificadorExterno`,
           `Protocolo`,
           REPLACE(`Titulo`, CHAR(0), ' ') AS `Titulo`,
           `Slug`,
           REPLACE(`Descricao`, CHAR(0), ' ') AS `Descricao`,
           `DataEnvio`,
           `FoiProrrogado`,
           `Anonimo`,
           `Ativo`
    FROM `pedidos`
    WHERE `Codigo` IN ({paste(cod_pedidos_ativos, collapse = ', ')})
    ;
  "
)

message("Query de `pedidos` foi carregada no global environment")