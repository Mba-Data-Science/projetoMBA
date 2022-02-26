#!/bin/bash

pv dados/estabelcimentos/*.ESTABELE > dados/estabelcimentos/tudojunto.txt
pv dados/empre/*.EMPRECSV > dados/empre/tudoJunto.txt
pv dados/tabelas/F.K03200\$Z.D20108.CNAECSV > dados/tabelas/cnae.txt

(
    CNPJ_BASICO                 TEXT,
    RASAO_SOCIAL                TEXT,
    NATUREZA_JURIDICA           TEXT,
    QUALIFICACAO_DO_RESPONSAVEL TEXT,
    CAPITAL_SOCIAL              TEXT,
    PORTE                       TEXT,
    ENTE_FEDERATIVO_RESPONSAVEL TEXT
);
