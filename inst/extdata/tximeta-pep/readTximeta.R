readTximeta <- function(pep, useHub) {
    require(tximeta)
    tximeta::tximeta(coldata=pep@samples, useHub=useHub)
}