readTximeta <- function(pep) {
    require(tximeta)
    return(tximeta::tximeta(coldata=pep@samples, skipSeqinfo=TRUE, skipMeta=TRUE))
}