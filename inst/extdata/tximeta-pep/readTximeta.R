readTximeta <- function(pep, useHub) {
    require(tximeta)
    require(tximportData)
    dir <- system.file("extdata/salmon_dm", package="tximportData")
    indexDir <- file.path(dir, "Dm.BDGP6.22.98_salmon-0.14.1")
    fastaFTP <- c("ftp://ftp.ensembl.org/pub/release-98/fasta/drosophila_melanogaster/cdna/Drosophila_melanogaster.BDGP6.22.cdna.all.fa.gz",
                  "ftp://ftp.ensembl.org/pub/release-98/fasta/drosophila_melanogaster/ncrna/Drosophila_melanogaster.BDGP6.22.ncrna.fa.gz")
    gtfPath <- file.path(dir,"Drosophila_melanogaster.BDGP6.22.98.gtf.gz")
    suppressPackageStartupMessages(library(tximeta))
    makeLinkedTxome(indexDir=indexDir,
                    source="Ensembl",
                    organism="Drosophila melanogaster",
                    release="98",
                    genome="BDGP6.22",
                    fasta=fastaFTP,
                    gtf=gtfPath,
                    write=FALSE)
    tximeta::tximeta(coldata=pep@samples, useHub=useHub)
}