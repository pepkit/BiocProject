parseEncodeRegions = function(project) {
  # get the data from the Project config
  url = samples(project)$remote_url[[1]]
  sampleName = samples(project)$sample_name[[1]]
  fileName = samples(project)$file_name[[1]]
  workDir = config(project)$metadata$output_dir
  # create the download dir if it does not exist
  if(!dir.exists(file.path(workDir))){
    message("\nCreating directory: ",workDir,"\n")
    dir.create(file.path(workDir))
  }
  # download the file
  download.file(url = url,destfile = file.path(workDir,fileName),method = "wget")
  # read it in
  df=read.table(file.path(workDir,fileName))
  # formatting
  colnames(df) = c('chr', 'start', 'end', 'name')
  # convert to GRanges object
  GenomicRanges::GRanges(df)
}
