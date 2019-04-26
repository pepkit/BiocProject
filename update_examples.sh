wget https://github.com/pepkit/example_peps/archive/master.zip
unzip master.zip
rm -rf inst/extdata/example_peps-master
mkdir -p inst/extdata/example_peps-master/output inst/extdata/example_peps-master/example_piface inst/extdata/example_peps-master/example_BiocProject inst/extdata/example_peps-master/example_BiocProject_remote
mv example_peps-master/output/ example_peps-master/example_piface/ example_peps-master/example_BiocProject example_peps-master/example_BiocProject_remote example_peps-master/example_BiocProject_exceptions inst/extdata/example_peps-master
rm -rf example_peps-master 
rm master.zip