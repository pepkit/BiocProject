wget https://github.com/pepkit/example_peps/archive/master.zip
unzip master.zip
rm -rf inst/extdata/example_peps-master
mkdir -p inst/extdata/example_peps-master/example_BiocProject
mv example_peps-master/example_BiocProject inst/extdata/example_peps-master
rm -rf example_peps-master 
rm master.zip