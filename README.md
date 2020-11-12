# Social Skills Paper README

## How to use this project

1. Upon opening the .Rproj file in Rstudio you will be invited to download all the packages associated with this project. 
2. From here you will then need to download the LSAC data from the [Australian Data Archive Dataverse](https://dataverse.ada.edu.au/dataverse/ada?q=LSAC) (assuming you have applied for and been granted access to the data) and put the associated file in the data folder. The current datafiles are symbolic links with the names of the files you will need. 
3. You should then be able to reproduce all analysis and results via: 
```
library(targets)
tar_make()
```

**NOTE** For review processes we exclude the .Rmd files which contain author information. Targets associated with these files will thus not run. Rmarkdown files will be made avaliable after the peer review process.
