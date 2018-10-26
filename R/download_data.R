library(dplyr)
library(tidyr)

#' Tyler R. Kartzinel, Jacob R. Goheen, Grace K. Charles, Elyse DeFranco, 
#' Janet E. Maclean, Tobias O. Otieno, Todd M. Palmer, and Robert M. Pringle. 
#' 2014. Plant and small-mammal responses to large-herbivore exclusion in an 
#' African savanna: five years of the UHURU experiment. Ecology 95:787
#' https://doi.org/10.1890/13-1023R.1

download.file('https://ndownloader.figshare.com/files/5629554', 'data/kartzinel-et-al-2014.txt')
# download.file('https://ndownloader.figshare.com/files/5629557', 'smammal-size/data/raw/kartzinel-et-al-2014/metadata.htm')

#' D. A. Kelt, P. L. Meserve, J. R. Gutiérrez, W. Bryan Milstead, and 
#' M. A. Previtali. 2013. Long-term monitoring of mammals in the face of
#'  biotic and abiotic influences at a semiarid site in north-central Chile. 
#'  Ecology 94:977. 
#'  http://dx.doi.org/10.1890/12-1811.1

# download.file('https://ndownloader.figshare.com/files/5625375', 'smammal-size/data/raw/kelt-et-al-2013/small_mammal_metadata.htm')
# download.file('https://ndownloader.figshare.com/files/5625402', 'smammal-size/data/raw/kelt-et-al-2013/metadata.htm')
download.file('https://ndownloader.figshare.com/files/5625372', 'data/kelt-et-al-2013.txt')

#' Merritt J. 1999. Long Term Mammal Data from Powdermill Biological 
#' Station 1979-1999. Environmental Data Initiative. 
#' https://doi.org/10.6073/pasta/101d5d3dec9c688a7feca3ab2e969369. 
#' Dataset accessed 7/18/2018.


ifelse(!dir.exists(file.path('smammal-size/data/raw', 'merritt-1999')), dir.create(file.path('smammal-size/data/raw', 'merritt-1999')), FALSE)

download.file('https://portal.lternet.edu/nis/metadataviewer?packageid=knb-lter-vcr.67.20', 'smammal-size/data/raw/merritt-1999/metadata.htm')
download.file('https://portal.lternet.edu/nis/dataviewer?packageid=knb-lter-vcr.67.20&entityid=82cf3f3ce41a598251f85e4b0da6caf8', 'smammal-size/data/raw/merritt-1999/long_term_mammal_data.txt')


#' Porter J., R. Dueser. 2016. Hog Island VA Small-Mammal Trapping 1989-2016. 
#' Environmental Data Initiative. 
#' https://doi.org/10.6073/pasta/a6745259025acbd2438f0a975f404978. 
#' Dataset accessed 7/18/2018.

ifelse(!dir.exists(file.path('smammal-size/data/raw', 'porter-dueser-2016')), dir.create(file.path('smammal-size/data/raw', 'porter-dueser-2016')), FALSE)

download.file('https://portal.lternet.edu/nis/metadataviewer?packageid=knb-lter-vcr.49.34', 'smammal-size/data/raw/porter-dueser-2016/metadata.htm')
download.file('https://portal.lternet.edu/nis/dataviewer?packageid=knb-lter-vcr.49.34&entityid=a9e58501c79f233385666d3612d48244', 'smammal-size/data/raw/porter-dueser-2016/rodent_trapping.csv')


#' Dueser R., S. McCuskey, G. Hogue, J. Porter. 1991. Survey of small mammals 
#' using trapping data on all Virginia Coast Reserve barrier islands south of 
#' Paramore Island (inclusive) 1975-1978. Environmental Data Initiative. 
#' https://doi.org/10.6073/pasta/0daca2db114bbfe69460a483f2bd417d. 
#' Dataset accessed 7/18/2018.


ifelse(!dir.exists(file.path('smammal-size/data/raw', 'dueser-et-al-1991')), dir.create(file.path('smammal-size/data/raw', 'dueser-et-al-1991')), FALSE)

download.file('https://portal.lternet.edu/nis/metadataviewer?packageid=knb-lter-vcr.1.21', 'smammal-size/data/raw/dueser-et-al-1991/metadata.htm')
download.file('https://portal.lternet.edu/nis/dataviewer?packageid=knb-lter-vcr.1.21&entityid=f15adf1ea6c4c673dbd7fda56990e969', 'smammal-size/data/raw/dueser-et-al-1991/rodent_trapping.txt')


#' Knops J. 2018. Small mammal abundance: Successional Dynamics on a Resampled 
#' Chronosequence. Environmental Data Initiative. 
#' https://doi.org/10.6073/pasta/bd761ec2c398fa992f744f3edeef61a5. 
#' Dataset accessed 7/18/2018.

ifelse(!dir.exists(file.path('smammal-size/data/raw', 'knops-2018')), dir.create(file.path('smammal-size/data/raw', 'knops-2018')), FALSE)

download.file('https://portal.lternet.edu/nis/metadataviewer?packageid=knb-lter-cdr.100.8', 'smammal-size/data/raw/knops-2018/metadata.htm')
download.file('https://portal.lternet.edu/nis/dataviewer?packageid=knb-lter-cdr.100.8&entityid=b4b1370518f1c381b897b09cf6d1a286', 'smammal-size/data/raw/knops-2018/rodent_trapping.txt')

#' Dueser R., R. Rose, J. Porter. 1998. Birdwood Mammal Trapping Data, 
#' Charlottesville, VA, 1974-1978. Environmental Data Initiative. 
#' https://doi.org/10.6073/pasta/266c3435f0fc4609b013f44df4dba5eb. 
#' Dataset accessed 7/18/2018


ifelse(!dir.exists(file.path('smammal-size/data/raw', 'dueser-et-al-1998')), dir.create(file.path('smammal-size/data/raw', 'dueser-et-al-1998')), FALSE)

download.file('https://portal.lternet.edu/nis/metadataviewer?packageid=knb-lter-vcr.62.21','smammal-size/data/raw/dueser-et-al-1998/metadata.htm')
download.file('https://portal.lternet.edu/nis/dataviewer?packageid=knb-lter-vcr.62.21&entityid=45c40d0e60fc4d5e566c8dba1bc510fa', 'smammal-size/data/raw/dueser-et-al-1998/rodent_trapping.txt')


#' Demers C. 2018. Small mammals surveys, 1981 - 1987, Adirondack 
#' Long-Term Ecological Monitoring Program Project No. 10 by 
#' Adirondack Ecological Center of the State University of 
#' New York College of Environmental Science and Forestry, 
#' Newcomb, New York. Environmental Data Initiative. 
#' Environmental Data Initiative. 
#' https://doi.org/10.6073/pasta/d610792849494819ee3c4a9181b91957. 
#' Dataset accessed 7/18/2018.


ifelse(!dir.exists(file.path('smammal-size/data/raw', 'demers-2018')), dir.create(file.path('smammal-size/data/raw', 'demers-2018')), FALSE)

download.file('https://portal.edirepository.org/nis/metadataviewer?packageid=edi.221.1','smammal-size/data/raw/demers-2018/metadata.htm')
download.file('https://portal.edirepository.org/nis/dataviewer?packageid=edi.221.1&entityid=f8b0047edc9aeca69518ec02e2abecae', 'smammal-size/data/raw/demers-2018/small_mammal.csv')


#' Newsome S. 2010. Small Mammal Mark-Recapture Population Dynamics at 
#' Core Research Sites at the Sevilleta National Wildlife Refuge, New 
#' Mexico (1989 - present). Environmental Data Initiative. 
#' https://doi.org/10.6073/pasta/cdd8f254ef97d854d6eb2efb7385b801. 
#' Dataset accessed 7/18/2018.


ifelse(!dir.exists(file.path('smammal-size/data/raw', 'newsome-2010')), dir.create(file.path('smammal-size/data/raw', 'newsome-2010')), FALSE)

download.file('https://portal.lternet.edu/nis/metadataviewer?packageid=knb-lter-sev.8.297976','smammal-size/data/raw/newsome-2010/metadata.htm')
download.file('https://portal.lternet.edu/nis/dataviewer?packageid=knb-lter-sev.8.297976&entityid=d70c7027949ca1d8ae053eb10300dc0e', 'smammal-size/data/raw/newsome-2010/small_mammal.csv')



