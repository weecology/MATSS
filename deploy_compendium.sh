Rscript -e 'MATSS::create_MATSS_compendium("../MATSSdemo", "Travis CI")'
Rscript -e 'devtools::install("../MATSSdemo")'
Rscript -e 'source("../MATSSdemo/analysis/pipeline.R")'
echo 'Successfully built compendium!'