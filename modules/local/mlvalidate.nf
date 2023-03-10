process MLVALIDATE {
    tag 'validate'
    label 'process_single'

    //This lists the required conda recipes for running  the process with conda, below the container URLs are listed that are built on "bioconda/multi-package-containers", see PR https://github.com/BioContainers/multi-package-containers/pull/2509
    conda "conda-forge::r-base conda-forge::r-tidyverse=1.3.1 conda-forge::r-data.table=1.14.6 conda-forge::r-caret=6.0_93 conda-forge::r-rjson=0.2.21 conda-forge::r-furrr=0.3.1 conda-forge::r-purrr=1.0.1 conda-forge::r-optparse=1.7.3 conda-forge::r-r6=2.5.1 conda-forge::r-tidymodels=1.0.0 conda-forge::r-pls=2.8_1"
    container "${ workflow.containerEngine == 'singularity' && !task.ext.singularity_pull_docker_container ?
        'https://depot.galaxyproject.org/singularity/mulled-v2-ca640e4b0420dd5c8440adb0c9bcc92768eb2cb5:206c711dbb9549c63abea09fc625ab10dda04329-0' :
        'quay.io/biocontainers/mulled-v2-ca640e4b0420dd5c8440adb0c9bcc92768eb2cb5:206c711dbb9549c63abea09fc625ab10dda04329-0' }"

    input:
    path(file_data)
    path(file_config)
    path(file_trained_model)
    path(test_data)
    path ml_custom_scripts

    output:
    path "*.csv", emit: csv
    path "*_mqc.csv", emit: multiqc_csv
    path "versions.yml"           , emit: versions

    when:
    task.ext.when == null || task.ext.when

    script:
    def args = task.ext.args ?: ''

    """
    ml_validate.R $file_data $file_config $file_trained_model $test_data
    cp *_eval.csv mlvalidate_mqc.csv
    cat <<-END_VERSIONS > versions.yml
    "${task.process}":
        mlvalidate: 0.3
        r-base: \$(echo \$(R --version 2>&1) | sed 's/^.*R version //; s/ .*\$//')
        r-tidyverse: \$(Rscript -e "library(tidyverse); cat(as.character(packageVersion('tidyverse')))")
        r-data.table: \$(Rscript -e "library(data.table); cat(as.character(packageVersion('data.table')))")
        r-caret: \$(Rscript -e "library(caret); cat(as.character(packageVersion('caret')))")
        r-furrr: \$(Rscript -e "library(furrr); cat(as.character(packageVersion('furrr')))")
        r-purrr: \$(Rscript -e "library(purrr); cat(as.character(packageVersion('purrr')))")
        r-optparse: \$(Rscript -e "library(optparse); cat(as.character(packageVersion('optparse')))")
        r-tidymodels: \$(Rscript -e "library(tidymodels); cat(as.character(packageVersion('tidymodels')))")
    END_VERSIONS
    """
}
