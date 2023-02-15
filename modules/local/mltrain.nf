process MLTRAIN {
    tag 'training'
    label 'process_single'

    //This lists the required conda recipes for running  the process with conda, below the container URLs are listed that are built on "bioconda/multi-package-containers", see PR https://github.com/BioContainers/multi-package-containers/pull/2509
    conda "conda-forge::r-base conda-forge::r-tidyverse=1.3.1 conda-forge::r-data.table=1.14.6 conda-forge::r-caret=6.0_93 conda-forge::r-rjson=0.2.21 conda-forge::r-furrr=0.3.1 conda-forge::r-purrr=1.0.1 conda-forge::r-optparse=1.7.3"
    container "${ workflow.containerEngine == 'singularity' && !task.ext.singularity_pull_docker_container ?
        'https://depot.galaxyproject.org/singularity/mulled-v2-4eab235fcbfce7261ccbf98de3bffb97f765d3d2:0b703f4d953cb368358a647821708bf28f1ebbad-0' :
        'quay.io/biocontainers/mulled-v2-4eab235fcbfce7261ccbf98de3bffb97f765d3d2:0b703f4d953cb368358a647821708bf28f1ebbad-0' }"

    input:
    path json_files

    output:
    path "*.rds", emit: rds
    path "*.log", emit: log
    path "versions.yml"           , emit: versions

    when:
    task.ext.when == null || task.ext.when

    script:
    def args = task.ext.args ?: ''

    """
    ml_train.R $json_files

    cat <<-END_VERSIONS > versions.yml
    "${task.process}":
        mltrain: 0.3
        r-base: \$(echo \$(R --version 2>&1) | sed 's/^.*R version //; s/ .*\$//')
        r-tidyverse: \$(Rscript -e "library(tidyverse); cat(as.character(packageVersion('tidyverse')))")
        r-data.table: \$(Rscript -e "library(data.table); cat(as.character(packageVersion('data.table')))")
        r-caret: \$(Rscript -e "library(caret); cat(as.character(packageVersion('caret')))")
    END_VERSIONS
    """
}
