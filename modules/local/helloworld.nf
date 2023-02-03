process HELLOWORLD {
    tag '$bam'
    label 'process_single'

    conda "conda-forge::r-tidyverse=1.3.2 conda-base::r-base=4.1.1"
    container "${ workflow.containerEngine == 'singularity' && !task.ext.singularity_pull_docker_container ?
        'https://depot.galaxyproject.org/singularity/mulled-v2-0c13ef770dd7cc5c76c2ce23ba6669234cf03385:63be019f50581cc5dfe4fc0f73ae50f2d4d661f7-0' :
        'quay.io/biocontainers/mulled-v2-0c13ef770dd7cc5c76c2ce23ba6669234cf03385:63be019f50581cc5dfe4fc0f73ae50f2d4d661f7-0' }"

    input:
    val whatever
    path config_yaml_file


    output:
    stdout

    when:
    task.ext.when == null || task.ext.when

    script:
    def args = task.ext.args ?: ''

    """
    helloworld.R arg1 arg2 arg3
    ml-train.R $config_yaml_file
    """
}
