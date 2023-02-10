//
// Check input samplesheet and get read channels
//

include { HELLOWORLD } from '../../modules/local/helloworld'

workflow INPUT_CHECK {
    take:
    helloworld // file: ../../modules/local/helloworld.nf

    main:
    HELLOWORLD ( file )
        .set { output}




    emit:
    output                                     // channel: [ val(meta), [ output ] ]
    versions = HELLOWORLD.out.versions // channel: [ versions.yml ]
}

