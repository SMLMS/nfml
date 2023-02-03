1.) use nf-core modules create to create a separate module for each step in the pipeline
 * Ideally name them as the r-script are named, easier for later
 * modules will be available then in ./modules/local/<name>.nf
 Have a look at "processes" section in Nextflow Documentation: https://www.nextflow.io/docs/latest/process.html#
 * Push the created modules to Github using the Git integration
2.) So we have a hierarchy in Nextflwo Workflows that work as such

main.nf --> main entry point for workflow
   calls -> workflow/<...>
     calls --> subworkflow/<...>
        calls --> module/<...>
          (which consists of a single process)
