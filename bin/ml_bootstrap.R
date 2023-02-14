library("optparse")
library("rjson")
library("data.table")
library("tidyverse")
library("purrr")
library("furrr")
library("parallel")

source("./Resampler.R")
source('./ml_funcs.R')

#' @name create_parser
#' @author Sebastian Malkusch
#' @title create_parser
#' @description Creates an object that defines and handles command line arguments.
#' @details A parser that organizes the communication between the user and th function.
#' It also provides a help message.
#' @return An instance of type 'optparse::OptionParser'.
create_parser = function(){
option_list = list(
  make_option(c("-f", "--file"), type="character", default=NULL, 
              help="config file name", metavar="character"),
  make_option(c("-m", "--method"), type="character", default="none", 
               help="method of permutation. Options are (none, features, response). [default= %default]", metavar="character"),
  make_option(c("-c", "--cores"), type="integer", default=1, 
              help="available cores for multi-processing [default= %default]", metavar="character")
)

opt_parser = optparse::OptionParser(option_list=option_list)
opt = optparse::parse_args(opt_parser)
return(opt)
}


#' @name create_resample_experiment
#' @author Sebastian Malkusch
#' @title create_resample_experiment
#' @description Creates an object of a resampling experiment.
#' @details Creates a resampling experiment.
#' It uses user defined parameters to set up the experiment.
#' It creates an instance of the Resampler object
#' and runs the experiment according to the user-defined parameters.
#' @param seed sets the seed for the random number generator to guarantee reproducibility.
#' (int)
#' @param data_df data frame to be learned from. 
#' (tibble::tibble)
#' @param parser_inst instance of parser object. (optparse::parse_args).
#' @param model_inst instance of caret_train object (caret::train).
#' @param config_inst list of config options (list).
#' @return An instance of type 'Resampler'.
create_resample_experiment = function(seed, data_df, parser_inst,  model_inst, config_inst, n_features){
  #set seed
  set.seed(seed)
  
  # create Resampler instance
  resampler_inst = Resampler$new(permute = parser_inst$method,
                                 n_resample = as.integer(config_inst$ml.bootstrap$n.resamples),
                                 ml_method = model_inst$method,
                                 ml_type = config_inst$ml.type,
                                 hyper_parameters = model_inst$bestTune,
                                 pre_process_lst = config_inst$ml.preprocess,
                                 response_var = config_inst$ml.response,
                                 n_features = as.integer(n_features),
                                 strata_var = NULL)
  # strata_var is not yet implemented!! How to handle empty variable?
  
  # Train model
  resampler_inst$fit(data_df = data_df)
  
  # return trained object 
  return(resampler_inst)
}


#' @name main
#' @author Sebastian Malkusch
#' @title main
#' @description Main function that sets up and runs a resampling experiment.
#' @details The experiment is run in parallel.
#' All results are written to files.
main = function(){
  # get command line arguments
  parser_inst <- create_parser()
  
  # set up environment for parallel computing
  n_cores <- parallel::detectCores()
  if(parser_inst$cores == 1){
    print("running sequential")
    future::plan(strategy = "sequential")
  }
  else if(n_cores < parser_inst$cores){
    print(sprintf("running in parallel on %i cores", n_cores))
    future::plan(strategy = "multisession", workers = n_cores)
  }else{
    print(sprintf("running in parallel on %i cores", parser_inst$cores))
    future::plan(strategy = "multisession", workers = parser_inst$cores)
  }
  
  # define start time
  start_time <- Sys.time()
  
  # read config
  config_inst <- rjson::fromJSON(file = parser_inst$file)
  
  # read model
  path_to_model <- paste0('./fits/', config_inst$fit.id, '.rds')
  model_inst <- read_rds(path_to_model)
  
  # read data
  data_df <- data.table::fread(config_inst$file.data) %>%
    tibble::column_to_rownames(config_inst$ml.sampleID) %>%
    as.data.frame()
  
  # read samples
  samples_lst <- read.csv(config_inst$file.samples.train, header = FALSE)$V1
  
  # read features
  train_features_lst <- read.csv(config_inst$file.features.train, header = FALSE)$V1
  resample_features_lst <- read.csv(config_inst$file.features.resample, header = FALSE)$V1
  complete_features_lst <- append(train_features_lst, resample_features_lst)
  n_features <- length(train_features_lst)
  
  # filter data
  filtered_df <- switch (parser_inst$method,
    'none' = {
      data_df[samples_lst,] %>%
        dplyr::mutate(!!as.symbol(config_inst$ml.response) := format_y(!!as.symbol(config_inst$ml.response), config_inst$ml.type)) %>%
        dplyr::select(dplyr::all_of(append(train_features_lst, config_inst$ml.response))) %>%
        return()
      },
    'response' = {
      data_df[samples_lst,] %>%
        dplyr::mutate(!!as.symbol(config_inst$ml.response) := format_y(!!as.symbol(config_inst$ml.response), config_inst$ml.type)) %>%
        dplyr::select(dplyr::all_of(append(train_features_lst, config_inst$ml.response))) %>%
        return()
    },
    'features' = {
      data_df[samples_lst,] %>%
        dplyr::mutate(!!as.symbol(config_inst$ml.response) := format_y(!!as.symbol(config_inst$ml.response), config_inst$ml.type)) %>%
        dplyr::select(dplyr::all_of(append(complete_features_lst, config_inst$ml.response))) %>%
        return()
    },
    stop(sprintf("Resampling method %s is unknown. Needs to be none, features or response.", parser_inst$method))
  )

  # run permutation experiment
  bootstrap_df <- tibble::tibble(permutations = seq(as.integer(config_inst$ml.bootstrap$n.permutations)), seed = as.integer(config_inst$ml.seed) + seq(as.integer(config_inst$ml.bootstrap$n.permutations))) %>%
    dplyr::mutate(permutation_type = parser_inst$method) %>%
    dplyr::mutate(resample_obj = furrr::future_map(.options = furrr::furrr_options(seed = TRUE), .x = seed, .f = purrr::possibly(.f = create_resample_experiment, otherwise = NULL), filtered_df, parser_inst,  model_inst, config_inst, n_features)) %>%
    dplyr::mutate(metrics = purrr::map(.x = resample_obj, .f = purrr::possibly(.f = function(x){x$metrics_df}, otherwise = NULL))) %>%
    dplyr::mutate(confusion = purrr::map(.x = resample_obj, .f = purrr::possibly(.f = function(x){x$confusion_df}, otherwise = NULL)))


  # write metrics
  metrics_df <- bootstrap_df %>%
    dplyr::select(permutation_type, permutations, seed, metrics) %>%
    tidyr::unnest(metrics)


  print(head(metrics_df))

  path_to_metrics_file <- sprintf("./fits/%s_permute_%s_bootstrap_metrics.csv", config_inst$fit.id,  parser_inst$method)
  readr::write_csv(metrics_df, path_to_metrics_file)


  # write confusion
  if(config_inst$ml.type == "classification"){
    confusion_df <- bootstrap_df %>%
      dplyr::select(permutation_type, permutations, seed, confusion) %>%
      tidyr::unnest(confusion)

    path_to_confusion_file <- sprintf("./fits/%s_permute_%s_bootstrap_confusion.csv", config_inst$fit.id,  parser_inst$method)
    readr::write_csv(confusion_df, path_to_confusion_file)
  }
  
  # log computation time
  end_time = Sys.time()
  run_time = end_time - start_time
  
  # save experimental conditions
  file.log = sprintf("./fits/%s_permute_%s_bootstrap.log", config_inst$fit.id,  parser_inst$method)
  write.table(t(
    data.frame(
      name.out = config_inst$fit.id,
      file.data = config_inst$file.data,
      file.samples.train = config_inst$file.samples.train,
      file.features.train = config_inst$file.features.train,
      file.features.resample = config_inst$file.features.resample,
      ml.model = path_to_model,
      ml.seed = config_inst$ml.seed,
      ml.type = config_inst$ml.type,
      ml.method = config_inst$ml.method,
      ml.response = config_inst$ml.response,
      ml.preProcess = config_inst$ml.preprocess,
      boot.permutation.method = parser_inst$method,
      boot.n.resamples = config_inst$ml.bootstrap$n.resamples,
      boot.n.permutations = config_inst$ml.bootstrap$n.permutations,
      boot.n.cores = parser_inst$cores,
      boot.run_time = sprintf("%.3f", run_time),
      note.log = config_inst$note)),
    file.log, row.names = TRUE, quote = FALSE, col.names = FALSE, sep='\t')

  # closing remarks
  cat(sprintf("\n\nRan bootstrap experiment with permutation type %s in %.3f seconds on %i cores.\n%s\n", parser_inst$method, run_time, parser_inst$cores, config_inst$note))
}


# call main function
main()

