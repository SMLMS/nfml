library("optparse")
library("rjson")
library("tidyverse")
library("caret")
library("fastshap")
library("vip")
source("./ml_funcs.R")

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
    make_option(c("-m", "--method"), type="character", default="permutation", 
                help="method for model interpretation. Options are (permutation, shap). [default= %default]", metavar="character"),
    make_option(c("-t", "--trained"), type="character", default = NULL,
                help = "trained caret object (*.rds).", metavar="character")
  )
  opt_parser = optparse::OptionParser(option_list=option_list)
  opt = optparse::parse_args(opt_parser)
  return(opt)
}

#' @name main
#' @author Sebastian Malkusch
#' @title main
#' @description Main function that sets up and runs a post-hoc interpretation of an ml experiment.
#' All results are written to rds files.
main = function(){
  # define start time
  start_time <- proc.time()
  # get command line arguments
  parser_inst <- create_parser()
  
  # read config
  config_inst <- rjson::fromJSON(file = parser_inst$file)
  
  # read data
  data_df <- data.table::fread(config_inst$file.data) %>%
    tibble::column_to_rownames(config_inst$ml.sampleID) %>%
    as.data.frame()
  
  # read samples
  samples_lst <- read.csv(config_inst$file.samples.train, header = FALSE)$V1
  
  # read features
  train_features_lst <- read.csv(config_inst$file.features.train, header = FALSE)$V1
  n_features <- length(train_features_lst)
  
  # filter data
  filtered_df <- data_df[samples_lst,] %>%
    dplyr::mutate(!!as.symbol(config_inst$ml.response) := format_y(!!as.symbol(config_inst$ml.response), config_inst$ml.type)) %>%
    dplyr::select(dplyr::all_of(append(train_features_lst, config_inst$ml.response)))
  
  # create resampler
  ml_resamples <- as.integer(config_inst$ml.shap$n.repeats)
  
  # read tuned model
  model_inst <- readRDS(file = parser_inst$trained)
  ml_resamples <- as.integer(config_inst$ml.interpret$n.repeats)
  ml_response <- config_inst$ml.response
  ml_mode <- config_inst$ml.type
  ml_metric <- switch(ml_mode,
                      "regression" = "rmse",
                      "classification" = "accuracy")
  ml_seed <- as.integer(config_inst$ml.seed)
  
  # run interpretation experiment.
  set.seed(ml_seed)
  interpret_inst <- switch (parser_inst$method,
                            "permutation" = {
                              vip::vi_permute(object = model_inst,
                                              target = ml_response,
                                              metric = ml_metric,
                                              pred_wrapper = predict,
                                              train = filtered_df,
                                              smaller_is_better = TRUE,
                                              type = "difference",
                                              nsim = ml_resamples) %>%
                                return()
                            },
                            "shap" = {
                              fastshap::explain(object = model_inst,
                                                feature_names = train_features_lst,
                                                pred_wrapper = predict,
                                                nsim = ml_resamples,
                                                X = as.data.frame(dplyr::select(filtered_df, dplyr::all_of(train_features_lst))),
                                                adjust = FALSE) %>%
                                return()
                            },
                            stop(sprintf("Interpretation method %s is unknown. Needs to be permutation or shap.", parser_inst$method))
  )
  
  # log computation time
  end_time = proc.time()
  run_time = end_time - start_time
  
  # save results
  path_to_results <- sprintf("./%s_interpretation_%s.rds", parser_inst$method, config_inst$fit.id)
  saveRDS(interpret_inst, file = path_to_results) 
  
  # closing remarks
  cat(sprintf("\n\nRan %s-based post hoc %s model interpretation experiment with %i resamples in %.3f minutes.\n%s\n", parser_inst$method, config_inst$ml.type, ml_resamples, run_time[[3]]/60, config_inst$note))
}

main()
