library("tidyverse")
library("tidymodels")
library("caret")
library("R6")

#' @title Resampler
#'
#' @description
#' Model validation by repeated bootstrapping
#'
#' @details
#' Uses repeated bootstrapping to validate models without a test data set.
#' For each experiment multiple metrics are measured.
#' For classification experiments the confusion matrix is calculated additionally.
#' In order to test hypotheses, either features or the response variable can be permuted.
#'
#' @format [R6::R6Class] object.
#'
#' @importFrom R6 R6Class
#' @importFrom tibble as_tibble tibble
#' @importFrom dplyr all_of if_else mutate pull select
#' @importFrom magrittr %>%
#' @importFrom caret confusionMatrix train trainControl
#' @importFrom rsample analysis bootstraps permutations rsample2caret
#'
#' @keywords validation, hypotheses
#'
#' @examples
#' \dontrun{
#'   library("caret")
#'   data(iris)
#'   
#'   iris_df <- iris
#'   
#'   trControl = caret::trainControl(
#'     method = "repeatedcv", 
#'     number = 5, 
#'     repeats = 10)
#'  
#'   cv_model = caret::train(
#'     y = iris_df$Species,
#'     x = dplyr::select(iris_df, -Species), 
#'     method = "ranger",
#'     preProcess = c('scale', 'center'),
#'     trControl = trControl,
#'     tuneGrid = NULL, # NOTE: if NULL tuneLength is used
#'     tuneLength = 10)
#'  
#'   resampler_inst = Resampler$new(permute = "response",
#'                                  n_resample = 5,
#'                                  ml_method = "ranger",
#'                                  ml_type = "classification",
#'                                  hyper_parameters = cv_model$bestTune,
#'                                  pre_process_lst = c('scale', 'center'),
#'                                  response_var = "Species",
#'                                  n_features = 4,
#'                                  strata_var = NULL)#config_inst$ml.bootstrap$strata_var
#'   
#'   resampler_inst$fit(data_df = iris_df)
#'}   
#'
#'
#' @author Sebastian Malkusch
#'
#' @export
#'

Resampler = R6::R6Class("Resampler",
                        private = list(
                          ######################
                          # instance variables #
                          ######################
                          .permute = "factor",
                          .permute_alphabet = "character",
                          .n_resample = "integer",
                          .ml_method = "character",
                          .ml_type = "factor",
                          .ml_type_alphabet = "character",
                          .pre_process_lst = "character",
                          .hyper_parameters = "list",
                          .response_var = "character",
                          .n_features = "integer",
                          .strata_var = "character",
                          .metrics_df = "tbl.df",
                          .confusion_df = "tbl_df",
                          #####################
                          # private functions #
                          #####################
                          #' @description 
                          #' checks, if permutation is requested.
                          #' If true, performs the permutation task.
                          .permute_data = function(data_df = "tbl_df"){
                            switch(as.character(private$.permute),
                                   "response" = {
                                     data_df %>%
                                       rsample::permutations(permute = private$.response_var, times = 1, apparent = FALSE) %>%
                                       dplyr::pull(splits) %>%
                                       .[[1]] %>%
                                       rsample::analysis() %>%
                                       return()
                                   },
                                   "features" = {
                                     sample_colnames <- colnames(data_df)[!sapply(colnames(data_df), function(s) { s == private$.response_var})] %>%
                                       sample(size = private$.n_features, replace = FALSE)
                                     data_df %>%
                                       dplyr::select(dplyr::all_of(c(sample_colnames, private$.response_var))) %>%
                                       return()
                                   },
                                   "none" = {
                                     return(data_df)
                                   }
                                   )
                          },
                          #' @description 
                          #' Checks if ml.type is classification.
                          #' If true, calculates confusion matrix.
                          .analyze_confusion = function(boot_mod_inst = "caret::train"){
                            private$.confusion_df <- tibble::tibble()
                            if(private$.ml_type == "classification"){
                              conf_mat <- caret::confusionMatrix(boot_mod_inst, norm='none')
                              private$.confusion_df <- conf_mat$table %>%
                                tibble::as_tibble() %>%
                                dplyr::mutate(n_resample = conf_mat$B)
                            }
                            
                          }
                        ),
                        ####################
                        # accessor methods #
                        ####################
                        active = list(
                          #' @field permute
                          #' returns the instance variable 'permute'.
                          #' (character)
                          permute = function(){
                            return(as.character(private$.permute))
                          },
                          #' @field permute_alphabet
                          #' returns the instance variable 'permute_alphabet'.
                          #' (character)
                          permute_alphabet = function(){
                            return(private$.permute_alphabet)
                          },
                          #' @field n_resample
                          #' returns the instance variable 'n_resample'.
                          #' (integer)
                          n_resample = function(){
                            return(private$.n_resample)
                          },
                          #' @field ml_method
                          #' returns the instance variable 'ml_method'.
                          #' (character)
                          ml_method = function(){
                            return(private$.ml_method)
                          },
                          #' @field ml_type
                          #' returns the instance variable 'ml_type'.
                          #' (character)
                          ml_type = function(){
                            return(as.character(private$.ml_type))
                          },
                          #' @field ml_type_alphabet
                          #' returns the instance variable 'ml_type_alphabet'.
                          #' (character)
                          ml_type_alphabet = function(){
                            return(private$.ml_type_alphabet)
                          },
                          #' @field pre_process_lst
                          #' returns the instance variable 'pre_process_lst'.
                          #' (character)
                          pre_process_lst = function(){
                            return(private$.pre_process_lst)
                          },
                          #' @field hyper_parameters
                          #' returns the instance variable 'hyper_parameters'.
                          #' (list)
                          hyper_parameters = function(){
                            return(private$.hyper_parameters)
                          },
                          #' @field response_var
                          #' returns the instance variable 'response_var'.
                          #' (character)
                          response_var = function(){
                            return(private$.response_var)
                          },
                          #' @field n_features
                          #' returns the instance variable 'n_features'.
                          #' (integer)
                          n_features = function(){
                            return(private$.n_features)
                          },
                          #' @field strata_var
                          #' returns the instance variable 'strata_var'.
                          #' (character)
                          strata_var = function(){
                            return(private$.strata_var)
                          },
                          #' @field metrics_df
                          #' returns the instance variable 'metrics_df'.
                          #' (tibble::tibble)
                          metrics_df = function(){
                            return(private$.metrics_df)
                          },
                          #' @field confusion_df
                          #' returns the instance variable 'confusion_df'.
                          #' (tibble::tibble)
                          confusion_df = function(){
                            return(private$.confusion_df)
                          }
                        ),
                        ##################
                        # public methods #
                        ##################
                        public = list(
                          #' @description
                          #' Creates and returns instance of
                          #' Resampler class.
                          #' @param n_resample
                          #' number of bootstrap resamples.
                          #' The default is 500 (integer)
                          #' @param ml_method
                          #' ML model that is being used.
                          #' The default is 'pcr' (character).
                          #' @param ml_type
                          #' ML model type. Needs to be 'classfification',
                          #' 'regression' or 'censored'.
                          #' Default is 'classification' (character).
                          #' @param hyper_parameters
                          #' List of model hyper parameters.
                          #' (list)
                          #' @param pre_process_lst
                          #' Vector of pre-processing steps.
                          #' Default is 'c("center", "scale")' (character).
                          #' @param permute
                          #' Permutation method. Needs to be 'none', 'features'
                          #' or 'response'.
                          #' (character)
                          #' @param n_features
                          #' Number of features to be chosen in the permutation
                          #' experiment.
                          #' Default is 0 (integer).
                          #' @param response_var
                          #' Response variable of the model (character).
                          #' @param strata_var
                          #' Stratification variable (character).
                          #' @return Resampler
                          initialize = function(n_resample = 500, ml_method = "pcr", ml_type = "classification", hyper_parameters = "list", pre_process_lst = c("center", "scale"), permute = NULL, n_features = 0, response_var="character", strata_var = NULL){
                            private$.permute_alphabet <- c("none", "response", "features")
                            private$.ml_type_alphabet <- c("classification", "regression", "censored")
                            if(!permute %in%  private$.permute_alphabet){
                              warning(sprintf("\npermute value %s is not known. Must be either 'response' or 'features'. response will be set to none\n", permute))
                              private$.permute <- factor("none", levels = private$.permute_alphabet)
                            }else{
                              private$.permute <- factor(permute, levels = private$.permute_alphabet)
                            }
                            if(!ml_type %in%  private$.ml_type_alphabet){
                              stop(sprintf("\nml_type value %s is not known. Must be either 'classification', 'regression' or 'censored'.\n", ml_type))
                              private$.ml_type <- factor(NULL, levels = private$.ml_type_alphabet)
                            }else{
                              private$.ml_type <- factor(ml_type, levels = private$.ml_type_alphabet)
                            }
                            private$.ml_type_alphabet <- c("classification", "regresseion", "censored")
                            private$.n_resample <- as.integer(n_resample)
                            private$.ml_method <- ml_method
                            private$.pre_process_lst <- pre_process_lst
                            private$.hyper_parameters <- hyper_parameters
                            private$.response_var = response_var
                            private$.strata_var <- strata_var
                            private$.n_features <- as.integer(n_features)
                            private$.metrics_df <- tibble::tibble()
                            private$.confusion_df <- tibble::tibble()
                          },
                          #' @description
                          #' Print instance variables of Resampler class.
                          #' @return character
                          print = function(){
                            s <- sprintf("\n Validation by resampling and permutation: \n")
                            s <- sprintf("%s \n\n", s)
                            s <- sprintf("%s model: %s\n", s, self$ml_method)
                            s <- sprintf("%s ml_type: %s\n", s, self$ml_type)
                            s <- sprintf("%s permutation method: %s\n", s, self$permute)
                            s <- sprintf("%s number of bootstrap resamples: %i\n", s, self$n_resample)
                            s <- sprintf("%s pre-processing steps:", s)
                            for (i in seq(length(self$pre_process_lst))){
                              s <- sprintf("%s %s", s,self$pre_process_lst[i])
                            }
                            s <- sprintf("%s\n", s)
                            s <- sprintf("%s response variable: %s\n", s, self$response_var)
                            s <- sprintf("%s stratification variable: %s\n", s, dplyr::if_else(is.null(self$strata_var), "NULL", self$strata_var))
                            s <- sprintf("%s number of features: %s (only used if permute == 'features')\n", s, self$n_features)
                            s <- sprintf("%s \n\n", s)
                            cat(s)
                            invisible(self)
                          },
                          #' @description
                          #' Runs the bootstrap analysis based on the instance
                          #' variables chosen under initialize. 
                          #' @param data_df
                          #' data set to be analyzed (tibble::tibble).
                          #' @return
                          #' None
                          fit = function(data_df = "tbl_df"){
                            # reset result variable
                            private$.metrics_df <- tibble::tibble()
                            
                            # permute data according to permute instance variable
                            permuted_df <- private$.permute_data(data_df = data_df)
                            
                            
                            # create bootstrap object
                            bootstrap_obj <- rsample::bootstraps(permuted_df, times = self$n_resample, strata = dplyr::all_of(self$strata_var), apparent = FALSE) %>%
                              rsample::rsample2caret()
                            
                            # design bootstrap by creating a caret trainControl object
                            bs_control_obj <- caret::trainControl(index = bootstrap_obj$index,
                                                                  indexOut = bootstrap_obj$indexOut,
                                                                  returnData = TRUE,
                                                                  returnResamp = "final",
                                                                  classProbs = FALSE,
                                                                  allowParallel = TRUE)
                            
                            # run experiment
                            boot_mod_inst <- caret::train(
                              x = dplyr::select(permuted_df, -dplyr::all_of(self$response_var)),
                              y = dplyr::pull(permuted_df, dplyr::all_of(self$response_var)),
                              method = self$ml_method,
                              preProcess = self$pre_process_lst,
                              tuneLength = 1,
                              tuneGrid = self$hyper_parameters,
                              trControl = bs_control_obj
                            )
                            
                            # Lumpensammler
                            private$.metrics_df <- boot_mod_inst$resample
                            private$.analyze_confusion(boot_mod_inst)
                          }
                        )
)