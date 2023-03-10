#!/usr/bin/env Rscript

library("optparse")
library("rjson")
library("data.table")
library("tidyverse")
library("purrr")
library("furrr")
library("parallel")
library("caret")

# v0.3

# start
start_time = Sys.time()

# pass arguments
# args[1] = config in JSON format
# args[2] = path to previously computed rds file
# FIXME: change inmput args (1: config, 2,
args = commandArgs(trailingOnly=TRUE)

# read config file
file.data = args[1]
file.config = args[2]
file.trained_model = args[3]
list.samples.test = args[4:length(args)]

# load config
config = rjson::fromJSON(file = file.config)

# source
source('./ml_funcs.R')

# load trained model
#file.rds = paste0('./fits/', config$fit.id, '.rds')
cv_model = readRDS(file.trained_model)

# data
# NOTE: using fread because it's faster
df.data = data.table::fread(file.data) %>%
    tibble::column_to_rownames(config$ml.sampleID)

# features
list.features = colnames(cv_model$trainingData)[-ncol(cv_model$trainingData)]

# prepare list with test samples
list.test = lapply(list.samples.test, function(x)
  # read in samples test
    read.csv(x, header = F)$V1) %>%
  # assign names
    magrittr::set_names(lapply(list.samples.test, function(x) x))

#' write function to
#' - bootstrap performance variable
#' - re-sample response variable
#' - evaluate performance in context of re-sampled runs

# evaluate on test data
df.eval = lapply(list.test, function(x){
  # predict
  y.model = predict(cv_model, df.data[x, list.features])
  # data vector
  y.data = format_y(df.data[x, config$ml.response], config$ml.type)
  # performance evaluation
  postResample(y.model, y.data)}) %>%
  bind_rows(.id='test_data') %>%
  data.table

# write output
fwrite(df.eval, paste0('./', config$fit.id, '_eval.csv'))


#' TODO:
#' How to take care of data pre-processing?
#' Should we add a resampling strategy?
