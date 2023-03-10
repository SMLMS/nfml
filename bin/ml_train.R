#!/usr/bin/env Rscript

library("optparse")
library("rjson")
library("data.table")
library("tidyverse")
library("purrr")
library("furrr")
library("parallel")

# start
start_time = Sys.time()

# pass arguments
args = commandArgs(trailingOnly=TRUE)
file.config = args[1]
file.data = args[2]
file.samples.train = args[3]
file.features.train = args[4]

# read config file
config = rjson::fromJSON(file = file.config)
#config = rjson::fromJSON(file = './example_data/config.example.regression.json')

# sources
# TODO: The grid library will be replaced by the dials package
source('./ml_grids.R')
source('./ml_funcs.R')

# output
# TODO: find out how relative paths work with nf
file.rds = paste0('./', config$fit.id, '.rds')
file.log = paste0('./', config$fit.id, '.log')

# data
# NOTE: using fread because it's faster
df.data = data.table::fread(file.data) %>%
  tibble::column_to_rownames(config$ml.sampleID)

# samples
list.samples = read.csv(file.samples.train, header = F)$V1

# features
list.features = read.csv(file.features.train, header = F)$V1

# set up trainControl
# TODO: implement other methods such as jackknife, bootstrap, ...
trControl = caret::trainControl(
  method = config$ml.cv$method,
  number = as.numeric(config$ml.cv$fold),
  repeats = as.numeric(config$ml.cv$repeats))

# train model
set.seed(as.numeric(config$ml.seed))
cv_model = caret::train(
  y = format_y(df.data[list.samples, config$ml.response], config$ml.type),
  x = df.data[list.samples, list.features, drop=F],
  method = config$ml.method,
  preProcess = config$ml.preprocess,
  trControl = trControl,
  tuneGrid = list.grids[[config$ml.cv$tune.grid]], # NOTE: if NULL tuneLength is used
  tuneLength = config$ml.cv$tune.length
  )

# ml run time
ml.run_time =
  cv_model$times$everything['elapsed'] + cv_model$times$final['elapsed']

# save
saveRDS(cv_model, file.rds)
write.table(t(
  data.frame(
    name.out = config$fit.id,
    file.data = file.data,
    file.samples.train = file.samples.train,
    file.features.train = file.features.train,
    ml.sampleID = config$ml.sampleID,
    ml.seed = config$ml.seed,
    ml.type = config$ml.type,
    ml.method = config$ml.method,
    ml.response = config$ml.response,
    ml.preProcess = config$ml.preprocess,
    ml.fold = config$ml.cv$fold,
    ml.repeats = config$ml.cv$repeats,
    ml.grid = config$ml.cv$tune.grid,
    ml.run_time = ml.run_time,
    note.log = config$note)),
  file.log, row.names = T, quote = F, col.names = F, sep='\t')

# stop
#Nextflow tracks runtime, so this is not required - we even get nicer plots / info on how things are running / perform with memory/cpu usage.
#end_time = Sys.time()
#run_time = end_time - start_time
#print(run_time)
