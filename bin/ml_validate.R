# v0.3

# start
start_time = Sys.time()

# pass arguments
args = commandArgs(trailingOnly=TRUE)

# read config file
config = rjson::fromJSON(file = args[1])
#config = rjson::fromJSON(file = './example_data/config.example.regression.json')

# source
source('./ml_funcs.R')

# load trained model
file.rds = paste0('./fits/', config$fit.id, '.rds')
cv_model = read_rds(file.rds)

# data
# NOTE: using fread because it's faster
df.data = data.table::fread(config$file.data) %>%
  tibble::column_to_rownames(config$ml.sampleID)

# features
list.features = colnames(cv_model$trainingData)[-ncol(cv_model$trainingData)]

# prepare list with test samples
list.test = lapply(config$list.samples.test, function(x)
  # read in samples test
  read.csv(x$file, header = F)$V1) %>%
  # assign names
  magrittr::set_names(lapply(config$list.samples.test, function(x) x$name))

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
fwrite(df.eval, paste0('./fits/', config$fit.id, '_eval.csv'))


#' TODO:
#' How to take care of data pre-processing?
#' Should we add a resampling strategy?