## Load packages ---------------------------------------------------------------
if (!require(needs)) install.packages("needs")
needs::needs(readr, tidyr, RColorBrewer, plotly)

## Constants -------------------------------------------------------------------
DATA_PATH <- "data"
R_PATH <- "R"
CORES <- 1  # For parallel processing
SEED <- 14563  # For a fair comparison between models
FOLDS <- 5
REPEATS <- 2
L <- 200
P_RANGE <- seq(from = 5, to = 95, by = 5)

## Source files ----------------------------------------------------------------
source(file.path(R_PATH, "bagging-functions.R"))
source(file.path(R_PATH, "cv-functions.R"))
source(file.path(R_PATH, "pruning-functions.R"))
source(file.path(R_PATH, "util-functions.R"))

## Read data into the workspace ------------------------------------------------
pima_data <- read_csv(file.path(DATA_PATH, "pima.csv"), 
                      col_names = TRUE, 
                      col_types = cols(
                        Class = col_factor(c("positive", "negative"))
                      ))

## Cross-validation process ----------------------------------------------------
results <- vector(mode = "list", length = length(P_RANGE))
for (i in seq_along(P_RANGE)){
  results[[i]] <- cvTrain(pima_data, 
                          method = "bagging", 
                          method_args = list(L = L, pruning = "EPIC", 
                                             pruning_args = list(p = P_RANGE[i]), 
                                             cores = CORES), 
                          folds = FOLDS, repeats = REPEATS, 
                          cores = CORES, seed = SEED)
}

## Assemble the results --------------------------------------------------------
results <- lapply(results, rbindList)
results <- lapply(results, function(x) rbind(colMeans(x)))
results <- lapply(seq_along(P_RANGE), 
                  function(i) cbind(results[[i]], data.frame(P = P_RANGE[i])))
results <- rbindList(results)
results_tidy <- gather(results, measure, value, -P)
results_tidy[["measure"]] <- factor(results_tidy[["measure"]])


## Plot the results ------------------------------------------------------------
pal <- brewer.pal(nlevels(results_tidy$measure), "Set1")
pl <- plot_ly(results_tidy, x = P, y = 1-value, color = measure, colors = pal)
pl <- layout(pl, 
             xaxis = list(title = "Percent of classifiers chosen by EPIC"), 
             yaxis = list(title = "Error"))
