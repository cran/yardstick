## -----------------------------------------------------------------------------
#| label: setup
#| include: false
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)


## -----------------------------------------------------------------------------
#| label: pkgs
#| message: false
library(yardstick)
library(dplyr)

data("hpc_cv")


## -----------------------------------------------------------------------------
#| label: hpc-cv
tibble(hpc_cv)


## -----------------------------------------------------------------------------
#| label: hpc-modify
set.seed(1)

hpc <-
  tibble(hpc_cv) |>
  mutate(batch = sample(c("a", "b"), nrow(hpc_cv), replace = TRUE)) |>
  select(-c(VF, F, M, L))

hpc


## -----------------------------------------------------------------------------
#| label: acc-1
hpc |> 
  filter(Resample == "Fold01") |>
  accuracy(obs, pred)


## -----------------------------------------------------------------------------
#| label: hpc-cv-2
hpc |> 
  group_by(Resample) |>
  accuracy(obs, pred)


## -----------------------------------------------------------------------------
#| label: res-1
hpc |> 
  filter(Resample == "Fold01")


## -----------------------------------------------------------------------------
#| label: acc-by-group
acc_by_group <- 
  hpc |> 
  filter(Resample == "Fold01") |>
  group_by(batch) |>
  accuracy(obs, pred)

acc_by_group


## -----------------------------------------------------------------------------
#| label: diff-acc
diff(c(acc_by_group$.estimate[2], acc_by_group$.estimate[1]))


## -----------------------------------------------------------------------------
accuracy_diff <-
  new_groupwise_metric(
    fn = accuracy,
    name = "accuracy_diff",
    aggregate = function(acc_by_group) {
      diff(c(acc_by_group$.estimate[2], acc_by_group$.estimate[1]))
    }
  )


## -----------------------------------------------------------------------------
#| label: acc-diff-class
class(accuracy_diff)


## -----------------------------------------------------------------------------
#| label: acc-diff-by
accuracy_diff_by_batch <- accuracy_diff(batch)


## -----------------------------------------------------------------------------
#| label: metric-classes
class(accuracy)

class(accuracy_diff_by_batch)


## -----------------------------------------------------------------------------
#| label: ex-acc-diff-by-batch
hpc |> 
  filter(Resample == "Fold01") |>
  accuracy_diff_by_batch(obs, pred)


## -----------------------------------------------------------------------------
#| label: ex-acc-diff-by-batch-ms
acc_ms <- metric_set(accuracy, accuracy_diff_by_batch)

hpc |> 
  filter(Resample == "Fold01") |>
  acc_ms(truth = obs, estimate = pred)


## -----------------------------------------------------------------------------
#| label: ex-acc-diff-by-batch-2
hpc |> 
  group_by(Resample) |>
  accuracy_diff_by_batch(obs, pred)

