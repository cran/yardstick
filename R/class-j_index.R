#' J-index
#'
#' @description
#' Youden's J statistic is defined as:
#'
#' [sens()] + [spec()] - 1
#'
#' A related metric is Informedness, see the Details section for the relationship.
#'
#' @details
#'
#' The value of the J-index ranges from \[0, 1\] and is `1` when there are
#' no false positives and no false negatives.
#'
#' The binary version of J-index is equivalent to the binary concept of
#' Informedness. Macro-weighted J-index is equivalent to multiclass informedness
#' as defined in Powers, David M W (2011), equation (42).
#'
#' @family class metrics
#' @templateVar metric_fn j_index
#' @template event_first
#' @template multiclass
#' @template return
#'
#' @inheritParams sens
#'
#' @author Max Kuhn
#'
#' @references
#'
#' Youden, W.J. (1950). "Index for rating diagnostic tests". Cancer. 3: 32-35.
#'
#' Powers, David M W (2011). "Evaluation: From Precision, Recall and F-Score to
#' ROC, Informedness, Markedness and Correlation". Journal of Machine Learning
#' Technologies. 2 (1): 37-63.
#'
#' @template examples-class
#'
#' @export
j_index <- function(data, ...) {
  UseMethod("j_index")
}
j_index <- new_class_metric(
  j_index,
  direction = "maximize"
)

#' @rdname j_index
#' @export
j_index.data.frame <- function(data,
                               truth,
                               estimate,
                               estimator = NULL,
                               na_rm = TRUE,
                               event_level = yardstick_event_level(),
                               ...) {
  metric_summarizer(
    metric_nm = "j_index",
    metric_fn = j_index_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    estimator = estimator,
    na_rm = na_rm,
    event_level = event_level,
    ... = ...
  )
}

#' @export
j_index.table <- function(data,
                          estimator = NULL,
                          event_level = yardstick_event_level(),
                          ...) {
  check_table(data)
  estimator <- finalize_estimator(data, estimator)

  metric_tibbler(
    .metric = "j_index",
    .estimator = estimator,
    .estimate = j_index_table_impl(data, estimator, event_level)
  )
}

#' @export
j_index.matrix <- function(data,
                           estimator = NULL,
                           event_level = yardstick_event_level(),
                           ...) {
  data <- as.table(data)
  j_index.table(data, estimator, event_level)
}

#' @rdname j_index
#' @export
j_index_vec <- function(truth,
                        estimate,
                        estimator = NULL,
                        na_rm = TRUE,
                        event_level = yardstick_event_level(),
                        ...) {
  estimator <- finalize_estimator(truth, estimator)

  j_index_impl <- function(truth, estimate) {
    xtab <- vec2table(
      truth = truth,
      estimate = estimate
    )

    j_index_table_impl(xtab, estimator, event_level)
  }

  metric_vec_template(
    metric_impl = j_index_impl,
    truth = truth,
    estimate = estimate,
    na_rm = na_rm,
    cls = "factor",
    estimator = estimator,
    ...
  )
}

j_index_table_impl <- function(data, estimator, event_level) {
  if(is_binary(estimator)) {
    j_index_binary(data, event_level)
  } else {
    w <- get_weights(data, estimator)
    out_vec <- j_index_multiclass(data, estimator)
    weighted.mean(out_vec, w)
  }
}

j_index_binary <- function(data, event_level) {
  # sens + spec - 1
  recall_binary(data, event_level) + spec_binary(data, event_level) - 1
}

j_index_multiclass <- function(data, estimator) {
  recall_multiclass(data, estimator) + spec_multiclass(data, estimator) - 1
}
