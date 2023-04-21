# binary - requires 1 column of probabilities

    Code
      classification_cost(two_class_example, truth, Class1:Class2)
    Condition
      Error in `classification_cost()`:
      ! You are using a binary metric but have passed multiple columns to `...`.

# costs must be a data frame with the right column names

    Code
      classification_cost(df, obs, A, costs = 1)
    Condition
      Error in `classification_cost()`:
      ! `costs` must be `NULL` or a data.frame.

---

    Code
      classification_cost(df, obs, A, costs = data.frame())
    Condition
      Error in `classification_cost()`:
      ! `costs` must be a data.frame with 3 columns.

---

    Code
      classification_cost(df, obs, A, costs = data.frame(x = 1, y = 2, z = 3))
    Condition
      Error in `classification_cost()`:
      ! `costs` must have columns: 'truth', 'estimate', and 'cost'.

# costs$estimate must contain the right levels

    Code
      classification_cost(df, obs, A, costs = costs)
    Condition
      Error in `classification_cost()`:
      ! `costs$estimate` can only contain 'A', 'B'.

# costs$truth must contain the right levels

    Code
      classification_cost(df, obs, A, costs = costs)
    Condition
      Error in `classification_cost()`:
      ! `costs$truth` can only contain 'A', 'B'.

# costs$truth, costs$estimate, and costs$cost must have the right type

    Code
      classification_cost(df, obs, A, costs = costs)
    Condition
      Error in `classification_cost()`:
      ! `costs$truth` must be a character or factor column.

---

    Code
      classification_cost(df, obs, A, costs = costs)
    Condition
      Error in `classification_cost()`:
      ! `costs$estimate` must be a character or factor column.

---

    Code
      classification_cost(df, obs, A, costs = costs)
    Condition
      Error in `classification_cost()`:
      ! `costs$cost` must be a numeric column.

# costs$truth and costs$estimate cannot contain duplicate pairs

    Code
      classification_cost(df, obs, A, costs = costs)
    Condition
      Error in `classification_cost()`:
      ! `costs` cannot have duplicate 'truth' / 'estimate' combinations.

# errors with class_pred input

    Code
      classification_cost_vec(cp_truth, estimate)
    Condition
      Error in `classification_cost_vec()`:
      ! `truth` should not a `class_pred` object.

