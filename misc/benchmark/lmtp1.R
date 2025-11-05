set.seed(8)

# Pacman used to install and load all packages
if (!require("pacman")) install.packages("pacman")
# Load packages
pacman::p_load(
          "lmtp",
          "data.table",
          "ggplot2",
          "knitr",
          "polle"
        )

dt1 <- data.table(
  durA = c(rep(0, 4), rep(1, 4), rep(2, 4)),
  Aspirin1 = c(rep(0, 4), rep(1, 8)),
  Dead1 = c(rep(0, 3), 1, rep(0, 3), 1, rep(0, 3), 1),
  Aspirin2 = c(rep(0, 3), NA, rep(0, 3), NA, rep(1, 3), NA),
  Dead2 = c(0, rep(1, 3), 0, rep(1, 3), 0, rep(1, 3))
)


dt1[, Censoring1 := c(1, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1)]
## dt1[, Censoring1 := c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)]


dt1[, Dead1 := ifelse(Censoring1 == 0, NA, Dead1)]
dt1[, Aspirin2 := ifelse(Censoring1 == 0, NA, Aspirin2)]
dt1[, Dead2 := ifelse(Censoring1 == 0, NA, Dead2)]

dt1[, Censoring2 := ifelse(Dead1 == 1, NA, 1)]

## dt1[11, Censoring2 := 0]
## dt1[11, Dead2 := NA]

dt1 <- dt1[,
 .(durA, Aspirin1, Censoring1, Dead1, Aspirin2, Censoring2, Dead2)]

# Show data.table
kable(dt1)

# Replicate the data set 100 times
dt <- replicate(100, dt1, simplify = FALSE)

# Stack all replicates on top of each other
dt <- rbindlist(dt)

# Assign new person ID
dt[, id := seq(1, dim(dt)[1], 1)]

# Set random baseline confounder with no confounding
# This is required for the lmtp software to run
#dt[, W1 := rbinom(n = nrow(dt), size = 1, prob = 0.5)]
dt[, W1 := 0]

# data.frame is required by lmtp package
## dt <- as.data.frame((dt))

# Never treat
d0 <- function(data, trt) {
  rep(0, nrow(data))
}

# Treat 1-year and then stop treatment
d_1yr <- function(data, trt) {
  if (trt == "Aspirin1") return(rep(1, nrow(data)))
  if (trt == "Aspirin2") return(rep(0, nrow(data)))
}

# Treat for 2-years (always treat)
d_2yrs <- function(data, trt) {
    rep(1, nrow(data))
}

A <- c("Aspirin1", "Aspirin2")
Y <- c("Dead1", "Dead2")
C <- c("Censoring1", "Censoring2")
W <- c("W1")

treat_2yr <- lmtp_survival(
  estimator = "lmtp_tmle",
  data = as.data.frame(dt),
  trt = A,
  outcome = Y,
  baseline = W,
  cens = C,
  shift = d_2yrs, # 2 years of treatment
  folds = 1,
  learners_trt = "SL.glm",
  learners_outcome = "SL.glm",
  control = lmtp_control(.return_full_fits = TRUE),
  k = Inf
)

pd <- melt(
    data = dt,
    id.vars = "id",
    measure.vars = list(
        A = c("Aspirin1", "Aspirin2", NA),
        cens = c(NA, "Censoring1", "Censoring2"),
        dead = c(NA, "Dead1", "Dead2")
    ),
    variable.name = "stage"
)
setkey(pd, id, stage)
pd[stage == 1, event := 0] # subjects cannot die or be censored before treatment 1
setcolorder(pd, c("id", "stage", "event", setdiff(names(pd), c("id", "stage", "event"))))

pd[stage == 1, U := 0] # subjects cannot die before treatment 1
pd[stage == 2 & cens == 0, event := 2] # subjects with a censoring event at stage 2
pd[stage == 2 & cens == 0, U := 0] # subjects cannot die before being censored
pd[stage == 2 & cens == 1 & dead == 0, event := 0] # regular treatment event
pd[stage == 2 & cens == 1 & dead == 0, U := 0] # subjects with a regular treatment event are not dead
pd[stage == 2 & cens == 1 & dead == 1, event := 1]
pd[stage == 2 & cens == 1 & dead == 1, U := 1]
pd[stage == 3 & cens == 1, event := 1]
pd[stage == 3 & cens == 1, U := dead] # final outcome for subjects which has not been censored
pd[stage == 3 & cens == 0, event := 2] # the final outcome (death) has been censored
pd[stage == 3 & cens == 0, U := NA] # the final outcome (death) has been censored

pd <- pd[!is.na(event), ]
pd[, dead := NULL]
pd[, cens := NULL]
pd$stage <- as.numeric(pd$stage)

## pd[, tmp := 0]
pd <- policy_data(
  data = pd,
  baseline_data = data.table(id = pd[stage == 1, get("id")], W = 0),
  type = "long",
  action = "A", # Treatment
  utility = "U", # Outcome
  id = "id",
  stage = "stage"
)

p_2yr <- policy_def(c(1, 1), name = "2yr")

# Apply each policy
pe <- policy_eval(
  policy_data = pd,
  policy = p_2yr,
  g_full_history = TRUE,
  q_full_history = TRUE,
  # Treatment/assignment model
  g_models = list(g_glm(formula = ~1), g_glm(formula = ~.)),
  # Outcome/q-model
  q_models = list(q_glm(formula = ~ W + A), q_glm(formula = ~ W + A_1 + A)),
  c_models = list(c_no_censoring(), g_glm(formula = ~ .), g_glm(formula = ~ .)),
  c_full_history = TRUE,
  m_model = q_glm(formula = ~ .),
  m_full_history = TRUE
)

## outcome values
treat_2yr[[2]]$outcome_reg
pe$q_values[stage == 2]

treat_2yr[[2]]$fits_m[[1]][[2]]$fitLibrary$SL.glm_All
pe$q_functions$stage_2


treat_2yr[[1]]$outcome_reg
pe$q_values[stage == 1]


dt1[!is.na(Dead2) & Dead1 == 0][, mean(Dead2), by = "Aspirin2"]

dt1[Dead1 == 1, tmp := 1]
dt1[Dead1 == 0, tmp := 2 / 3]
dt1[Censoring1 == 0, tmp := 2 / 3]

glm(tmp ~ Aspirin1, data = dt1) |> predict()
pe$q_values[stage == 1]
