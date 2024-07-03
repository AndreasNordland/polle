
test_that("policy_eval with target sub_effect checks inputs", {
    d <- sim_single_stage(1e2, seed = 1)
    pd <- policy_data(d, action = "A", covariates = c("Z"), utility = "U")
    p <- policy_def(1)

    expect_error(
        policy_eval(
            policy_data = pd,
            policy = p,
            target = "test"
        ),
        "target must be either 'value' or 'sub_effect'."
    )

    d <- sim_single_stage_multi_actions(1e2, seed = 1)
    pd <- policy_data(d, action = "a", covariates = c("z"), utility = "u")
    p <- policy_def(1)

    expect_error(
        policy_eval(
            policy_data = pd,
            policy = p,
            g_models = g_empir(),
            target = "sub_effect"
        ),
        "subgroup effect evaluation is not implemented for more than two actions."
    )

    d <- sim_two_stage(1e2, seed = 1)
    pd <- policy_data(
        data = d,
        action = c("A_1", "A_2"),
        covariates = list(L = c("L_1", "L_2")),
        utility = "U_3"
    )
    p <- policy_def(1, reuse = TRUE)

    expect_error(
        policy_eval(
            policy_data = pd,
            policy = p,
            g_models = g_empir(),
            target = "sub_effect"
        ),
        "subgroup effect evaluation is not implemeted for multiple stages."
    )
})

test_that("policy_eval with target sub_effect agrees with targeted::cate", {

    n <- 1e3
    Z <- rnorm(n = n)
    A <- rbinom(size = 1, n = n, prob = 0.5)
    U <- rnorm(mean = Z + Z * A, n = n)
    d <- data.table(Z = Z, A = A, U = U)
    pd <- policy_data(d, action = "A", covariates = c("Z"), utility = "U")
    p <- policy_def(function(Z) (Z > 0) * 1)

    pe <- policy_eval(
        policy_data = pd,
        policy = p,
        g_models = g_glm(~1),
        q_models = q_glm(~ A * Z),
        target = "subeffect"
    )

    ## implementation from the targeted package:
    d$d <- p(pd)$d
    library(targeted)

    ca <- cate(
        treatment = A ~ factor(d) - 1,
        response = U ~ A * Z,
        propensity_model = A ~ 1,
        data = d,
        nfolds = 1
    )

    expect_equal(
        unname(coef(pe)),
        unname(coef(ca)["factor(d)1"])
    )

    expect_equal(
        pe$IC,
        ca$estimate$IC[, "factor(d)1"] |> unname()
    )

    set.seed(1)
    pe <- policy_eval(
        policy_data = pd,
        policy = p,
        g_models = g_glm(~1),
        q_models = q_glm(~ A * Z),
        target = "subeffect",
        M = 2
    )

    set.seed(1)
    ca <- cate(
        treatment = A ~ factor(d) - 1,
        response = U ~ A * Z,
        propensity_model = A ~ 1,
        data = d,
        nfolds = 2,
        type = "dml1"
    )

    expect_equal(
        unname(coef(pe)),
        unname(coef(ca)["factor(d)1"])
    )

    expect_equal(
        pe$IC,
        ca$estimate$IC[, "factor(d)1"] |> unname()
    )




    
})

test_that("policy_eval with target sub_effect has all expected outputs", {
    test_output <- function(pe) {
        ## value_estimate
        expect_true(
            !is.null(pe$value_estimate) & is.numeric(pe$value_estimate)
        )

        ## IC should be zero for Z < 0
        expect_true(
            all(pe$IC[d$Z <= 0] == 0)
        )

        ## id
        expect_true(
            all(pe$id == 1:1e2)
        )

        ## type
        expect_equal(
            pe$type,
            "dr"
        )

        ## target
        expect_equal(
            pe$target,
            "sub_effect"
        )
    }

    d <- sim_single_stage(1e2, seed = 1)
    pd <- policy_data(d, action = "A", covariates = c("Z"), utility = "U")
    p <- policy_def(function(Z) (Z > 0) * 1)

    pe <- policy_eval(
        policy_data = pd,
        policy = p,
        target = "sub_effect"
    )

    test_output(pe)

    pe <- policy_eval(
        policy_data = pd,
        policy = p,
        target = "sub_effect",
        M = 2
    )

    test_output(pe)
})





## test_that("asymptotics of the subgroup effect estimate", {
##     onerun <- function() {
##         n <- 1e3
##         Z <- rnorm(n = n)
##         A <- rbinom(size = 1, n = n, prob = 0.5)
##         U <- rnorm(mean = Z + Z * A, n = n)
##         d <- data.table(Z = Z, A = A, U = U)
##         pd <- policy_data(d, action = "A", covariates = c("Z"), utility = "U")
##         p <- policy_def(function(Z) (Z > 0) * 1)

##         pe <- policy_eval(
##             policy_data = pd,
##             policy = p,
##             g_models = g_glm(~1),
##             q_models = q_glm(~ A * Z),
##             target = "subeffect"
##         )

##         c(coef(pe), sqrt(vcov(pe)))
##     }

##     ## true value of E[U^{(1)} - U^{(0)}| Z > 0]
##     target <- dnorm(0) / (0.5)

##     res <- lava::sim(onerun, R = 2e3, seed = 1)

##     summary(res, estimate = c(1), se = c(2), true = target)
## })
