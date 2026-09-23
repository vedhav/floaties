## Code to prepare the synthetic `tumor_change` dataset.
##
## Column names are deliberately not CDISC names, to show that floaties works
## with any data standard.

set.seed(20260923)

n <- 40
best_change <- round(c(
  runif(4, -100, -80),
  runif(14, -79, -30),
  runif(12, -29, 19),
  runif(8, 20, 90),
  c(135, 210)
), 1)
best_change[1] <- -100

response <- ifelse(
  best_change <= -100, "CR",
  ifelse(best_change <= -30, "PR", ifelse(best_change < 20, "SD", "PD"))
)

tumor_change <- data.frame(
  patient = sprintf("PT-%03d", sample(n)),
  arm = sample(c("Drug A", "Drug B"), n, replace = TRUE),
  best_change = best_change,
  response = factor(response, levels = c("CR", "PR", "SD", "PD")),
  age = sample(35:80, n, replace = TRUE)
)
tumor_change <- tumor_change[sample(n), ]
rownames(tumor_change) <- NULL

usethis::use_data(tumor_change, overwrite = TRUE)
