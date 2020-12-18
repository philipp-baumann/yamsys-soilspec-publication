#!/usr/bin/env Rscript

renv::restore()

library("drake")
r_make()

cat("build_time_seconds ", round(sum(build_times()$elapsed), 2),
  file = here::here("_build-time.txt"))