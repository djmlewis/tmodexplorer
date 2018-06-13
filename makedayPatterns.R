
dayPats <- list(
  STAMARIL = c(-1, 0, 1, 2, 3, 4, 5, 7, 14, 21, 28),
  VARILRIX = c(-1, 0, 1, 2, 3, 4, 5, 7, 14, 21, 28),
  AGRIPPAL = c(-1, 0, 1, 2, 3, 4, 5, 7, 14, 21, 28),
  `FLUAD C` = c(-1, 0, 1, 2, 3, 4, 5, 7, 14, 21, 28),
  `ENGERIXB 1` = c(-1, 0, 1, 2, 3, 4, 5, 7, 14, 21, 28),
  `ENGERIXB 3` = c(-1, 0, 1, 2, 3, 4, 5, 7, 14, 21, 28),
  `PLACEBO AB1C` = c(-1, 0, 1, 2, 3, 4, 5, 7, 14, 21, 28),
  `PLACEBO B3` = c(-1, 0, 1, 2, 3, 4, 5, 7, 14, 21, 28),
  `FLUAD D` = c(0, 1, 3, 7, 21),
  `PLACEBO D` = c(0, 1, 3, 7, 21),
  `PLACEBO D2` = c(0, 1, 2, 3, 7, 28),
  BOOSTRIX = c(0, 1, 2, 3, 7, 28)
)

write_rds(dayPats,"dayPats.rds")