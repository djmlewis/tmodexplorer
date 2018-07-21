
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
  BOOSTRIX = c(0, 1, 2, 3, 7, 28),
  VSVHIVGAG = c(0, 1, 3, 7, 14),
  `PLACEBO VSVHIVGAG` = c(0, 1, 3, 7, 14),
  `VSVHIVGAG SHAM` = c(0, 1, 3, 7, 14),
  `NONE VSVHIVGAG` = c(0, 1, 3, 7, 14)
)

write_rds(dayPats,"dayPats.rds")

vaccinecolours <-
  c(
    AGRIPPAL = "darkorchid4",
    BOOSTRIX = "red3",
    `ENGERIX B1` = "deeppink",
    `ENGERIXB 1` = "deeppink",
    `ENGERIX B3` = "magenta",
    `ENGERIXB 3` = "magenta",
    `FLUAD C SURREY` = "darkgreen",
    `FLUAD C` = "darkgreen",
    `FLUAD D GENT` = "limegreen",
    `FLUAD D` = "limegreen",
    `PLACEBO AB1C` = "navyblue",
    `PLACEBO B3` = "darkcyan",
    `PLACEBO D FLUAD` = "deepskyblue",
    `PLACEBO D` = "deepskyblue",
    `PLACEBO D2 BOOSTRIX` = "dodgerblue4",
    `PLACEBO D2` = "dodgerblue4",
    STAMARIL = "orange",
    VARILRIX = "black",
    QTIV = "darkorchid4",
    VSVHIVGAG = "orangered1",
    `VSVHIVGAG SHAM` = "lightsteelblue1",
    `PLACEBO VSVHIVGAG` = "lightsteelblue1",
    `NONE VSVHIVGAG` = "grey50",
    black = "black"
  )

write_rds(vaccinecolours,"vaccinecolours.rds")
