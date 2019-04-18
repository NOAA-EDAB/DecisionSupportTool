Action <- c("Closure",
            "Constraint_Fishery",
            "Constraint_Spatial",
            "EndlineModification",
            "MinTrawlLength",
            "TrapCap",
            "TrapReduction")
LMAs <- c("All",
          "A1",
          "A2",
          "A2_3overlap",
          "A3")
States <- c("All",
            "ME",
            "NH",
            "MA")

Fishery <- c("All",
             "NonExempt",
             "Exempt",
             "Midshelf_Lobster",
             "Midshelf_Crab",
             "Offshore_Lobster",
             "Offshore_Crab")
# select multiple
StatArea <- c(464, 465, 511,
              512, 513, 514,
              515, 521, 522,
              561, 562, 537,
              538, 539)
TrapRedistributionArea <- c("WithinStatArea",
                            "AdjacentStatAreas",
                            "AcrossLMA",
                            "None")
TrapRedistributionMethod <- c("Even",
                              "IDW")


DF <- data.frame(Action = rep(NA,10),
                 LMAs = rep(NA, 10),
                 States = rep(NA, 10),
                 Fishery = rep(NA, 10),
                 StatArea = rep(NA, 10),
                 TrapRedistributionArea = rep(NA, 10),
                 TrapRedistributionMethod = rep(NA, 10))
