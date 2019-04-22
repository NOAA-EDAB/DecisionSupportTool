Action <- c("Closure",
            "Constraint_Fishery",
            "Constraint_Spatial",
            "EndlineModification",
            "MinTrawlLength",
            "TrapCap",
            "TrapReduction")
LMA <- c("All",
          "A1",
          "A2",
          "A2_3overlap",
          "A3")
State <- c("All",
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
Months <- 1:12
Percentage <- ""
Shapefile <- ""
#Data frame to hold 
DF <- data.frame(Action = as.character(rep(NA,10)),
                 LMA = as.character(rep(NA, 10)),
                 State = as.character(rep(NA, 10)),
                 StatArea = as.character(rep(NA, 10)),
                 Fishery = as.character(rep(NA, 10)),
                 Shapefile = as.character(rep(NA, 10)),
                 Months = as.character(rep(NA, 10)),
                 Percentage = as.character(rep(NA, 10)),
                 TrapRedistributionArea = as.character(rep(NA, 10)),
                 TrapRedistributionMethod = as.character(rep(NA, 10)))

# #get existing scenarios for listing as scenaerio inputs
# existing_input_csvs <- list.files(here::here("InputSpreadsheets"))
# existing_input_scenarios <- stringr::str_remove(existing_input_csvs, ".csv|.xlsx")

existing_input_scenarios <- ""


