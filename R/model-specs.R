Action <- c("Closure",
            "Constraint_Fishery",
            "Constraint_Spatial",
            "MaxRopeDia",
            "TrawlLength",
            # "EndlineModification",
            # "MinTrawlLength",
            "TrapCap",
            "TrapReduction",
            "BuoylineDevice",
            "RopelessDevice")

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

TrawlRegulation <- c("Min",
                     "Max",
                     "Exactly")

MaxRopeDia <- c("1,700", "3/8in",
                "7/16in","1/2in",
                "9/16in","5/8in")

BuoylineDevice <- c("1,700@100m", 
                    "SSS_Regular",
                    "SSS_To500m",
                    "TimedTensionCutter")

RopelessDevice <- c("TimedRelease",
                    "AcousticRelease")
Months <- 1:12
Percentage <- ""
Shapefile <- ""
TrapCap <- ""

#Data frame to hold 
DF <- data.frame(Action = as.character(rep(NA,10)),
                 LMA = as.character(rep(NA, 10)),
                 State = as.character(rep(NA, 10)),
                 StatArea = as.character(rep(NA, 10)),
                 Fishery = as.character(rep(NA, 10)),
                 Shapefile = as.character(rep(NA, 10)),
                 Months = as.character(rep(NA, 10)),
                 Percentage = as.character(rep(NA, 10)),
                 TrawlRegulation	= as.character(rep(NA, 10)),
                 TrawlLen	= as.character(rep(NA, 10)),
                 MaxRopeDia= as.character(rep(NA, 10)),
                 BuoylineDevice= as.character(rep(NA, 10)),
                 RopelessDevice = as.character(rep(NA, 10)),
                 TrapCap = as.character(rep(NA, 10)),
                 Comment = as.character(rep(NA, 10)))

DF_names <- names(DF)    

# #get existing scenarios for listing as scenaerio inputs
existing_input_csvs <- list.files(here::here("InputSpreadsheets"))
existing_input_scenarios <- stringr::str_remove(existing_input_csvs, ".csv|.xlsx")
# existing_input_scenarios <- ""

#Get list of shapefiles
shapefile_names <- unique(stringr::str_remove(list.files(here::here("InputShapefiles")),"\\..*$"))

