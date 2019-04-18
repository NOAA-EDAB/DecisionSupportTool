InputSpreadsheetName <- "ExampleRun2.csv"
OutputDir <- gsub(".csv", "", InputSpreadsheetName)




rmarkdown::render(input = here::here("template.Rmd"), params = list(set_title = InputSpreadsheetName,
                                                                    set_path = OutputDir))
