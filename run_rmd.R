InputSpreadsheetName <- "ExampleRun2.csv"
OutputDir <- gsub(".csv", "", InputSpreadsheetName)
OutputPathDir <- here::here("Scenarios",OutputDir)



rmarkdown::render(input = here::here("template.Rmd"), output_format="html_document",output_file=paste0(OutputDir,".html"),output_dir=OutputPathDir,params = list(set_title = InputSpreadsheetName,
                                                                    set_path = OutputDir))
