#### Test read_vitek_pdf_full.R ####

source(here::here("scripts", "packages.R"))
source(here::here("functions", "read_vitek_pdf_full.R"))

#### Test ####
input_file <- here::here("data_raw", "example_report_01.pdf")

full_pdf <- read_vitek_pdf_full(input_file)

full_pdf$metadata
full_pdf$page_data
cat(full_pdf$page_text[1])



#### Test parse_ast_chart_report_full.R ####
source(here::here("scripts", "packages.R"))
source(here::here("functions", "read_vitek_pdf_full.R"))
source(here::here("functions", "parse_ast_chart_report_full.R"))

#### Test ####
input_file <- here::here("data_raw", "example_report_01.pdf")

full_pdf <- read_vitek_pdf_full(input_file)
parsed_full <- parse_ast_chart_report_full(full_pdf)

parsed_full$metadata
parsed_full$ast_long
parsed_full$ast_wide

readr::write_csv(
  parsed_full$ast_wide,
  here::here("data_processed", "ast_full_wide.csv")
)

View(parsed_full$ast_wide)



source(here::here("scripts", "packages.R"))
source(here::here("functions", "read_pdf_manifest.R"))

manifest_df <- read_pdf_manifest()

manifest_df


source(here::here("scripts", "packages.R"))
source(here::here("functions", "read_pdf_manifest.R"))
source(here::here("functions", "get_unprocessed_files.R"))

manifest_df <- read_pdf_manifest()
files_to_run <- get_unprocessed_files(manifest_df)

files_to_run
