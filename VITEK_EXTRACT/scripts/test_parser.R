##Test read_vitek_pdf.R
source(here::here("scripts", "packages.R"))
source(here::here("functions", "read_vitek_pdf.R"))

input_file <- here::here("data_raw", "AST Chart Report.pdf")

raw_pdf <- read_vitek_pdf(file_path = input_file)

raw_pdf$n_pages
cat(raw_pdf$page_text[1])


##Test parse_ast_chart_report.R
source(here::here("scripts", "packages.R"))
source(here::here("functions", "read_vitek_pdf.R"))
source(here::here("functions", "parse_ast_chart_report.R"))

input_file <- here::here("data_raw", "AST Chart Report.pdf")

raw_pdf <- read_vitek_pdf(input_file)

parsed <- parse_ast_chart_report(raw_pdf)

parsed$metadata
parsed$ast_long
parsed$ast_wide

readr::write_csv(parsed$ast_wide, here::here("data_processed", "ast_chart_report_wide.csv"))

View(parsed$ast_wide)