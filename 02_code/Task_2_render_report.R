# Render Task 2 report

# List packages that we need
pkgs <- c("rmarkdown")

# Check they are installed
pkgs_2_install <- pkgs[!(pkgs %in% installed.packages())]

# Install if not on system
for (pkg in pkgs_2_install) {
  install.packages(pkg)
}

# Load packages
for (pkg in pkgs) {
  library(pkg,character.only=TRUE)
}

# Render
render("02_code/Task_2_Report.Rmd", output_file = "../03_output/03_reports/T2_report.pdf")
