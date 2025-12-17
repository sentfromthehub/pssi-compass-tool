# COMPASS (Campus Online Matching of Personalized Academic & Student Supports)
## Overview

COMPASS is a tool designed to facilitate the implementation of the Post-Secondary Student Stressors Index (PSSI), a validated 46-item instrument created to help post-secondary institutions and researchers evaluate and monitor student stress (Linden & Stuart, 2019). This project is part of a larger study, *Pilot Launch of the Post-Secondary Student Stressors Index (PSSI) as a Student Wellness Services Tool*, which aims to integrate the PSSI into clinical settings during the 2025–2026 academic year. The tool is currently under development.

**Live snapshot**: https://sentfromthehub.shinyapps.io/pssi-compass-tool/

---
## Usage
To run locally:
### Prerequisites
* R (version 4.0 or higher) - [https://cran.r-project.org/](https://cran.r-project.org/)
* RStudio IDE (Recommended) - [https://posit.co/download/rstudio-desktop/](https://posit.co/download/rstudio-desktop/)
### Installation & Setup
1.  **Clone the repository**:
    ```bash
    git clone https://github.com/sentfromthehub/pssi-compass-tool.git
    ```
2.  **Navigate to the project directory**:
    ```bash
    cd pssi-triage-tool
    ```
3.  **Install the required R packages**. Open R or RStudio and run the following command in the console:
    ```r
    install.packages(c(
      "jose", "rsconnect", "shiny", "shinyjs", "dplyr", "tibble",
      "ggplot2", "ggrepel", "RColorBrewer", "fmsb", "scales", "stringr"
    ))
    ```
4.  **Run the application**. Open the `app.R` file in RStudio and click the "Run App" button, or run the following command in the console:
    ```r
    shiny::runApp()
    ```
---
## Work Cited
Linden, B., & Stuart, H. (2019). Psychometric assessment of the Post- Secondary Student Stressors Index (PSSI). BMC Public Health, 19(1). https://doi.org/10.1186/s12889-019-7472-z
