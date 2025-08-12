# drug-overdose-us-youth10-24
## License

This project is licensed under the GNU General Public License v3.0 (GPL-3.0).  
You may freely use, modify, and distribute this software under the same license.  
See the [LICENSE](LICENSE) file for details.

## Dependencies

This project uses the following R packages:

| Package        | License | URL |
|----------------|---------|-----|
| shiny          | GPL-3   | https://cran.r-project.org/package=shiny |
| ggplot2        | MIT     | https://cran.r-project.org/package=ggplot2 |
| tidyverse      | GPL-3 / MIT | https://cran.r-project.org/package=tidyverse |
| plotly         | MIT     | https://cran.r-project.org/package=plotly |
| shinyWidgets   | GPL-3   | https://cran.r-project.org/package=shinyWidgets |
| grid           | GPL     | (part of R base) |
| ggtext         | MIT     | https://cran.r-project.org/package=ggtext |

## Data Source

The data used in this dashboard are from the CDC WONDER database:

Centers for Disease Control and Prevention, National Center for Health Statistics.  
CDC WONDER Online Database, Underlying Cause of Death, 1999–2023.  
Available from: https://wonder.cdc.gov/

CDC WONDER data are in the public domain as a U.S. Government work.  
Users can download the latest data directly from the CDC WONDER portal.  
This repository does not include the dataset due to size and update frequency. You can download the data directly from CDC WONDER. After downloading, save the file in the `data/` folder (create it if it doesn't exist),
and update the file path in `app.R` if necessary.
