# For fuzzywuzzyR. Check Python config and initialize Python with reticulate
reticulate::py_config()

library(tidyverse)
library(openxlsx)
library(scales)
library(lubridate)

# for maps
library(sf)
library(tmap)
library(leaflet)
library(leafem)
library(leaflegend)
library(grid)
library(plotly)
library(rsconnect)

library(extrafont)
library(extrafontdb)

library(pdftools)
library(fuzzyjoin)
library(fuzzywuzzyR)
library(stringi)
library(showtext)
library(htmlwidgets)
library(webshot2)

library(ggrepel)
library(colorspace)

library(reactable)
#library(reactablefmtr) # Conflicts with theme element_text margin. Use ad-hoc ::
library(htmltools)
library(hrbrthemes)
