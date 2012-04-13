helper.function <- function()
{
  return(1)
}
library(ggplot2)
theme_update(theme_bw())
# The palette with grey:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# The palette with black:
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# To use for fills, add
col.fill <-  scale_fill_manual(values=cbPalette)

# To use for line and point colors, add
col.ptos <-   scale_colour_manual(values=cbPalette)
  
 