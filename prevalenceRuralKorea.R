## Refractive errors in a rural Korean adult population: the Namil Study
## http://www.nature.com/eye/journal/v27/n12/full/eye2013195a.html

library(reshape2)
library(ggplot2)
library(grid)
library(RColorBrewer)

data <- read.delim("data.txt")
data[,3:9] <- sapply(data[,3:9], function(x) gsub(substr(data[1,3],11,11), "-", x), USE.NAMES = F)
data <- melt(data, id.vars = (c("Age", "n", "Sex")), variable.name = "error")
data$pct <- sapply(data$value, function (x) strsplit(x, " ")[[1]][1], USE.NAMES = F)
data$ci <- sapply(data$value, function (x) strsplit(x, "\\(")[[1]][2], USE.NAMES = F)
data$ci <- sapply(data$ci, function (x) substr(x, 1, nchar(x) - 1), USE.NAMES = F)
data$lower <- sapply(data$ci, function (x) strsplit(x, "-")[[1]][1], USE.NAMES = F)
data$upper <- sapply(data$ci, function (x) strsplit(x, "-")[[1]][2], USE.NAMES = F)
data$ci <- NULL
data$value <- NULL
data$Sex <- as.factor(data$Sex)
for (i in 5:7) data[,i] <- as.numeric(data[,i])
data$lower[is.na(data$lower)] <- 0
data$upper[is.na(data$upper)] <- 0
data$Sex <- factor(data$Sex, levels =c("M", "F", "Both"))
data$Age <- factor(data$Age, levels =c("40-49", "50-59", "60-69", "70-79", "Over 80", "All"))

mytheme <- theme_bw() +
        theme(strip.background = element_rect(fill = "midnightblue"),
              strip.text = element_text(face = "bold", color = "white", size = rel(1.5)),
              strip.text.y = element_text(angle = 0),
              legend.position = "none",
              panel.grid.major = element_line(colour = "gray20"),
              panel.grid.minor = element_blank(),
              panel.margin.y = unit(1, "cm"),
              panel.background = element_rect(fill = "grey10"),
              plot.title = element_text(face = "bold", size = rel(1.5), vjust = 2),
              axis.text.y = element_text(size = rel(1.4), color = "midnightblue"),
              axis.title.y = element_blank()
        )

ggplot(data, aes(x = reorder(error, pct), y = pct, ymin = lower, ymax = upper, colour = error)) +
        geom_pointrange() +
        geom_errorbar(size = 1, width = 0.5) +
        ylab("%") +
        ggtitle("Prevalence of refractive error by age and sex") +
        facet_grid(Age ~ Sex) +
        coord_flip() +
        mytheme +
        scale_colour_brewer(palette = "Pastel1")
   
