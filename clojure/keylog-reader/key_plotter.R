library(MASS)
library(RColorBrewer)


n.colors <- 128
n.buckets <- 800
n.samples <- 500
f.jitter <- function() runif(1) * 10
size.kernel <- 40


rf <- colorRampPalette(rev(brewer.pal(11,'Spectral')))
r <- rf(n.colors)
#library(data.table)


counts <- read.csv("/Users/dgopstein/script/clojure/keylog-reader/csv/count_2016-05-27.csv",
                 col.names=c("count", "key"))

#93 + seq(0, 12)*28
layout <- read.csv("/Users/dgopstein/script/clojure/keylog-reader/keyboard_layout.csv",
                 header = TRUE)

keys <- merge(counts, layout, by="key")

sample_presses <- keys[sample(x = 1:nrow(keys), n.samples, replace = T, prob = keys$count),]
# Add visual noise
sample_presses$x <- sapply(sample_presses$x, function(x) x + f.jitter())
sample_presses$y <- sapply(sample_presses$y, function(x) x + f.jitter())


system.time(kde <- kde2d(sample_presses$x, sample_presses$y, n=n.buckets, h=size.kernel, lims = c(0, 684, 0, 225)))
image(kde, col=r, ylim=c(225,0))