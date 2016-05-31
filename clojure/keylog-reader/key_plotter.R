library(MASS)
library(RColorBrewer)
library(rimage)
library(grid)
library(data.table)
library(ppls)


n.colors <- 128
n.buckets <- 800
n.buckets <- 200
#n.buckets <- 20
n.samples <- 1000
#f.jitter <- function() (runif(1) - 0.5) * 10
size.kernel <- 50


rf <- colorRampPalette(rev(brewer.pal(11,'Spectral')))
r <- rf(n.colors)
#library(data.table)


counts <- read.csv("/Users/dgopstein/script/clojure/keylog-reader/csv/count_2016-05-30.csv",
                 col.names=c("count", "key"), header = FALSE)

#93 + seq(0, 12)*28
layout <- read.csv("/Users/dgopstein/script/clojure/keylog-reader/keyboard_layout.csv",
                 header = TRUE)

keys <- data.table(merge(counts, layout[complete.cases(layout),], by="key"))

#sample_presses <- keys[sample(x = 1:nrow(keys), n.samples, replace = T, prob = keys$count, base=100000),] # probablistic
#sample_presses <- keys[,keys[rep(.I, count / 10)],] # proportional
sample_presses <- keys[,keys[rep(.I, round(normalize.vector(count^0.6)*n.samples))],] # sqrt'd


# Add visual noise
# sample_presses$x <- sapply(sample_presses$x, function(x) x + f.jitter())
# sample_presses$y <- sapply(sample_presses$y, function(x) x + f.jitter())

# Correct image offset
# sample_presses$x = sample_presses$x - 5
# sample_presses$y = sample_presses$y - 5


system.time(kde <- kde2d(sample_presses$x, sample_presses$y, n=n.buckets, h=size.kernel, lims = c(0, 684, 0, 225)))
image(kde, col=r, ylim=c(225,0), axes=FALSE, mar=c(0,0,0,0))

