library(MASS)
library(RColorBrewer)
library(data.table)
library(ppls)

n.colors <- 128
n.buckets <- 800
n.samples <- 1000
size.kernel <- 50
rf <- colorRampPalette(rev(brewer.pal(11,'Spectral')))

#n.buckets <- 100
size.kernel <- 20
rf <- colorRampPalette((brewer.pal(10,'Paired')))



r <- rf(n.colors)

counts <- read.csv("/Users/dgopstein/script/clojure/keylog-reader/csv/count_2016-05-30.csv", header = TRUE)

#93 + seq(0, 12)*28
layout <- read.csv("/Users/dgopstein/script/clojure/keylog-reader/keyboard_layout.csv", header = TRUE)

keys <- data.table(merge(counts, layout[complete.cases(layout),], by="key"))

sample_presses <- keys[,keys[rep(.I, round(normalize.vector(count^0.6)*n.samples))],] # sqrt'd

system.time(kde <- kde2d(sample_presses$x, sample_presses$y, n=n.buckets, h=size.kernel, lims = c(0, 684, 0, 225)))
par(mar=c(0,0,0,0))
image(kde, col=r, ylim=c(225,0), axes=FALSE)