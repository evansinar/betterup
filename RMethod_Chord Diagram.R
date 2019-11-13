library(circlize)

chorddata <- read.csv(file="datafiles/Chord Diagram Test.csv", header=TRUE, sep=",")

circos.par(start.degree = 90)

chordDiagram(chorddata, annotationTrack = c("name", "grid"),
annotationTrackHeight = c(0.03, 0.07), big.gap = 20)
