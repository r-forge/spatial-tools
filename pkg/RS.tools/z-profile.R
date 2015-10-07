library(raster)

y <- brick(system.file("external/tahoe_highrez.tif", package="spatial.tools"))
y <- tahoe_highrez
plotRGB(tahoe_highrez)
extract(click(tahoe_highrez))

clicked <- click(y,n=1)
plot(click(y),1:nlayers(y))

plot(1:3,as.numeric(clicked),axes=TRUE)
