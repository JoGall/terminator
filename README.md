# terminator

This is a simple R port of the [Leaflet](https://rstudio.github.io/leaflet/morefeatures.html) plugin [Leaflet.Terminator.js](https://github.com/joergdietrich/Leaflet.Terminator/blob/master/L.Terminator.js) intended to indicate day and night regions on a map.

Although this function worked for `leaflet` maps, I made this function in order to visualise terminators on `ggplot` maps, such as animations like this:

![terminator-animation](https://user-images.githubusercontent.com/17113779/35389041-096c2786-01ce-11e8-98b6-cfa915f8e989.gif)

Produced like so:

```{r}
library(dplyr)
library(ggplot2)
library(ggthemes)
library(gganimate)
library(animation)

terminatorLatLon <- lapply(seq(0, 23, 1), function(x) {
  
  t0 <- as.POSIXct(Sys.Date()) + (60*60*x)
  
  terminator(t0, -180, 190, 0.5) %>%
    mutate(frame = x)
}) %>%
  plyr::rbind.fill()

chart <- ggplot(terminatorLatLon, aes(frame = frame)) +
  borders("world", colour = "gray90", fill = "gray85") +
  geom_ribbon(aes(lat, ymax = lon), ymin = 90, alpha = 0.2) +
  coord_equal(xlim = c(-180, 190), ylim = c(-58, 85), expand = 0) +
  theme_map()

gganimate(chart, interval = 0.1, ani.width=1000, ani.height=600, filename = "nightday3.gif")
```
