# ADDS A TERMINATOR TO A MAP TO SHOW DAYTIME / NIGHTTIME REGIONS
# Returns a dataframe of latitude and longitude for the line that separates illuminated day and dark night for any given time
# This is just a port of the Javascript Leaflet.Terminator plugin (https://github.com/joergdietrich/Leaflet.Terminator/blob/master/L.Terminator.js)

rad2deg <- function(rad) {
  (rad * 180) / (pi)
}

deg2rad <- function(deg) {
  (deg * pi) / (180)
}

getJulian <- function(time) {
  # get Julian day (number of days since noon on January 1, 4713 BC; 2440587.5 is number of days between Julian epoch and UNIX epoch)
  (as.integer(time) / 86400) + 2440587.5
}

getGMST <- function(jDay) {
  # calculate Greenwich Mean Sidereal Time
  d <- jDay - 2451545.0
  (18.697374558 + 24.06570982441908 * d) %% 24
}

sunEclipticPosition <- function(jDay) {
  # compute the position of the Sun in ecliptic coordinates
  # days since start of J2000.0
  n <- jDay - 2451545.0
  # mean longitude of the Sun
  L <- 280.460 + 0.9856474 * n
  L = L %% 360
  # mean anomaly of the Sun
  g <- 357.528 + 0.9856003 * n
  g = g %% 360
  # ecliptic longitude of Sun
  lambda <- L + 1.915 * sin(deg2rad(g)) + 0.02 * sin(2 * deg2rad(g))
  # distance from Sun in AU
  R <- 1.00014 - 0.01671 * cos(deg2rad(g)) - 0.0014 * cos(2 * deg2rad(g))
  
  data.frame(lambda, R)
}

eclipticObliquity <- function(jDay) {
  # compute ecliptic obliquity
  n <- jDay - 2451545.0
  # Julian centuries since J2000.0
  T <- n / 36525
  # compute epsilon
  23.43929111 -
    T * (46.836769 / 3600
         - T * (0.0001831 / 3600
                + T * (0.00200340 / 3600
                       - T * (0.576e-6 / 3600
                              - T * 4.34e-8 / 3600))))
}

sunEquatorialPosition <- function(sunEclLng, eclObliq) {
  # compute the Sun's equatorial position from its ecliptic position
  alpha <- rad2deg(atan(cos(deg2rad(eclObliq)) *
    tan(deg2rad(sunEclLng))))
  delta <- rad2deg(asin(sin(deg2rad(eclObliq)) 
  * sin(deg2rad(sunEclLng))))
  
  lQuadrant  = floor(sunEclLng / 90) * 90
  raQuadrant = floor(alpha / 90) * 90
  alpha = alpha + (lQuadrant - raQuadrant)
  
  data.frame(alpha, delta)
}

hourAngle <- function(lng, sunPos, gst) {
  # compute the hour angle of the sun for a longitude on Earth
	lst <- gst + lng / 15
	lst * 15 - sunPos$alpha
}

longitude <- function(ha, sunPos) {
	# for a given hour angle and sun position, compute the latitude of the terminator
	rad2deg(atan(-cos(deg2rad(ha)) / tan(deg2rad(sunPos$delta))))
}

terminator <- function(time, from = -180, to = 180, by = 0.1) {
  # calculate latitude and longitude of terminator within specified range using time (in POSIXct format, e.g. `Sys.time()`)
  jDay = getJulian(time)
	gst = getGMST(jDay)

	sunEclPos = sunEclipticPosition(jDay)
	eclObliq = eclipticObliquity(jDay)
	sunEqPos = sunEquatorialPosition(sunEclPos$lambda, eclObliq)
	
	lapply(seq(from, to, by), function(i) {
    ha = hourAngle(i, sunEqPos, gst)
    lon = longitude(ha, sunEqPos)
    data.frame(lat = i, lon)
	}) %>%
	  plyr::rbind.fill()
}


# # EXAMPLES
# # terminator for current time on world map
#
# # add terminator at current time to world map as shaded region using `geom_ribbon``
# ggplot2::ggplot() +
#   borders("world", colour = "gray90", fill = "gray85") +
#   geom_ribbon(data = terminator(Sys.time(), -180, 190), aes(lat, ymax = lon), ymin = 90, alpha = 0.2) +
#   coord_equal() +
#   ggthemes::theme_map()
# 
# # add terminator at a specific time to map of Europe, using a `coord_*` function to crop after drawing shaded region with `geom_ribbon`
# ggplot2::ggplot() +
#   borders("world", colour = "gray90", fill = "gray85") +
#   geom_ribbon(data = terminator(as.POSIXct("2018-01-01 07:00:00 GMT"), -180, 190, 0.1), aes(lat, ymax = lon), ymin = 90, alpha = 0.2) +
#   coord_equal(xlim = c(35, -12), ylim = c(35, 72), expand = 0) +
#   ggthemes::theme_map()
