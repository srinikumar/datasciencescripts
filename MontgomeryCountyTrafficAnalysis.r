# Importing RODBC so that we can connect to the database
library(RODBC); 

library(party);
conn <- odbcConnect("LocalSQLServer");
result <- sqlQuery(conn, "select * from md_violations");

# To lower case so that we don't sprain our fingers :-)
names(result) <- tolower(names(result));
rxViolations <- rxImport(result);
odbcClose(conn);

# The following worked in slightly less than 60 seconds
violation_count_tree <- rxDTree(violations ~ commercial_vehicle + alcohol + work_zone + state + 
                    vehicletype + year + make + model + color + race + gender + 
                    driver_city + driver_state + drivers_license_state + 
                    month_of_year + day_of_week + hour_of_day, 
                  data=rxViolations)

# The following did not work
cran_violation_count_tree <- ctree(data=rxViolations, formula=violations ~ commercial_vehicle + alcohol + work_zone + state + 
                                     vehicletype + year + make + model + color + race + gender + 
                                     driver_city + driver_state + drivers_license_state + 
                                     month_of_year + day_of_week + hour_of_day)


# Creating a linear model with many factor variables
violation_type_tree <- rxDTree(violation_type ~ commercial_vehicle + alcohol + work_zone + state + 
                                  vehicletype + year + make + model + color + race + gender + 
                                  driver_city + driver_state + drivers_license_state + 
                                  month_of_year + day_of_week + hour_of_day,
                                data=rxViolations, maxUnorderedLevels = 8)
# Comparison with CRAN glm
vmod <- rxLinMod(data=rxViolations, formula=violations ~ month_of_year + 
                   day_of_week + hour_of_day + commercial_vehicle + vehicletype + 
                   make + race + gender + drivers_license_state + alcohol + work_zone + state)
cranvmod <- glm(data=rxViolations, formula=violations ~ month_of_year + 
                  day_of_week + hour_of_day + commercial_vehicle + vehicletype + 
                  make + race + gender + drivers_license_state + alcohol + work_zone + state)

vt <- rxDTree(violation_type ~ work_zone + vehicletype + race + gender + day_of_week + hour_of_day, data=rxViolations);


# Get Montgomery County geocode first to get the map for it
montgomery_county_gc <- geocode("Montgomery County, MD")

# Now, get the actual map
montgomery_county_map <- get_map(location = c(montgomery_county_gc$lon, montgomery_county_gc$lat))


# Plotting for a month. The sin function is to attempt to match
# the shade of color with where the day is between noon and midnight.
# You don't need it, but am putting it here for you to experiment with it.
# Thanks to Jocelyn Barker for the suggestion to use a harmonic function there.
plotmonth = function(month) {mymap = ggmap(montgomery_county_map) + 
  geom_point(data=alldata[alldata$num_month_of_year==month,], 
             aes(longitude, latitude, color=sin(2*pi * hour_of_day/48))); 
show(mymap); }

# Plotting map for an hour
plothour = function(month, hour) {
  somemap = ggmap(montgomery_county_map) + 
  geom_point(data=alldata[alldata$hour_of_day == hour & alldata$num_month_of_year == month,], 
             aes(longitude, latitude, color=sin(2*pi * hour_of_day/48)));
  show(somemap);}

# If we want to save it as HTML - has nice controls as well.
saveHTML({for (i in seq(1, 12, 1)) plotmonth(i)}, interval = 1.0, htmlfile = "bymonth.html")

# snapshot
plotbyhourinmonth = function(month) {for (i in seq(0, 23, 1)) plothour(month, i)}

animateplotbyhourinmonth = function(month) {
  saveHTML(plotbyhourinmonth(month), interval = 0.5, 
           htmlfile = paste("byhourinmonth", as.character(month), ".html", sep = ""))
}


ggplot(data=alldata, aes(num_month_of_year, fill=VehicleType)) + geom_bar()

saveHTML({for (i in seq(0, 23, 1)) plothour(i)}, interval = 1.0, htmlfile = "byhour.html")