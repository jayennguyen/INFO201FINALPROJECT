## this method take a dataframe input and abb state and typeof data 
## this return the earning ratio of the each states according
## to the type of summary of data the client choose
get.one.state.all.city.earning.ratio <- function(data.input, state.name, type.you.want) {
  if(type.you.want == 1) {
    result.data <- mutate(data.input, "earning_tuition_ratio" = MN_EARN_WNE_P10 / realcost)
  } else if (type.you.want == 2) {
    result.data <- mutate(data.input, "earning_tuition_ratio" = MN_EARN_WNE_P6 / realcost)
  } else {
    result.data <- mutate(data.input, "mean_earning_of_3_terms" = (MN_EARN_WNE_P10 + MN_EARN_WNE_P8 + MN_EARN_WNE_P6)/3)
    result.data <- mutate(result.data, "earning_tuition_ratio" = mean_earning_of_3_terms/realcost)
  }
  prepare.for.city.summary <- get.one.state(result.data, state.name)
  city.summary <- group_by(prepare.for.city.summary, CITY) %>% summarise(avg_earning_tuition_ration=mean(earning_tuition_ratio))
  return(city.summary)
}

## this method take a dataframe input and abb state
## this method return the dataframe which is used to plot the
## box and whisker diagram
get.data.for.box.whisker <- function(data.input, state.name) {
  bwd.data <- get.one.state(data.input, state.name)
  bwd.data.result <- select(bwd.data, only.mean.earning)
  bwd.data.result <- mutate(bwd.data.result, "test" = MN_EARN_WNE_P10)
  bwd.data.result <- melt(bwd.data.result, id.vars = "test" )
  bwd.data.result <- select(bwd.data.result, "variable", "value")
  bwd.data.result <- sapply(bwd.data.result, as.numeric)
  bwd.data.result <- as.data.frame(bwd.data.result)
  return(bwd.data.result)
}


## this method take a dataframe input and abb state 
## this to get the top 10 univerisity in that state
get.the.top10.highest.earning.in.one.state <- function(data.input, state.code) {
  prepare.for.summary <- get.one.state(data.input, state.code)
  prepare.for.summary <- head(prepare.for.summary[order(prepare.for.summary$MN_EARN_WNE_P10, decreasing = TRUE),],10)
  return(prepare.for.summary)
}

## this method take a dataframe input and abb state 
## this method returns the earning which only involve this
## country's data
get.one.state <- function(data.input, state.code) {
  data.input <- filter(data.input, STABBR == state.code)
  return (data.input)
}

## this method take a dataframe input and abb state 
## this return the summary of the data, which summarize the data
## the mean of the earning in each time period
get.one.state.summary <- function(data.input, state.code){
  prepare.for.summary <- get.one.state(data.input, state.code)
  prepare.for.summary <- select(prepare.for.summary, columns.earning)
  prepare.for.summary <- sapply(prepare.for.summary, as.numeric)
  prepare.for.summary <- as.data.frame(prepare.for.summary)
  result.summary <- summarise(prepare.for.summary,
                              avg.10yrs = mean(MN_EARN_WNE_P10),
                              avg.8yrs = mean(MN_EARN_WNE_P8),
                              avg.6yrs = mean(MN_EARN_WNE_P6)
  )
  return(result.summary)
  
}

## this method take in a date input
## and get all state summary, which is the mean of each
## time period
get.all.state.summary <- function(data.input) {
  group.state <- data.input
  group.state[columns.earning] <- sapply(group.state[columns.earning], as.numeric)
  group.state <- group_by(group.state, STABBR) %>% summarise(
    avg.10yrs = mean(MN_EARN_WNE_P10),
    avg.8yrs = mean(MN_EARN_WNE_P8),
    avg.6yrs = mean(MN_EARN_WNE_P6)
  )
  return(group.state)
}

## this method take in a date input
## and get all state summary, which is the mean of median of each
## time period
get.all.state.median<- function(data.input) {
  group.state <- data.input
  group.state[columns.earning] <- sapply(group.state[columns.earning], as.numeric)
  group.state <- group_by(group.state, STABBR) %>% summarise(
    median.10yrs = mean(MD_EARN_WNE_P10),
    median.8yrs = mean(MD_EARN_WNE_P8),
    median.6yrs = mean(MD_EARN_WNE_P6)
  )
  return(group.state)
}

## this method take in a date input
## and get all state summary, which is the mean of sd of each
## time period
get.all.state.sd <- function(data.input) {
  group.state <- data.input
  group.state[columns.earning] <- sapply(group.state[columns.earning], as.numeric)
  group.state <- group_by(group.state, STABBR) %>% summarise(
    sd.10yrs = mean(SD_EARN_WNE_P10),
    sd.8yrs = mean(SD_EARN_WNE_P8),
    sd.6yrs = mean(SD_EARN_WNE_P6)
  )
  return(group.state)
}

## this method takes in lat and long and return the state
## which is clicked
latlong2state <- function(pointsDF) {
  # Prepare SpatialPolygons object with one SpatialPolygon
  # per state (plus DC, minus HI & AK)
  states <- map('state', fill=TRUE, col="transparent", plot=FALSE)
  IDs <- sapply(strsplit(states$names, ":"), function(x) x[1])
  states_sp <- map2SpatialPolygons(states, IDs=IDs,
                                   proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Convert pointsDF to a SpatialPoints object 
  pointsSP <- SpatialPoints(pointsDF, 
                            proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Use 'over' to get _indices_ of the Polygons object containing each point 
  indices <- over(pointsSP, states_sp)
  
  # Return the state names of the Polygons object containing each point
  stateNames <- sapply(states_sp@polygons, function(x) x@ID)
  stateNames[indices]
}