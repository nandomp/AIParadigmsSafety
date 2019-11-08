# =======================================================================================
#
# This is R code for the following paper:
#
#  - 
#
# This code implements: 
#
# This code has been developed by
#   FERNANDO MARTINEZ-PLUMED, UNIVERSITAT POLITECNICA DE VALENCIA, SPAIN
#   fmartinez@dsic.upv.es
#   JOSE HERNANDEZ-ORALLO, UNIVERSITAT POLITECNICA DE VALENCIA, SPAIN
#   jorallo@dsic.upv.es
#
# LICENCE:
#   GPL
#
# VERSION HISTORY:
#  - V.1.0    20 Feb 2017. DBLP Scrapping initial functionalities
#  - V.1.1    23 Feb 2017. Data from IJCAI & AAAI conferences.
#  - V.1.2    01 Aug 2019. Mapping AI safety issues.
#
# FUTURE FEATURES:
#
# =======================================================================================


# =======================================================================================
# ====================================== Libraries ======================================
# =======================================================================================

.lib<- c("stringr", "dplyr", "tidyr", "ggplot2", "ggforce", "lubridate", "splines", "viridis", "RColorBrewer", "circlize")
.inst <- .lib %in% installed.packages()
if (length(.lib[!.inst])>0) install.packages(.lib[!.inst], repos=c("http://rstudio.org/_packages", "http://cran.rstudio.com")) 
lapply(.lib, require, character.only=TRUE)


# =======================================================================================
# ====================================== Load Data ======================================
# =======================================================================================


aiAll <- readRDS("./AITopics data/aiTopicscomplete.clean.rds")
View(aiAll)

#initialise groupTAG
aiAll$groupTAG <- NA



# =======================================================================================
# ====================================== Functions ======================================
# =======================================================================================

openPDFEPS <- function(file, height= PDFheight, width= PDFwidth, PDFEPS = 1) {
  if (PDFEPS == 1) {
    pdf(paste(file, ".pdf", sep=""), width, height)
  } else if (PDFEPS == 2) {
    postscript(paste(file, ".eps", sep=""), width, height, horizontal=FALSE)
  }
}


# Return those documents which include the "tag" in columns: concepts | title | abstract
tagFilter <- function(data, tag){
  data$AllConcepts <- tolower(data$AllConcepts)
  data$title <- tolower(data$title)
  data$summary <- tolower(data$summary)
  
  concepts <- grepl(tag,data$AllConcepts)
  title <- grepl(tag,data$title)
  abstract <- grepl(tag,data$summary)
  
  res <- concepts | title | abstract
  return(data[res,])
}


createGroupsTags <- function(data, tagOld, tagNew){
  
  data$AllConcepts <- tolower(data$AllConcepts)
  data$title <- tolower(data$title)
  data$summary <- tolower(data$summary)
  
  tagOld <- tolower(tagOld)
  tagNew <- tolower(tagNew)
  
  for(i in tagOld){
    concepts <- grepl(i,data$AllConcepts)
    title <- grepl(i,data$title)
    abstract <- grepl(i,data$summary)
    res <- concepts | title | abstract
    print(paste("TAG:", i, "appears:", sum(res),"times"))
    data[res,"groupTAG"] <- tolower(tagNew)
    #    data[res,"AllConcepts"] <- tolower(paste(data[res,"AllConcepts"], tagNew ,sep=", "))
  }
  return(data)
  
  
}


createGroupsFromCSV <- function(kw, data){
  kw <- read.csv(paste0(kw,".csv"))
  data$AllConcepts <- tolower(data$AllConcepts)
  kw$originalTag <- tolower(kw$originalTag)
  kw$newTag <- tolower(kw$newTag)
  
  require(dplyr)
  newTagS <- unique(kw$newTag)
  for(t in 1:length(newTagS)){
    #print(newTagS[t])
    oldTags <- filter(kw, newTag == newTagS[t])$originalTag
    data <- createGroupsTags(data, oldTags, newTagS[t])
  }
  return(data)
}



#### Plot the evolution of a series of DIFFERENT tags ###
#########################################################

plotAImultTopicS <- function(tags, aiAll, ylim = 500, legendT = "Concept", groupby= "WEEK") {
  
  
  tags <- tolower(tags)
  AiAll.summ <- data.frame()
  
  # Totals 
  if (groupby == "WEEK") {
    docsPerWeek <- group_by(aiAll, year, week)
  } else {
    docsPerWeek <- group_by(aiAll, year)
  }  
  docsPerWeek <- summarise(docsPerWeek, Total = n(), Date_week = min(timestamp_1))
  if (groupby == "WEEK") {
    docsPerWeek$week <- as.integer(docsPerWeek$week)
  }
  
  for(t in tags){
    AIAll.f <- tagFilter(aiAll, t)
    if (groupby == "WEEK") {
      # grouping by week
      AIAll.f <- group_by(AIAll.f, year, week)
    } else {
      AIAll.f <- group_by(AIAll.f, year)
    }    
    AIAll.f <- summarise(AIAll.f, totalDocs = n())  
    
    if(nrow(AIAll.f)!=0){
      AIAll.f$concept <- t
      AiAll.summ <- dplyr::bind_rows(AiAll.summ, AIAll.f)
    }
  }
  
  if (groupby == "WEEK") {
    AiAll.summ <- merge(AiAll.summ, docsPerWeek, by.x = c("year","week"))
  } else {
    AiAll.summ <- merge(AiAll.summ, docsPerWeek, by.x = c("year"))    
  }
  AiAll.summ$DocsPercentage <- (AiAll.summ$totalDocs/AiAll.summ$Total)*100
  #issue: 100% whenever totalDocs = Total = 1
  AiAll.summ <- filter(AiAll.summ, Total > 1)
  
  
  ###plot1 (1 PLOT)
  
  #AiAll.summ$time <- paste(AiAll.summ$year,AiAll.summ$week, sep ="/")
  #AiAll.summ <- AiAll.summ[with(AiAll.summ, order(year, week)),]
  
  plot <- ggplot(AiAll.summ, aes(Date_week, DocsPercentage, group = concept, colour = concept)) + 
    # geom_line(size= 0.9, alpha = 1)  + 
    geom_bar(stat = "identity") +
    xlim(ymd("1970-01-01"),ymd("2017-12-31")) +
    facet_zoom(xy =  AiAll.summ$Date_week > ymd("2016-01-01") & DocsPercentage < 5,  horizontal = T, zoom.size = 6) +
    theme_light()  + xlab("") +
    theme(legend.position = "bottom",
          #legend.key.size =  unit(0.1, "in"),
          axis.title = element_text(size = 16),
          axis.text=element_text(size=8),
          axis.text.x = element_text(size = 7,angle = 45, hjust = 1),
          axis.text.y = element_text(size = 10, hjust = 1),
          panel.grid.minor = element_blank(),
          panel.border = element_blank()) +
    scale_color_discrete(legendT)
  
  
  
  ###plot2 (WRAP PLOTS)
  
  
  # AiAll.summ$time <- paste(AiAll.summ$year,AiAll.summ$week, sep ="/")
  # AiAll.summ <- AiAll.summ[with(AiAll.summ, order(year, week)),]
  
  plot2 <- ggplot(AiAll.summ, aes(Date_week, DocsPercentage, group = concept, colour = concept)) + 
    #    geom_line(size= 0.9, alpha = 1) + 
    facet_wrap(~concept, scales="free") + 
    geom_bar(stat = "identity") +
    xlim(ymd("1970-01-01"),ymd("2017-12-31")) +
    theme_light() + xlab("") +
    theme(legend.position = "bottom",
          #legend.key.size =  unit(0.1, "in"),
          axis.title = element_text(size = 16),
          axis.text=element_text(size=8),
          axis.text.x = element_text(size = 7,angle = 45, hjust = 1),
          axis.text.y = element_text(size = 10, hjust = 1),
          panel.grid.minor = element_blank(),
          panel.border = element_blank()) +
    scale_color_discrete(legendT)
  
  
  return(list(plot,plot2))
}


### Plot the evolution of one tag (or the agreggation of several tags) ###
##########################################################################

plotAIOnetopic <- function(tags, aiAll, groupby= "WEEK"){
  tags <- tolower(tags)
  
  # Totals 
  if (groupby == "WEEK") {
    docsPerWeek <- group_by(aiAll, year, week)
  } else {
    docsPerWeek <- group_by(aiAll, year)
  }
  
  
  docsPerWeek <- summarise(docsPerWeek, Total = n(), Date_week = min(timestamp_1))
  
  if (groupby == "WEEK") {
    docsPerWeek$week <- as.integer(docsPerWeek$week)
  }
  
  AiAll.summ <- data.frame()
  commonTopic <- tags[1] # first tag as "common" name for the grouping of tags
  for(t in tags){
    AIAll.f <- tagFilter(aiAll, t)
    if(nrow(AIAll.f)!=0){
      AIAll.f$commonTopic <- commonTopic
      AiAll.summ <- dplyr::bind_rows(AiAll.summ, AIAll.f)
    }else{
      print("TAG NOT CONTAINED")
      return()
    }
  }
  
  
  if (groupby == "WEEK") {
    AiAll.summ <- group_by(AiAll.summ, year, week, commonTopic)
  } else {
    AiAll.summ <- group_by(AiAll.summ, year, commonTopic)
  }  
  AiAll.summ <- summarise(AiAll.summ, totalDocs = n())
  
  
  if (groupby == "WEEK") {
    AiAll.summ <- merge(AiAll.summ, docsPerWeek, by.x = c("year","week"))
  } else {
    # Just merge per year
    AiAll.summ <- merge(AiAll.summ, docsPerWeek, by.x = c("year"))
  }
  
  AiAll.summ$DocsPercentage <- (AiAll.summ$totalDocs/AiAll.summ$Total)*100
  #issue: 100% whenever totalDocs = Total = 1
  AiAll.summ <- filter(AiAll.summ, Total > 1)
  
  
  #AiAll.summ$time <- paste(AiAll.summ$year,AiAll.summ$week, sep ="/")
  #AiAll.summ <- AiAll.summ[with(AiAll.summ, order(year, week)),]
  plot <- ggplot(AiAll.summ, aes(Date_week, DocsPercentage)) + 
    # geom_line(size= 0.9, alpha = 1) +
    xlim(ymd("1970-01-01"),ymd("2017-12-31")) +
    geom_bar(stat = "identity") +
    #geom_point() +
    #geom_histogram() +  # doesn't work
    #geom_vline() +
    theme_light() + xlab("") +
    theme(legend.position = "bottom",
          #legend.key.size =  unit(0.1, "in"),
          axis.title = element_text(size = 16),
          axis.text=element_text(size=8),
          axis.text.x = element_text(size = 7,angle = 45, hjust = 1),
          axis.text.y = element_text(size = 10, hjust = 1),
          panel.grid.minor = element_blank(),
          panel.border = element_blank()) 
  
  
  return(plot)
}

plotAIGroupedtopic <- function(tag, aiAll, groupby= "WEEK"){
  tag <- tolower(tag)
  # Totals 
  if (groupby == "WEEK") {
    docsPerWeek <- group_by(aiAll, year, week)
  } else {
    docsPerWeek <- group_by(aiAll, year)
  }
  
  docsPerWeek <- summarise(docsPerWeek, Total = n(), Date_week = min(timestamp_1))
  
  if (groupby == "WEEK") {
    docsPerWeek$week <- as.integer(docsPerWeek$week)
  }
  
  # find those docs with groupTAG contains tag
  res <- grepl(tag,tolower(aiAll$groupTAG))
  AiAll.summ <- aiAll[res,]
  
  if(nrow(AiAll.summ)==0){
    print("TAG NOT CONTAINED")
    return()
  }
  
  
  if (groupby == "WEEK") {
    AiAll.summ <- group_by(AiAll.summ, year, week, groupTAG)
  } else {
    AiAll.summ <- group_by(AiAll.summ, year, groupTAG)
  }  
  AiAll.summ <- summarise(AiAll.summ, totalDocs = n())
  
  
  if (groupby == "WEEK") {
    AiAll.summ <- merge(AiAll.summ, docsPerWeek, by.x = c("year","week"))
  } else {
    # Just merge per year
    AiAll.summ <- merge(AiAll.summ, docsPerWeek, by.x = c("year"))
  }
  
  AiAll.summ$DocsPercentage <- (AiAll.summ$totalDocs/AiAll.summ$Total)*100
  #issue: 100% whenever totalDocs = Total = 1
  AiAll.summ <- filter(AiAll.summ, Total > 1)
  
  
  #AiAll.summ$time <- paste(AiAll.summ$year,AiAll.summ$week, sep ="/")
  #AiAll.summ <- AiAll.summ[with(AiAll.summ, order(year, week)),]
  plot <- ggplot(AiAll.summ, aes(Date_week, DocsPercentage)) + 
    # geom_line(size= 0.9, alpha = 1) +
    xlim(ymd("1970-01-01"),ymd("2017-12-31")) +
    geom_bar(stat = "identity") +
    #geom_point() +
    #geom_histogram() +  # doesn't work
    #geom_vline() +
    theme_light() + xlab("") +
    theme(legend.position = "bottom",
          #legend.key.size =  unit(0.1, "in"),
          axis.title = element_text(size = 16),
          axis.text=element_text(size=8),
          axis.text.x = element_text(size = 7,angle = 45, hjust = 1),
          axis.text.y = element_text(size = 10, hjust = 1),
          panel.grid.minor = element_blank(),
          panel.border = element_blank()) 
  
  
  return(plot)
}

### Plot the evolution of multiple GROUP tags                          ###
##########################################################################
plotAIGroupedtopicS <- function(tagS, data, groupby= "WEEK", my_begin = "1970-01-01", my_end = "2017-12-31", smooth = TRUE, mthd ="loess"){
  tagS <- tolower(tagS)
  # Totals 
  if (groupby == "WEEK") {
    docsPerWeek <- group_by(data, year, week)
  } else {
    docsPerWeek <- group_by(data, year)
  }
  
  docsPerWeek <- summarise(docsPerWeek, Total = n(), Date_week = min(timestamp_1))
  
  if (groupby == "WEEK") {
    docsPerWeek$week <- as.integer(docsPerWeek$week)
  }
  
  # find those docs with groupTAG contains tag
  
  
  AiAll.summ <- data.frame()
  for(t in tagS){
    res <- grepl(t,tolower(data$groupTAG))
    AIAll.f <- data[res,]
    
    if(nrow(AIAll.f)!=0){
      AiAll.summ <- dplyr::bind_rows(AiAll.summ, AIAll.f)
    }else{
      print("TAG NOT CONTAINED")
    }
  }
  
  
  if(nrow(AiAll.summ)==0){
    print("NO TAGS, NO PLOT")
    return()
  }
  
  
  if (groupby == "WEEK") {
    AiAll.summ <- group_by(AiAll.summ, year, week, groupTAG)
  } else {
    AiAll.summ <- group_by(AiAll.summ, year, groupTAG)
  }  
  AiAll.summ <- summarise(AiAll.summ, totalDocs = n())
  
  
  if (groupby == "WEEK") {
    AiAll.summ <- merge(AiAll.summ, docsPerWeek, by.x = c("year","week"))
  } else {
    # Just merge per year
    AiAll.summ <- merge(AiAll.summ, docsPerWeek, by.x = c("year"))
  }
  
  AiAll.summ$DocsPercentage <- (AiAll.summ$totalDocs/AiAll.summ$Total)*100
  #issue: 100% whenever totalDocs = Total = 1
  AiAll.summ <- filter(AiAll.summ, Total > 1)
  
  
  # library(magrittr)
  # AiAll.summ$groupTAG2 <- sortLvls.fnc(AiAll.summ$groupTAG, c(1, 2, 3, 2, 1))
  #AiAll.summ$time <- paste(AiAll.summ$year,AiAll.summ$week, sep ="/")
  #AiAll.summ <- AiAll.summ[with(AiAll.summ, order(year, week)),]
  plot <- ggplot(AiAll.summ, aes(Date_week, DocsPercentage, group = groupTAG, colour = groupTAG)) + 
    facet_wrap(~groupTAG, scales="free", ncol = 3) + 
    # geom_line(size= 0.9, alpha = 1) +
    xlim(ymd(my_begin),ymd(my_end)) 
  
  if(smooth){
    
    plot <- plot + 
      geom_point(size = 2.5) + 
      #geom_smooth(aes(fill= groupTAG), alpha = 0.2, size = 0.8, method = mthd) 
      geom_smooth(linetype = "dashed", size = 0.8, colour = "black", method = mthd)        
  }else{
    plot <- plot + geom_bar(stat = "identity") 
  }
  #geom_point() +
  #geom_histogram() +  # doesn't work
  #geom_vline() +
  plot <- plot + theme_light() + xlab("") +
    theme(legend.position = "none",
          #legend.key.size =  unit(0.1, "in"),
          axis.title = element_text(size = 12),
          axis.text=element_text(size=7),
          axis.text.x = element_text(size = 10,angle = 0, hjust = 1),
          axis.text.y = element_text(size = 10, hjust = 1),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          strip.background = element_blank(),
          strip.text=element_text(vjust=0, colour = "black", face = "bold",hjust = 0, size = 11)
    )  + ylab("Documents (%)")
  
  
  return(plot)
}


### Plot the evolution of multiple GROUP tags                          ###
##########################################################################

# tagS=tags; data=aiAll.filter.multipleGroups.p2 
# groupby= "YEAR"; my_begin = "1970-01-01"; my_end = "2017-12-31"
plotAIGroupedtopicS <- function(tagS, data, groupby= "WEEK", my_begin = "1970-01-01", my_end = "2017-12-31", smooth = TRUE, mthd ="loess"){
  tagS <- tolower(tagS)
  
  years <- year(my_begin):year(my_end)
  grid <- expand.grid(year = years, groupTAG = tagS)
  
  
  # Totals 
  if (groupby == "WEEK") {
    docsPerWeek <- group_by(data, year, week)
  } else {
    docsPerWeek <- group_by(data, year)
  }
  
  docsPerWeek <- summarise(docsPerWeek, Total = n(), Date_week = min(timestamp_1))
  
  if (groupby == "WEEK") {
    docsPerWeek$week <- as.integer(docsPerWeek$week)
  }
  
  # find those docs with groupTAG contains tag
  
  
  AiAll.summ <- data.frame()
  for(t in tagS){
    res <- grepl(t,tolower(data$groupTAG))
    AIAll.f <- data[res,]
    
    if(nrow(AIAll.f)!=0){
      AiAll.summ <- dplyr::bind_rows(AiAll.summ, AIAll.f)
    }else{
      print("TAG NOT CONTAINED")
    }
  }
  
  
  if(nrow(AiAll.summ)==0){
    print("NO TAGS, NO PLOT")
    return()
  }
  
  
  if (groupby == "WEEK") {
    AiAll.summ <- group_by(AiAll.summ, year, week, groupTAG)
  } else {
    AiAll.summ <- group_by(AiAll.summ, year, groupTAG)
  }  
  AiAll.summ <- summarise(AiAll.summ, totalDocs = n())
  
  
  # complete years without documents with 0s
  AiAll.summ <- merge(grid, AiAll.summ, by = c("year","groupTAG"), all.x = TRUE) 
  AiAll.summ$totalDocs[is.na(AiAll.summ$totalDocs)] <- 0
  
  
  if (groupby == "WEEK") {
    AiAll.summ <- merge(AiAll.summ, docsPerWeek, by.x = c("year","week"))
  } else {
    # Just merge per year
    AiAll.summ <- merge(AiAll.summ, docsPerWeek, by.x = c("year"))
  }
  
  AiAll.summ$DocsPercentage <- (AiAll.summ$totalDocs/AiAll.summ$Total)*100

  #issue: 100% whenever totalDocs = Total = 1
  AiAll.summ <- filter(AiAll.summ, Total > 1)
  
  
  # library(magrittr)
  # AiAll.summ$groupTAG2 <- sortLvls.fnc(AiAll.summ$groupTAG, c(1, 2, 3, 2, 1))
  #AiAll.summ$time <- paste(AiAll.summ$year,AiAll.summ$week, sep ="/")
  #AiAll.summ <- AiAll.summ[with(AiAll.summ, order(year, week)),]
  plot <- ggplot(AiAll.summ, aes(Date_week, DocsPercentage, group = groupTAG, colour = groupTAG)) + 
    facet_wrap(~groupTAG, scales="free", ncol = 3) + 
    # geom_line(size= 0.9, alpha = 1) +
    xlim(ymd(my_begin),ymd(my_end)) 
  
  if(smooth){
    
    plot <- plot + 
      geom_point(size = 2.5) + 
      #geom_smooth(aes(fill= groupTAG), alpha = 0.2, size = 0.8, method = mthd) 
      geom_smooth(linetype = "dashed", size = 0.8, colour = "black", method = mthd)        
  }else{
    plot <- plot + geom_bar(stat = "identity") 
  }
  #geom_point() +
  #geom_histogram() +  # doesn't work
  #geom_vline() +
  plot <- plot + theme_light() + xlab("") +
    theme(legend.position = "none",
          #legend.key.size =  unit(0.1, "in"),
          axis.title = element_text(size = 12),
          axis.text=element_text(size=7),
          axis.text.x = element_text(size = 10,angle = 0, hjust = 1),
          axis.text.y = element_text(size = 10, hjust = 1),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          strip.background = element_blank(),
          strip.text=element_text(vjust=0, colour = "black", face = "bold",hjust = 0, size = 11)
    )  + ylab("Documents (%)")
  
  
  return(plot)
}


### Plot the evolution of multiple GROUP tags  (stacked area)          ###
##########################################################################
plotAIStackedTopicS <- function(tagS, data, my_begin = "1970-01-01", my_end = "2018-12-31", smooth = TRUE, degrees = 5, whereLegend = "bottom"){
  
  tagS <- tolower(tagS)
  years <- year(my_begin):year(my_end)
  grid <- expand.grid(year = years, groupTAG = tagS)
  
  # find those docs with groupTAG contains tag
  AiAll.summ <- data.frame()
  for(t in tagS){
    res <- grepl(t,tolower(data$groupTAG))
    AIAll.f <- data[res,]
    
    if(nrow(AIAll.f)!=0){
      AiAll.summ <- dplyr::bind_rows(AiAll.summ, AIAll.f)
    }else{
      print("TAG NOT CONTAINED")
    }
  }
  
  if(nrow(AiAll.summ)==0){
    print("NO TAGS, NO PLOT")
    return()
  }
  
  AiAll.summ <- group_by(AiAll.summ, year, groupTAG)
  AiAll.summ <- summarise(AiAll.summ, totalDocs = n())
  
  # complete years without documents with 0s
  AiAll.summ <- merge(grid, AiAll.summ, by = c("year","groupTAG"), all.x = TRUE) 
  AiAll.summ$totalDocs[is.na(AiAll.summ$totalDocs)] <- 0.01
  #AiAll.summ$totalDocs <- AiAll.summ$totalDocs + 1
  
  
  byyear <- group_by(AiAll.summ,year)
  byyear <- summarise(byyear, yearDocs = sum(totalDocs))
  AiAll.summ.B <- merge(AiAll.summ, byyear, by = c("year"), all.x = TRUE) 
  AiAll.summ.B$DocsPercentage <- (AiAll.summ.B$totalDocs / AiAll.summ.B$yearDocs)*100
  
  
  #gs.pal <- colorRampPalette(c("goldenrod","deepskyblue4"),bias=.1,space="rgb")
  if(length(tagS)<= 6){
    library(RColorBrewer)
    myColors <- brewer.pal(length(tagS),"Paired")
    names(myColors) <- tagS
  }else{
    if (length(tagS) <= 16){
      # myColors <-  rainbow(length(tagS))
      # names(myColors) <- tagS
      
      getPalette = colorRampPalette(brewer.pal(11, "Spectral"))
      myColors <- getPalette(length(tagS))
      names(myColors) <- tagS
      
      }else{# scale continuos
      library(viridis)
      myColors <-  viridisLite::viridis(length(tagS))
      names(myColors) <- tagS
    }
  }
  
  colScale <- scale_colour_manual(name = "Venue", values = myColors)
  fillScale <- scale_fill_manual(name = "Venue",  values = myColors)
  shapeScale <- scale_shape_manual(name = "Venue", values = c(19,17))
  
  
  if (smooth){
    
    plot <- ggplot(AiAll.summ.B, aes(year, DocsPercentage, group = groupTAG)) +  
      # stat_smooth(aes(fill = groupTAG), colour = "white",
      #             geom = 'area', method = 'loess',
      #             alpha = 0.7, position = "fill") + 
      
      stat_smooth(aes(fill = groupTAG), method="glm",  method.args=list(family="quasipoisson"), #family="quasipoisson", 
                    formula = y ~ ns(x, degrees),
                  colour = "white", geom = 'area', alpha = 0.7, position = "fill") +
    
      xlim(year(my_begin),year(my_end)) +
      #      ylim(0,1) +
      theme_light() + xlab("") + ylab("Share") + 
      theme(legend.position = whereLegend,#c(0.8,0.85),
            #legend.key.size =  unit(0.1, "in"),
            legend.background = element_blank(),
            legend.title = element_blank(),
            axis.title = element_text(size = 12),
            axis.text=element_text(size=7),
            axis.text.x = element_text(size = 10,angle = 0, hjust = 1),
            axis.text.y = element_text(size = 10, hjust = 1),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            strip.background = element_blank(),
            strip.text=element_text(vjust=0, colour = "black", face = "bold",hjust = 0)
      )+  #+ scale_colour_manual(values=gs.pal(length(tagS))) + scale_fill_manual(values=gs.pal(length(tagS)))
      colScale + fillScale + coord_cartesian(ylim = c(0, 1))
    
  }else{
    plot <- ggplot(AiAll.summ, aes(year, totalDocs, group = groupTAG)) + 
      geom_area(aes(fill = groupTAG),  colour = "white", alpha = 0.7, position="fill") +
      xlim(year(my_begin),year(my_end)) +
      #      ylim(0,1) +
      theme_light() + xlab("") + ylab("Share") +
      theme(legend.position = c(0.8,0.85),
            #legend.key.size =  unit(0.1, "in"),
            legend.background = element_blank(),
            legend.title = element_blank(),
            axis.title = element_text(size = 12),
            axis.text=element_text(size=7),
            axis.text.x = element_text(size = 10,angle = 0, hjust = 1),
            axis.text.y = element_text(size = 10, hjust = 1),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            strip.background = element_blank(),
            strip.text=element_text(vjust=0, colour = "black", face = "bold",hjust = 0)
      )  + colScale + fillScale + coord_cartesian(ylim = c(0, 1)) #scale_colour_manual(values=gs.pal(length(tagS))) + scale_fill_manual(values=gs.pal(length(tagS)))
  }
  
  
  return(plot)
}




# =======================================================================================
# ================================= TEST: Ploting stuff =================================
# =======================================================================================


# ------------------------------ ECAI 2020 - Paradigms -------------------------------- #

aiAll <- readRDS("./AITopics data/aiTopicscomplete.clean.rds")

research.sources = c("AAAI Conferences",
                     "AI Magazine",
                     "arXiv.org Artificial Intelligence",
                     "Communications of the ACM",
                     "IEEE Computer",
                     "IEEE Spectrum",
                     "IEEE Spectrum Robotics Channel",
                     "MIT Technology Review",
                     "Nature",
                     "New Scientist",
                     "New Scientist Online News",
                     "Science",
                     "Classics",
                     "Classics (Collection 2)")


my_groupby = "YEAR"
if (my_groupby == "WEEK") {
  my_begin <- "2016-01-01"
  my_end <- "2018-12-31"
} else {
  my_begin <- "1970-01-01"
  my_end <- "2018-12-31"
}

aiAll.filter <- filter(aiAll, timestamp_1 > ymd(my_begin), timestamp_1 < ymd(my_end))
aiAll.research <- filter(aiAll.filter, source %in% research.sources) 
aiAll.media <- filter(aiAll.filter, !(source %in% research.sources)) 


# Safety related documents

# filename <- "./AI topics tags/mapping-AAAI-safety"
# readings <- read.csv(paste0(filename, ".csv"))
# tags <- c(as.character(unique(readings[,2])))
# 
# grouped.all.safety <- createGroupsFromCSV(filename, aiAll.filter)
# all.safety <- filter(temp, groupTAG == "ai risk and safety")
# saveRDS(all.safety, file = "./AITopics data/aiTopicsSafety.clean.rds")

aiall.safety <- readRDS("./AITopics data/aiTopicsSafety.clean.rds")
aiAll.safety.research <- filter(aiall.safety, source %in% research.sources) 
aiAll.safety.media <- filter(aiall.safety, !(source %in% research.sources)) 





# Techniques ----------------------------------------------------------------

filename.tech <- "./AI topics tags/mapping-techniques"
readings.tech <- read.csv(paste0(filename.tech, ".csv"))
tags.tech <- c(as.character(unique(readings.tech[,2])))


# Research ALL
groups.research <- createGroupsFromCSV(filename.tech, aiAll.research)
# grouped.research <- plotAIGroupedtopicS(tags.tech, groups.research, groupby = my_groupby, my_begin=my_begin, my_end=my_end, smooth=T)
stacked.research <- plotAIStackedTopicS(tags.tech, groups.research, my_begin=my_begin, my_end=my_end, smooth = T)

#Media ALL
groups.media <- createGroupsFromCSV(filename.tech, aiAll.media)
# grouped.media <- plotAIGroupedtopicS(tags.tech, groups.media, groupby = my_groupby, my_begin=my_begin, my_end=my_end, smooth=T)
stacked.media <- plotAIStackedTopicS(tags.tech, groups.media, my_begin="2016-01-01", my_end="2017-01-01", smooth = T, degrees =0, whereLegend = "right")

# ALL
# groups.all <- createGroupsFromCSV(filename.tech, aiAll.filter)
# grouped.all <- plotAIGroupedtopicS(tags.tech, groups.all, groupby = my_groupby, my_begin=my_begin, my_end=my_end, smooth=T)
# stacked.all <- plotAIStackedTopicS(tags.tech, groups.all, my_begin=my_begin, my_end=my_end, smooth = T)




# Research Safety
groups.safety.research <- createGroupsFromCSV(filename.tech, aiAll.safety.research)
#grouped.safety.research <- plotAIGroupedtopicS(tags.tech, groups.safety.research, groupby = my_groupby, my_begin=my_begin, my_end=my_end, smooth=T)
stacked.safety.research <- plotAIStackedTopicS(tags.tech, groups.safety.research, my_begin=my_begin, my_end=my_end, smooth = T)

#Media safety
groups.safety.media <- createGroupsFromCSV(filename.tech, aiAll.safety.media)
#grouped.safety.media <- plotAIGroupedtopicS(tags.tech, groups.safety.media, groupby = my_groupby, my_begin=my_begin, my_end=my_end, smooth=T)
stacked.safety.media <- plotAIStackedTopicS(tags.tech, groups.safety.media, my_begin="2016-01-01", my_end="2017-01-01", smooth = T, degrees =0, whereLegend = "right")

# All safety
#groups.safety.all <- createGroupsFromCSV(filename.tech, aiall.safety)
#grouped.safety.all <- plotAIGroupedtopicS(tags.tech, groups.safety.all, groupby = my_groupby, my_begin=my_begin, my_end=my_end, smooth=T)
#stacked.safety.all <- plotAIStackedTopicS(tags.tech, groups.safety.all, my_begin=my_begin, my_end=my_end, smooth = T)



openPDFEPS(filename.tech, height= 14, width= 10)
gridExtra::grid.arrange(stacked.research, 
                        stacked.safety.research,
                        ncol = 1)
dev.off()

openPDFEPS(paste0(filename.tech,"-noSafety"), height= 7, width= 12)
stacked.research
dev.off()
openPDFEPS(paste0(filename.tech,"-Safety"), height= 7, width= 12)
stacked.safety.research
dev.off()


openPDFEPS(paste0(filename.tech,"MEDIA2017-noSafety"), height= 7, width= 6)
stacked.media
dev.off()
openPDFEPS(paste0(filename.tech,"MEDIA2017-Safety"), height= 7, width= 6)
stacked.safety.media
dev.off()


# Artefacts ----------------------------------------------------------------

filename.art <- "./AI topics tags/mapping-artefacts"
readings.art <- read.csv(paste0(filename.art, ".csv"))
tags.art <- c(as.character(unique(readings.art[,2])))

# Research ALL
groups.research.art <- createGroupsFromCSV(filename.art, aiAll.research)
# grouped.research.art <- plotAIGroupedtopicS(tags.art, groups.research.art, groupby = my_groupby, my_begin=my_begin, my_end=my_end, smooth=T)
stacked.research.art <- plotAIStackedTopicS(tags.art, groups.research.art, my_begin=my_begin, my_end=my_end, smooth = T)

# Media ALL
groups.media.art <- createGroupsFromCSV(filename.art, aiAll.media)
# grouped.media.art <- plotAIGroupedtopicS(tags.art, groups.media.art, groupby = my_groupby, my_begin=my_begin, my_end=my_end, smooth=T)
stacked.media.art <- plotAIStackedTopicS(tags.art, groups.media.art, my_begin="2016-01-01", my_end="2017-01-01", smooth = T, degrees =0, whereLegend = "right")
# 
# # ALL
# groups.all.art <- createGroupsFromCSV(filename.art, aiAll.filter)
# grouped.all.art <- plotAIGroupedtopicS(tags.art, groups.all.art, groupby = my_groupby, my_begin=my_begin, my_end=my_end, smooth=T)
# stacked.all.art <- plotAIStackedTopicS(tags.art, groups.all.art, my_begin=my_begin, my_end=my_end, smooth = T)




# Research Safety
groups.safety.research.art <- createGroupsFromCSV(filename.art, aiAll.safety.research)
# grouped.safety.research.art <- plotAIGroupedtopicS(tags.art, groups.safety.research.art, groupby = my_groupby, my_begin=my_begin, my_end=my_end, smooth=T)
stacked.safety.research.art <- plotAIStackedTopicS(tags.art, groups.safety.research.art, my_begin=my_begin, my_end=my_end, smooth = T)

# Media safety
groups.safety.media.art <- createGroupsFromCSV(filename.art, aiAll.safety.media)
# grouped.safety.media.art <- plotAIGroupedtopicS(tags.art, groups.media.art, groupby = my_groupby, my_begin=my_begin, my_end=my_end, smooth=T)
stacked.safety.media.art <- plotAIStackedTopicS(tags.art, groups.safety.media.art, my_begin="2016-01-01", my_end="2017-01-01", smooth = T, degrees =0, whereLegend = "right")

# All safety
# groups.safety.all.art <- createGroupsFromCSV(filename.art, aiall.safety)
# grouped.safety.all.art <- plotAIGroupedtopicS(tags.art, groups.safety.all.art, groupby = my_groupby, my_begin=my_begin, my_end=my_end, smooth=T)
# stacked.safety.all.art <- plotAIStackedTopicS(tags.art, groups.safety.all.art, my_begin=my_begin, my_end=my_end, smooth = T)




openPDFEPS(filename.art, height= 14, width= 10)
gridExtra::grid.arrange(stacked.research.art, 
                        stacked.safety.research.art,
                        ncol = 1)
dev.off()


openPDFEPS(paste0(filename.art,"-noSafety"), height= 7, width= 12)
stacked.research.art
dev.off()
openPDFEPS(paste0(filename.art,"-Safety"), height= 7, width= 12)
stacked.safety.research.art
dev.off()

openPDFEPS(paste0(filename.art,"MEDIA2017-noSafety"), height= 7, width= 6)
stacked.media.art
dev.off()
openPDFEPS(paste0(filename.art,"MEDIA2017-Safety"), height= 7, width= 6)
stacked.safety.media.art
dev.off()




# Techniques vs Safety ----------------------------------------------------------------


filename.tech <- "./AI topics tags/mapping-techniques"
readings.tech <- read.csv(paste0(filename.tech, ".csv"))
tags.tech <- c(as.character(unique(readings.tech[,2])))

file.name.safe <- "./AI topics tags/mapping-safetyissues"
readings.safe <- read.csv(paste0(file.name.safe, ".csv"))
tags.safe <- c(as.character(unique(readings.safe[,2])))


file.name.art <- "./AI topics tags/mapping-artefacts"
readings.art <- read.csv(paste0(file.name.art, ".csv"))
tags.art <- c(as.character(unique(readings.art[,2])))


# Research 
groups.tech <- createGroupsFromCSV(filename.tech, aiAll.research)
groups.art <- createGroupsFromCSV(file.name.art, aiAll.research)
groups.safe <- createGroupsFromCSV(file.name.safe, aiAll.research)



nrow(groups.tech)
nrow(groups.safe)
nrow(groups.art)


groups.tech.safe <- groups.tech
groups.tech.safe$groupTAGsafe <- groups.safe$groupTAG

groups.art.safe <- groups.art
groups.art.safe$groupTAGsafe <- groups.safe$groupTAG


get.cirz <- function(groups.both, name, from = "1970-01-01", to = "2018-12-31"){
  
  
  print(paste0("Original -> Rows: ", nrow(groups.both), " ( ",paste(from, to, collapse = " - ")," )"))
  groups.both <- filter(groups.both, timestamp_1 > ymd(from), timestamp_1 < ymd(to))
  print(paste0("Filtered -> Rows: ", nrow(groups.both), " ( ",paste(from, to, collapse = " - ")," )"))
  
  g <- group_by(groups.both, groupTAG, groupTAGsafe)
  s <- summarise(g, Frequency = n())
  # View(s)
  df <- s[complete.cases(s),]
  colnames(df) <- c("from","to","value")
  
  #chordDiagram
  # install.packages("circlize")
  # library(circlize)
  
  # df <-filter(df, value > 4)
  # View(df)


  
  getPalette = colorRampPalette(brewer.pal(11, "Spectral"))
  myColors <- getPalette(length(unique(df$from)))
  names(myColors) <- unique(df$from)
  
  set.seed(48664)
  circos.clear()
  circos.par(start.degree = 90, clock.wise = FALSE)
  openPDFEPS(paste0(name, " (", from, " - ", to,")"), height= 20, width= 20)
  chordDiagram(df, big.gap = 10, annotationTrack = "grid", preAllocateTracks = 1, grid.col = myColors)
  circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
    xlim = get.cell.meta.data("xlim")
    ylim = get.cell.meta.data("ylim")
    sector.name = get.cell.meta.data("sector.index")
    circos.text(mean(xlim), ylim[1] + .1, sector.name, facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
    circos.axis(h = "top", labels.cex = 0.5, major.tick.percentage = 0.2, sector.index = sector.name, track.index = 2)
  }, bg.border = NA)
  dev.off()
  
}
  
get.cirz(groups.tech.safe, name = "Techniques_vs_SafeIssues_v5")
get.cirz(groups.tech.safe, name = "Techniques_vs_SafeIssues_v5", from = "2000-01-01", to = "2018-12-31")
get.cirz(groups.tech.safe, name = "Techniques_vs_SafeIssues_v5", from = "2010-01-01", to = "2018-12-31")


get.cirz(groups.art.safe, name = "Artefacts_vs_SafeIssues_v5")
get.cirz(groups.art.safe, name = "Artefacts_vs_SafeIssues_v5", from = "2000-01-01", to = "2018-12-31")
get.cirz(groups.art.safe, name = "Artefacts_vs_SafeIssues_v5", from = "2010-01-01", to = "2018-12-31")

