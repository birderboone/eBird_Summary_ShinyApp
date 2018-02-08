library(shiny)
library(ggplot2)
library(gridExtra)
library(RColorBrewer)
library(cowplot)

#Where is the APP and its components located?
setwd('C:/Users/birde/Dropbox/git/ebird/ShinySummaryApp')

### Start APP. Do not edit
ui <- fluidPage(
  
  # App title ----
  titlePanel("Plot your eBirding!!!!"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(width=3,
                 
                 # Input: Select the random distribution type ----
                 fileInput("ebirdInput", "eBird Observations",accept='.csv'),
                 
                 # br() element to introduce extra vertical spacing ----
                 
                 sliderInput(inputId='gridsize',
                             label='Grid Cell Size (Birding Grid)',
                             value=1,min=0.25,max=20,step=.25)),
    
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("US Locations", plotOutput("pointPlot")),
                  tabPanel("by County", plotOutput("countyPlot")),
                  tabPanel("per Time", plotOutput("birdsOverTime")),
                  tabPanel("US Grid", plotOutput("blockBirds")),
                  tabPanel("by State", plotOutput("LifebirdsState")),
                  tabPanel("by Month", plotOutput("birdsByMonth")),
                  tabPanel("by Year", plotOutput('birdsByYear'))
                  
      )
      
    )
  )
)


server<-function(input, output){
  ####read in base files
  #data<-read.csv('file:///C:/Users/birde/Downloads/ebird_1515702195443 (1)/MyEBirdData.csv',stringsAsFactors=F)
  aba<-read.csv('aou_simple.csv')
  state.table<-read.csv('state_table.csv')
  cdate<-paste0('X',rep(1:12,1,each=4),'.',1:4)
  myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
  bl <- colorRampPalette(c("navy","royalblue","lightskyblue"))(200)                      
  re <- colorRampPalette(c("mistyrose", "red2","darkred"))(200)
  #read in and modify the observations data
  get_data<-reactive({
    data<-read.csv(input$ebirdInput$datapath,stringsAsFactors=F)
    
    #readies the input data file of bird observations
    data$Date<-as.Date(as.character(data$Date),format='%m-%d-%Y')
    data$State<-sapply(strsplit(as.character(data$State.Province),'-'),function(x)x[2])
    data$County<-as.character(data$County)
    data<-data[!data$Count=='0',]
    #attempts to simplify observations to their specific epithets ignoring sub.species
    data<-data[!(grepl("sp\\.",data$Scientific.Name) | grepl(" x ",data$Scientific.Name) | grepl("/",data$Scientific.Name) | grepl("(Domestidddc)",data$Scientific.Name)) ,]
    data$Scientific.Name<-paste0(sapply(strsplit(data$Scientific.Name,' '),function(x)x[1]), ' ', sapply(strsplit(data$Scientific.Name,' '),function(x)x[2]))
    
    #we have to split the data into mainland and AK HI for plotting purposes
    
    data1<-subset(data, !State.Province%in%c('US-HI','US-AK') & grepl('US',State.Province))
    list(data=data,data1=data1)
  })
  ####
  
  #modify the state table (maybe need to do outside of R)
  state.table$name<-tolower(state.table$name)
  state.table$abbreviation<-tolower(state.table$abbreviation)
  
  #read in the map data for later use
  map.state <- map_data('state')
  
  usa<-map_data('usa')
  us.county<-map_data('county')
  us.county<-merge(us.county,state.table,by.x='region',by.y='name')
  world<-map_data('world2')
  hi<-subset(world,subregion=='Hawaii')
  ak<-subset(world,subregion=='Alaska')
  ###################################################
  # start specific projects
  
  output$pointPlot <- renderPlot({ 
    req(input$ebirdInput)
    
    data1<-get_data()$data1
    subdata<-data.frame(unique(paste0(data1$Latitude,'_',data1$Longitude)))
    subdata<-data.frame(Latitude=as.numeric(sapply(strsplit(as.character(subdata[,1]),'_'),function(x)x[1])),Longitude=as.numeric(sapply(strsplit(as.character(subdata[,1]),'_'),function(x)x[2])))
    
    base<-ggplot() + geom_polygon(data = map.state, aes(x=long, y = lat,group=group),color='black',fill='gray') + coord_fixed(1.3)
    
    base + geom_point(data=subdata,aes(x=Longitude,y=Latitude),color='darkblue')
  })
  
  output$countyPlot <-renderPlot({
    data<-get_data()$data
    d<-tapply(data$Common.Name,paste0(data$State,'-',data$County) ,function(x)length(unique(x)))
    d<-data.frame(State=tolower(substr(names(d),1,2)),county=tolower(substr(names(d),4,nchar(names(d)))),total=d)
    d<-d[nchar(as.character(d$county))!=0,]
    
    final<-merge(us.county,d, by.x=c('abbreviation','subregion'),by.y=c('State','county'),all.x=T)
    final<-final[order(final$order,final$group),]
    
    #final$total[is.na(final$total)]<-0
    ggplot() + geom_polygon(data = final, aes(x=long, y = lat,group=group,fill=total),color='darkgray') + coord_fixed(1.3)+ 
      scale_fill_gradientn(colours=myPalette(10),name='Total',na.value="white")+ geom_polygon(data = map.state, aes(x=long, y = lat,group=group),fill=NA,color='darkblue')
    
    #+scale_fill_continuous(low="thistle1", high="darkred", guide="colorbar",na.value="white") 
    
    
  })
  
  output$birdsOverTime <-renderPlot({
    data<-get_data()$data
    f<-tapply(data$Date,data$Scientific.Name,function(x)as.character(sort(x)[1]))
    f1<-tapply(data$Common.Name,data$Scientific.Name,function(x)as.character(sort(x)[1]))
    first.df<-data.frame(species=names(f), fdate=f)
    first.df$fdate<-as.POSIXlt(first.df$fdate)
    first.df$year<-first.df$fdate$year+1900
    first.df$month<-first.df$fdate$mon+1
    #tapply(first.df$year,first.df$year, length)
    
    minyear<-min(first.df$fdate$year[first.df$fdate$year!=0])+1900
    maxyear<-as.numeric(substr(Sys.Date(),1,4))
    maxmonth<-as.numeric(substr(Sys.Date(),6,7))
    myCategories<-colorRampPalette(rev(brewer.pal(8, "Dark2")))
    cum.df<-data.frame(year=rep(minyear:maxyear,each=12), month=rep(1:12, (maxyear-minyear+1)))
    colorframe<-data.frame(year=(maxyear-95):maxyear,color=as.character(rep(myCategories(8),times=12)))
    cum.df<-merge(cum.df, colorframe,by='year',all.x=T)
    g<-aggregate(first.df[,'species'],by=list(first.df$year,first.df$month),length)
    
    colnames(g)<-c('year','month','sum')
    starter<-sum(subset
                 
                 (g, year==1900)$sum)
    cum.df<-merge(cum.df, g, by=c('year','month'),all.x=T)
    cum.df$sum[1]<-starter
    cum.df$sum[is.na(cum.df$sum)]<-0
    cum.df$total<-cumsum(cum.df$sum)
    cum.df$comb<-factor(paste0(cum.df$month,'/ ',cum.df$year),levels=paste0(cum.df$month,'/ ',cum.df$year))
    cum.df<-cum.df[1:which(cum.df$year==maxyear & cum.df$month==maxmonth),]
    cum.df$comb<-factor(cum.df$comb, levels=cum.df$comb)
    cum.df$group<-1
    
    gtext<-data.frame(year=minyear:maxyear,x=(6.5+ 12*0:(length(minyear:maxyear)-1)),y=(tapply(cum.df$total, cum.df$year, max))+5)
    
    if(length(cum.df$month[cum.df$year==maxyear])!=12){gtext<-gtext[gtext$year!=maxyear,]}
    # plot
    
    ggplot(data=cum.df,aes(x=comb,y=total)) + geom_col(fill=cum.df$color) + coord_cartesian(ylim=c(min(cum.df$total),max(cum.df$total))) +theme_bw() + geom_text(data=gtext,aes(x=x,y=y,label=year),size=5) + scale_x_discrete(breaks=cum.df$comb[seq(1,nrow(cum.df),by=3)],labels=cum.df$month[seq(1,nrow(cum.df),by=3)]) + theme(axis.text=element_text(size=12),axis.title=element_text(size=16,face="bold"),plot.title=element_text(size=16)) +
      labs(title="Life Birds", 
           y="Birds",x='Month')
    
  })
  
  output$LifebirdsState <- renderPlot({
   
    data<-get_data()$data
    df1<-tapply(data$Scientific.Name, data$State, function(x)length(unique(x)))
    df2<-data.frame(total=df1,ab=tolower(names(df1)))
    df2<-merge(df2, state.table, by.x='ab',by.y='abbreviation',all.x=T)
    
    fillmin<-min(df2$total)
    fillmax<-max(df2$total)
    map.state1<-merge(map.state,df2, by.x='region',by.y='name',all.x=T)
    map.state1<-map.state1[order(map.state1$order,map.state1$group),]
    a<-ggplot() + geom_polygon(data = map.state1, aes(x=long, y = lat,group=group,fill=total),color='black') + coord_fixed(1.3)+ scale_fill_gradientn(colours = myPalette(10),name='Total',lim=c(fillmin, fillmax))
    #base
    hi$subregion<-tolower(hi$subregion)
    hi1<-merge(hi,df2, by.x='subregion',by.y='name')
    hi1<-hi1[order(hi1$order,hi1$group),]
    b<-ggplot() + geom_polygon(data = hi1, aes(x=long, y = lat,group=group,fill=total),color='black') + coord_fixed(1.3) + scale_fill_gradientn(colours = myPalette(10),name='Total',lim=c(fillmin, fillmax)) + guides(fill=FALSE) +theme_void()
    
    ak$subregion<-tolower(ak$subregion)
    ak1<-merge(ak,df2, by.x='subregion',by.y='name')
    ak1<-ak1[order(ak1$order,ak1$group),]
    c<-ggplot() + geom_polygon(data = ak1, aes(x=long, y = lat,group=group,fill=total),color='black') + coord_fixed(1.3) + scale_fill_gradientn(colours = myPalette(10),name='Total',lim=c(fillmin, fillmax))+ guides(fill=FALSE) +theme_void()
    
    
    ggdraw() +
      draw_plot(a , 0, 0, 1,1) +
      draw_plot(c ,x=0.1,y=.1,width=.25,height=.25) +
      draw_plot(b ,x=0.7,y=.15,width=.15,height=.15) 
  })
  
  output$blockBirds <-renderPlot({
    #option1<-1
    #data2<-data1
    option1<-input$gridsize
    data2<-get_data()$data1
    data2$newlat<-floor(data2$Latitude/option1)*option1 + option1/2
    data2$newlong<-floor(data2$Longitude/option1)*option1  + option1/2
    answer3<-data.frame(value=tapply(data2$Scientific.Name, paste0(data2$newlat,'_',data2$newlong), function(x)length(unique(x))))
    answer3$lat<-NA
    answer3$long<-NA
    answer3[,c('lat','long')]<-matrix(as.numeric(unlist(strsplit(row.names(answer3),'_'))),ncol=2,byrow=T)
    
    #base map
    map.state3<-usa
    shift<-ifelse(option1<=5,option1,0)
    
    map.state3$newlat<-floor(map.state3$lat/option1)*option1 + option1/2
    map.state3$newlong<-floor(map.state3$long/option1)*option1  + option1/2
    
    lat1<-seq((min(map.state3$newlat)-shift),max(map.state3$newlat),by=option1)
    long1<-seq(min(map.state3$newlong),max(map.state3$newlong),by=option1)
    grid1<-data.frame(lat=rep(lat1, each=length(long1)),
                      long=rep(long1, times=length(lat1)))
    
    ggplot()  +
      geom_tile(data=grid1,aes(x=long, y=lat),fill='lightgrey',alpha=.5,color='lightgray') +geom_raster(data=answer3, aes(x=long,y=lat,fill=value)) +
      scale_fill_gradientn(colours = myPalette(10)) +
      coord_fixed(1.3)+  geom_polygon(data = map.state, aes(x=long, y = lat,group=group),color='black',fill=NA)  +ggtitle(paste0(option1,' degree'))
    
    
    ##############
  })
  
  output$birdsByMonth<-renderPlot({
    ##########
    data<-get_data()$data
    df7<-data.frame(min= tapply(data$Duration..Min,data$Submission.ID,max),
                    date=tapply(data$Date,data$Submission.ID,function(x)as.character(x)[1]))
    total.hours<-tapply(df7$min, substr(df7$date,1,7),sum,na.rm=T)/60
    total.birds<-tapply(data$Scientific.Name,substr(data$Date,1,7), function(x) length(unique(x) ))
   

    first.df<- data.frame(total.birds,total.hours)
    first.df$year<-as.numeric(substr(row.names(first.df),1,4))
    first.df$month<-as.numeric(substr(row.names(first.df),6,7))
    #tapply(first.df$year,first.df$year, length)
    
    minyear<-min(first.df$year[first.df$year!=1900])
    maxyear<-max(first.df$year[first.df$year!=1900])
    maxmonth<-max(first.df$month[first.df$year==maxyear])
    myCategories<-colorRampPalette(rev(brewer.pal(8, "Dark2")))
    
    first.df<-first.df[first.df$year!=1900,]
    cum.df<-data.frame(year=rep(minyear:maxyear,each=12), month=rep(1:12, (maxyear-minyear+1)))
    cum.df$dnames<-paste0(cum.df$year,'-',cum.df$month)
    cum.df<-merge(cum.df, first.df, by=c('year','month'),all.x=T)
    cum.df$total.hours[is.na(cum.df$total.hours)]<-0
    cum.df$total.birds[is.na(cum.df$total.birds)]<-0
    colorframe<-data.frame(year=(maxyear-95):maxyear,color=as.character(rep(myCategories(8),times=12)))
    cum.df<-merge(cum.df, colorframe,by='year',all.x=T)
    cum.df$group<-1
    cum.df$dnames<-factor(cum.df$dnames, levels=cum.df$dnames)
    gtext<-data.frame(year=minyear:maxyear,x=(6.5+ 12*0:(length(minyear:maxyear)-1)),y=(tapply(first.df$total.birds, first.df$year, max))+5)
    if(length(cum.df$month[cum.df$year==maxyear]<6)){ gtext$x[gtext$year==maxyear]<-gtext$x[gtext$year==maxyear]-5.5
      }
   cum.df<-cum.df[!(cum.df$year==maxyear & cum.df$month>maxmonth),]
    
   ggplot(data=cum.df,aes(x=dnames,y=total.birds)) + geom_col(fill=cum.df$color) + coord_cartesian(ylim=c(min(cum.df$total.birds),max(cum.df$total.birds))) +theme_bw() + geom_text(data=gtext,aes(x=x,y=y,label=year),size=5) + scale_x_discrete(breaks=cum.df$dnames[seq(1,nrow(cum.df),by=3)],labels=cum.df$month[seq(1,nrow(cum.df),by=3)]) + theme(axis.text=element_text(size=12),axis.title=element_text(size=16,face="bold"),plot.title=element_text(size=16)) +
      labs(title="Total Birds Seen by Month", 
           y="Birds",x='Month')
  })
  output$birdsByYear<-renderPlot({
    #cumulative birds seen
    data<-get_data()$data
    ddates<-as.POSIXlt(data$Date)
    minyear<-min(ddates$year[ddates$year!=0])+1900
    maxyear<-max(ddates$year[ddates$year!=0])+1900
    maxmonth<-max(ddates$mon[ddates$year==(maxyear-1900)]+1)
    myCategories<-colorRampPalette(rev(brewer.pal(8, "Dark2")))
    mon.cum.sum<-list()
   for(i in minyear:maxyear){
     #i<-2013
     sub.df<-subset(data,as.numeric(substr(data$Date,1,4))==i)
     f<-tapply(sub.df$Date,sub.df$Scientific.Name,function(x)as.character(sort(x)[1]))
    g1<-tapply(f, as.POSIXlt(f)$mon,length)
     g2<-data.frame(total=g1,month=as.numeric(names(g1))+1)
     g3<-merge(data.frame(month=1:12), g2, by='month',all.x=T)
     g3$total[is.na(g3$total)]<-0
     g3$cumsum<-cumsum(g3$total)
     mon.cum.sum[[paste0(i)]]<-g3
   }
    cum.df<-do.call(rbind, mon.cum.sum)
    cum.df$year<-as.numeric(substr(row.names(cum.df),1,4))

    cum.df$dnames<-paste0(cum.df$year,'-',cum.df$month)

    colorframe<-data.frame(year=(maxyear-95):maxyear,color=as.character(rep(myCategories(8),times=12)))
    cum.df<-merge(cum.df, colorframe,by='year',all.x=T)
    cum.df$group<-1
    cum.df$dnames<-factor(cum.df$dnames, levels=cum.df$dnames)
    gtext<-data.frame(year=minyear:maxyear,x=(6.5+ 12*0:(length(minyear:maxyear)-1)),y=(tapply(cum.df$cumsum, cum.df$year, max))+5)
    if(length(cum.df$month[cum.df$year==maxyear]<6)){ gtext$x[gtext$year==maxyear]<-gtext$x[gtext$year==maxyear]-5.5
    }
    cum.df<-cum.df[!(cum.df$year==maxyear & cum.df$month>maxmonth),]
    
    ggplot(data=cum.df,aes(x=dnames,y=cumsum)) + geom_col(fill=cum.df$color) + coord_cartesian(ylim=c(min(cum.df$cumsum),max(cum.df$cumsum))) +theme_bw() + geom_text(data=gtext,aes(x=x,y=y,label=year),size=5) + scale_x_discrete(breaks=cum.df$dnames[seq(1,nrow(cum.df),by=3)],labels=cum.df$month[seq(1,nrow(cum.df),by=3)]) + theme(axis.text=element_text(size=12),axis.title=element_text(size=16,face="bold"),plot.title=element_text(size=16)) +
      labs(title="Total Birds Seen by Year", 
           y="Birds",x='Month')
      
  })
  
}




shinyApp(ui=ui,server=server)
