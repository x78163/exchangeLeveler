library(jsonlite)
library(readxl)
library(ggplot2)
library(shinydashboard)
library(shiny)
library(reshape2)

#---------> Import Fee Table-------------------

#fee <- read_excel("C:/Users/josep/Desktop/Exchange Fees.xlsx")

exchange = c("poloniex", "Coinbase", "Bittrex", "gemeni", "bitstamp", "cex", "bitsquare", "Bitfinex")
In = c(.0015, .04, .0025, .0025, .0025, .07, .002, .001) #CEX lied...their fiat to BTC exchange is 7%
Out = c(.0025, .04, .0025, .0025, .0025, .07, .002, .002)
fee = data.frame(exchange, In, Out)


#---------->  Collect data from all Exchanges--------------------------------
poloniex <- fromJSON("https://poloniex.com/public?command=returnTicker")
coinbase <- fromJSON("https://api.coinbase.com/v2/prices/spot?currency=USD")
bittrex <- fromJSON("https://bittrex.com/api/v1.1/public/getmarketsummary?market=usdt-btc")
gemeni <- fromJSON("https://api.gemini.com/v1/pubticker/btcusd")
bitstamp <- fromJSON("https://www.bitstamp.net/api/v2/ticker/btcusd/")
bitfinex <- fromJSON("https://api.bitfinex.com/v1/pubticker/btcusd")
cex <-fromJSON("https://cex.io/api/ticker/BTC/USD")
bitsquare <-fromJSON("https://markets.bisq.network/api/ticker?market=btc_usd")


#---------->  Isolate all "Last" Values from JSON Data --------------------------------
poloniex = poloniex$USDT_BTC$last
coinbase = coinbase$data$amount
bittrex = bittrex$result$Last
gemeni = gemeni$last
bitstamp = bitstamp$last
cex = cex$last
bitsquare = bitsquare$last
bitfinex = bitfinex$last_price

#---------->  Build Data Frame of all "Last" Values --------------------------------
current <- data.frame(time=Sys.time())
current$poloniex = as.numeric(poloniex)
current$coinbase = as.numeric(coinbase)
current$bittrex =as.numeric(bittrex)
current$gemeni = as.numeric(gemeni)
current$bitstamp = as.numeric(bitstamp)
current$cex =as.numeric(cex)
current$bitsquare = as.numeric(bitsquare)
current$bitfinex = as.numeric(bitfinex)


#----------> Split Current Into Min and Max Data Frames for Fee Application--------------

inMin = current

inMin$poloniex = inMin$poloniex + (inMin$poloniex*as.numeric(fee[1,2]))
inMin$coinbase = inMin$coinbase + (inMin$coinbase*as.numeric(fee[2,2]))
inMin$bittrex = inMin$bittrex + (inMin$bittrex*as.numeric(fee[3,2]))
inMin$gemeni = inMin$gemeni + (inMin$gemeni*as.numeric(fee[4,2]))
inMin$bitstamp = inMin$bitstamp + (inMin$bitstamp*as.numeric(fee[5,2]))
inMin$cex = inMin$cex + (inMin$cex*as.numeric(fee[6,2]))
inMin$bitsquare = inMin$bitsquare + (inMin$bitsquare*as.numeric(fee[7,2]))
inMin$bitfinex = inMin$bitfinex + (inMin$bitfinex*as.numeric(fee[8,2]))


outMax = current

outMax$poloniex = outMax$poloniex - (outMax$poloniex*as.numeric(fee[1,3]))
outMax$coinbase = outMax$coinbase - (outMax$coinbase *as.numeric(fee[2,3]))
outMax$bittrex = outMax$bittrex - (outMax$bittrex *as.numeric(fee[3,3]))
outMax$gemeni = outMax$gemeni - (outMax$gemeni *as.numeric(fee[4,3]))
outMax$bitstamp = outMax$bitstamp - (outMax$bitstamp *as.numeric(fee[5,3]))
outMax$cex = outMax$cex - (outMax$cex *as.numeric(fee[6,3]))
outMax$bitsquare = outMax$bitsquare - (outMax$bitsquare *as.numeric(fee[7,3]))
outMax$bitfinex = outMax$bitfinex - (outMax$bitfinex *as.numeric(fee[8,3]))

#----------> Calculate Exchanges with Min and Max Values --------------------------------
max = which.max(outMax[2:ncol(outMax)])
min = which.min(inMin[2:ncol(inMin)])

#----------> Calculate Column Index Values with Min and Max Values --------------------------------
maxIndex = as.integer(max)+1
minIndex = as.integer(min)+1

#----------> Calculate BTC to USD Values with Min and Max Values --------------------------------
minValue = as.numeric(inMin[,minIndex])
maxValue = as.numeric(outMax[,maxIndex])

#----------> Identify Exchanges with Min and Max Values --------------------------------
minName = names(min)
maxName = names(max)

#----------> Create Winner Data Frame-------------------------------------------------------
name <- c(maxName, minName)
value <- c(maxValue, minValue)
winner <- data.frame(name, value)


#----------> Calcuate Price Delta & ROI between Min and Max --------------------------------

delta = maxValue- minValue
ROI = (((maxValue-minValue)/minValue)*100)

#----------> Generate Output Message for User With Purchasing Advice --------------------------------
#text = paste("The highest yield is: $", round(delta, digits = 2), "If you buy 1x Bitcoin on",minName , " for: $",round(minValue, digits = 2), ", you can sell it on: ",maxName , " for: $", round(maxValue, digits = 2), ".  This is a:", round(ROI, digits=0),"% Return on Investment" )

plotCurrent = current[2:ncol(current)]
plotOutMax = outMax[2:ncol(outMax)]
plotInMin = inMin[2:ncol(inMin)]
plotCurrent = melt(plotCurrent)
plotOutMax = melt(plotOutMax)
plotInMin = melt(plotInMin)
plotCombi = plotCurrent
plotCombi$inMin = plotInMin$value
plotCombi$outMax = plotOutMax$value


#----------> Calculate y Axis Plot Limits --------------------------------
maxPlot = which.min(outMax[2:ncol(outMax)])
minPlot = which.max(inMin[2:ncol(inMin)])

#----------> Calculate Min and Max values for Plot Limits --------------------------------
maxIndexPlot = as.integer(maxPlot)+1
minIndexPlot = as.integer(minPlot)+1

#----------> Calculate dollar values of min and max values for plot limits-----------------
maxValuePlot = as.numeric(inMin[,minIndexPlot])
minValuePlot = as.numeric(outMax[,maxIndexPlot])


#------> Combined Plot---------------
x = plotCombi$variable
y2 = plotCombi$inMin
y1 =plotCombi$value
y3 = plotCombi$outMax

to_plot <- data.frame(x=x,y1=y2,y2=y1, y3=y3)
melted<-melt(to_plot, id="x")

plot1 = ggplot(melted,aes(x=x,y=value,fill=variable)) + geom_bar(stat="identity",position = "identity", alpha=1)+ coord_cartesian(
  ylim = c(minValuePlot-100, maxValuePlot+100))+ labs(x = "Exchanges", y="Bitcoin Value in $", title = "Overall Exchange Prices (Sell, Spot and Buy)")+ scale_fill_discrete(name="Prices with Fees", breaks=c("y1", "y2", "y3"),labels=c("Buying (In) Price", "Exchange Price", "Selling Price (Out)"))+
  geom_text((aes(label=value)),  nudge_y = 100, check_overlap = T)

plot2 = ggplot(winner,aes(x=name,y=value, fill=factor(name))) + geom_bar(stat="identity",position = "identity", alpha=1)+ coord_cartesian(
  ylim = c(minValuePlot-100, maxValuePlot+100))+ labs(x = "Exchanges", y="Bitcoin Value in $", title = "Best Exchanges to Buy and Then Sell") + scale_fill_discrete(name="Exchanges", breaks=c(minName, maxName),labels=c("Buying Exchange", "Selling Exchange"))+
  geom_text((aes(label=value)),nudge_y = 100)


## app.R ##
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Bitcoin Exchange Calculator"),
  dashboardSidebar(
    a(h4("Go Home"), href = paste0("http://www.joe-data.com/"))
    
  ),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      #   box(title = text),
      infoBoxOutput("text"),
      box(plotOutput("plot1", height = 250)),
      box(plotOutput("plot2", height = 250))
      
    )
  )
)

server <- function(input, output) {
  
  
  output$text <- renderInfoBox({
    #---------->  Collect data from all Exchanges--------------------------------
    poloniex <- fromJSON("https://poloniex.com/public?command=returnTicker")
    coinbase <- fromJSON("https://api.coinbase.com/v2/prices/spot?currency=USD")
    bittrex <- fromJSON("https://bittrex.com/api/v1.1/public/getmarketsummary?market=usdt-btc")
    gemeni <- fromJSON("https://api.gemini.com/v1/pubticker/btcusd")
    bitstamp <- fromJSON("https://www.bitstamp.net/api/v2/ticker/btcusd/")
    bitfinex <- fromJSON("https://api.bitfinex.com/v1/pubticker/btcusd")
    cex <-fromJSON("https://cex.io/api/ticker/BTC/USD")
    bitsquare <-fromJSON("https://markets.bisq.network/api/ticker?market=btc_usd")
    
    
    #---------->  Isolate all "Last" Values from JSON Data --------------------------------
    poloniex = poloniex$USDT_BTC$last
    coinbase = coinbase$data$amount
    bittrex = bittrex$result$Last
    gemeni = gemeni$last
    bitstamp = bitstamp$last
    cex = cex$last
    bitsquare = bitsquare$last
    bitfinex = bitfinex$last_price
    
    #---------->  Build Data Frame of all "Last" Values --------------------------------
    current <- data.frame(time=Sys.time())
    current$poloniex = as.numeric(poloniex)
    current$coinbase = as.numeric(coinbase)
    current$bittrex =as.numeric(bittrex)
    current$gemeni = as.numeric(gemeni)
    current$bitstamp = as.numeric(bitstamp)
    current$cex =as.numeric(cex)
    current$bitsquare = as.numeric(bitsquare)
    current$bitfinex = as.numeric(bitfinex)
    
    
    #----------> Split Current Into Min and Max Data Frames for Fee Application--------------
    
    inMin = current
    
    inMin$poloniex = inMin$poloniex + (inMin$poloniex*as.numeric(fee[1,2]))
    inMin$coinbase = inMin$coinbase + (inMin$coinbase*as.numeric(fee[2,2]))
    inMin$bittrex = inMin$bittrex + (inMin$bittrex*as.numeric(fee[3,2]))
    inMin$gemeni = inMin$gemeni + (inMin$gemeni*as.numeric(fee[4,2]))
    inMin$bitstamp = inMin$bitstamp + (inMin$bitstamp*as.numeric(fee[5,2]))
    inMin$cex = inMin$cex + (inMin$cex*as.numeric(fee[6,2]))
    inMin$bitsquare = inMin$bitsquare + (inMin$bitsquare*as.numeric(fee[7,2]))
    inMin$bitfinex = inMin$bitfinex + (inMin$bitfinex*as.numeric(fee[8,2]))
    
    
    outMax = current
    
    outMax$poloniex = outMax$poloniex - (outMax$poloniex*as.numeric(fee[1,3]))
    outMax$coinbase = outMax$coinbase - (outMax$coinbase *as.numeric(fee[2,3]))
    outMax$bittrex = outMax$bittrex - (outMax$bittrex *as.numeric(fee[3,3]))
    outMax$gemeni = outMax$gemeni - (outMax$gemeni *as.numeric(fee[4,3]))
    outMax$bitstamp = outMax$bitstamp - (outMax$bitstamp *as.numeric(fee[5,3]))
    outMax$cex = outMax$cex - (outMax$cex *as.numeric(fee[6,3]))
    outMax$bitsquare = outMax$bitsquare - (outMax$bitsquare *as.numeric(fee[7,3]))
    outMax$bitfinex = outMax$bitfinex - (outMax$bitfinex *as.numeric(fee[8,3]))
    
    #----------> Calculate Exchanges with Min and Max Values --------------------------------
    max = which.max(outMax[2:ncol(outMax)])
    min = which.min(inMin[2:ncol(inMin)])
    
    #----------> Calculate Column Index Values with Min and Max Values --------------------------------
    maxIndex = as.integer(max)+1
    minIndex = as.integer(min)+1
    
    #----------> Calculate BTC to USD Values with Min and Max Values --------------------------------
    minValue = as.numeric(inMin[,minIndex])
    maxValue = as.numeric(outMax[,maxIndex])
    
    #----------> Identify Exchanges with Min and Max Values --------------------------------
    minName = names(min)
    maxName = names(max)
    
    #----------> Create Winner Data Frame-------------------------------------------------------
    name <- c(maxName, minName)
    value <- c(maxValue, minValue)
    winner <- data.frame(name, value)
    
    
    #----------> Calcuate Price Delta & ROI between Min and Max --------------------------------
    
    delta = maxValue- minValue
    ROI = (((maxValue-minValue)/minValue)*100)
    #
    #----------> Generate Output Message for User With Purchasing Advice --------------------------------
    text = paste("The highest yield is: $", round(delta, digits = 2), "If you buy 1x Bitcoin on",minName , " for: $",round(minValue, digits = 2), ", you can sell it on: ",maxName , " for: $", round(maxValue, digits = 2), ".  This is a:", round(ROI, digits=0),"% Return on Investment" )
    
    infoBox(
      
      "Best Exchange Data!:", text, icon = icon("exchange"),
      color = "purple", fill = TRUE
    )
  })
  
  output$plot1 <- renderPlot({
    plot1
  })
  
  output$plot2 <- renderPlot({
    plot2
  })
  
  
}

shinyApp(ui, server)
