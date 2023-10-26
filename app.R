library(shiny)
library(shinythemes)
w <- read.table("weights.txt", sep=",", head=FALSE)
r <- read.table("responds.txt", sep=",", head=FALSE)
colnames(w) <- c("fert", "bottle_lid")

ui <-
tagList(
	fluidPage(
		tags$head(includeHTML(("google-analytics.html"))),
		theme=shinytheme("spacelab"),
		titlePanel("Fertilizer point application rate calculator by UFCL"),
		sidebarLayout(sidebarPanel(
			h3("Enter field area in ha or acres"),
			numericInput("field", label=NULL, value=1),
			h3("Enter distance between rows in cm"),
			numericInput("inter_row", label=NULL, value=75),
			h3("Enter distance between the plants in the row in cm"),
			numericInput("in_row", label=NULL, value=40),
			h3("Choose fertilizer from drop down list"),
			selectInput("fert", label=NULL, choices=paste(w$fert), selected=paste(w$fert[7])),
			h3("Enter fertilizer application rate in kg/ha or kg/acre"),
			numericInput("aprate", label=NULL, value=200),
			actionButton("calculate", "Click to calculate poin application rates", class = "btn-primary")		
			),
		mainPanel(
			div(img(src="img2.jpg", width=400), style="text-align: center;"),
			h2(strong(textOutput("text1")), align="center"),
			h4(span(em(textOutput("text2")), style="color:red"), align="center")
		)),
		hr(),
		p("For more information visit UFCL web site",
            br(),
            a(href="https://ufertilizers.com/home", "www.ufertilizers.com",target="_blank"),style="text-align:center;color:black")
	)
)

server <-
function(input, output) {
	
roundf <- function(a, b=floor(a), d=a-b){
	if(a<0.5){
		c(0,1)
	} else if(a<0.75 & a>=0.5){
		c(0.5,2)
	} else if(a<1 & a>=0.75){
		c(1,3)
	} else if(d>=0.75){
		c(b+1,4)
	} else if(d<0.75 & d>=0.25){
		c(b+0.5,5)
	} else {
		c(b,6)
	}
}

observe<-observeEvent(input$calculate,{
	pointapl <- roundf(w[which(w$fert==input$fert),2]/input$aprate*10^5/input$inter_row/input$in_row)

	output$text1 <- renderText({
		input$calculate
		if(pointapl[2]==1){
			paste0("The quantity of the fertilizer is too large for bottle lid point application. Please check whether all input data are correct or devide application rate into doses.")
		} else {
		paste0("Point apply ", isolate(input$aprate*input$field), " kg of ", isolate(input$fert), r[pointapl[2],1], pointapl[1]*r[pointapl[2],2], r[pointapl[2],3])
		}
	})
	output$text2 <- renderText({
		paste0("If something is left after applying to all plants – spread the rest evenly across the field")
	})

})

}

shinyApp(ui, server)