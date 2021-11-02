#GroupD6

source("buttonrows.R")
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(tidyverse)

#what we need to read in excel file
#install.packages("readxl") <_ run this line if you get an error with the package
library(readxl)

ui <- dashboardPage(
  dashboardHeader(title = "Group D6"),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    fluidRow(
      column(width=6,
             box(
               width = NULL,
               height = 800,
               h3 ("Elements of the group"),
               h4("The identity"),
               controlRow1(
                 "ctrlI"
               ),  #agb
               h4("Order 6 elements"),
               controlRow2_w_labels(
                 c("ctrl_R1", "ctrl_R5"),
                 c("R1", "R5")
               ),
               h4("Order 3 elements (rotations)"),
               controlRow2_w_labels(
                 c("ctrl_R2","ctrl_R4"), #ids of the buttons
                 c( "R2", "R4") #labels of the buttons
               ),
               h4("Order 2 elements (180 degree rotation)"),
               controlRow1_w_labels(
                 c("ctrl_R3"),
                 c("R3")
               ),
               h4("Order 2 elements (Edge-Edge flips)"),
               controlRow3_w_labels(
                 c("ctrl_r1","ctrl_r3","ctrl_r5"),
                 c("r1", "r3", "r5")
               ),
               h4("Order 2 elements (Vertex-Vertex flips)"),
               controlRow3_w_labels(
                 c("ctrl_r0","ctrl_r2","ctrl_r4"), #ids of the buttons
                 c("r0", "r2", "r4") #labels of the buttons
               )  #agb
               
             ),#box
             box(
               width = NULL,
               height = NULL,
               title = "Subgroups",
               #actionButton("show_subgroups", "Show subgroups"),
               #tableOutput("subgroup_df_o")
               
               h4("C1 subgroup"),
               buttonRow1(
                 inputIds = c("btnC1"),
                 labels = c("Show C1"),
                 btnStyle = "padding:4px;font-size:150%"
               ), 
               
               h4("C2 subgroup"),
               buttonRow1(
                 inputIds = c("btnC2_1"),
                 labels = c("Show C2"),
                 btnStyle = "padding:4px;font-size:150%"
               ), 
               buttonRow3(
                 inputIds = c("btnC2_2","btnC2_3","btnC2_4"),
                 labels = c("Show C2", "Show C2", "Show C2"),
                 btnStyle = "padding:4px;font-size:150%"
               ), 
               buttonRow3(
                 inputIds = c("btnC2_5","btnC2_6","btnC2_7"),
                 labels = c("Show C2", "Show C2", "Show C2"),
                 btnStyle = "padding:4px;font-size:150%"
               ),
               
               h4("C3 subgroup"),
               buttonRow1(
                 inputIds = c("btnC3"),
                 labels = c("Show C3"),
                 btnStyle = "padding:4px;font-size:150%"
               ),
               
               h4("C6 subgroup"),
               buttonRow1(
                 inputIds = c("btnC6"),
                 labels = c("Show C6"),
                 btnStyle = "padding:4px;font-size:150%"
               ),
               
               h4("S3 subgroup"),
               buttonRow2(
                 inputIds = c("btnS3_1","btnS3_2"),
                 labels = c("Show S3", "Show S3"),
                 btnStyle = "padding:4px;font-size:150%"
               ),
               
               h4("V4 subgroup"),
               buttonRow3(
                 inputIds = c("btnV4_1","btnV4_2","btnV4_3"),
                 labels = c("Show V4","Show V4", "Show V4"),
                 btnStyle = "padding:4px;font-size:150%"
               ),
               
               h4("D6 subgroup"),
               buttonRow1(
                 inputIds = c("btn_D6"),
                 labels = c("Show D6"),
                 btnStyle = "padding:4px;font-size:150%"
               )
               
             )#box
      ),  #col
      column(
        width = 6,
        box(
          width = NULL,
          h3("Inputs and Products"),
          height = 350,
          htmlOutput("results"),
          tags$head(tags$style("#results{color:red; font-size:20px;
                    font-style:italic; overflow-y:scroll;
                    max-height: 300px; background: ghostwhite;}")),
        ),
        box(width = NULL, height = 60,actionBttn("reset", "Clear Inputs and Products") ),
        
        box(
          width = NULL,
          height = 100,
          title = "Cosets",
          buttonRow2(
            inputIds = c("btnLC", "btnRC"),
            labels = list("Left Cosets", "Right Cosets"),
            btnStyle = "padding:4px;font-size:120%"
          )  #agb
        ),
        
        box(
          width = NULL,
          height = 120,
          title = "Conjugate Subgroup",
          buttonRow2(
            inputIds = c("btnmark", "btnconj"),
            labels = list("Select a", "Generate Subgroup"),
            btnStyle = "padding:4px;font-size:120%"
          ),  
          h4(uiOutput("conjmsg"))
        ),#box
        box(
          width = NULL,
          height = 120,
          title = "Generate a Subgroup",
          buttonRow4(
            inputIds = c("btnmarkgena", "btnmarkgenb", "btngen", "btnclear"),
            labels = list("Generator a", "Generator b","Generate","Clear"),
            btnStyle = "padding:4px;font-size:120%"
          ),  
          h4(uiOutput("genmsg"))
        )#box
        
      )
    ),  #fluid
    
    box(width = "100%",
        height = "100%",
        h3("Multiplication Table"),
        tableOutput("multable")
    )
    
  ) #body
)

source("permutecalc.R")
source("d6calc.R")
#Computes a product as specified by "a" and "b" in vector v
evaluate <- function(v,a,b) {
  result <- "I"
  for (i in 1:length(v)){
    result <- Perm.multiply(result,ifelse(v[i]=="a",a,b))
  }
  return (result)
}
#evaluate(c("a","b"),"(123)","(12)")



#Everything that follows involves something in the UI
server <- function(input, output, session) {
  #Global variables accessible to server()
  N <- 12
  neutral <- "gray90"
  D6DF <- makeS3data(neutral)
  #Elements in the chosen subgroup
  subgroup <- numeric(0)
  #Color for subgroup buttons
  subcolor <- "yellow"
  #Output to display in the text box
  result.list <- ""
  #Result of all multiplications so far
  product <- "I"
  
  
  
  #Variables for cosets and conjugate subgroups
  conjugating <- FALSE
  generating <- 0
  a <-"I"
  gena <- "I"
  genb <- "I"
  
  #read in the dataframe we created
  subgroup_df = read_xlsx("Subgroup+matrix.xlsx")
  output$subgroup_df_o = renderTable({
    req(input$show_subgroups)
    subgroup_df
  })
  
  #read in the coset dataframe we created
  coset_df_l = read_xlsx("COSETS.xlsx", sheet = "Left Cosets", skip = 1)
  colnames(coset_df_l)[1] = "Button"
  
  coset_df_r = read_xlsx("COSETS.xlsx", sheet = "Right Cosets", skip = 1)
  colnames(coset_df_r)[1] = "Button"
  
  
  displayButton = function(i) {
    renderUI({actionButton(D6DF[i,1],D6DF[i,2],
                           style=paste("padding:4px;
                   font-size:180%;background:",D6DF[i,3]))}) 
  }
  #show all the buttons
  showButtons <- function() {
    #show the identity button
    output$ctrlI <- displayButton(1)
    
    #show the order 3 elements (i.e the rotations)
    output$ctrl_R1 <- displayButton(2)                                     
    output$ctrl_R2 <- displayButton(3)
    output$ctrl_R3 <- displayButton(4)
    output$ctrl_R4 <- displayButton(5)
    output$ctrl_R5 <- displayButton(6)
    
    #show the order 2 elements (i.e the flips)
    output$ctrl_r0 <- displayButton(7)                                     
    output$ctrl_r1 <- displayButton(8)                                     
    output$ctrl_r2 <- displayButton(9)
    output$ctrl_r3 <- displayButton(10)
    output$ctrl_r4 <- displayButton(11)
    output$ctrl_r5 <- displayButton(12)
  }
  showButtons()
  
  #Display the multiplication table
  tbl <- outer(D6DF[,2],D6DF[,2],Vectorize(Perm.multiply,c("a","b")))
  colnames(tbl) <- D6DF[,2]
  rownames(tbl) <- D6DF[,2] 
  output$multable <- renderTable(tbl,rownames = TRUE)
  #Multiplies by a specified permutation and displays all calculations so far
  compute.and.show <- function(perm){
    if (conjugating) {
      a <<- perm
      output$conjmsg <- renderUI(paste0("Conjugating by element ",perm,collapse=""))
      conjugating <<- FALSE
      return()
    }
    if (generating==1) {
      gena <<- perm
      output$genmsg <- renderUI(paste0("Generating with element ",gena,collapse=""))
      return()
    }
    if (generating==2) {
      genb <<- perm
      output$genmsg <- 
        renderUI(paste0("Generating with elements ",gena," and ", genb,collapse=""))
      return()
    }
    product <<- Perm.multiply(perm,product)
    line.out <- paste(perm,product,sep = "&emsp;")
    result.list <<- paste(result.list, line.out, sep = "<br/>")
    output$results<-renderUI(HTML(result.list))
  }
  #Marks all elements in a subgroup with a color
  mark.subgroup <- function() {
    for (i in 1:nrow(D6DF)){
      D6DF[i,3] <<- ifelse(i %in% subgroup,subcolor,neutral)
    }
    
  }
  
  #Event handlers for all the element buttons 
  observeEvent(input$btn_R1,{compute.and.show("(123456)")})
  observeEvent(input$btn_R2,{compute.and.show("(135)(246)")})
  observeEvent(input$btn_R3,{compute.and.show("(14)(25)(36)")})
  observeEvent(input$btn_R4,{compute.and.show("(153)(264)")})
  observeEvent(input$btn_R5,{compute.and.show("(165432)")})
  observeEvent(input$btn_r0,{compute.and.show("(26)(35)")})
  observeEvent(input$btn_r1,{compute.and.show("(12)(36)(45)")})
  observeEvent(input$btn_r2,{compute.and.show("(13)(46)")})
  observeEvent(input$btn_r3,{compute.and.show("(14)(23)(56)")})
  observeEvent(input$btn_r4,{compute.and.show("(15)(24)")})
  observeEvent(input$btn_r5,{compute.and.show("(16)(25)(34)")})
  observeEvent(input$btnI,{compute.and.show("I")})
  #The reset button clears the output and reinitializes the product
  observeEvent(input$reset,{
    result.list <<- ""
    product <<- "I"
    output$results<-renderUI(HTML(result.list))
  })
  
  #make a reactiveValues object to store the last button clicked
  rv_list = reactiveValues(
    button_clicked = "btnC1"
  )
  
  #Event handlers for the subgroup buttons
  observeEvent(input$btnC1,{
    rv_list$button_clicked = "btnC1"
    subgroup <<- c(1)
    mark.subgroup()
    showButtons()
  })
  
  observeEvent(input$btnC2_1,{
    rv_list$button_clicked = "btnC2_1"
    subgroup <<- c(1,4)
    mark.subgroup()
    showButtons()
  })
  
  observeEvent(input$btnC2_2,{
    rv_list$button_clicked = "btnC2_2"
    subgroup <<- c(1,7)
    mark.subgroup()
    showButtons()
  })
  
  observeEvent(input$btnC2_3,{
    rv_list$button_clicked = "btnC2_3"
    subgroup <<- c(1,8)
    mark.subgroup()
    showButtons()
  })
  
  observeEvent(input$btnC2_4,{
    rv_list$button_clicked = "btnC2_4"
    subgroup <<- c(1,9)
    mark.subgroup()
    showButtons()
  })
  
  observeEvent(input$btnC2_5,{
    rv_list$button_clicked = "btnC2_5"
    subgroup <<- c(1,10)
    mark.subgroup()
    showButtons()
  })
  
  observeEvent(input$btnC2_6,{
    rv_list$button_clicked = "btnC2_6"
    subgroup <<- c(1,11)
    mark.subgroup()
    showButtons()
  })
  
  observeEvent(input$btnC2_7,{
    rv_list$button_clicked = "btnC2_7"
    subgroup <<- c(1,12)
    mark.subgroup()
    showButtons()
  })
  
  observeEvent(input$btnC3,{
    rv_list$button_clicked = "btnC3"
    subgroup <<- c(3, 5)
    mark.subgroup()
    showButtons()
  })
  
  observeEvent(input$btnC6,{
    rv_list$button_clicked = "btnC6"
    subgroup <<- c(1:6)
    mark.subgroup()
    showButtons()
  })
  
  observeEvent(input$btnS3_1,{
    rv_list$button_clicked = "btnS1_1"
    subgroup <<- c(1,3,5,8,10,12)
    mark.subgroup()
    showButtons()
  })
  
  observeEvent(input$btnS3_2,{
    rv_list$button_clicked = "btnS1_2"
    subgroup <<- c(1,3,5,7,9,11)
    mark.subgroup()
    showButtons()
  })
  
  observeEvent(input$btnV4_1,{
    rv_list$button_clicked = "btnV4_1"
    subgroup <<- c(1,4,8,11)
    mark.subgroup()
    showButtons()
  })
  
  observeEvent(input$btnV4_2,{
    rv_list$button_clicked = "btnV4_2"
    subgroup <<- c(1,4,9,12)
    mark.subgroup()
    showButtons()
  })
  
  observeEvent(input$btnV4_3,{
    rv_list$button_clicked = "btnV4_3"
    subgroup <<- c(1,4,7,10)
    mark.subgroup()
    showButtons()
  })
  
  observeEvent(input$btn_D6,{
    rv_list$button_clicked = "btnD6"
    subgroup <<- c(1,2,3,4,5,6,7,8,9,10,11,12)
    mark.subgroup()
    showButtons()
  })
  
  #we write out own custom logic for the left and right cosets
  observeEvent(input$btnLC, {
    
    button_clicked = rv_list$button_clicked
    color_list_f = coset_df_l %>% filter(Button == button_clicked) %>% select(-Button) %>% t() 
    
    for(i in 1:nrow(D6DF)){
      D6DF[i,3] <<- color_list_f[i]
    }
    
    #update the colors of the buttons
    showButtons()
  })
  
  observeEvent(input$btnRC, {
    
    button_clicked = rv_list$button_clicked
    color_list_f = coset_df_r %>% filter(Button == button_clicked) %>% select(-Button) %>% t() 
    
    for(i in 1:nrow(D6DF)){
      D6DF[i,3] <<- color_list_f[i]
    }
    
    #update the colors of the buttons
    showButtons()
  })
  
  observeEvent(input$btnmark,{
    conjugating <<- TRUE
    output$conjmsg <- renderUI("Click the button for the desired element a")
  })
  observeEvent(input$btnmarkgena,{
    generating <<- 1
    D6DF[,3] <<- rep(neutral,N)
    showButtons()
    output$genmsg <- renderUI("Click the button for generator a")
  })
  observeEvent(input$btnmarkgenb,{
    generating <<- 2
    D6DF[,3] <<- rep(neutral,N)
    showButtons()
    output$genmsg <- renderUI("Click the button for generator b")
  })
  #Generate random sequences of generators.
  #If we generate more than half the group, it's the entire group
  #This algorithm could turn out to be inefficient,and in principle it can fail
  observeEvent(input$btngen,{
    subgroup <<-  numeric(0)
    for (j in 1:(4*N)) {
      v <- sample(c("a","b"),sample(7:10,1),replace = TRUE)
      element <- evaluate(v,gena,genb)
      k <- which(D6DF[,2] == element)[1]
      if(!(k %in% subgroup)){
        subgroup <<- c(subgroup,k)
        D6DF[k,3] <<- subcolor
      }
      #If subgroup has more than N/2 elements, it's the entire group
      if (length(subgroup) > N/2){
        subgroup <<- 1:N
        break
      } 
    }  
    mark.subgroup()
    showButtons()
    output$genmsg <- 
      renderUI(paste0("The subgroup generated by ",gena," and ", genb," is now yellow"))
  })
  observeEvent(input$btnclear,{
    subgroup <<- numeric(0)
    generating <<- 0
    gena <<- "I"
    genb <<- "I"
    mark.subgroup()
    showButtons()
    output$genmsg <- renderUI("")
  })
  observeEvent(input$btnconj,{
    aInv <- Perm.inverse(a)
    D6DF[,3] <<- rep(neutral,N)
    for (j in 1:N) {
      if (j %in% subgroup){
        element <- Perm.conjugate(a,D6DF[j,2])
        k <- which(D6DF[,2] == element)[1]
        D6DF[k,3] <<- "pink"
      }
    }
    showButtons()
    output$conjmsg <- renderUI(paste0("The subgroup ",a,"H",aInv," is now pink"))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
