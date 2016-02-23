library(shiny)
library(abind)

shiftMatrix <- function(current.state, row.shift, col.shift){
  n.row.min <- nrow(current.state) + 1
  n.row.max <- nrow(current.state)*2
  n.col.min <- ncol(current.state) + 1
  n.col.max <- ncol(current.state)*2
  tri.matrix <- cbind(current.state, current.state, current.state)
  tri.matrix <- rbind(tri.matrix, tri.matrix, tri.matrix)
  new.matrix <- tri.matrix[(n.row.min+row.shift):(n.row.max+row.shift),(n.col.min+col.shift):(n.col.max+col.shift)]
  return(new.matrix)
}

nextState <- function(current.state){
  newmatrix.list <- lapply(shift.indices, FUN = function(x){shiftMatrix(current.state,x[1],x[2])})
  new.array <- do.call(abind,c(newmatrix.list,list(along=3)))
  neighbors <- apply(new.array, c(1,2), sum)
  
  new.matrix <- matrix(0, nrow=nrow(current.state), ncol=ncol(current.state))
  new.matrix[neighbors%in%c(2,3) & current.state==1] <- 1
  new.matrix[neighbors%in%3 & current.state==0] <- 1
  
  return(new.matrix)
}

plotState <- function(array, step){
  par(mar=c(0,0,0,0))
  plot(0,0,type="n",xlim=c(0,1),ylim=c(0,1),frame=F,xlab="",ylab="",xaxt="n",yaxt="n",xaxs="i",yaxs="i")
  rasterImage(1-array[,,step],0,0,1,1,interpolate = F)
}

n.row <- 50
n.col <- 50
n <- 100
prob <- 0.25
shift.indices <- cbind(c(1,1,1,0,0,-1,-1,-1),c(1,0,-1,1,-1,1,0,-1))
shift.indices <- as.list(as.data.frame(t(shift.indices)))

newArray <- function(prob=0.1){ 
  raster.array <- array(NA, dim=c(n.row,n.col,n))
  raster.array[,,1] <- matrix(rbinom(n.row*n.col,1,prob=prob))
  
  for(ii in 2:n){
    raster.array[,,ii] <- nextState(raster.array[,,ii-1])
  }
  return(raster.array)
}

shinyServer(function(input,output){
  output$golPlot <- renderPlot({
    plotState(new.array(), input$step)
  })
  
  new.array <- reactive({
    input$Refresh
    newArray(isolate(input$prob))
  })
})