
library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyWidgets)
library(shinybusy)
library(shinyalert)
library(shinycssloaders)
library(ggplot2 )
library(wesanderson)
library(plotly)

source("heuristic.R")
source("hybrid_ga.R")
source("hybrid_sa.R")

# List of authors for benchmark instances
authors <- c("Bengtsson", "Jakobs", "Poshyanonda & Dagli", "Ratanapan & Dagli", "Ratanapan & Dagli", 
             "Babu", "Burke & Kendall",  "Hifi", "Hopper C", "Hopper TN", "Wang & Valenzuela", "Burke, Kendall & Whitwell", 
             "Pinto & Oliveira", "Bortfeldt & Gehring", "Imahori & Yagiura", "Leung & Zhang")

# Year paper and benchmark instance was published
year_published <- c(1982, 1996, 1997, 1997, 1998, 1999, 1999, 1999, 2000, 2000, 2001, 2004, 
                    2005, 2006, 2010, 2011)

# Path to benchmark instances
instances_path <- "benchmark_instances"

# Papers relating to benchmark instances
papers <- list.files(instances_path)
personal <- papers[length(papers)]
names(personal) <- "Personal"
papers <- papers[-length(papers)]
names(papers) <- authors

visualise_instance <- function(data, file){
  lay <- data.frame(bl.x = 0, bl.y = 0, tr.x = data[1, "width"], tr.y = data[1, "height"], item.id = data[1, "id"])
  
  n <- nrow(data)
  for(i in 2:n){
    temp <- c(bl.x = lay$tr.x[i-1] + 1,
              bl.y = 0,
              tr.x = lay$tr.x[i-1] + 1 + data[i, "width"],
              tr.y = data[i, "height"],
              item.id = data[i, "id"])
    
    lay <- rbind(lay, temp)
  }
  center.x <- lay$bl.x + (lay$tr.x - lay$bl.x)/2
  center.y <- lay$bl.y + (lay$tr.y - lay$bl.y)/2
  
  pallete <- wes_palette("Zissou1", n, type = "continuous")
  
  p <- ggplot(lay, aes(xmin = bl.x, xmax = tr.x, ymin = bl.y, ymax = tr.y, fill = item.id,
                          text = paste("Item Id: ", item.id, " \n",
                                       "Item Height: ", tr.y - bl.y, " \n", 
                                       "Item Width: ", tr.x - bl.x, sep = ""))) + 
    geom_rect(aes(color = item.id), alpha = 0.3, show.legend = FALSE) + 
    scale_fill_gradientn(colours = pallete) + 
    annotate("text", x = center.x, y = center.y, label = lay$item.id) + 
    labs(y = "Height", title = file) + 
    theme_classic() + 
    theme(plot.background = element_rect(fill = "#fafafa"),
          panel.background = element_rect(fill = "#fafafa"),
          plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
          axis.title = element_text(size = 12))
  
  p <- ggplotly(p, tooltip = "text")
  
  return(p)
  
}

vis_output <- function(layout, heuristic, meta){
  
  if(meta == "None Selected"){
    title = paste("Layout:", heuristic, "Heuristic")
  } else{
    if(heuristic == "Improved Best-Fit"){
      title = paste("Layout:", meta, "with IBF", "Heuristic")
    } else if(heuristic == "Constructive"){
      title = paste("Layout:", meta, "with CH")
    }
  }
  
  center.x <- layout$bl.x + (layout$tr.x - layout$bl.x)/2
  center.y <- layout$bl.y + (layout$tr.y - layout$bl.y)/2
  
  pallete <- wes_palette("Zissou1", nrow(layout), type = "continuous")
  
  p <- ggplot(layout, aes(xmin = bl.x, xmax = tr.x, ymin = bl.y, ymax = tr.y, fill = item.id,
                          text = paste("Item Id: ", item.id, " \n",
                                       "Item Height: ", tr.y - bl.y, " \n", 
                                       "Item Width: ", tr.x - bl.x, sep = ""))) + 
    geom_rect(aes(color = item.id), alpha = 0.3, show.legend = FALSE) + 
    scale_fill_gradientn(colours = pallete) + 
    annotate("text", x = center.x, y = center.y, label = layout$item.id) + 
    labs(x = "Instance Width", y = "Height", title = title) + 
    theme_classic() + 
    theme(plot.background = element_rect(fill = "grey99"),
          panel.background = element_rect(fill = "grey99"),
          plot.title = element_text(size = 14, face = "bold", hjust = 0),
          axis.title = element_text(size = 12))
  
  p <- ggplotly(p, tooltip = "text")
  
  return(p)
}

# Sorting function
sort_fun <- function(items, sorting){
  method = sorting[[1]]
  decreasing = sorting[[2]]
  if(decreasing){
    if(method == "perimeter" | method == 1){
      return(items[order(items$width + items$height, decreasing = decreasing), ])
    } else if(method == "area" | method == 2){
      return(items[order(items$width*items$height, decreasing = decreasing), ])
    } else if(method == "width" | method == 3){
      return(items[order(items$width, decreasing = decreasing), ])
    } else if(method == "height" | method == 4){
      return(items[order(items$height, decreasing = decreasing), ])
    }
  } else {
    if(method == "perimeter" | method == 1){
      return(items[order(items$width + items$height, decreasing = decreasing), ])
    } else if(method == "area" | method == 2){
      return(items[order(items$width*items$height, decreasing = decreasing), ])
    } else if(method == "width" | method == 3){
      return(items[order(items$width, decreasing = decreasing), ])
    } else if(method == "height" | method == 4){
      return(items[order(items$height, decreasing = decreasing), ])
    }
  }
}

sort_to_words <- function(sorting){
  if(sorting == "dec.W"){
    return("Decreasing Width")
  } else if(sorting == "dec.H"){
    return("Decreasing Height")
  } else if(sorting == "dec.A"){
    return("Decreasing Area")
  } else if(sorting == "inc.W"){
    return("Increasing Width")
  } else if(sorting == "inc.H"){
    return("Increasing Height")
  } else if(sorting == "inc.A"){
    return("Increasing Area")
  } else {
    return("Default Order")
  }
}

stop_to_words <- function(criteria){
  if(criteria == "time"){
    return("Time limit reached")
  } else if(criteria == "optimal"){
    return("Optimal height found")
  } else if(criteria == "generations"){
    return("Maximum number of generations reached")
  } else if(criteria == "temperature"){
    return("Temperature lower bound reached")
  } else if(criteria == "iterations"){
    return("Maximum number of iterations reached")
  } else {
    return("NULL")
  }
}











