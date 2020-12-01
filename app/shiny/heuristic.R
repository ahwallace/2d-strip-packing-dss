# Heuristic Packing Function
# WLLAID002/WXXLES001
# 24/11/2020

heuristic <- function(items, strip_width, score_fun, init_sort = NA){
  # items = dataframe containing id, height, width of items
  # strip_width = width of strip
  # score_fun = function to score items for packing (must return a vector of length two with 
  #             the item score and the side to pack the item (1 = left, 0 = right) - in that order)
  # init_sort = character string of giving the initializing sorting order
  #             possible arguments: inc.H, dec.H, inc.W, dec.W, inc.A, dec.A
  
  update_spaces <- function(spaces, item, side){
    
    lowestLeftIndex <- find_lowest_left_index(spaces)
    lowestLeft <- spaces[lowestLeftIndex, ]
    
    h1 <- Inf
    h2 <- Inf
    
    h1.temp <- ifelse(lowestLeftIndex == 1, h1, lowestLeft$h1 - item$height)
    h2.temp <- ifelse(lowestLeftIndex == nrow(spaces), h2, lowestLeft$h2 - item$height)
    
    s1 <- c(lowestLeft$bl.x,
            lowestLeft$bl.y,
            lowestLeft$width - item$width,
            lowestLeft$h1,
            item$height)
    
    s2 <- c(lowestLeft$bl.x + item$width,
            lowestLeft$bl.y,
            lowestLeft$width - item$width,
            item$height,
            lowestLeft$h2)
    
    if(side == 1){
      
      if(item$width == lowestLeft$width){
        
        if(item$height == lowestLeft$h1 & item$height == lowestLeft$h2){
          
          s1 <- c(spaces$bl.x[lowestLeftIndex-1],
                  spaces$bl.y[lowestLeftIndex-1],
                  sum(spaces$width[c(lowestLeftIndex-1, lowestLeftIndex, lowestLeftIndex+1)]),
                  spaces$h1[lowestLeftIndex-1],
                  spaces$h2[lowestLeftIndex+1])
          spaces[lowestLeftIndex-1, ] <- s1
          spaces <- spaces[-c(lowestLeftIndex, lowestLeftIndex+1), ]
          
        } else if(item$height == lowestLeft$h1 & item$height > lowestLeft$h2){
          
          s1 <- c(spaces$bl.x[lowestLeftIndex-1],
                  spaces$bl.y[lowestLeftIndex-1],
                  spaces$width[lowestLeftIndex-1] + lowestLeft$width,
                  spaces$h1[lowestLeftIndex-1],
                  h2)
          spaces[lowestLeftIndex+1, "h1"] <- lowestLeft$bl.y + item$height - spaces$bl.y[lowestLeftIndex+1]
          spaces[lowestLeftIndex-1, ] <-  s1
          spaces <- spaces[-lowestLeftIndex, ]
          
        } else if(item$height == lowestLeft$h1 & item$height < lowestLeft$h2){
          
          s1 <- c(spaces$bl.x[lowestLeftIndex-1],
                  spaces$bl.y[lowestLeftIndex-1],
                  spaces$width[lowestLeftIndex-1] + lowestLeft$width,
                  spaces$h1[lowestLeftIndex-1],
                  h2.temp)
          spaces[lowestLeftIndex-1, ] <- s1
          spaces <- spaces[-lowestLeftIndex, ]
          
        } else if(item$height == lowestLeft$h2 & item$height > lowestLeft$h1){
          
          s1 <- c(lowestLeft$bl.x,
                  spaces$bl.y[lowestLeftIndex+1],
                  lowestLeft$width + spaces$width[lowestLeftIndex+1],
                  h1,
                  spaces$h2[lowestLeftIndex+1])
          spaces[lowestLeftIndex-1, "h2"] <- lowestLeft$bl.y + item$height - spaces$bl.y[lowestLeftIndex-1]
          spaces[lowestLeftIndex, ] <- s1
          spaces <- spaces[-c(lowestLeftIndex+1), ]
          
        } else if(item$height == lowestLeft$h2 & item$height < lowestLeft$h1){
          
          s1 <- c(lowestLeft$bl.x,
                  lowestLeft$bl.y + item$height,
                  lowestLeft$width + spaces$width[lowestLeftIndex+1],
                  h1.temp,
                  spaces$h2[lowestLeftIndex+1])
          spaces[lowestLeftIndex, ] <- s1
          spaces <- spaces[-c(lowestLeftIndex+1), ]
          
        } else if(item$height > lowestLeft$h1 & item$height > lowestLeft$h2){
          
          s1 <- c(lowestLeft$bl.x,
                  lowestLeft$bl.y + item$height,
                  lowestLeft$width,
                  h1,
                  h2)
          spaces[lowestLeftIndex-1, "h2"] <- lowestLeft$bl.y + item$height - spaces$bl.y[lowestLeftIndex-1]
          spaces[lowestLeftIndex+1, "h1"] <- lowestLeft$bl.y + item$height - spaces$bl.y[lowestLeftIndex+1]
          spaces[lowestLeftIndex, ] <- s1
          
        } else if(item$height > lowestLeft$h1 & item$height < lowestLeft$h2){
          
          s1 <- c(lowestLeft$bl.x,
                  lowestLeft$bl.y + item$height,
                  lowestLeft$width,
                  h1,
                  h2.temp)
          spaces[lowestLeftIndex-1, "h2"] <- lowestLeft$bl.y + item$height - spaces$bl.y[lowestLeftIndex-1]
          spaces[lowestLeftIndex, ] <- s1
          
        } else if(item$height < lowestLeft$h1 & item$height > lowestLeft$h2){
          
          s1 <- c(lowestLeft$bl.x,
                  lowestLeft$bl.y + item$height,
                  lowestLeft$width,
                  h1.temp,
                  h2)
          spaces[lowestLeftIndex+1, "h1"] <- lowestLeft$bl.y + item$height - spaces$bl.y[lowestLeftIndex+1]
          spaces[lowestLeftIndex, ] <- s1
          
        } else if(item$height < lowestLeft$h1 & item$height < lowestLeft$h2){
          
          s1 <- c(lowestLeft$bl.x,
                  lowestLeft$bl.y + item$height,
                  lowestLeft$width,
                  h1.temp,
                  h2.temp)
          spaces[lowestLeftIndex, ] <- s1
          
        }
        
      } else if(item$width != lowestLeft$width){
        
        if(item$height == lowestLeft$h1){
          
          s1 <- c(spaces$bl.x[lowestLeftIndex-1],
                  spaces$bl.y[lowestLeftIndex-1],
                  spaces$width[lowestLeftIndex-1] + item$width,
                  spaces$h1[lowestLeftIndex-1],
                  h2)
          spaces[lowestLeftIndex-1, ] <- s1
          spaces[lowestLeftIndex, ] <- s2
          
        } else if(item$height > lowestLeft$h1){
          
          s1 <- c(lowestLeft$bl.x,
                  lowestLeft$bl.y + item$height,
                  item$width,
                  h1,
                  h2)
          spaces[lowestLeftIndex-1, "h2"] <- lowestLeft$bl.y + item$height - spaces$bl.y[lowestLeftIndex-1]
          spaces[lowestLeftIndex, ] <- s1
          
          if(lowestLeftIndex == nrow(spaces)){
            spaces <- rbind(spaces, s2)
          } else{
            spaces <- rbind(spaces[1:lowestLeftIndex, ], s2, spaces[(lowestLeftIndex+1):nrow(spaces), ])
          }
          
        } else if(item$height < lowestLeft$h1){
          
          s1 <- c(lowestLeft$bl.x,
                  lowestLeft$bl.y + item$height,
                  item$width,
                  h1.temp,
                  h2)
          spaces[lowestLeftIndex, ] <- s1
          
          if(nrow(spaces) == 1 | lowestLeftIndex == nrow(spaces)){
            spaces <- rbind(spaces, s2)
          } else if(nrow(spaces) != 1 & lowestLeftIndex != nrow(spaces)){
            spaces <- rbind(spaces[1:lowestLeftIndex, ], s2, spaces[(lowestLeftIndex+1):nrow(spaces), ])
          }
          
        }
        
      }
      
    } else if(side == 0){
      
      if(item$height == lowestLeft$h2){
        
        s2 <- c(lowestLeft$bl.x + lowestLeft$width - item$width,
                spaces$bl.y[lowestLeftIndex+1],
                item$width + spaces$width[lowestLeftIndex+1],
                h1,
                spaces$h2[lowestLeftIndex+1])
        spaces[lowestLeftIndex, ] <- s1
        spaces[lowestLeftIndex+1, ] <- s2
        
      } else if(item$height > lowestLeft$h2){
        
        s2 <- c(lowestLeft$bl.x + lowestLeft$width - item$width,
                lowestLeft$bl.y + item$height,
                item$width,
                h1,
                h2)
        spaces[lowestLeftIndex, ] <- s1
        spaces[lowestLeftIndex+1, "h1"] <- lowestLeft$bl.y + item$height - spaces$bl.y[lowestLeftIndex+1]
        spaces <- rbind(spaces[1:lowestLeftIndex, ], s2, spaces[(lowestLeftIndex+1):nrow(spaces), ])
        
      } else if(item$height < lowestLeft$h2){
        
        s2 <- c(lowestLeft$bl.x + lowestLeft$width - item$width,
                lowestLeft$bl.y + item$height,
                item$width,
                h1,
                h2.temp)
        spaces[lowestLeftIndex, ] <- s1
        if(lowestLeftIndex == nrow(spaces)){
          spaces <- rbind(spaces, s2)
        } else if(lowestLeftIndex != nrow(spaces)){
          spaces <- rbind(spaces[1:lowestLeftIndex, ], s2, spaces[(lowestLeftIndex+1):nrow(spaces), ])
        }
        
      }
      
    }
    return(spaces)
  }
  
  no_pack_spaces_update <- function(spaces, strip_width){
    
    lowestLeftIndex <- find_lowest_left_index(spaces)
    lowestLeft <- spaces[lowestLeftIndex, ]
    
    h1 <- Inf
    h2 <- Inf
    
    if(nrow(spaces) == 2){
      
      s1 <- c(0, 
              lowestLeft$bl.y + min(c(lowestLeft$h1, lowestLeft$h2)),
              strip_width,
              h1, 
              h2)
      spaces[1, ] <- s1
      spaces <- spaces[-2, ]
      
    } else if(nrow(spaces) != 2){
      
      if(lowestLeft$h1 == lowestLeft$h2){
        
        s1 <- c(spaces$bl.x[lowestLeftIndex-1],
                spaces$bl.y[lowestLeftIndex-1],
                sum(spaces$width[c(lowestLeftIndex-1, lowestLeftIndex, lowestLeftIndex+1)]),
                spaces$h1[lowestLeftIndex-1],
                spaces$h2[lowestLeftIndex+1])
        spaces[lowestLeftIndex-1, ] <- s1
        spaces <- spaces[-c(lowestLeftIndex, lowestLeftIndex+1), ]
        
      } else if(lowestLeft$h1 < lowestLeft$h2){
        
        h2.temp <- ifelse(lowestLeftIndex == nrow(spaces), h2, lowestLeft$h2 - lowestLeft$h1)
        s1 <- c(spaces$bl.x[lowestLeftIndex-1],
                spaces$bl.y[lowestLeftIndex-1],
                spaces$width[lowestLeftIndex-1] + lowestLeft$width,
                spaces$h1[lowestLeftIndex-1],
                h2.temp)
        spaces[lowestLeftIndex-1, ] <- s1
        spaces <- spaces[-lowestLeftIndex, ]
        
      } else if(lowestLeft$h1 > lowestLeft$h2){
        
        h1.temp <- ifelse(lowestLeftIndex == 1, h1, lowestLeft$h1 - lowestLeft$h2)
        s1 <- c(lowestLeft$bl.x, 
                spaces$bl.y[lowestLeftIndex+1],
                lowestLeft$width + spaces$width[lowestLeftIndex+1],
                h1.temp,
                spaces$h2[lowestLeftIndex+1])
        spaces[lowestLeftIndex, ] <- s1
        spaces <- spaces[-c(lowestLeftIndex+1), ]
      }
    }
  }
  
  find_lowest_left_index <- function(spaces){
    lowest <- spaces[which(spaces$bl.y == min(spaces$bl.y)), ]
    return(which(spaces$bl.x == min(lowest[, 1])))
  }
  
  if(!is.na(init_sort)){
    ord <- substr(init_sort, 1, 3)
    arr <- substr(init_sort, 5, 5)
    if (ord == "inc")
      ifelse(arr=="W", items <- items[order(items$width), ],
             ifelse(arr=="H", items <- items[order(items$height), ],
                    items <- items[order(items$width*items$height), ]))
    else
      ifelse(arr=="W", items <- items[order(items$width, decreasing = TRUE), ],
             ifelse(arr=="H", items <- items[order(items$height, decreasing = TRUE), ],
                    items <- items[order(items$width*items$height, decreasing = TRUE), ]))
  }
  
  scoreFunction <- match.fun(score_fun)
  
  n <- nrow(items)
  
  layout <- data.frame(bl.x = NA, bl.y = NA, tr.x = NA, tr.y = NA, item.id = NA)
  
  # available spaces from left to right
  spaces <- data.frame(bl.x = 0, bl.y = 0, width = strip_width, h1 = Inf, h2 = Inf)
  
  packingOrder <- c()
  
  toBePacked <- items$id
  
  packed <- 0
  while(packed < n){
    
    lowestLeftIndex <- find_lowest_left_index(spaces)
    lowestLeft <- spaces[lowestLeftIndex, ]
    
    itemsToPack <- items[which(items$id %in% toBePacked), ]
    
    if(any(itemsToPack$width <= lowestLeft$width)){
      
      # id of items with width less than or equal to lowest left width
      possibleFit <- itemsToPack$id[which(itemsToPack$width <= lowestLeft$width)]
      
      itemsPossibleFit <- items[which(items$id %in% possibleFit), ]
      
      itemScores <- apply(itemsPossibleFit, 1, scoreFunction, lowestLeft)     
      itemScores <- data.frame(score = itemScores[1, ], side = itemScores[2, ], id = itemScores[3, ])
      
      # id of item to pack
      itemToPack <- itemScores$id[which.max(itemScores[, 1])]
      
      # side to pack item
      sideToPack <- itemScores$side[which.max(itemScores[, 1])]
      
      packed <- packed + 1
      
      # remove item from list of items to be packed
      toBePacked <- toBePacked[-which(toBePacked == itemToPack)]
      
      # add item to packing order
      packingOrder[packed] <- itemToPack
      
      # item to pack
      item <- items[which(items$id == itemToPack), ]
      
      # Update layout for graphical display
      if(sideToPack == 1){
        layout[packed, ] <- c(lowestLeft$bl.x, 
                              lowestLeft$bl.y, 
                              lowestLeft$bl.x + item$width, 
                              lowestLeft$bl.y + item$height,
                              item$id)
      } else if(sideToPack == 0){
        layout[packed, ] <- c(lowestLeft$bl.x + (lowestLeft$width - item$width),
                              lowestLeft$bl.y, lowestLeft$bl.x + lowestLeft$width, 
                              lowestLeft$bl.y + item$height,
                              item$id)
      }
      
      # Update spaces
      spaces <- update_spaces(spaces, item, sideToPack)
      
    } else{
      # update space if no item fits (incorrect atm)
      spaces <- no_pack_spaces_update(spaces, strip_width)
      
    }
  }
  
  return(list(layout = layout, packing_order = packingOrder, height = max(layout$tr.y)))
}

# Constructive heuristic score
ch_score <- function(item, space){
  h1 <- space$h1
  h2 <- space$h2
  spaceWidth <- space$width
  
  itemId <- item["id"]
  itemWidth <- item["width"]
  itemHeight <- item["height"]
  
  if(h1 >= h2){
    if(itemWidth == spaceWidth){
      if(itemHeight == h1){
        return(c(4, 1, itemId))
      } else if(itemHeight > h1){
        return(c(3, 1, itemId))
      } else{
        return(c(2, 1, itemId))
      }
    } else {
      if(itemHeight == h1){
        return(c(1, 1, itemId))
      } else{
        return(c(0, 1, itemId))
      }
    }
  } else {
    if(itemWidth == spaceWidth){
      if(itemHeight == h2){
        return(c(4, 1, itemId))
      } else if(itemHeight > h2){
        return(c(3, 1, itemId))
      } else{
        return(c(2, 1, itemId))
      }
    } else {
      if(itemHeight == h2){
        return(c(1, 0, itemId))
      } else{
        return(c(0, 0, itemId))
      }
    }
  }
}

# Improved constructive Heuristic score
improved_score <- function(item, space){
  
  h1 <- space$h1
  h2 <- space$h2
  spaceWidth <- space$width
  
  itemId <- item["id"]
  itemWidth <- item["width"] 
  itemHeight <- item["height"]
  
  if(itemWidth == spaceWidth){
    
    if(h1 == itemHeight & h2 == itemHeight){
      return(c(5, 1, itemId))
    } else if(h1 == itemHeight | h2 == itemHeight){
      return(c(4, 1, itemId))
    } else if(h1 < itemHeight & h2 < itemHeight){
      return(c(3, 1, itemId))
    } else if(h1 > itemHeight | h2 > itemHeight){
      return(c(2, 1, itemId))
    }
    
  } else if(itemWidth != spaceWidth){
    
    if(h1 == itemHeight){
      return(c(1, 1, itemId))
    } else if(h2 == itemHeight){
      return(c(1, 0, itemId))
    }else if(h1 != itemHeight & h2 != itemHeight){
      if(h1 >= h2){
        return(c(0, 1, itemId))
      } else{
        return(c(0, 0, itemId))
      }
    }
  }
}

# Improved Best-Fit heuristic score
bf_score <- function(item, space){
  
  s.w <- space[3]; s.lh <- space[4]; s.rh <- space[5]
  
  itemId <- item["id"]
  itemWidth <- item["width"]
  itemHeight <- item["height"]
  
  #### rectangle with fitness 3 ####
  # the rectangle is of perfect fit to all 3 sides
  
  
  if(s.lh == s.rh){
    if(itemHeight == s.lh & itemWidth == s.w)
      return (c(3,1,itemId))
  }
  ####
  
  #### rectangle with fitness 2 ####
  # s.w = r.w AND one of s.lh = r.h or s.rh = r.h
  if(s.w == itemWidth){
    if(s.lh == itemHeight) 
      return (c(2,1,itemId))
    if (s.rh == itemHeight)
      return (c(2,1,itemId))
  }
  ####
  
  #### rectangle with fitness 1 ####
  # one of (s.w = r.w,  s.lh = r.h, s.rh = r.h)
  # the last two must be checked for r.w <= s.w
  if(s.w == itemWidth)
    return (c(1,1,itemId))
  
  if(s.w >= itemWidth){
    if(s.lh == itemHeight)
      return(c(1,1,itemId))
    if(s.rh == itemHeight)
      return(c(1,0,itemId))
  }
  
  #### rectangle with fitness 0 ####
  if (s.lh >= s.rh | s.rh == Inf){
    return (c(0,1,itemId))
  }else{
    return (c(0,0,itemId))
  }
}
