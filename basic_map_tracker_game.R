
map <- data.frame("label" = c("Player", "Enemy A", "Enemy B", "Exit"), 
                  "north" = c(0, 7, -3, 9),  "east" = c(0, 7, -3, -9), 
                  "type" = c("Player", "Enemy", "Enemy", "Exit"))

### Travel ####
travel <- function() {
  move_range <- list("w" = c("north" =  1), "a" = c("east" = -1), 
                     "d" = c("east" = 1), "s" = c("north" =  -1))
  
  move <- readline(prompt="go north (w), west (a), east(d), or south(s): ")
  
  if (move %in% names(move_range)) {
    direction <- unname(move_range[[move]]) #movement: +1 or -1
    map[1, names(move_range[[move]])] <- map[1, names(move_range[[move]])] +  direction #adds movement to proper column (north or east) of the map
    
    ###enemy movement
    for (i in c(1:nrow(map))) {
      if (map$type[i] == "Enemy") { #enemy AI
        distance <- sqrt( (map[1, "north"] - map[i, "north"])^2 + (map[1, "east"] - map[i, "east"])^2)
        #computes smallest distance btw player and enemy
    
        if (distance <= 6) { #if distance between enemy and player is small enough
          
          random_direction <- sample(c("north", "east"), 1) #pick one direction
          enemy_move <- c(-1, 1)[1 + (map[1, random_direction] >= map[i, random_direction])] 
          #enemy moves in direction closest to player, based on who is more north/east
          map[2, random_direction] <- map[i, random_direction] +  enemy_move 
          
          } else { #otherwise random movement
            
            random_direction <- sample(c("north", "east"), 1)
            random_move <- sample(c(-1, 0, 1), 1)
            map[i, random_direction] <- map[i, random_direction] +  random_move 
    }
      }
    }
    

    
    } else {
    return(print("Wrong input."))
  }
  
  assign("map", map, envir = globalenv()) #saves results in global environment
  
  plot(map$east, map$north, #updates location map
       xlab = "West                  East",  
       ylab = "South                  North", main = "",
       xlim = c(-10, 10),  ylim = c(-10, 10)) + 
    grid(10, 10, col = "lightgray", lty = "dotted", lwd = par("lwd"), equilogs = TRUE) +
    text(map$east, map$north,  map$label, cex=0.75, pos=4, col="red")
  return(location)
}
