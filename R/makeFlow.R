requireNamespace("dplyr")
requireNamespace("RColorBrewer")
requireNamespace("grDevices")
requireNamespace("magrittr")
utils::suppressForeignCheck(names = "n")
utils::globalVariables(c("n","%>%","select_","group_by_","summarise","mutate"))

makeFlow <-
function(data, classFields, rotate = F, gatecolors= NA, minVerticalBtwnGates = .1 ,connectingAlpha = .5, bg="white",plotTitle="",titleAdj=.5, txtColor="black", distanceBtwnGates=50, gateWidth=7 , gateBorder = NA,labels=T, fieldLabels=NA , showPercentages=T,showConnectPercentages=F, percentTextColor="black", showCounts=T,countTextColor="black"){
  gateFields <- classFields
  
  
  FieldCats <- c() #filled in next
  unique_cats <- c() #filled in next
  
  #count the number of unique categories are found across all classFields:
  for(i in 1:length(gateFields)){
    temp <- dplyr::select_(.data=data,gateFields[i])
    temp <- dplyr::group_by_(.data = temp,gateFields[i])
    temp <- dplyr::summarise(.data = temp)
    FieldCats <- c(FieldCats,nrow(temp))
    unique_cats <- c(unique_cats,as.character(temp[[1]]))
  }
  
  unique_cats <- unique(as.character(unlist(unique_cats))) #final step for unique categories
  
  #Determine how the gates will be colored.
    if(length(unique_cats)<=length(gatecolors)){ ##if we have more than/enough colors, use however many we need:
        gatecolors <- ifelse(is.na(gatecolors),RColorBrewer::brewer.pal("Set3", n=1), gatecolors[1:length(gatecolors)] )
        
        if(!all(gatecolors %in% grDevices::colors())){
          gatecolors <- RColorBrewer::brewer.pal("Set3",n = length(unique_cats))
          warning(paste0("One or more of the provided 'gatecolors' is invalid. See colors() for valid colors. Using palette 'Set3' instead."))
        }
        
    } else{                                      #if we don't have enough colors, use preset palette "Set3" & alert user:
        gatecolors <- RColorBrewer::brewer.pal("Set3",n = length(unique_cats))
        warning(paste0("Incorrect number of colors provided. Required: ",length(unique_cats),". Using palette 'Set3' instead."))
    }
  
  #Using our colors and unique categories, make a reference table for later:
  colorChart <<- as.data.frame(cbind(unique_cats,gatecolors),stringsAsFactors = F)
  names(colorChart) <- c("cat","colors")
  
  
  #initialize a blank plot:
  n_cats <- max(FieldCats)
  
  plot_hgt <- 1 + minVerticalBtwnGates*max(FieldCats)
  plot_rt <- (distanceBtwnGates+gateWidth)*(length(gateFields)+1)
  plot_lft<- -(distanceBtwnGates+gateWidth)
  graphics::par(bg= bg)
  
  #dimensions of the plot will vary depending on if user specifies rotate=TRUE
  if(rotate==F){
    graphics::plot(NA,type="n",xlim = c(plot_lft,plot_rt),ylim=c(-.2,plot_hgt+.15),axes = F,xlab = "",ylab = "")
    graphics::title(plotTitle,col.main=txtColor,adj=titleAdj)
  }else{
    graphics::plot(NA,type="n",xlim= c(plot_lft-50, plot_rt),ylim=c(-.75,plot_hgt+.4),axes=F,xlab="",ylab="")
    graphics::text(srt=90,font=2, x = -30,y= plot_hgt/2,labels = plotTitle,col = txtColor,cex=1.5)
    
  }
  
  ##The following loop runs through line 263
  for(field in 1:(length(gateFields)-1)){
    
   #Get summaries of starts, connections, and end gates
    #start gates:
    smry1 <- data %>% select_(gateFields[field]) %>% group_by_(gateFields[field]) %>% summarise(n=n()) %>% mutate(freq=n/sum(n))
    smry1[[1]] <- as.character(smry1[[1]])
    
    # connections:
    smry2 <- data %>% select_(gateFields[field],gateFields[field+1]) %>% group_by_(gateFields[field],gateFields[field+1]) %>% summarise(n=n()) %>% mutate(freq=n/sum(n))
    smry2[[1]] <- as.character(smry2[[1]])
    smry2[[2]] <- as.character(smry2[[2]])
    
    #first connection end gates:
    smry3 <- data %>% select_(gateFields[field+1]) %>% group_by_(gateFields[field+1]) %>% summarise(n=n()) %>% mutate(freq=n/sum(n))
    smry3[[1]] <- as.character(smry3[[1]])
    
    ##### Our approach to plotting can change slightly in this function
    ##### It may be more efficient to plot gate + connections, then loop back to add next gate + connections
    ##### So left to right, what you see is the next item to get added
    
    
    #Start by plotting the leading gates. Continues through line 121:
    
    #We can keep track of some important plotting features with:
    hgt_strt1<- ifelse(nrow(smry1)==1, (plot_hgt-1)/2, 0) #what is the lowest y-coordinate of the gates
    dist_btwn1 <- (plot_hgt - 1) / (nrow(smry1)-1)  #how much vertical distance between gates
    centers1 <- c()   #center (vertical) of a gate
    cat1_tops <- c()  #highest point of a gate
    
    #determine which colors will be used in the gates:
    strt_colors <- colorChart$colors[which(colorChart$cat %in% smry1[[1]])]
    
    #define the x-coordinate for the left bound of the gate. Diagram will start at 0:
    lftgate <- ifelse(field==1, 0, (field-1)*(distanceBtwnGates+gateWidth))
    
    #using the above specifications for gates and colors, plot the stacked gates of categories from classFields[field]
    #NOTE: going from nrow:1, we will get a plot that represents working top-down in our summary outputs!!
    for(q in nrow(smry1):1){
      graphics::rect(xleft = lftgate,xright = lftgate+gateWidth,ybottom = hgt_strt1,ytop = hgt_strt1+smry1$freq[q], col = strt_colors[which(colorChart$cat[which(colorChart$colors%in%strt_colors)]==smry1[[1]][q])],border = gateBorder)
      centers1 <- c(centers1, (hgt_strt1+hgt_strt1+smry1$freq[q])/2)
      cat1_tops <- c(cat1_tops, hgt_strt1+smry1$freq[q])
      hgt_strt1 <- hgt_strt1 + dist_btwn1 +smry1$freq[q]
    }
    
    #because we plot bottom-up, we can reverse the stored gate-tops if we want to add text:
    cat1_tops<-rev(cat1_tops)
    centers1 <-rev(centers1)
    
    
    
    #Now add the end gates:
    #the next values are very nice to keep track of..
    hgt_strt2 <- ifelse(nrow(smry3)==1, (plot_hgt-1)/2, 0)
    dist_btwn2 <- (plot_hgt - 1) / (nrow(smry3)-1)
    centers2 <- c()
    cat2_tops <- c()
    
    #define new end_colors for end points:
    end_colors<- colorChart$colors[which(colorChart$cat %in% smry3[[1]])]
    
    #add end blocks of first connection:
    
    lftgate2 <- field*(distanceBtwnGates+gateWidth)
    
    for(q in nrow(smry3):1){
      #                 here vvvv we want space to have text labels
      graphics::rect(xleft = lftgate2,xright = lftgate2+gateWidth,ybottom = hgt_strt2,ytop = hgt_strt2+smry3$freq[q], col = end_colors[which(colorChart$cat[which(colorChart$colors%in%end_colors)]==smry3[[1]][q])],border = gateBorder)
      centers2 <- c(centers2, (hgt_strt2+hgt_strt2+smry3$freq[q])/2)
      cat2_tops <- c(cat2_tops, hgt_strt2+smry3$freq[q])
      hgt_strt2 <- hgt_strt2 + dist_btwn2 +smry3$freq[q]
    }
    
    cat2_tops<-rev(cat2_tops)
    centers2 <-rev(centers2)
    
    #add text labels above blocks for non-rotated plots:
    if(labels==T){
      
      if(rotate == F){
        
        if(field==1){
          graphics::text(as.character(smry1[[1]]), x = -1, y = centers1,pos = 2,col = txtColor,font = 2)
          if(field!=length(classFields)-1){
            graphics::text(as.character(smry3[[1]]), x= lftgate2+.5*gateWidth,y=cat2_tops+.005, pos=3,cex = .8,col = txtColor,font = 2)
          }else{
            graphics::text(as.character(smry3[[1]]), x= lftgate2+gateWidth+1,y=centers2+.005, pos=4,col = txtColor,font = 2)
          }
          
        }else if(field!=length(classFields)-1){
          graphics::text(as.character(smry3[[1]]), x= lftgate2+.5*gateWidth,y=cat2_tops+.005, pos=3,cex = .8,col = txtColor,font = 2)
        }else{
          graphics::text(as.character(smry3[[1]]), x= lftgate2+gateWidth+5,y=centers2+.005, pos=4,col = txtColor,font = 2)
        }
      }
    }
    
    #insert connections: relies on smry2 table
    
    #using start colors, apply an alpha (transparency) for connections:
    conn_cols <- addAlpha(strt_colors,alpha = connectingAlpha)
    
    if(showConnectPercentages==FALSE){
    
    
        for(q in 1:length(cat1_tops)){
          #select from the connections summary the observations containing the same start gate
          temp_dta<- smry2[which(smry2[[1]]==smry1[[1]][q]),]
          
          for(j in 1:nrow(temp_dta)){
            #draw connection:
            graphics::polygon(x=c(lftgate+gateWidth,lftgate+gateWidth,lftgate2,lftgate2), y=c(cat1_tops[q],cat1_tops[q]-temp_dta$freq[j]*smry1$freq[q], cat2_tops[which(smry3[[1]]==temp_dta[[2]][j])]-temp_dta$freq[j]*smry1$freq[q]   ,cat2_tops[which(smry3[[1]]==temp_dta[[2]][j])]), col=conn_cols[which(strt_colors== colorChart$colors[colorChart$cat==smry1[[1]][q]])],border = NA)                     
            
            #lower the top for next category branch:
            cat1_tops[q] <- cat1_tops[q]-temp_dta$freq[j]*smry1$freq[q]
            cat2_tops[which(smry3[[1]]==temp_dta[[2]][j])] <- cat2_tops[which(smry3[[1]]==temp_dta[[2]][j])]-temp_dta$freq[j]*smry1$freq[q]
          }
        }
      
      
    }else{
      
      
      
      for(q in 1:length(cat1_tops)){
        #select from the connections summary the observations containing the same start gate
        temp_dta<- smry2[which(smry2[[1]]==smry1[[1]][q]),]
        
        for(j in 1:nrow(temp_dta)){
          #draw connection:
          graphics::polygon(x=c(lftgate+gateWidth,lftgate+gateWidth,lftgate2,lftgate2), y=c(cat1_tops[q],cat1_tops[q]-temp_dta$freq[j]*smry1$freq[q], cat2_tops[which(smry3[[1]]==temp_dta[[2]][j])]-temp_dta$freq[j]*smry1$freq[q]   ,cat2_tops[which(smry3[[1]]==temp_dta[[2]][j])]), col=conn_cols[which(strt_colors== colorChart$colors[colorChart$cat==smry1[[1]][q]])],border = NA)                     
          
          
          if(rotate==F){
          #add the percentage of the origin gate moving to particular next stage:
            graphics::text( paste0(round(temp_dta[[4]][j]*100,2),"%") , x = 1/2*(lftgate+gateWidth+lftgate2), y = 1/2*( (cat1_tops[q]-temp_dta$freq[j]*smry1$freq[q]) + cat2_tops[which(smry3[[1]]==temp_dta[[2]][j])]), col = percentTextColor, cex=.6)
          }else{
            graphics::text( paste0(round(temp_dta[[4]][j]*100,2),"%") , x = 1/2*(lftgate+gateWidth+lftgate2), y = 1/2*( (cat1_tops[q]-temp_dta$freq[j]*smry1$freq[q]) + cat2_tops[which(smry3[[1]]==temp_dta[[2]][j])]), col = percentTextColor, cex=.6, srt=90) 
          }
          
          
          #lower the top for next category branch:
          cat1_tops[q] <- cat1_tops[q]-temp_dta$freq[j]*smry1$freq[q]
          cat2_tops[which(smry3[[1]]==temp_dta[[2]][j])] <- cat2_tops[which(smry3[[1]]==temp_dta[[2]][j])]-temp_dta$freq[j]*smry1$freq[q]
        }
      }
    
    }
    
    
    #add text labels :
    if(labels==T){
      if(rotate==T){  #text on left, such that rotating the image produces a vertical fall:
        
        if(field==1){
          graphics::text(as.character(smry1[[1]]), x = -5, y = centers1,cex = .8,col = txtColor,srt=90,font = 2)
          if(field!=length(classFields)-1){
            graphics::text(as.character(smry3[[1]]), x= lftgate2-.25*gateWidth,y=centers2, cex = .75,col = txtColor, srt=90,font = 2)
          }else{
            graphics::text(as.character(smry3[[1]]), x= lftgate2+gateWidth+1,y=centers2,cex=.8, col = txtColor,srt=90,font = 2)
          }
        } else if(field!=length(classFields)-1){
          graphics::text(as.character(smry3[[1]]), x= lftgate2-.25*gateWidth,y=centers2, cex = .75,col = txtColor, srt=90,font = 2)
        }else{
          graphics::text(as.character(smry3[[1]]), x= lftgate2+gateWidth*1.5,y=centers2,cex=.8, col = txtColor,srt=90,font = 2)
        }
      }
    }
    
    
    
    
    if(showPercentages == TRUE){
      for(y in 1:length(smry1[[1]])){
        if(rotate==F){
          graphics::text(paste0(100*round(smry1[[3]][y],4),"%"), x = lftgate+1/2*gateWidth, y = centers1[y]+.025,col = percentTextColor,cex = .65)
        }else{
          graphics::text(paste0(100*round(smry1[[3]][y],4),"%"), x = lftgate+1/4*gateWidth, y = centers1[y],col = percentTextColor,cex = .65,srt=90)
        }
      }
      for(z in 1:length(smry3[[1]])){
        if(rotate==F){
          graphics::text(paste0(100*round(smry3[[3]][z],4),"%"), x = lftgate2+1/2*gateWidth,y = centers2[z]+.025,col=percentTextColor, cex=.65)
        }else{
          graphics::text(paste0(100*round(smry3[[3]][z],4),"%"), x = lftgate2+1/4*gateWidth,y = centers2[z],col=percentTextColor, cex=.65,srt=90)  
        }
      }
    }
    
    
    
    
    
    if(showCounts ==TRUE){
      for(y in 1:length(smry1[[1]])){
        if(rotate==F){
          graphics::text(paste0("(",smry1[[2]][y],")"), x = lftgate+1/2*gateWidth, y = centers1[y]-.025,col = countTextColor,cex = .65)
        }else{
          graphics::text(paste0("(",smry1[[2]][y],")"), x = lftgate+3/4*gateWidth, y = centers1[y],col = countTextColor,cex = .65,srt=90)
        }
      }
      for(z in 1:length(smry3[[1]])){
        if(rotate==F){
          graphics::text(paste0("(",smry3[[2]][z],")"), x = lftgate2+1/2*gateWidth,y = centers2[z]-.025,col=countTextColor, cex=.65)
        }else{
          graphics::text(paste0("(",smry3[[2]][z],")"), x = lftgate2+3/4*gateWidth,y = centers2[z],col=countTextColor, cex=.65,srt=90)  
        }
      }
    }
    
    
  }
  
  #add text to bottom to show labels for original field names
  if( !is.na(fieldLabels) && length(fieldLabels)==length(classFields)){
    
    if(rotate==F){
      for(w in 1:length(fieldLabels)){
        graphics::text(fieldLabels[w],x = (w-1)*(distanceBtwnGates+gateWidth) +1/2*gateWidth ,y = -.03,pos = 1,cex = 2,col = txtColor,font = 2)
      }
    }else{
      for(w in 1:length(fieldLabels)){
        graphics::text(fieldLabels[w],x = (w-1)*(distanceBtwnGates+gateWidth) ,y = -.3,pos = 1,cex = 2,col = txtColor,font = 2,srt=90)
      }
    }
    
  }

}
