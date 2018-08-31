


################################################################################
######################--------------Functions-----------########################

# Function to evaluate 

performance_test <-
  function(data_set,subset){
    
    inputs  <- data_set[,-ncol(data_set)]
    output  <- data_set[,ncol(data_set)]
    
    
    tr_data <-
      train(x=inputs[subset],y=output,method = 'rf',
            trControl = trainControl(method = 'cv',number = 5))
    
    tr_sults <- tr_data$results[c('RMSE', 'Rsquared')]
    tr_sults[which.max(tr_sults$RMSE),]
    
  }

# Filters based in entropy 

entropyIndexes <- function(data_set){  
  
  # split and order matrix
  
  inputs      <- data_set[,1:(ncol(data_set)-1)]
  
  output      <- data_set[,(ncol(data_set))]
  
  # Convert in quartiles the dataste
  
  output_splt <- quantileCut(output,3)
  
  df_data_set <- data.frame(inputs,output=output_splt)
  
  head(df_data_set)
  
  infoGain <- information.gain(output~.,data = df_data_set)
  
  infoGainDS <- data.frame( variables =  row.names(infoGain), relevances = infoGain[,1])
  
  ig <- infoGainDS[order(infoGainDS$relevances,decreasing = T),]
  
  
  gainRatio <- gain.ratio(output~.,data = df_data_set)
  
  infoGainRatioDS <- data.frame( variables =  row.names(gainRatio), relevances = gainRatio[,1])
  
  gr <- infoGainRatioDS[order(infoGainRatioDS$relevances,decreasing = T),]
  
  
  
  symmetricalUncertainty <- symmetrical.uncertainty(output~., data = df_data_set)
  
  symmetricalUncertaintyDS <- data.frame( variables =  row.names(symmetricalUncertainty), relevances = symmetricalUncertainty[,1])
  
  su <- symmetricalUncertaintyDS[order(symmetricalUncertaintyDS$relevances,decreasing = T),]
  
  list(infoGain=ig,gainRatio=gr,symmetricalUncertainty=su)
}

################################################################################
#---------------------------------Implementation-------------------------------#



evaluateFilters <- function(entIdx,dataset){
  
  orderRelvance <- lapply(entIdx,function(x){as.character(x[,1])})

  # split each group of variables

  variable_infoGain <- orderRelvance[[1]]

  variable_gainRatio <- orderRelvance[[2]]

  variable_symmetricalUncertainty <- orderRelvance[[3]]

  tg <- list()

  varsToTest_infoGain <-
    lapply(1:length(variable_infoGain),function(w){
    variable_infoGain[1:w]}
   )

  varsToTest_gainRatio <-
    lapply(1:length(variable_gainRatio),function(w){
    variable_gainRatio[1:w]}
  )


  varsToTest_symmetricalUncertainty <-
    lapply(1:length(variable_symmetricalUncertainty),function(w){
    variable_symmetricalUncertainty[1:w]}
  )



  # Performance

  set.seed(123)

  performances_infoGain <- 
    lapply(seq(length(varsToTest_infoGain)),function(w){
    performance_test(dataset,varsToTest_infoGain[[w]])
  })

  set.seed(123)

  performances_gainRatio <- 
    lapply(seq(length(varsToTest_gainRatio)),function(w){
    performance_test(dataset,varsToTest_gainRatio[[w]])
  })


  set.seed(123)

  performances_symmetricalUncertainty <- 
    lapply(seq(length(varsToTest_symmetricalUncertainty)),function(w){
    performance_test(dataset,varsToTest_symmetricalUncertainty[[w]])
  })

  list(performances_infoGain=performances_infoGain,performances_gainRatio=performances_gainRatio,
       performances_symmetricalUncertainty = performances_symmetricalUncertainty)
}
