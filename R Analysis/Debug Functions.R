calculate_imageWise_correlations <- function(fileDir, partData, targetFile){
  #find the .mat files in the given directory.
  files <- list.files(fileDir, full.names = T, pattern = ".mat")
  
  #get all of the IDs to search the files for
  IDs <- as.character(unique(interaction(partData$Participant_Number, partData$Session_Number, sep="_")))
  
  #Now load the targets before the loop. readMat is slow, let's do it as few times as possible.
  targets <- readMat(targetFile)
  
  partData$RelevantImageWiseCorrelation <- NA
  partData$nonRelevantImageWiseCorrelation <- NA
  
  #iterate through the IDs
  for(ID in IDs){
    
    #convert ID to numbers for accessing PartData
    ParticipantNumber <- as.numeric(strsplit(ID, "_")[[1]][1])
    SessionNumber <- as.numeric(strsplit(ID, "_")[[1]][2])
    TargetID <- as.numeric(unique(filter(partData, Participant_Number == ParticipantNumber, Session_Number == SessionNumber)$Target_Type))
    
    #select the relevant file.
    file <- files[grep(paste("/",ID,".mat",sep=''), files)]
    
    #load the relevant file.
    data <- readMat(file)
    
    #Discard data that isn't useful
    data <- array(aperm(data$whitenoise,c(2,1,3)), c(2500,2000))
    
    #load the appropriate target image into memory.
    correctTargetImage <- array(t(targets[[TargetID]]), c(2500))
    #incorrectTargetImage <- array(targets[[-TargetID]], c(2500))
    
    #now calculate the correlations
    correctCorrelations <- apply(data, 2, cor, y=correctTargetImage)
    #incorrectCorrelations <- apply(data, 2, cor, y=incorrectTargetImage)
    
    partData$RelevantImageWiseCorrelation[partData$Participant_Number==ParticipantNumber & partData$Session_Number==SessionNumber & partData$Experiment_Phase==" experimental"] <- correctCorrelations[partData$Trial_Number[partData$Participant_Number==ParticipantNumber & partData$Session_Number==SessionNumber & partData$Experiment_Phase==" experimental"]]
    
  }
  
  return(partData)
}


calculate_CI_correlations <- function(fileDir, partData, EXPNAME='EXP2'){
  #find the .mat files in the given directory.
  files <- list.files(fileDir, full.names = T, pattern = ".mat")
  
  #get all of the IDs to search the files for
  IDs <- as.character(unique(interaction(partData$Participant_Number, partData$Session_Number, sep="_")))
  
  #iterate through the IDs
  for(ID in IDs){
    
    #convert ID to numbers for accessing PartData
    ParticipantNumber <- as.numeric(strsplit(ID, "_")[[1]][1])
    SessionNumber <- as.numeric(strsplit(ID, "_")[[1]][2])
    TargetID <- as.numeric(unique(filter(partData, Participant_Number == ParticipantNumber, Session_Number == SessionNumber)$Target_Type))
    
    #select the relevant file.
    file <- files[grep(paste("/",ID,".mat",sep=''), files)]
    
    #load the relevant file.
    data <- readMat(file)
    
    #Discard data that isn't useful
    data <- array(aperm(data$whitenoise,c(2,1,3)), c(2500,2000))
    
    data <- data[,partData$Trial_Number[partData$Participant_Number==ParticipantNumber & partData$Session_Number==SessionNumber & partData$Experiment_Phase=="experimental"]]
    
    responses <- partData$Response[partData$Participant_Number==ParticipantNumber & partData$Session_Number==SessionNumber & partData$Experiment_Phase=="experimental"]*2 - 1
    
    data <- t(t(data) * responses)
    
    if(SessionNumber==1){
      assign(paste0('image', ParticipantNumber), as.matrix(rowSums(data), nrow=1))
    }
    else{
      assign(paste0('image', ParticipantNumber), cbind(get(paste0('image', ParticipantNumber)), as.matrix(rowSums(data), nrow=1)))
      
      toPNG <- imager::as.cimg(rowSums(get(paste0('image', ParticipantNumber))))
      
      imager::save.image(toPNG, file = paste0("CIs/",EXPNAME,"/", ParticipantNumber, ".png"))
    }
    
  }
  for(ID in IDs){
    
    #convert ID to numbers for accessing PartData
    ParticipantNumber <- as.numeric(strsplit(ID, "_")[[1]][1])
    SessionNumber <- as.numeric(strsplit(ID, "_")[[1]][2])
    TargetID <- as.numeric(unique(filter(partData, Participant_Number == ParticipantNumber, Session_Number == SessionNumber)$Target_Type))
    
    #select the relevant file.
    file <- files[grep(paste("/",ID,".mat",sep=''), files)]
    
    #load the relevant file.
    data <- readMat(file)
    
    #Discard data that isn't useful
    data <- array(aperm(data$whitenoise,c(2,1,3)), c(2500,2000))
    
    #now calculate the correlations
    CICorrelations <- apply(data, 2, cor, y=rowSums(get(paste0('image', ParticipantNumber))))
    #incorrectCorrelations <- apply(data, 2, cor, y=incorrectTargetImage)
    
    partData$CIImageWiseCorrelation[partData$Participant_Number==ParticipantNumber & partData$Session_Number==SessionNumber & partData$Experiment_Phase==" experimental"] <- CICorrelations[partData$Trial_Number[partData$Participant_Number==ParticipantNumber & partData$Session_Number==SessionNumber & partData$Experiment_Phase==" experimental"]]
    
  }
  return(partData)
}
