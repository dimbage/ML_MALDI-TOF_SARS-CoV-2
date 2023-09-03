
MLmodel <- function(
    iM,
    ncores = 1,
    nfolds = 2,
    seed = 42,
    algorithmList = c("C5.0", "knn", "naive_bayes", "rf", "svmLinear", "svmRadial", "xgbTree"),
    sFunction = c(caret::twoClassSummary, caret::multiClassSummary),
    #algorithm_list = c("J48", "knn", "naive_bayes", "rf", "svmLinear", "svmRadial", "xgbTree")
    ...
    ){
    suppressMessages(library(caret))
    suppressMessages(library(progressr))
    suppressMessages(library(cli))
    #suppressMessages(library(magrittr))
    suppressMessages(library(tidytable))
    #suppressMessages(library(xgboost))
    
    if (nfolds < 2 ) {stop("nfolds should be 2 or more")}

    options(warn=-1)
    progressr::handlers(global = TRUE)
    progressr::handlers("cli")

    #algorithmList <- c("J48", "knn", "naive_bayes", "rf", "svmLinear", "svmRadial", "xgbTree") # in NATURE paper
    #algorithmList <- algorithm_list #c("C5.0", "knn", "naive_bayes", "rf", "svmLinear", "svmRadial", "xgbTree")
             #algorithmListNames <-  c("DT4", "KNN", "NB", "RF", "SVM-L", "SVM-R", "XGBoost") # in NATURE paper
    
    steps <- 1:(nfolds*length(algorithmList)+3)
    p <- progressr::progressor(along = steps)

    set.seed(seed)
    cl <- parallel::makePSOCKcluster(ncores)
    doParallel::registerDoParallel(cl)

    trainData <-  list()
    trainSampleName <-  list()
    testData <- list()
    models <- list()
    
    index  <- 0

    p(message = "train control")
    trainControl <- trainControl(
        method = 'repeatedcv',
        number = 10,
        repeats = 5,
        search = 'random',
        classProbs = TRUE,
        savePred = TRUE,
        summaryFunction = sFunction
    )
    
    
    training.samples <- createFolds(iM$fold_gr, k = nfolds, returnTrain = TRUE)
      
    p(message = paste0('training samples (', nfolds, ' folds on ',length(algorithmList),' Models)'))
   
    for(fold in 1:length(training.samples)){

        index <- index + 1
        
        train.data      <- iM |> tidytable::slice(training.samples[[fold]]) |> tidytable::select(-c(sample, fold_gr, class_ori))
        test.data       <- iM |> tidytable::slice(-training.samples[[fold]]) |> tidytable::select(-c(sample, fold_gr, class_ori))
        test.idSample   <- iM |> tidytable::slice(-training.samples[[fold]]) |> tidytable::pull(sample) 
        test.class_ori  <- iM |> tidytable::slice(-training.samples[[fold]]) |> tidytable::pull(class_ori) 

  ##########################################################################
  
        count <- 0
        
        for (algorithm in algorithmList){

            train.data %>% tidytable::select(-class) |> is.na() |>table()

            index <- index + 1
            count <- count + 1
            
            set.seed(seed)

            p(message = paste("fold:", fold, " ", "algorithm:", algorithmList[count], sep = ""))
            #print(paste("Fold: ", fold, " - ", "Algorithm: ", algorithmListNames[count], sep = ""))

            mlModel <- train(
                x = train.data %>% tidytable::select(-class),
                y = train.data %>% tidytable::pull(class),
                trControl = trainControl,
                method = algorithm,
                tuneLength = 20,
                metric = "AUC"
            )
            
            trainData[[paste0('tr',index)]] <- train.data 
            trainSampleName[[paste0('tr_s',index)]]  <- iM |> tidytable::slice(training.samples[[fold]]) |> tidytable::pull(sample) |> toString() 
            testData[[paste0('te', index)]] <- test.data |> tidytable::mutate(
                sample = {{test.idSample}},
                class_ori = {{test.class_ori}}
                )
            models[[paste0('mo', index)]] <- mlModel
    
        }
    }
    
    parallel::stopCluster(cl)

    p(message = "Analysis finished!")
    return(
        tidytable::tidytable(
            trainData = trainData,
            trainSampleName = trainSampleName,
            testData = testData,
            model_for_prediction = models
            )
        )
}
