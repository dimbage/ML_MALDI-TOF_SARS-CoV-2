ModelEstimation <- function(
    iM,
    mlModels,
    f_meas = FALSE,
    recall = FALSE,
    accuracy = FALSE,
    spec = FALSE,
    sens = FALSE,
    ppv = FALSE,
    npv = FALSE,
    roc_auc = FALSE,
    ...
    ){
    #suppressMessages(library(tidyverse))

    suppressMessages(library(tidytable))

    if(!any(grep('list', class(iM)))) {iM <- list(iM)}
    #if(class(iM)[1] != 'list'){iM <- list(iM)}  

    
    prediction <- tidytable(
        data = iM,
        #id = names(tdModels$model_for_prediction[[1]]),
        model = mlModels
        ) |>
        tidytable::mutate(
            inputed_sample = map(data, \(x) x |> select(sample) |> pull()), # |> toString()),
            inputed_class = map(data, \(x) x |> select(class)  |> pull()),
            predicted = map2(data, model, \(data, model) predict(model, data |> select(matches('^[0-9]')))),
            #predicted = map(predicted, \(x) if(length(x)==1){deframe(x)}else{x}),
            predicted_prob = map2(data, model, \(data, model) predict(model, data |> select(matches('^[0-9]')), type = "prob") |> pull(1))
        )
    
    if(f_meas){
        prediction <- prediction |>
            tidytable::mutate(
                f_meas = tidytable::map2_dbl(inputed_class, predicted, \(inputed_class, predicted) yardstick::f_meas_vec(inputed_class, predicted))
            )
    }
    if(recall){
        prediction <- prediction |>
            tidytable::mutate(
                recall = tidytable::map2_dbl(inputed_class, predicted, \(inputed_class, predicted) yardstick::recall_vec(inputed_class, predicted))
            )
    }
    if(accuracy){
        prediction <- prediction |>
            tidytable::mutate(
                accuracy = tidytable::map2_dbl(inputed_class, predicted, \(inputed_class, predicted) yardstick::accuracy_vec(inputed_class, predicted))
            )
    }
    if(spec){
        prediction <- prediction |>
            tidytable::mutate(
                spec = tidytable::map2_dbl(inputed_class, predicted, \(inputed_class, predicted) yardstick::spec_vec(inputed_class, predicted))
            )
    }
    if(sens){
        prediction <- prediction |>
            tidytable::mutate(
                sens = tidytable::map2_dbl(inputed_class, predicted, \(inputed_class, predicted) yardstick::sens_vec(inputed_class, predicted))
            )
    }
    if(ppv){
        prediction <- prediction |>
            tidytable::mutate(
                ppv = tidytable::map2_dbl(inputed_class, predicted, \(inputed_class, predicted) yardstick::ppv_vec(inputed_class, predicted))
            )
    }
    if(npv){
        prediction <- prediction |>
            tidytable::mutate(
                npv = tidytable::map2_dbl(inputed_class, predicted, \(inputed_class, predicted) yardstick::npv_vec(inputed_class, predicted))
            )
    }
    if(roc_auc){
        prediction <- prediction |>
            tidytable::mutate(
                roc_auc = tidytable::map2_dbl(inputed_class, predicted_prob, \(inputed_class, predicted_prob) yardstick::roc_auc_vec(inputed_class, predicted_prob))
            )
    }

    return(prediction)
}