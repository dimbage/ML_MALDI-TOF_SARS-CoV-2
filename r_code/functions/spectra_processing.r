spectra_processing <- function(
    object_or_path,
    trim_range = c(3000, 15500),
    int_transformation = 'sqrt',
    int_smoothing = 'SavitzkyGolay',
    baseline_removing = 'TopHat',
    int_calibration = 'TIC',
    halfwindowsize = 10,
    snr = 2,
    bin_tol = 0.003,
    wf = NULL,
    warp_gr = FALSE,
    peak_filt_cutoff = 0,
    reference = FALSE,
    ref_im = FALSE,
    ref_im_aver = 'mean',
    sample_averaging = FALSE,
    sample_averfun = 'mean',
    spectra_table = FALSE,
    spectra_list = FALSE,
    ...){
    
    needed_packages <- c('progressr', 'cli', 'tidytable','MALDIquantForeign', 'MALDIquant', 'MALDIrppa')                                        # Specify your packages
    not_installed <- needed_packages[!(needed_packages %in% installed.packages()[ , "Package"])]    # Extract not installed packages
    if(length(not_installed)) install.packages(not_installed)  



    suppressMessages(library(progressr))
    suppressMessages(library(cli))
    #suppressMessages(library(tidyverse))
    suppressMessages(library(tidytable))
    #suppressMessages(library(magrittr))

    
    options(warn=-1)
    progressr::handlers(global = TRUE)
    progressr::handlers("cli")
##########################################################################
#if(!reference && ref_im){
#        stop('Set reference argument TRUE')
#}
#if(reference && !ref_im){
#    stop('Provide intesity matrix for reference peaks creating')
#}
##########################################################################
steps = 1:11
p <- progressr::progressor(along = steps)

##########################################################################
# load spectra
 p(message = "Spectra loading")
if(is.list(object_or_path)){
    spectra01 <- object_or_path
    } else if(is.character(object_or_path)){
        spectra01 <- MALDIquantForeign::import(object_or_path[1], verbose = FALSE)
        if(length(object_or_path)>1){
            for(l in 2:length(object_or_path)) {
                spectra01 <- c(spectra01, MALDIquantForeign::import(object_or_path[l], verbose = FALSE))
            }
        }
} else {
    top('Provide MS object or PATH to MS object')
}

##########################################################################
# all needed variables
samples <- sapply(spectra01, \(x) MALDIquant::metaData(x)$file[1] |> strsplit("[\\]") |> unlist() |> (\(x) x[length(x)])()) |> as.character()
subClasses <- sapply(spectra01, \(x) MALDIquant::metaData(x)$file[1] |> strsplit("[\\]") |> unlist() |> (\(x) x[length(x)-1])()) |>as.character()
#classes <- ifelse(grepl(paste(covid_gr_names, collapse = '|'), subClasses, , ignore.case=TRUE), 'Covid', 'Control') %>% factor

subClass <- unique(subClasses)
totalSubClass <- length(subClass)

for(i in 1:length(subClass)){
  assign(paste0("sub_", sprintf("%06d", i)), grep(subClass[i], subClasses))
  }

sub_N <- ls()[grep("^sub_0", ls())]

for(s in 1:totalSubClass){
        samples[get(sub_N[s])] <- paste0('gr_', sprintf("%02d", s),'_', samples[get(sub_N[s])])
    }

##########################################################################
# spectra processis
p(message = "Spectra trimming")
spectra02 <- MALDIquant::trim(spectra01, range = trim_range)

p(message = "Spectra transforming")
spectra03 <- MALDIquant::transformIntensity(spectra02, method = int_transformation)

p(message = "Spectra smoothing")
spectra04 <- MALDIrppa::wavSmoothing(spectra03, method = int_smoothing)

p(message = "Baseline removing")
spectra05 <- MALDIquant::removeBaseline(spectra04, method = baseline_removing)

p(message = "Spectra calibrating")
spectra06 <- MALDIquant::calibrateIntensity(spectra05, method = int_calibration)

##########################################################################
# Warping MassSpectra and trimming within group

if(all(!is.null(wf), exists('warp_gr'))) {
    for(w in warp_gr){
        spectra06[get(sub_N[w])] <- MALDIquant::warpMassSpectra(spectra06[get(sub_N[w])], list(wf))
        spectra06[get(sub_N[w])] <- MALDIquant::trim(spectra06[get(sub_N[w])], range = trim_range)
        }
    }

##########################################################################
# sample averaging
if(sample_averaging){
    for(s in 1:totalSubClass){
        samples[get(sub_N[s])] <- gsub('_[0-9].mzML$|_[0-9][0-9].mzML$|_[0-9][0-9][0-9].mzML$', '.mzML', samples[get(sub_N[s])]) |> paste0(s,':', x = _)
    }
    
    spectra06 <- MALDIquant::averageMassSpectra(spectra06, labels = samples, method = sample_averfun)

    samples <- sapply(spectra06, \(x) MALDIquant::metaData(x)$file[1] |> strsplit("[\\]") |> unlist() |> (\(x) x[length(x)])()) |> as.character()
    subClasses <- sapply(spectra06, \(x) MALDIquant::metaData(x)$file[1] |> strsplit("[\\]") |> unlist() |> (\(x) x[length(x)-1])()) |>as.character()

    #classes <- ifelse(grepl(paste(covid_gr_names, collapse = '|'), subClasses, , ignore.case=TRUE), 'Covid', 'Control') %>% factor

    subClass <- unique(subClasses)
    totalSubClass <- length(subClass)

    for(i in 1:length(subClass)){
      assign(paste0("sub_", sprintf("%06d", i)), grep(subClass[i], subClasses))
      }

    sub_N <- ls()[grep("^sub_0", ls())]
}

##########################################################################
# reference peaks
if(all(reference, exists('ref_im'))) {
    ref_peaks <- ref_im |>
        tidytable::summarise(tidytable::across(matches('^[0-9]'), !!sym(ref_im_aver))) |>
        (\(x) MALDIquant::createMassPeaks(mass = as.numeric(colnames(x)), intensity=as.numeric(x)))()
    
    ref_spectra <- ref_im |>
        tidytable::summarise(tidytable::across(matches('^[0-9]'), !!sym(ref_im_aver))) |>
        (\(x) MALDIquant::createMassSpectrum(mass = as.numeric(colnames(x)), intensity=as.numeric(x)))()
}
##########################################################################
# Spectra creation
if(spectra_table){
p(message = "Spectra creation")
    iM_s <- spectra06 |>
        tidytable::map_dfr(
            \(x) tidytable::tidytable(mass = round(MALDIquant::mass(x), 0), intesity = MALDIquant::intensity(x)) |>
            tidytable::group_by(mass) |>
            tidytable::summarise(intesity = mean(intesity)) |>
            tidytable::ungroup(),
            .id = 'id'
            ) |>
            tidytable::pivot_wider(names_from = mass, values_from = intesity) |>
            tidytable::mutate(
                class = subClasses,
                #subclasses = subClasses,
                sample = samples
                ) |>
            tidytable::arrange(
                class,
                #subclasses,
                order(gtools::mixedorder(sample))
            ) |>
            tidytable::select(-id)
}
##########################################################################
# Peak detection and processing
p(message = "Peak detection")
allPeaks <- MALDIquant::detectPeaks(spectra06, halfWindowSize = halfwindowsize, SNR = snr)

p(message = "Peak binning")
for (b in 1:totalSubClass) {
    if(reference) {
        allPeaks[get(sub_N[b])] <- MALDIquant::binPeaks(c(ref_peaks, allPeaks[get(sub_N[b])]), method = 'reference', tolerance = bin_tol)[-1]
        } else {
        allPeaks[get(sub_N[b])] <- MALDIquant::binPeaks(allPeaks[get(sub_N[b])], tolerance = bin_tol)
    }
       
    if(peak_filt_cutoff != 0){
        allPeaks[get(sub_N[b])] <- MALDIquant::filterPeaks(allPeaks[get(sub_N[b])], minFrequency = peak_filt_cutoff) # default peak_filt_cutoff = 0
    }
    
}


if(reference){
    allPeaks <- MALDIquant::binPeaks(c(ref_peaks, allPeaks), method = 'reference', tolerance = bin_tol)
} else {
    allPeaks <- MALDIquant::binPeaks(allPeaks, tolerance = bin_tol)
}

##########################################################################
# Intensity Matrix
p(message = "Peak creation")
if(reference){
    iM_p <- MALDIquant::intensityMatrix(allPeaks, c(ref_spectra,spectra06))[, as.character(MALDIquant::mass(ref_peaks))][-1,] |>
        tidytable::as_tidytable() |>
        tidytable::mutate(
            class = subClasses,
            #subclasses = subClasses,
            sample = samples
        ) |>
        tidytable::arrange(
            class,
            #subclasses,
            order(gtools::mixedorder(sample))
            )
        } else {
    iM_p <- MALDIquant::intensityMatrix(allPeaks, spectra06) |>
        tidytable::as_tidytable() |>
        tidytable::mutate(
            class = subClasses,
            #subclasses = subClasses,
            sample = samples
        ) #|>
        #tidytable::arrange(
        #    class,
        #    #subclasses,
        #    order(gtools::mixedorder(sample))
        #    )    
        }


p(message = "Job done")
Sys.sleep(2)
    iM <- list(iM_peak = iM_p)
    if(spectra_table){
        iM$iM_spectra <- iM_s
    }

    if(spectra_list){
        iM$spectra_list <- spectra06
    }
return(iM)
}
