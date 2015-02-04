library(testthat)
library(stringr)

.get_section_indices <- function(scan_file_list, pattern='\\[.*\\]'){
    pattern <- pattern
    indices <- grep(pattern = pattern, x = scan_file_list)
    return(indices)
}

.get_section_index_names <- function(scan_file_list, pattern='\\[.*\\]'){
    indices <- .get_section_indices(scan_file_list = scan_file_list,
                                     pattern = pattern)
    raw_names <- scan_file_list[indices]
    clean_names <- sapply(X = raw_names, FUN = stringr::str_replace_all,
                          pattern = '\\[|\\]',
                          replacement = '',
                          USE.NAMES = FALSE,
                          simplify = TRUE)
    return(clean_names)
}

get_sections <- function(scan_file_list, pattern='\\[.*\\]'){
    indices <-.get_section_indices (scan_file_list = scan_file_list,
                                     pattern = pattern)
    names <- .get_section_index_names(scan_file_list = scan_file_list,
                                      pattern = pattern)

    testthat::expect_equal(length(indices), length(names))

    values_in_sections <- list()
    for(i in 1:length(indices)){
        index <- indices[i]
        if(index != indices[length(indices)]){
            # not the last element
            start_index <- index + 1
            end_index <- indices[i + 1] - 1
        } else{
            # last element
            start_index <- index + 1
            end_index <- length(scan_file_list)
        }
        value_in_section <- scan_file_list[start_index:end_index]
        value_in_section <- list(value_in_section)
        names(value_in_section) <- names[i]
        values_in_sections <- c(values_in_sections, value_in_section)
    }
    testthat::expect_equal(length(names), length(values_in_sections))
    return(values_in_sections)
}

scan_file <- scan(file = 'tests/test_config.ini',
                  what = character(0),
                  sep = '\n')

get_sections(scan_file_list = scan_file)
