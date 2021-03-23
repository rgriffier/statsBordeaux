#' @title A conveinient method to manage multiple column data and label dictionnary
#' @description  A conveinient method to manage multiple column data and label dictionnary
#' @param data a data.frame, containing the data with the indicator variable created by manageMultipleColumnVariable
#' @param labelVar a data.frame with two columns : 1) the variable colname, 2) the clean label to display
#' @param labelQL a data.frame containing three columns : 1) the variable names, 2) one modality as digit, 3) the label of the modality.
#' @param listVarToMerge a character vector, containing the colname of the column to merge
#' @param nonAppChar a character vector of length one, containing the colname of the column to merge
#' @param varOutputLabel a character vector of length one, containing the prefix of the columns to create
#' @return a list of three data.frame : 1) data, 2) labelVar, 3) labelQL
#' @export
manageMultipleColumnVariableAndDictionnary <- function(data, labelVar, labelQL, listVarToMerge, nonAppChar = "NonApp", varOutputLabel) {
  data <- int_manageMultipleColumnVariable(data = data, listVarToMerge = listVarToMerge, nonAppChar = nonAppChar, varOutputLabel = varOutputLabel)
  manageDictionnary <- int_manageLabelDictionnaryMultipleColumnVariable(data = data, labelVar = labelVar, labelQL = labelQL, varOutputLabel = varOutputLabel)
  return(list(data, manageDictionnary[[1]], manageDictionnary[[2]]))
}


#' @title A conveininet method to check if some vector could be cast as numeric
#' @description  A conveinineg metho to check if some vector could be cast as numeric
#' @param vector a vector, which need to be cast as numeric
#' @return a boolean vector of length 1.
isCastableAsNumeric <- function(vector) {
  stopifnot(is.atomic(vector) || is.list(vector))
  if(!class(vector) %in% c("numeric", "character", "integer", 'factor')){
    return(FALSE)
  }
  numNAs <- sum(is.na(vector))
  numNAs_new <- suppressWarnings(sum(is.na(as.numeric(as.character(vector)))))
  return(numNAs_new == numNAs)
}


#' @title Send happy-birthday message
#' @description Send happy-birthday message
#' @export
happyBirthdayHSL <- function(){

  message("
          @@                   @.                ,*
          @@                   @@@@@@@.   @@     @#
          @@                   @@    ,@    @@   #@
          @@%%%@@/             @@@@@@@/     @@ &@(
          @@    *@. @@@#  *@@  @@   /@@@@@@  %@@
          @@     @(  *@@@@@@@. @@    @,  (@   @*
          #@     @( &@     .@. @@    @&//,    @*                                                              .@@@@@@@@@@@(
          .@.    @(  #@@@@@@@  @@    @&       @*                                                            @@*            @@(
          @@,                *@&
          .@#                    @%
          @@                                                                                           .@/                     %@
          @@             @,          @&            @#        @@      %@,                               ,@.                     (@
          @@  ,**.       @@, /@,     @&            @#         #@.   @@                                 ,@,                    .@@
          @@@,   .@@  /  @/   @#/@   @@((%@@       @#          .@&@@.                                   @@@@@@@@@@@@@@@@@@@@@# @@
          @@       @@.@, @/  @@@@@@@*@&    @@    /%@(*@@&,  #@% *@                                       @&,#              .@ .@/
          @@       @@.@, @/     /@   @&    *@ ,@%  @#  &@@@@@@@ *@                                #@@@@@@@@%                  @@
          @@      @@. @%*@      /@   @&    *@ @@  /@# @&     #@ *@                             #@&         @@.               @%
          @@.@@@@@.   @(.@      /@   @&    /@ .@@@.@( .(@@@@@@@ *@                           *@%            (@@@.          .@#
                                                                                             @#              &@ %@%       ,@*
                                                                                            @#              .@*  *@@    %@*
                                                                        .%%#.                @@             @@@*    @@ @@(
                                                                       @*  /@               /@%@@@@@@@@@@@(.,@*   @@@@@@@@
                                                                        ,@,                  #@ .(   @*  &, #@       ##   @@@**,(@@&
                                                                        %@,                   *@#           &@       .@ ,@(        .@@
                                                                          &@*                   #@/         @%       .@ @/          /@.
                                                                       @@/                        (@@/     @@        %* @           /@.
                                                                        @@                            .*@@ *@@       @  @@@/      *@@@
                                                                        @  @*                           ..*@.        @  ,@/*..*((( &@
                                                                     #@@@@@@@@@.                           *&        (%  .@%      %@.
                                                                     @@      .@,                            ,@       (%    @@   &@&
                                                   &@@@@@@@@@@@@@@@@@&//////////////(&@@@@@@@@(              ,@     *&      /@ @@.
                                                   @&                                          .(@@%         .@    .@       *#@@&.
                                                   @@                                             @(         *&    #(        %#
                                                   %@     .@@@                                    @*         &/    #(      /@
                                                @@@@@    ,@  %@                      ,@# @@      &@          *&    ,@    %%
                                                .@*#@    .@#,@(                       @# &@     /@@@@          &%   @,  %(
                                                 @@,@.                                 @@@      @@ @@            &( .@  @.
                                                 #@ @&*@&%@@                            .@@@@/ #@ /@,             @  #% @,
                                                    @@ *@@@           @/    &@           @@@@( @&,@/              *@ ,@ %(
                                                    /@                 (%@@@#                 %@.,               @@%@*@ @,
                                                     @,                                       @&                @@*@@&*&*
                                                      .&@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@/                 @@ /@@@.@@
                                                                      @(***%@                                .@@. @@@@@&@.
                                                                      @#**&@@                               @@. /@# &*&/ &,
                                                               .%%%&@@@@. *@@%%#.                        .@@,  @@   /&*&  %&.
                                                             @@                  .@@%                   @@  *@@      @(&
                                                          (@@(                      @@*%&&&&@@@@@@@@@@@@* @@%        .@.
                                                      @@@# @%                        @&*,.          @%  @@           @*@
                                                  /@@,   #@@.@@@@@@@@@@@@@&&%/**  &@*&@                             @..@
                                               @@@  .%@@# @/&@  .@@@ *@@**. .&@ @&/@@,@/                           /& ##
                                          .@(//@@@@#     *@ @&     %@@/   @@@   .@@ @&@@                           .@%#
                                           /@@@@%@@,     *@ @@@@@@@@@@@@@@@@@@@@@@@@@( @*                          .@%  %*     &           (%
                                              *@@@( #@@% *@                            @,                          /..@#    /(             @
")
  message("                                        &@&  *@@@@@@@@@@@@&/                  ,**#@@@@@@@@@@#/*.                   &,&  %,#%* .
                                          ,&@@@@@@@@&*(@@/   @@@                                            .(@@@@@@@@(.     .*   /&,   / *#@(@(%@(%*
                                   ,@@@@#.               *@@@@(  .#@@*                                                  ,%@@@@@&,&&@&(#&#
                            /@@@@#,                        @@ &@%#@@@@@@     &@                                                  @@@@@/   #
                     ,%@@@@*           /@.     @@      (@@, ,@@/@@  &@.#@.  @@@@.     *@@@.    .@@      /                         &    .%@@@,
               ,@@@@%.                #@%@@@ /@*,@&   *@@@@/  %@@%  .@@@@   .#&/      @@@@#   @@.%@   *@@@@                                 .@@
          .@@@,                       @@@@#    .@@@@.  #@@@@  @@*@* @@ %@   @@ @#    #@&@@/    */*    *@@@@
                                        @@,@%  *@/ @@   @& @% (@.%@ %@ @@   *@ %@    &@ @@    &@(@.  @@@@@
                                        .@,#@   @@ #@  .@* @@ .@,.@.*@.@&   ,@  @@   @@ @@   #@,@/  @@ (@
                                        .@, @(&@@@@@@@%(//*...             .  *//(&@@@@@@@@@@@@@@*  @@.@(
                                     *@@@@%*     @      %        .,(((((((((((,,...     /&     /.  ,&@@@@@(
                                    #@,&/  ./#@@@@@@@@%*,.....                        ,*#%@@@@@@@&(,**   @@
                                      @@@&.                                                          .%@@@
                                       @&                                                              #@
                                       .@#                                                            .@/
                                        %@                                                            @#
                                        ,@.  %@@&                                               /(, ,@#
                                         @@ %@,(@   ,@@ #, *                              ..  .@. @&#@
                                   .&@@@&&@@@/,                   @@@@@@@@@@@@       @@/(@% #(     &@@@@@@#,
                                &@(             .,#@@@@@@@@@@@@@@@@@&&//*       .,,*(&@@@@@@@@@@(,          (@@
                                @@                                                                          %@@
                                   *&@@@@@@%(,                                                    ./@@@@@@#.
                                                   ./(#&@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@&#(/,



")
}


#' @title Clear text label
#' @description Clear text label
#' @param x a character vector of length one
#' @importFrom rvest html_text
#' @importFrom xml2 read_html
#' @export
clearLabel <- Vectorize(function(x){
  if(!is.vector(x) | !is.character(x) | length(x) != 1){
    stop("x must be a character vector of length one")
  }
  text <- tryCatch({
    rvest::html_text(xml2::read_html(x))
  }, error = function(error_condition) {
    return(x)
  })
  return(text)
}, vectorize.args = 'x', SIMPLIFY = TRUE, USE.NAMES = FALSE)


#' @title Capitalize the first letter of a string
#' @description Capitalize the first letter of a string
#' @param x a character vector of length one
#' @return a character vector of length one, with the first letter capitalize
#' @export
firstUp <- Vectorize(function(x) {
  if(!is.vector(x) | !is.character(x) | length(x) != 1){
    stop("x must be a character vector of length one")
  }
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  return(x)
}, vectorize.args = 'x', SIMPLIFY = TRUE, USE.NAMES = FALSE)


#' @title Save column's attribute to a data.frame
#' @description Save column's attribute to a data.frame
#' @param data a data.frame
#' @return a data.frame with the column's attribute saved
#' @export
saveAttributes <- function(data = .){
  if(!is.data.frame(data)){
    stop('.data must be a data.frame')
  }
  statsBordeaux.savedAttributes <<- lapply(data, attributes)
  return(data)
}

#' @title Save column's attribute to a data.frame
#' @description Save column's attribute to a data.frame
#' @param data a data.frame
#' @param listAttributes the list of attribute to restore
#' @return a data.frame with the column's attribute restored
#' @export
restoreAttributes <- function(data = ., listAttributes = c('label', 'var_label')){
  if(!is.data.frame(data)){
    stop('data must be a data.frame')
  }
  if(!is.null(listAttributes)){
    if(!is.vector(listAttributes) || !is.character(listAttributes)){
      stop('listAttributes must be a character vector')
    }
  }

  if(!exists('statsBordeaux.savedAttributes')){
    stop("attributes must have been saved with saveAttributes() function")
  }

  sapply(colnames(data), function(currentColname){
    currentAttr <- statsBordeaux.savedAttributes[[currentColname]]
    if(!is.null(listAttributes)){
      currentAttr <- currentAttr[names(currentAttr) %in% listAttributes]
    }
    attributes(data[, currentColname]) <<- c(attributes(data[, currentColname]), currentAttr)
  })

  rm(statsBordeaux.savedAttributes, envir = .GlobalEnv)

  return(data)
}


#' @title Set non-applicable tag depend on condition
#' @description A convenient method to set non-applicable tag depend on condition
#' @param data a data.frame
#' @param colname a character vector of length one, in which non-applicable tag must be set
#' @param condition the condition that trigger the set of non-applicable tag
#' @param notApplicableChar a character vector of length one, containing the non-applicable tag. Default to 'NonApp'
#' @return a data.frame with the non-applicable tag set
#' @export
setNonApp <- function(data = ., colname, condition, notApplicableChar = 'NonApp'){
  if(!is.data.frame(data)){
    stop('data must be a data.frame')
  }
  if(!is.vector(colname) | !is.character(colname) | length(colname) != 1){
    stop("colname must be a character vector of length one.")
  }
  if(!colname %in% colnames(data)){
    stop("colname must be the name of a column in data.")
  }
  if(!is.vector(notApplicableChar) | !is.character(notApplicableChar) | length(notApplicableChar) != 1){
    stop("notApplicableChar must be a character vector of length one.")
  }

  data[, colname] <- ifelse(condition(data), notApplicableChar, as.character(data[, colname]))
  return(data)
}


#' @title Set label to variable in a data.frame
#' @title Set label to variable in a data.frame
#' @param .data a data.frame
#' @return a data.frame with variable label set
#' @export
addLabelToVariable <- function(.data, ..., .labels = NA) {
  values <- rlang::dots_list(...)
  if (length(values) > 0) {
    if (!all(names(values) %in% names(.data)))
      stop("some variables not found in .data")
    for (v in intersect(names(values), names(.data))){
      attributes(.data[[v]])$label <- values[[v]]
      attributes(.data[[v]])$var_label <- values[[v]]
    }
  }
  return(.data)
}

