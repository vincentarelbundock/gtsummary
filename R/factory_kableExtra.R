#' Internal function to build table with `gt`
#'
#' @inheritParams modelsummary
#' @keywords internal
#' @return tbl_gt object
factory_kableExtra <- function(tab,
                               title = NULL,
                               stars = FALSE,
                               notes = NULL,
                               hrule = NULL,
                               span = NULL,
                               output_file,
                               output_format) {

    tab <- kableExtra::kable(tab,
                        format = output_format,
                        caption = title,
                        booktabs = TRUE, 
                        linesep = "")

    # horizontal rule to separate coef/gof not supported in markdown
    # TODO: support HTML
    if (!is.null(hrule)) {
        if (output_format %in% 'latex') {
            for (pos in hrule) {
                tab <- tab %>% 
                       kableExtra::row_spec(row = pos - 1,  
                                            extra_latex_after = '\\midrule')
            }
        }
    }

    # stars note
    if (!isFALSE(stars)) {
        threeparttable <- knitr::is_latex_output()
        stars_note <- make_stars_note(stars)
        tab <- tab %>% 
               kableExtra::add_footnote(label = stars_note, notation = 'none', 
                                        threeparttable = threeparttable)
    }

    # user-supplied notes at the bottom of table
    if (!is.null(notes)) {
        # threeparttable only works with 1 note
        threeparttable <- knitr::is_latex_output() & (length(notes) == 1)
        for (n in notes) {
            tab <- tab %>% 
                   kableExtra::add_footnote(label = n, notation = 'none', 
                                            threeparttable = threeparttable)
        }
    }
    
    if (!is.null(span)) {
        # add_header_above not supported in markdown
        if (output_format %in% c('latex', 'html')) {
            span <- rev(span) # correct vertical order
            for (s in span) {
                tab <- tab %>%
                    kableExtra::add_header_above(s)
            }   
        }
    }

    # styling (can be overriden manually by calling again)
    if (output_format %in% c('latex', 'html')) {
        tab <- tab %>% kableExtra::kable_styling(full_width = FALSE)
    }

    # if knitting, keep the `kable` object. Otherwise, convert to
    # human-readable character.
    if (!knitr::is_latex_output() & !knitr::is_html_output()) {
        tab <- as.character(tab)
    }

    # output
    if (is.null(output_file)){
        return(tab)
    } else {
        # function stolen from kableExtra (MIT license). Not exported and CRAN
        # doesn't like :::
        solve_enc <- function(x) {
            out <- enc2utf8(as.character(base::format(x, trim = TRUE, justify = 'none')))
            mostattributes(out) <- attributes(x)
            return(out)
        }
        filecon <- file(output_file)
        writeLines(solve_enc(tab), con = filecon, useBytes = TRUE)
        close(filecon)
    } 

}