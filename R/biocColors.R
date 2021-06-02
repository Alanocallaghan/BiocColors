#' Colours for Bioconductor
#' @param n Integer which controls how many colors should be retrieved.
#' @param palette_name The name of the palette to be returned.
#' @return A character vector of length \code{n}, comprising colors from
#' the palette specified by \code{palette_name}.
#' 
#' 
#' 
#' @examples
#' biocColors("ditto")
#'
#' @author Daniel Bunis
#' @author Aaron Lun
#' @export
#' 
#' 
biocColors <- function(
        n,
        palette_name = c(
            "tableau20",
            "tableau10medium",
            "colorblind10",
            "colourblind10",
            "trafficlight",
            "purplegray12",
            "bluered12",
            "greenorange12",
            "cyclic",
            "ditto"
        )
    ) {
    palette_name <- match.arg(palette_name)
    palette <- switch(palette_name,
        tableau20 = c(
            "#1F77B4", "#AEC7E8", "#FF7F0E", "#FFBB78", "#2CA02C",
            "#98DF8A", "#D62728", "#FF9896", "#9467BD", "#C5B0D5",
            "#8C564B", "#C49C94", "#E377C2", "#F7B6D2", "#7F7F7F",
            "#C7C7C7", "#BCBD22", "#DBDB8D", "#17BECF", "#9EDAE5"
        ),
        tableau10medium = c(
            "#729ECE", "#FF9E4A", "#67BF5C", "#ED665D",
            "#AD8BC9", "#A8786E", "#ED97CA", "#A2A2A2",
            "#CDCC5D", "#6DCCDA"
        ),
        colorblind10 = c(
            "#006BA4", "#FF800E", "#ABABAB", "#595959",
            "#5F9ED1", "#C85200", "#898989", "#A2C8EC",
            "#FFBC79", "#CFCFCF"
        ),
        colourblind10 = c(
            "#006BA4", "#FF800E", "#ABABAB", "#595959",
            "#5F9ED1", "#C85200", "#898989", "#A2C8EC",
            "#FFBC79", "#CFCFCF"
        ),
        trafficlight = c(
            "#B10318", "#DBA13A", "#309343", "#D82526",
            "#FFC156", "#69B764", "#F26C64", "#FFDD71",
            "#9FCD99"
        ),
        purplegray12 = c(
            "#7B66D2", "#A699E8", "#DC5FBD", "#FFC0DA",
            "#5F5A41", "#B4B19B", "#995688", "#D898BA",
            "#AB6AD5", "#D098EE", "#8B7C6E", "#DBD4C5"
        ),
        bluered12 = c(
            "#2C69B0", "#B5C8E2", "#F02720", "#FFB6B0", "#AC613C",
            "#E9C39B", "#6BA3D6", "#B5DFFD", "#AC8763", "#DDC9B4",
            "#BD0A36", "#F4737A"
        ),
        greenorange12 = c(
            "#32A251", "#ACD98D", "#FF7F0F", "#FFB977",
            "#3CB7CC", "#98D9E4", "#B85A0D", "#FFD94A",
            "#39737C", "#86B4A9", "#82853B", "#CCC94D"
        ),
        cyclic = c(
            "#1F83B4", "#1696AC", "#18A188", "#29A03C", "#54A338",
            "#82A93F", "#ADB828", "#D8BD35", "#FFBD4C", "#FFB022",
            "#FF9C0E", "#FF810E", "#E75727", "#D23E4E", "#C94D8C",
            "#C04AA7", "#B446B3", "#9658B1", "#8061B4", "#6F63BB"
        ),
        ditto = c(  # Colors as of dittoSeq-v0.2.10
            "#E69F00", "#56B4E9", "#009E73", "#F0E442",
            "#0072B2", "#D55E00", "#CC79A7", "#666666",
            "#AD7700", "#1C91D4", "#007756", "#D5C711",
            "#005685", "#A04700", "#B14380", "#4D4D4D",
            "#FFBE2D", "#80C7EF", "#00F6B3", "#F4EB71",
            "#06A5FF", "#FF8320", "#D99BBD", "#8C8C8C",
            "#FFCB57", "#9AD2F2", "#2CFFC6", "#F6EF8E",
            "#38B7FF", "#FF9B4D", "#E0AFCA", "#A3A3A3",
            "#8A5F00", "#1674A9", "#005F45", "#AA9F0D",
            "#00446B", "#803800", "#8D3666", "#3D3D3D"
        )
    )
    if (n > length(palette)) {
        stop("n is too large for ", palette_name, " palette")
    }
    palette[seq_len(n)]

}


