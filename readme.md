# R Workshop

A repository for my work during the R workshops hosted at INSAT.

### Note:

``` R
    save_pdf <- function(t, pdf_name) {
        path <- paste(
            "your/path/here",
            pdf_name,
            ".pdf",
            sep = ""
        )
        pdf(file = path)
        barplot(t)
        dev.off()
    }
```

Don't forget to update the path to where you want the PDF version of your plots to be saved! You will find this function at the top of _distributions.r_

**Enjoy!**
