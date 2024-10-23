## code to prepare `interactome_hs` dataset goes here

destfile <- "C:/Users/WKS/Downloads/STRINGDB_intractome.HS.txt.gx"
download.file("https://stringdb-downloads.org/download/protein.links.detailed.v12.0/9606.protein.links.detailed.v12.0.txt.gz",
              destfile = destfile)

interactome_hs <- build_interactome(directory_interactome = destfile,tax_ID = 9606)
interactome_hs$neighborhood_transferred <- NULL
interactome_hs$coexpression_transferred <- NULL

usethis::use_data(interactome_hs, overwrite = TRUE, compress = "xz")

