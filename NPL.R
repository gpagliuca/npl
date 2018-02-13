
## Libraries
library(sqldf)

## Constants
url <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
out_dir<-"data"
data_path <- "data/final/"
working_path <- "data/sampled/"


## Raw Data downloading
get_data <- function() {
    if (!file.exists("Coursera-SwiftKey.zip")) {
        download.file(url, "Coursera-SwiftKey.zip")
    }
    if (!dir.exists(out_dir)) {
        unzip("Coursera-SwiftKey.zip", exdir=out_dir)
    }
    
}

## Sampling data
sampling_file <- function(fname, sample_size) {
    set.seed(42)
    lang <- unlist(strsplit(fname, "\\."))[1]
    fobj <- file(paste(data_path, lang, '/', fname, sep=""),"r")
    file <- readLines(fobj)
    sample_file <- file[rbinom(n = length(file), size = 1, prob = sample_size) == 1]
    close(fobj)
    sampled_fobj <- file(paste(working_path, 
                               as.character(sample_size), 
                               '.', 
                               fname, 
                               sep=""), "w")
    writeLines(sample_file, con = sampled_fobj)
    close(sampled_fobj)
}

sampling <- function(sample_size=0.01) {
    print('Sampling en_EN')
    sampling_file('en_US.blogs.txt',   sample_size)
    sampling_file('en_US.news.txt',    sample_size)
    sampling_file('en_US.twitter.txt', sample_size)
    print('Sampling de_DE')
    sampling_file('de_DE.blogs.txt',   sample_size)
    sampling_file('de_DE.news.txt',    sample_size)
    sampling_file('de_DE.twitter.txt', sample_size)
    print('Sampling fi_FI')
    sampling_file('fi_FI.blogs.txt',   sample_size)
    sampling_file('fi_FI.news.txt',    sample_size)
    sampling_file('fi_FI.twitter.txt', sample_size)
    print('Sampling ru_RU')
    sampling_file('ru_RU.blogs.txt',   sample_size)
    sampling_file('ru_RU.news.txt',    sample_size)
    sampling_file('ru_RU.twitter.txt', sample_size)
}

## Sampled data loading

loading_sampled <- function(lang, type, sample_size) {
    # English
    if (lang=='en' & type =='blog') {
        return(readLines(paste(working_path, sample_size, "_en_US.blogs.txt", sep='')))
    }
    if (lang=='en' & type =='news') {
        return(readLines(paste(working_path, sample_size, "_en_US.news.txt", sep='')))
    }
    if (lang=='en' & type =='twitter') {
        return(readLines(paste(working_path, sample_size, "_en_US.twitter.txt", sep='')))
    }
    # German
    if (lang=='de' & type =='blog') {
        return(readLines(paste(working_path, sample_size, "_de_DE.blogs.txt", sep='')))
    }
    if (lang=='de' & type =='news') {
        return(readLines(paste(working_path, sample_size, "_de_DE.news.txt", sep='')))
    }
    if (lang=='de' & type =='twitter') {
        return(readLines(paste(working_path, sample_size, "_de_DE.twitter.txt", sep='')))
    }
    # Finnish
    if (lang=='fi' & type =='blog') {
        return(readLines(paste(working_path, sample_size, "_fi_FI.blogs.txt", sep='')))
    }
    if (lang=='fi' & type =='news') {
        return(readLines(paste(working_path, sample_size, "_fi_FI.news.txt", sep='')))
    }
    if (lang=='fi' & type =='twitter') {
        return(readLines(paste(working_path, sample_size, "_fi_FI.twitter.txt", sep='')))
    }
    # Russian
    if (lang=='ru' & type =='blog') {
        return(readLines(paste(working_path, sample_size, "_ru_RU.blogs.txt", sep='')))
    }
    if (lang=='ru' & type =='news') {
        return(readLines(paste(working_path, sample_size, "_ru_RU.news.txt", sep='')))
    }
    if (lang=='ru' & type =='twitter') {
        return(readLines(paste(working_path, sample_size, "_ru_RU.twitter.txt", sep='')))
    }
}


#-------------------------------------------------------------------------------
## Main

# Sampling the data
sampling(0.01)

# Loading sampled data
en_blog <- loading_sampled('en', 'blog', 0.01)
en_news <- loading_sampled('en', 'news', 0.01)
en_tw   <- loading_sampled('en', 'twitter', 0.01)

de_blog <- loading_sampled('de', 'blog', 0.01)
de_news <- loading_sampled('de', 'news', 0.01)
de_tw   <- loading_sampled('de', 'twitter', 0.01)

fi_blog <- loading_sampled('fi', 'blog', 0.01)
fi_news <- loading_sampled('fi', 'news', 0.01)
fi_tw   <- loading_sampled('fi', 'twitter', 0.01)

ru_blog <- loading_sampled('ru', 'blog', 0.01)
ru_news <- loading_sampled('ru', 'news', 0.01)
ru_tw   <- loading_sampled('ru', 'twitter', 0.01)



























