
#-------------------------------------------------------------------------------
## Libraries
library(tokenizers)
library(ggplot2)
#library(tidytext)
#library(dplyr)
#library(tm)

#-------------------------------------------------------------------------------
## Constants
url <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
out_dir<-"data"
data_path <- "data/final/"
working_path <- "data/sampled/"

#-------------------------------------------------------------------------------
## Raw Data downloading
get_data <- function() {
    if (!file.exists("Coursera-SwiftKey.zip")) {
        download.file(url, "Coursera-SwiftKey.zip")
    }
    if (!dir.exists(out_dir)) {
        unzip("Coursera-SwiftKey.zip", exdir=out_dir)
    }
    
}

#-------------------------------------------------------------------------------
## Sampling data
sampling_file <- function(fname, sample_size) {
    set.seed(42)
    lang <- unlist(strsplit(fname, "\\."))[1]
    fobj <- file(paste(data_path, lang, '/', fname, sep=""),"r")
    file <- readLines(fobj, skipNul=TRUE)
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
    sampling_file('en_US.blogs.txt',   sample_size)
    sampling_file('en_US.news.txt',    sample_size)
    sampling_file('en_US.twitter.txt', sample_size)
    sampling_file('de_DE.blogs.txt',   sample_size)
    sampling_file('de_DE.news.txt',    sample_size)
    sampling_file('de_DE.twitter.txt', sample_size)
    sampling_file('fi_FI.blogs.txt',   sample_size)
    sampling_file('fi_FI.news.txt',    sample_size)
    sampling_file('fi_FI.twitter.txt', sample_size)
    sampling_file('ru_RU.blogs.txt',   sample_size)
    sampling_file('ru_RU.news.txt',    sample_size)
    sampling_file('ru_RU.twitter.txt', sample_size)
}

#-------------------------------------------------------------------------------
## Sampled data loading
loading_sampled <- function(lang, type, sample_size) {
    # English
    if (lang=='en' & type =='blog') {
        return(readLines(paste(working_path, sample_size, ".en_US.blogs.txt", sep='')))
    }
    if (lang=='en' & type =='news') {
        return(readLines(paste(working_path, sample_size, ".en_US.news.txt", sep='')))
    }
    if (lang=='en' & type =='twitter') {
        return(readLines(paste(working_path, sample_size, ".en_US.twitter.txt", sep='')))
    }
    # German
    if (lang=='de' & type =='blog') {
        return(readLines(paste(working_path, sample_size, ".de_DE.blogs.txt", sep='')))
    }
    if (lang=='de' & type =='news') {
        return(readLines(paste(working_path, sample_size, ".de_DE.news.txt", sep='')))
    }
    if (lang=='de' & type =='twitter') {
        return(readLines(paste(working_path, sample_size, ".de_DE.twitter.txt", sep='')))
    }
    # Finnish
    if (lang=='fi' & type =='blog') {
        return(readLines(paste(working_path, sample_size, ".fi_FI.blogs.txt", sep='')))
    }
    if (lang=='fi' & type =='news') {
        return(readLines(paste(working_path, sample_size, ".fi_FI.news.txt", sep='')))
    }
    if (lang=='fi' & type =='twitter') {
        return(readLines(paste(working_path, sample_size, ".fi_FI.twitter.txt", sep='')))
    }
    # Russian
    if (lang=='ru' & type =='blog') {
        return(readLines(paste(working_path, sample_size, ".ru_RU.blogs.txt", sep='')))
    }
    if (lang=='ru' & type =='news') {
        return(readLines(paste(working_path, sample_size, ".ru_RU.news.txt", sep='')))
    }
    if (lang=='ru' & type =='twitter') {
        return(readLines(paste(working_path, sample_size, ".ru_RU.twitter.txt", sep='')))
    }
}


# Loading sampled data
loading <- function(sample_size) {
    en_blog <- loading_sampled('en', 'blog', sample_size)
    en_news <- loading_sampled('en', 'news', sample_size)
    en_tw   <- loading_sampled('en', 'twitter', sample_size)
    de_blog <- loading_sampled('de', 'blog', sample_size)
    de_news <- loading_sampled('de', 'news', sample_size)
    de_tw   <- loading_sampled('de', 'twitter', sample_size)
    fi_blog <- loading_sampled('fi', 'blog', sample_size)
    fi_news <- loading_sampled('fi', 'news', sample_size)
    fi_tw   <- loading_sampled('fi', 'twitter', sample_size)
    ru_blog <- loading_sampled('ru', 'blog', sample_size)
    ru_news <- loading_sampled('ru', 'news', sample_size)
    ru_tw   <- loading_sampled('ru', 'twitter', sample_size)
    l <- list("en_blog"=en_blog, "en_news"=en_news, "en_tw"=en_tw, 
              "de_blog"=de_blog, "de_news"=de_news, "de_tw"=de_tw, 
              "fi_blog"=fi_blog, "fi_news"=fi_news, "fi_tw"=fi_tw, 
              "ru_blog"=ru_blog, "ru_news"=ru_news, "ru_tw"=ru_tw)
    return(l)
}

text_preprocessing <- function(text, cutoff) {
    text <- paste(text, collapse=' ')
    text <- gsub("[[:punct:]]", '', text)
    text <- gsub("[[:digit:]]", '', text)
    text <- strsplit(text, ' ')
    text <- as.data.frame(text)
    text <- text[nchar(as.character(text[, 1])) > cutoff, ]
    return(as.character(text))
}



# Merge text
merge_text <- function(d, cutoff=4) {
    # English
    print(' * Merging... English')
    merged_bl_en <- text_preprocessing(d$en_blog, cutoff)
    merged_nw_en <- text_preprocessing(d$en_news, cutoff)
    merged_tw_en <- text_preprocessing(d$en_tw, cutoff)
    merged_en <- c(merged_bl_en, merged_nw_en, merged_tw_en)
    
    # German
    print(' * Merging... German')
    merged_bl_de <- text_preprocessing(d$de_blog, cutoff)
    merged_nw_de <- text_preprocessing(d$de_news, cutoff)
    merged_tw_de <- text_preprocessing(d$de_tw, cutoff)
    merged_de <- c(merged_bl_de, merged_nw_de, merged_tw_de)
    
    # Finnish
    print(' * Merging... Finnish')
    merged_bl_fi <- text_preprocessing(d$fi_blog, cutoff)
    merged_nw_fi <- text_preprocessing(d$fi_news, cutoff)
    merged_tw_fi <- text_preprocessing(d$fi_tw, cutoff)
    merged_fi <- c(merged_bl_fi, merged_nw_fi, merged_tw_fi)
    
    # Russian
    print(' * Merging... Russian')
    merged_bl_ru <- text_preprocessing(d$ru_blog, cutoff)
    merged_nw_ru <- text_preprocessing(d$ru_news, cutoff)
    merged_tw_ru <- text_preprocessing(d$ru_tw, cutoff)
    merged_ru <- c(merged_bl_ru, merged_nw_ru, merged_tw_ru)
    
    l <- list('en'=merged_en,
              'de'=merged_de, 
              'fi'=merged_fi,
              'ru'=merged_ru)
    return(l)
}


# Calculate the frequencies
freq <- function(text, label) {
    text <- table(text)
    d <- sort(text, index.return = TRUE, decreasing = TRUE)
    d <- as.data.frame(d)
    names(d) <- c(label, 'Freq')
    return(d)
}

# plots --> unique words to partially cover the dictionary


#-------------------------------------------------------------------------------
## Main 
sample_size <- 0.05

## Sampling
print('Sampling... ')
#sampling(sample_size)
sampled_data <- loading(sample_size)

## Text merging
print('Merging text... ')
merged_text <- merge_text(sampled_data, cutoff=0)

## Frequencies
print('Calculating freqs... ')
freq_en <- freq(merged_text$en, 'English')
freq_de <- freq(merged_text$de, 'overall')
freq_fi <- freq(merged_text$fi, 'overall')
freq_ru <- freq(merged_text$ru, 'overall')
fr <- (cbind(head(freq_en, 10),
             head(freq_de, 10), 
             head(freq_fi, 10), 
             head(freq_ru, 10)))

## Plotting 
d1_df <- data.frame(English=100*cumsum(freq_en$Freq)/sum(freq_en$Freq))
d2_df <- data.frame(German= 100*cumsum(freq_de$Freq)/sum(freq_de$Freq))
d3_df <- data.frame(Finnish=100*cumsum(freq_fi$Freq)/sum(freq_fi$Freq))
d4_df <- data.frame(Russian=100*cumsum(freq_ru$Freq)/sum(freq_ru$Freq))

g <- ggplot() + 
    geom_line(data=d1_df, aes(x=1:nrow(d1_df), y=English), color='green') +
    geom_line(data=d2_df, aes(x=1:nrow(d2_df), y=German),  color='black') +
    geom_line(data=d3_df, aes(x=1:nrow(d3_df), y=Finnish), color='blue') +
    geom_line(data=d4_df, aes(x=1:nrow(d4_df), y=Russian), color='yellow') +
    geom_hline(yintercept=50, linetype="dashed", color = "red", size=1) +
    geom_hline(yintercept=80, linetype="dashed", color = "red", size=1)
print(g)   

## N-grams model

en_data <- paste0(merged_text$en)
















