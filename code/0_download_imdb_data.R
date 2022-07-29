
download_from_imdb <- function(url){
  options(timeout=600)
  download.file(url,
                file.path("raw_data/",basename(url)))
}

download_from_imdb("https://datasets.imdbws.com/name.basics.tsv.gz")
download_from_imdb("https://datasets.imdbws.com/title.akas.tsv.gz")
download_from_imdb("https://datasets.imdbws.com/title.basics.tsv.gz")
download_from_imdb("https://datasets.imdbws.com/title.crew.tsv.gz")
download_from_imdb("https://datasets.imdbws.com/title.episode.tsv.gz")
download_from_imdb("https://datasets.imdbws.com/title.principals.tsv.gz")
download_from_imdb("https://datasets.imdbws.com/title.ratings.tsv.gz")

