library(data.table)

# data: average rating, numvotes
ratings <- fread("raw_data/title.ratings.tsv.gz")
#setnames(season_episode_map, old = c("parentTconst","tconst"), new = c("show_id","episode_id"))

# directors writers
#dir_writers <- fread("raw_data/data-3.tsv",na.strings = '\\N')

## type, title, startyear, runtime, genres
basics <- fread("raw_data/title.basics.tsv.gz")

# Subset TV series as entities
tv_shows <- basics[titleType %in% c("tvSeries","tvMiniSeries")]
setnames(tv_shows, old = c("tconst"), new = c("show_id"))

# Custom drop
tv_shows <- tv_shows[!(show_id %in% c("tt0132666"))]

# Subset episodes of series
episodes <- basics[titleType %in% c("tvEpisode")]
setnames(episodes, old = c("tconst"), new = c("episode_id"))

## data-5: persons (nconst as ID, names, professions, title mapping)
# https://datasets.imdbws.com/name.basics.tsv.gz
#people <- fread("raw_data/data-5.tsv",nrows=10000)

## data-6: title translations
#transl <- fread("raw_data/data-6.tsv",nrows = 10000)

## data-7: person to title mapping
#people_title_mapping <- fread("raw_data/data-7.tsv",nrows = 1000000)

## Data: season number episode number
season_episode_map <- fread("raw_data/title.episode.tsv.gz")
setnames(season_episode_map, old = c("parentTconst","tconst"), new = c("show_id","episode_id"))

season_episode_map <- season_episode_map[,seasonNumber := ifelse(seasonNumber == "\\N",0,seasonNumber)
                   ][,c("episodeNumber","seasonNumber") := .(as.numeric(episodeNumber), as.numeric(seasonNumber))]

## Prepare episode-level data
tv_episodes <- season_episode_map[tv_shows, on = "show_id"]

# Attach ratings
tv_episodes <- ratings[tv_episodes, on = .(tconst = episode_id)
                       ][!is.na(averageRating) | !is.na(numVotes)]

# Convert ep and season numbers
tv_episodes <- tv_episodes[, c("episodeNumber","seasonNumber") := .(as.numeric(episodeNumber),
                                                                    as.numeric(seasonNumber))
                           ][!is.na(episodeNumber) | !is.na(seasonNumber)
                            ][order(show_id,seasonNumber,episodeNumber)]

tv_episodes <- tv_episodes[,episode_number_mutated := (episodeNumber) / (.N + 1),
                           by=c("show_id","seasonNumber")
                           ][,id := seasonNumber+episode_number_mutated]
setnames(tv_episodes, old = c("tconst"), new = c("episode_id"))


episodes_title <- episodes[,eptitle := primaryTitle][,.(episode_id,eptitle)]
tv_episodes <- episodes_title[tv_episodes, on = c("episode_id")]

tv_episodes <- tv_episodes[,.(show_id,episode_id,primaryTitle,seasonNumber,episodeNumber,eptitle,
                              averageRating,numVotes,startYear,endYear,runtimeMinutes,genres,episode_number_mutated,id)]

tv_episodes[,title := paste0(primaryTitle," (",startYear,")")]
tv_episodes[,id_prep := id-0.5]
tv_episodes <- tv_episodes[,season := as.factor(seasonNumber)
                           ][order(numVotes,decreasing = T,seasonNumber,episodeNumber)]

shows_by_votes <- tv_episodes[,.(sum_votes = sum(numVotes)),by=c("title,show_id")][sum_votes > 100]

tv_episodes <- tv_episodes[show_id %in% shows_by_votes$show_id]

tv_episodes <- shows_by_votes[tv_episodes, on = c("show_id","title")
                              ][order(sum_votes,decreasing = T)]

tv_episodes <- tv_episodes[!is.na(episodeNumber)]


## Prepare season-level data
#tv_seasons_means <- tv_episodes[,.(season_mean = mean(averageRating),
 #                                 season_sd = sd(averageRating)),by=c("show_id","primaryTitle","seasonNumber")]
# Save show-level data
#saveRDS(tv_seasons_means,"data/seasons.rds",compress = "xz")


## Prepare show-level data
# Add ratings
tv_shows <- tv_shows[show_id %in% tv_episodes$show_id]
tv_shows_full <- ratings[tv_shows, on = .(tconst = show_id)]
setnames(tv_shows_full, old = c("tconst"), new = c("show_id"))

# Subset by available nonNA votes
#tv_shows_full <- tv_shows_full[, .SD[!(any(is.na(averageRating)) | any(is.na(numVotes)))], by = show_id]
#tv_shows_full <- tv_shows_full[show_id %in% shows_by_votes$show_id]

# Add seasons/episodes info
shows_s_e_data <- tv_episodes[,.(n_of_seasons = max(seasonNumber),
                                 n_of_episodes = max(episodeNumber),
                                 total_episodes = .N), by = show_id]
tv_shows_full <- shows_s_e_data[tv_shows_full, on = "show_id"]

# Impute runtime by median
tv_shows_full[,runtimeMinutes := as.numeric(runtimeMinutes)]
rtmed <- median(tv_shows_full$runtimeMinutes, na.rm =T)
tv_shows_full[is.na(runtimeMinutes),runtimeMinutes := rtmed]

#Subset by available rating
tv_shows_full <- tv_shows_full[!is.na(averageRating) & !is.na(n_of_seasons) & !is.na(n_of_episodes)][,total_length := total_episodes * runtimeMinutes]

# Add title+year var
tv_shows_full[,title := paste0(primaryTitle," (",startYear,")")]

tv_episodes <- tv_episodes[show_id %in% tv_shows_full$show_id]

# Save episode-level data
saveRDS(tv_episodes,"data/episodes.rds",compress = "xz")
saveRDS(tv_episodes,"shiny/episodes.rds",compress = "xz")

# Save show-level data
#saveRDS(tv_shows_full,"data/shows.rds",compress = "xz")


#### Prepare slopes

### Look at the slopes

slopes <- tv_episodes[,list(intercept=coef(lm(averageRating~seasonNumber+episodeNumber))[1],
                        coef_season=round(coef(lm(averageRating~seasonNumber+episodeNumber))[2],2),
                        coef_episodes=round(coef(lm(averageRating~seasonNumber+episodeNumber))[3],2),
                        n_of_episodes = length(unique(episodeNumber)),
                        n_of_seasons = length(unique(seasonNumber)),
                        total_episodes = .N)
                  ,by=c("show_id","primaryTitle")
                      ][total_episodes > 1]

slopes_imputed <- slopes[,coef := ifelse(is.na(coef_season),coef_episodes,coef_season)
                         ][,.(show_id,intercept,coef)
                           ][intercept < 10 & intercept > 0 & coef > -5 & coef < 5]


shows_slopes <- slopes_imputed[tv_shows_full, on = "show_id"
                                 ][,startYear := as.numeric(startYear)
                                     ][,runtimeMinutes := as.numeric(runtimeMinutes)
                                       ][!is.na(coef) & !is.na(runtimeMinutes)]

shows_slopes <- shows_slopes[,genres := gsub("\\N","",genres,fixed = T)]

#library(fastDummies)

#temp <- dummy_cols(shows_slopes,split = ",",select_columns = "genres")


saveRDS(shows_slopes,"data/slopes.rds",compress = "xz")
saveRDS(shows_slopes,"shiny/slopes.rds",compress = "xz")







# Add people
# people_title_mapping_subset <- people_title_mapping[tconst %in% tv_shows_full$show_id]
# people_title_mapping_wide <- dcast(people_title_mapping_subset, tconst ~ nconst,)
# 
# # Add directors_writers
# dir_writers_subset <- dir_writers[tconst %in% tv_shows_full$show_id]
# dir_writers_wide <-  dcast(melt(dir_writers_subset[1:2,], id.vars='tconst'),sep = ",", tconst ~ variable + value, fun = length)
# dir_writers_wide <- melt(dir_writers_subset[1:2,], id.vars='tconst')
# tstrsplit(dir_writers_wide$value, split = ",")
# 
# dir_writers_wide <- dummy_cols(dir_writers_subset[1:10,],split = ",",select_columns = c("directors","writers"),remove_selected_columns = T)
# 
# 
# 
# #
# library(fastDummies)
# tv_shows_full <- dummy_cols(tv_shows_full,split = ",",select_columns = "genres")

