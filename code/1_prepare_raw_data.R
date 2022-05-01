library(data.table)

# data: average rating, numvotes
ratings <- fread("raw_data/data.tsv")
#setnames(season_episode_map, old = c("parentTconst","tconst"), new = c("show_id","episode_id"))

# data-3: directors writers
#dir_writers <- fread("raw_data/data-3.tsv",na.strings = '\\N')

## data-4: type, title, startyear, runtime, genres
dat <- fread("raw_data/data-4.tsv")

# Subset into TV series
tv_shows <- dat[titleType %in% c("tvSeries")]
setnames(tv_shows, old = c("tconst"), new = c("show_id"))

# Subset episodes of series
episodes <- dat[titleType %in% c("tvEpisode")]
setnames(episodes, old = c("tconst"), new = c("episode_id"))

## data-5: persons (nconst as ID, names, professions, title mapping)
#people <- fread("raw_data/data-5.tsv",nrows=10000)

## data-6: title translations
#transl <- fread("raw_data/data-6.tsv",nrows = 10000)

## data-7: person to title mapping
#people_title_mapping <- fread("raw_data/data-7.tsv",nrows = 1000000)

## data-2: season number episode number
season_episode_map <- fread("raw_data/data-2.tsv")
setnames(season_episode_map, old = c("parentTconst","tconst"), new = c("show_id","episode_id"))
season_episode_map <- season_episode_map[,seasonNumber := ifelse(seasonNumber == "\\N",NA,seasonNumber)
                   ][,c("episodeNumber","seasonNumber") := .(as.numeric(episodeNumber), as.numeric(seasonNumber))][!is.na(episodeNumber) &!is.na(seasonNumber) ]

## Prepare episode-level data
tv_episodes <- season_episode_map[tv_shows, on = "show_id"]

# Attach ratings
tv_episodes <- ratings[tv_episodes, on = .(tconst = episode_id)][, .SD[!(any(is.na(averageRating)) | any(is.na(numVotes)))], by = show_id]

# Convert ep and season numbers
tv_episodes <- tv_episodes[, c("episodeNumber","seasonNumber") := .(as.numeric(episodeNumber),
                                                                    as.numeric(seasonNumber))
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
tv_episodes[,season := as.factor(seasonNumber)]

# Save episode-level data
saveRDS(tv_episodes,"data/episodes.rds",compress = "xz")
saveRDS(tv_episodes,"code/shiny/episodes.rds",compress = "xz")

## Prepare season-level data
tv_seasons_means <- tv_episodes[,.(season_mean = mean(averageRating),
                                  season_sd = sd(averageRating)),by=c("show_id","primaryTitle","seasonNumber")][order(show_id,seasonNumber)]
# Save show-level data
saveRDS(tv_seasons_means,"data/seasons.rds",compress = "xz")



## Prepare show-level data
# Add ratings
tv_shows_full <- ratings[tv_shows, on = .(tconst = show_id)]
setnames(tv_shows_full, old = c("tconst"), new = c("show_id"))

# Subset by available nonNA votes
tv_shows_full <- tv_shows_full[, .SD[!(any(is.na(averageRating)) | any(is.na(numVotes)))], by = show_id]

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

# Save show-level data
saveRDS(tv_shows_full,"data/shows.rds",compress = "xz")


#### Prepare slopes

### Look at the slopes

slopes <- tv_seasons_means[,list(intercept=coef(lm(season_mean~seasonNumber))[1],
                        coef=round(coef(lm(season_mean~seasonNumber))[2],2))
                  ,by=c("show_id","primaryTitle")]

slopes_atleast2s <- slopes[,.(show_id,coef)]

shows_slopes <- slopes_atleast2s[tv_shows_full, on = "show_id"
                                 ][n_of_seasons > 1
                                   ][,startYear := as.numeric(startYear)
                                     ][,runtimeMinutes := as.numeric(runtimeMinutes)
                                       ][!is.na(coef) & !is.na(runtimeMinutes)]

saveRDS(shows_slopes,"data/slopes.rds",compress = "xz")
saveRDS(shows_slopes,"code/shiny/slopes.rds",compress = "xz")


#saveRDS(shows_slopes,"data/slopes.rds",compress = "xz")







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

