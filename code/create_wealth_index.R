# this function serves to create the wealth index to be used in the regression

create_wealth_index <- function(df){

    wealth_df <- df %>%
        filter(!is.na(w1_best_mthpid) & !is.na(w1_best_fthpid) |
                   !is.na(w1_best_mthpid) |
                   !is.na(w1_best_fthpid)) %>%  # filters for those observations where at least one pid is present
        filter(w1_best_age_yrs >=14 & w1_best_age_yrs <=23) %>%  # filters for observations in target age group
        select(pid, w1_best_mthpid, w1_best_fthpid, w1_hhid, w1_best_age_yrs) # keep only the relevant identifier columns

    # create separate df for those observations which have values for father pid to extract value of father's asset ownership
    father_pid_df <- wealth_df %>%
        filter(!is.na(w1_best_fthpid))


# now join info from main df (containing info on asset ownership) to this df of father pids
    join_fthdf <- father_pid_df %>%
        left_join(w1 %>%
                      rename("w1_best_fthpid" = "pid"),
                  by = "w1_best_fthpid") #%>%  # joining by the id of the father
        #filter(w1_a_fthhh == 1) # only want those entries where pid stays with their father

    # create vector of all columns containing relevant asset ownership information (resale value of assets)

    asset_ownership_columns <- c("w1_a_ownrad_v", "w1_a_ownvehpri_v", "w1_a_ownmot_v", "w1_a_owncom_v", "w1_a_owncel_v")

    # just want info relevant to asset ownership of father and ids
    join_fthdf <- join_fthdf %>%
        select(all_of(asset_ownership_columns), pid, w1_best_mthpid, w1_best_fthpid)

    # now also want to add information from mother's asset ownership

    # create separate df for those observations which have values for mother pid to extract value of mother's asset ownership
    mother_pid_df <- wealth_df %>%
        filter(!is.na(w1_best_mthpid))

    join_mthdf <- mother_pid_df %>%
        left_join(w1 %>%
                      rename("w1_best_mthpid" = "pid"),
                  by = "w1_best_mthpid") #%>%  # joining by the id of the mother
        #filter(w1_a_mthhh == 1) # only want those entries where pid stays with their mother

    # just want info relevant to asset ownership of mother and ids
    join_mthdf <- join_mthdf %>%
        select(all_of(asset_ownership_columns), pid, w1_best_mthpid, w1_best_fthpid)

    join_mthdf

    # join info from mother and father together into one df

    asset_df <- join_fthdf %>%
        bind_rows(join_mthdf) %>%
        nids_miss(vars = c("w1_a_ownrad_v", "w1_a_ownvehpri_v", "w1_a_ownmot_v", "w1_a_owncom_v", "w1_a_owncel_v", "w1_best_fthpid", "w1_best_mthpid")) %>%  # code missings as NA
        group_by(pid) %>% # want to total assets for parents of a particular pid
        summarise(total_rad = sum(w1_a_ownrad_v, na.rm = T),
                  total_vehpriv = sum(w1_a_ownvehpri_v, na.rm = T),
                  total_mot = sum(w1_a_ownmot_v, na.rm = T),
                  total_com = sum(w1_a_owncom_v, na.rm = T),
                  total_cel = sum(w1_a_owncel_v, na.rm = T))

    # now want to get nr of parents and kids living in hh
    # just select col containing info of nr of kids living in house (only mothers were asked this Q)
    kids_df <- w1 %>%
        select(pid, w1_a_bhlive_n)

    # add mthpid back into wealth_df for later merging

    asset_df <- asset_df %>%
        left_join(w1_best %>% select(pid, w1_best_mthpid, w1_best_fthpid),
                  by = "pid")

    # now join kids_df to this

    asset_df <- asset_df %>%
        left_join(kids_df %>%
                      rename("w1_best_mthpid" = "pid"),
                  by = "w1_best_mthpid")

    # now want to get nr of parents in hh using the parent ids

    asset_df <- asset_df %>%
        mutate(nr_of_parents = rowSums(!is.na(across(c("w1_best_mthpid", "w1_best_fthpid"))))) %>%
        mutate(nr_hh_memb = ifelse(is.na(w1_best_mthpid) & !is.na(w1_best_fthpid),
                                   nr_of_parents + 1,
                                   nr_of_parents + w1_a_bhlive_n))   # this creates 920 missing variables due to missing values for nr of kids

    # bear with me as the following code all works to specify the nr of hh members of these 920 missings


    final_df <- asset_df %>%
        mutate(total_assets = rowSums(across(c("total_rad", "total_vehpriv", "total_mot", "total_com", "total_cel")))) %>%
        mutate(total_assets_pc = total_assets/nr_hh_memb)

    final_df
}

# join_test <- create_wealth_index(w1_best)
#
# join_test2 <- join_test %>%
#     filter(is.na(nr_hh_memb))
#
# join_test3 <- join_test2 %>%
#     mutate(parent_combo = paste(w1_best_mthpid, w1_best_fthpid, sep = "_")) %>%  # Combine parent IDs
#     group_by(parent_combo) %>%
#     mutate(n_siblings = n(),                            # total children with same parents
#            hh_members = nr_of_parents + n_siblings) %>%  # parents + siblings
#     ungroup()
