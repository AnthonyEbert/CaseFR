#' @export
#' @import dplyr
generate_coronadf <- function(
  Country,
  Province,
  L,
  enddate = NULL,
  usealpha3 = FALSE,
  Country2 = Country,
  Province2 = Province){

  label1 <- paste(Country, Province, sep = "-")
  label2 <- paste(Country2, Province2, sep = "-")

  if(usealpha3){
    covid19_data <- COVID19data::covid19_sorted %>%
      ungroup() %>%
      mutate(
        Country.Region = paste(alpha3, Province.State, sep = "-")
      )
  } else {
    covid19_data <- COVID19data::covid19_sorted %>%
      ungroup() %>%
      mutate(
        Country.Region = paste(Country.Region, Province.State, sep = "-")
      )
  }

  covid19_data <- covid19_data %>%
    filter(Country.Region %in% c(label1, label2))


  # if(usealpha3){
  #   covid19_data <- COVID19data::covid19_sorted %>%
  #     filter(alpha3 %in% c(Country, Country2), Province.State %in% c(Province, Province2)) %>%
  #     ungroup() %>%
  #     mutate(
  #       Country.Region = paste(alpha3, Province.State, sep = "-")
  #     )
  # } else if(!usealpha3){
  #   covid19_data <- COVID19data::covid19_sorted %>%
  #     filter(Country.Region %in% c(Country, Country2), Province.State %in% c(Province, Province2))
  # }

  if(!is.null(enddate)){
    covid19_data <- covid19_data %>% filter(date <= enddate)
  }

  covid19_data <- covid19_data %>%
    group_by(Country.Region) %>%
    padr::pad(group = "Country.Region") %>%
    arrange(date) %>%
    do(data.frame(time = 1:I(length(.$recovered)-1), recovered = diff(.$recovered), deaths = diff(.$deaths), confirmed = diff(.$confirmed))) %>%
    ungroup()

  covid19_data$Country.Region <- forcats::fct_recode(covid19_data$Country.Region, `1` = label1, `2` = label2) %>% as.character() %>% as.numeric()

  covid19_data <- covid19_data %>% arrange(Country.Region, time)

  jh_mat1 <- as.matrix(covid19_data)

  if(Country == Country2 && Province == Province2){
    len = dim(jh_mat1)[1]
    ref = simulate_reference_distribution(jh_mat1,T=len)
    jh_mat = rbind(ref,jh_mat1)
  } else {
    jh_mat = jh_mat1
  }

  # jh_mat2 <- as.matrix(covid19_data)
  #
  # jh_mat2[,1] <- 2
  #
  # jh_mat <- rbind(jh_mat1, jh_mat2)
  #
  jh_mat <- jh_mat[,c(2,1,3,4,5)]

  colnames(jh_mat) <- c("time", "grp", "R", "D", "N")

  # # Pad with L zeros. This is to protect against assumed.nu being too big.
  # # Meanwhile, align the curves by aligning the beginning of each outbreak.
  # time_orig = dim(jh_mat)[1]/2
  # c1 = jh_mat[1:time_orig,]
  # c2 = jh_mat[(time_orig+1):(time_orig*2),]
  # #TODO: Curve alignment:
  # #t1 = min(which(c1[,"N"]>100))
  # #t2 = min(which(c2[,"N"]>100))
  # c1[,"time"] = c1[,"time"] + L
  # c2[,"time"] = c2[,"time"] + L
  # z1 = cbind(c(1:L),rep(1,L),rep(0,L),rep(0,L),rep(0,L))
  # z2 = cbind(c((1:L)),rep(2,L),rep(0,L),rep(0,L),rep(0,L))
  # jh_mat = rbind(z1,c1,z2,c2)
  return(jh_mat)
}

