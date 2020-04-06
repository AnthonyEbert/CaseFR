#' @export
#' @import dplyr
generate_coronadf <- function(Country, Province, L){

  covid19_data <- COVID19data::covid19_sorted %>%
    filter(Country.Region == Country, Province.State == Province) %>%
    group_by(Country.Region, Province.State) %>%
    arrange(date) %>%
    do(data.frame(time = 1:I(length(.$recovered)-1), recovered = diff(.$recovered), deaths = diff(.$deaths), confirmed = diff(.$confirmed))) %>%
    ungroup() %>%
    select(-Province.State)

  covid19_data$Country.Region <- forcats::fct_recode(covid19_data$Country.Region, `1` = Country, `2` = Country) %>% as.character() %>% as.numeric()

  jh_mat1 <- as.matrix(covid19_data)

  len = dim(jh_mat1)[1]
  ref = simulate_reference_distribution(jh_mat1,T=len)
  jh_mat = rbind(ref,jh_mat1)

  # jh_mat2 <- as.matrix(covid19_data)
  #
  # jh_mat2[,1] <- 2
  #
  # jh_mat <- rbind(jh_mat1, jh_mat2)
  #
  jh_mat <- jh_mat[,c(2,1,3,4,5)]

  colnames(jh_mat) <- c("time", "grp", "R", "D", "N")

  # Pad with L zeros. This is to protect against assumed.nu being too big.
  # Meanwhile, align the curves by aligning the beginning of each outbreak.
  time_orig = dim(jh_mat)[1]/2
  c1 = jh_mat[1:time_orig,]
  c2 = jh_mat[(time_orig+1):(time_orig*2),]
  #TODO: Curve alignment:
  #t1 = min(which(c1[,"N"]>100))
  #t2 = min(which(c2[,"N"]>100))
  c1[,"time"] = c1[,"time"] + L
  c2[,"time"] = c2[,"time"] + L
  z1 = cbind(c(1:L),rep(1,L),rep(0,L),rep(0,L),rep(0,L))
  z2 = cbind(c((1:L)),rep(2,L),rep(0,L),rep(0,L),rep(0,L))
  jh_mat = rbind(z1,c1,z2,c2)
  return(jh_mat)
}

