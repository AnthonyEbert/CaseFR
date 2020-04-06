#' @export
absolute_CFR <- function(
  Country,
  Province,
  L = 2,
  enddate = NULL,
  usealpha3 = FALSE,
  DESIRED_L = 20,
  MTTD = 14,
  min.cases = 100,
  assumed.nu = NULL,
  output = "EM.rel.cfr",
  verb = FALSE,
  SEM.var = TRUE,
  max.iter = 2000,
  tol = 1e-04){

  if(is.null(assumed.nu)){
    assumed.nu = dnorm(1:DESIRED_L,mean=MTTD,sd=1)
    assumed.nu[assumed.nu<5e-2] = 0
    assumed.nu = assumed.nu/sum(assumed.nu)
  }

  data = generate_coronadf(Country, Province, DESIRED_L + L, enddate = enddate, usealpha3 = usealpha3)

  # This code fixes some errors in the JHU dataset (negative entries, all-zero rows).
  # data = reindex_time(data,DESIRED_L, min.cases)["data"][[1]]
  # data = data[ ((data[,"N"] > 0) | is.na(data[,"new.times"])) & (data[,"D"]>=0),  ]
  # data = data[,-6]
  out_list = reindex_time(data,DESIRED_L, min.cases = min.cases)
  data = out_list["data"][[1]]
  T = out_list["T"][[1]]
  first.t = out_list["first.t"][[1]]
  last.t = out_list["last.t"][[1]]
  total_time = dim(data)[1]/2

  alpha.start <- runif(T-1)

  cfr.out <- coarseDataTools::EMforCFR(assumed.nu = assumed.nu,
                      alpha.start.values = alpha.start,
                      full.data = data,
                      verb = verb,
                      SEM.var = SEM.var,
                      max.iter = max.iter,
                      tol = tol)

  if(is.null(output)){
    return_value <- cfr.out
  } else {
    return_value <- cfr.out[output]
  }

  return(return_value)
}
