
#' Title
#'
#' @param data data
#' @param years select the year to analysis
#' @param ages such as 'Age-standardized'
#' @param metrics such as 'Incidence'
#' @param measures look up the website
#' @param gender look up the website
#' @param rounds rounds
#' @param EAPC calculate EAPC
#' @param EC_change CALCULATE EC_change
#'
#' @return the data
#' @importFrom utils write.csv
#' @examples EC;extract_data(EC,1990)
#' @export extract_data


extract_data <- function(data, years = NULL, ages = 'All Ages',
                         metrics = 'Number', measures = 'Incidence',
                         gender = 'Both', rounds = 0, EAPC = F, EC_change = F) {
    # 对年份和性别进行筛选
    data <- data  |>
        dplyr::filter(age == ages,
               metric == metrics,
               measure == measures,
               sex == gender) |>
        dplyr::select(-age, -metric, -measure, -sex)

    if (!is.null(years)) {
        data <- data |> dplyr::filter(year %in% years)
    }

    if (EAPC) {
        # 计算EAPC
        data <- data |>
            dplyr::group_by(location) |>
            dplyr::mutate(y = log(val)) |>
            dplyr::summarize(EAPC = (exp(lm(y ~ year)$coefficients[2])-1) * 100,
                      LCI = (exp(lm(y ~ year)$coefficients[2] - 1.96 * summary(lm(y ~ year))$coefficients[2, 2]) - 1) * 100,
                      UCI = (exp(lm(y ~ year)$coefficients[2] + 1.96 * summary(lm(y ~ year))$coefficients[2, 2]) - 1) * 100) |>
            dplyr::mutate(across(c(EAPC, UCI, LCI), ~ round(., rounds)),
                   EAPC_CI = paste0("(", LCI, " to ", UCI, ")"),
                   EAPC_CI = paste0(format(EAPC, nsmall = rounds), "%", EAPC_CI, sep = "\n")) |>
            dplyr::select(location, EAPC, LCI, UCI, EAPC_CI)

    }else if (EC_change) {
        # 计算数值变化和区间
        data <- data |>
            dplyr::mutate(dplyr::across(c(val, lower, upper), round, rounds)) |>
            dplyr::mutate(Num_change = paste0("(", lower, " to ", upper, ")"),
                   Num_change = paste0(format(val, nsmall = rounds), "%", Num_change, sep = "\n")) |>
            dplyr::select(location, val,upper,lower, Num_change)

    } else {

        data$val <- round(data$val, rounds)
        data$lower <- round(data$lower, rounds)
        data$upper <- round(data$upper, rounds)

        newcolname <- paste("Num_", years)
        data[[newcolname]] <- paste0("(", data$lower, " to ", data$upper, ")")
        data[[newcolname]] <- paste(data$val, data[[newcolname]], sep = "\n")
        data <- data[,c("location", "val","upper","lower",newcolname)]


    }

    return(data)
}
