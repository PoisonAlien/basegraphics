browsers = c("opera", "safari", "explorer", "firefox", "chrome")

browsers_data = lapply(browsers, function(browser){
  browser_url = paste0("https://www.w3schools.com/browsers/browsers_", browser, ".asp")
  data = rvest::html_table(rvest::read_html(x = browser_url))
  data = lapply(data, data.table::setDT)
  names(data) = unlist(lapply(data, function(x) colnames(x)[1]))
  data = lapply(data, function(x) {colnames(x)[1] = "month"; x})
  data = data.table::rbindlist(l = data, use.names = TRUE, fill = TRUE, idcol = "year")
  #data = data[,.(year, month, Total)] |> data.table::dcast(formula = month ~ year, value.var = "Total", fill = 0)
  data[,.(year, month, Total)]
})

names(browsers_data) = browsers
browsers_data = data.table::rbindlist(l = browsers_data, use.names = TRUE, fill = TRUE, idcol = "browser")
#browsers_data$month = factor(browsers_data$month, levels = month.name)

browsers_data_fixed = lapply(split(browsers_data, browsers_data$year), function(yearly){
  yearly = lapply(split(yearly, yearly$month), function(monthly){
    if(nrow(monthly) > 0){
      if(sum(monthly$Total) < 100){
        unknown_brwsr = data.table::data.table(
          browser = "Other",
          year = monthly$year[1],
          month = as.character(monthly$month[1]),
          Total = 100 - sum(monthly$Total)
        )
        monthly = data.table::rbindlist(l = list(monthly, unknown_brwsr), use.names = TRUE, fill = TRUE)
      }
    }
    monthly
  })
  data.table::rbindlist(l = yearly, use.names = TRUE, fill = TRUE)
})
browsers_data = data.table::rbindlist(l = browsers_data_fixed, use.names = TRUE, fill = TRUE)
browsers_data[, date_obs := paste0(
  browsers_data$year,
  "-",
  factor(
    x = browsers_data$month,
    levels = month.name,
    labels = 1:12
  )
)]

data.table::fwrite(x = browsers_data, file = "data/15-pieanimate_browserWars_compileData.tsv", sep = "\t")
