#' Download a REDCap report as a tibble
#'
#' @param redcapTokenName The name of the environment variable holding the REDCap API token
#' @param redcapUrl The base URL of the REDCap server
#' @param redcapReportId The ID of the REDCap report to download
#' @return A tibble containing the REDCap report data
#' @import httr
#' @examples
#' redcapTokenName = "REDCAP_TOKEN"
#' redcapUrl = "https://redcap.emory.edu/api/"
#' redcapReportId = "46524"
#' report_data = downloadRedcapReport(redcapTokenName, redcapUrl, redcapReportId)
#' }
#'
#' @export
downloadRedcapReport = function(redcapTokenName, redcapUrl, redcapReportId) {
  library(httr)
  token = Sys.getenv(redcapTokenName)
  formData = list(
    "token" = token,
    content = 'report',
    format = 'csv',
    report_id = redcapReportId,
    csvDelimiter = '',
    rawOrLabel = 'raw',
    rawOrLabelHeaders = 'raw',
    exportCheckboxLabel = 'false',
    returnFormat = 'csv'
  )
  response = POST(redcapUrl, body = formData, encode = "form")
  result = content(response, as = "text", encoding = "UTF-8")
  result_tbl = read.csv(text = result, stringsAsFactors = FALSE)
  return(result_tbl)
}
