#' Sidekick Calibration Data
#'
#' An example of the sidekick calibration in the Meadows V2 field site.
#'
#' @format ## `sidekick_cal` A data frame with 716 rows and 8 columns:
#' \describe{
#'   \item{tag_type}{Tag type detected}
#'   \item{tag_id}{Tag ID}
#'   \item{time_utc}{Time in UTC}
#'   \item{rssi}{Received signal strength indicator in dBm}
#'   \item{lat}{Latitude}
#'   \item{lon}{Longitude}
#'   \item{heading}{Cardinal direction}
#'   \item{antenna_angle}{Angle of antenna}
#' }
"sidekick_cal"

#' Meadows Deployment Data
#'
#' Tag deployment data for the Meadows V2 project
#'
#' @format ## `deployments` A data frame with 125 rows and 5 columns:
#'
#' \describe{
#'   \item{TagId}{Tag ID}
#'   \item{DeployDate}{Date tag was deployed in the field}
#'   \item{Species}{Alpha-numeric code for species}
#'   \item{TagType}{Tag Type: Power, Hybride, Life, Blu}
#'   \item{AntennaType}{1/8 wave or 1/4 wave}
#' }
"deployments"
