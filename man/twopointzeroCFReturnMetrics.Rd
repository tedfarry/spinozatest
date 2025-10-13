\name{twopointzeroCFReturnMetrics}
\alias{twopointzeroCFReturnMetrics}
\title{Calculate Cash Flow Return Metrics (Version 2.0)}
\description{
Enhanced version of cash flow return metrics calculator with additional features including flexible exit value types, acquisition/asset management fees, and loan-to-cost ratio support.
}
\usage{
twopointzeroCFReturnMetrics(RentAmt, RentAppRate, ValAppRate, ATenStay, DLQ, 
                            PMFee, LC, InsRate, TurnCost, CapexMaint, taxrate, 
                            ppval, hoamonthly, sqft, HoldPeriod, RehabVal, 
                            CCRate, SaleCC, IntRate, Amort, Points, ARV, 
                            VacRate, LTC, RenewalRate, AcqFee, AMFee, DispFee, 
                            ExitValueType, ExitCapRate)
}
\arguments{
  \item{RentAmt}{Monthly rent amount}
  \item{RentAppRate}{Annual rent appreciation rate (as percentage)}
  \item{ValAppRate}{Annual property value appreciation rate (as percentage)}
  \item{ATenStay}{Average tenant stay in months}
  \item{DLQ}{Delinquency rate (as percentage)}
  \item{PMFee}{Property management fee (as percentage)}
  \item{LC}{Leasing commission (as percentage)}
  \item{InsRate}{Insurance rate (as percentage)}
  \item{TurnCost}{Cost per square foot for turnover}
  \item{CapexMaint}{Annual capital expenditures and maintenance cost}
  \item{taxrate}{Property tax rate (as percentage)}
  \item{ppval}{Purchase price value}
  \item{hoamonthly}{Monthly HOA dues}
  \item{sqft}{Square footage of property}
  \item{HoldPeriod}{Number of years to hold the property}
  \item{RehabVal}{Rehabilitation budget}
  \item{CCRate}{Closing costs rate (as percentage)}
  \item{SaleCC}{Sale closing costs (as percentage)}
  \item{IntRate}{Annual interest rate (as percentage)}
  \item{Amort}{Amortization period in years (0 for interest-only)}
  \item{Points}{Loan points (as percentage)}
  \item{ARV}{After-repair value}
  \item{VacRate}{Vacancy rate (as percentage)}
  \item{LTC}{Loan-to-cost ratio (as percentage)}
  \item{RenewalRate}{Annual cost of tenant renewal}
  \item{AcqFee}{Acquisition fee (as percentage)}
  \item{AMFee}{Annual asset management fee (as percentage)}
  \item{DispFee}{Disposition fee (as percentage)}
  \item{ExitValueType}{Type of exit valuation ("Cap Rate" or other)}
  \item{ExitCapRate}{Exit capitalization rate (as percentage) if ExitValueType is "Cap Rate"}
}
\value{
Returns a JSON string containing:
\item{IRR}{Internal Rate of Return}
\item{TotalReturn}{Total cash return over hold period}
\item{GrossYield}{Gross rental yield}
\item{PurchaseCap}{Purchase capitalization rate}
\item{EquityMultiple}{Equity multiple on investment}
}
\examples{
\dontrun{
result <- twopointzeroCFReturnMetrics(
  RentAmt = 1500,
  RentAppRate = 5,
  ValAppRate = 5,
  ATenStay = 24,
  DLQ = 2,
  PMFee = 8,
  LC = 8,
  InsRate = 0.5,
  TurnCost = 2,
  CapexMaint = 1000,
  taxrate = 1.2,
  ppval = 200000,
  hoamonthly = 50,
  sqft = 1500,
  HoldPeriod = 10,
  RehabVal = 20000,
  CCRate = 2.5,
  SaleCC = 6,
  IntRate = 8,
  Amort = 30,
  Points = 0,
  ARV = 250000,
  VacRate = 5,
  LTC = 80,
  RenewalRate = 500,
  AcqFee = 4,
  AMFee = 2,
  DispFee = 3,
  ExitValueType = "Market",
  ExitCapRate = 5
)
}
}
\keyword{finance}
