\name{CFReturnTable}
\alias{CFReturnTable}
\title{Generate Cash Flow Return Table for Salvage Properties}
\description{
Creates a formatted HTML table showing detailed year-by-year cash flow projections for salvage property investments.
}
\usage{
CFReturnTable(RentAmt, RentAppRate, ValAppRate, TurnTime, ATenStay, DLQ, 
              PMFee, LC, InsRate, TurnCost, CapexMaint, taxrate, ppval, 
              hoamonthly, sqft, HoldPeriod, RehabVal, CCRate, SaleCC, 
              DiscountToPurchase, LTV, IntRate, Amort, Points)
}
\arguments{
  \item{RentAmt}{Monthly rent amount}
  \item{RentAppRate}{Annual rent appreciation rate (as percentage)}
  \item{ValAppRate}{Annual property value appreciation rate (as percentage)}
  \item{TurnTime}{Tenant turnover time in months}
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
  \item{DiscountToPurchase}{Discount to purchase price (as percentage)}
  \item{LTV}{Loan-to-value ratio (as percentage)}
  \item{IntRate}{Annual interest rate (as percentage)}
  \item{Amort}{Amortization period in years}
  \item{Points}{Loan points (as percentage)}
}
\value{
Returns an HTML table (flextable object) showing year-by-year projections including:
\itemize{
  \item{Rent, Vacancy, and Delinquency}
  \item{Gross Income}
  \item{Operating expenses (PM fees, leasing, insurance, maintenance, taxes, HOA)}
  \item{Total Expenses and Net Income}
  \item{Property Value}
  \item{Outstanding Loan Balance and Loan Payments}
  \item{Levered Cash Flow}
}
}
\examples{
\dontrun{
table <- CFReturnTable(
  RentAmt = 1500,
  RentAppRate = 5,
  ValAppRate = 5,
  TurnTime = 2,
  ATenStay = 24,
  DLQ = 2,
  PMFee = 8,
  LC = 8,
  InsRate = 100,
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
  DiscountToPurchase = 5,
  LTV = 80,
  IntRate = 4.5,
  Amort = 30,
  Points = 0
)
}
}
\keyword{finance}
