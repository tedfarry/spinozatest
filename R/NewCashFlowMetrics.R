CFReturnMetrics <- function(RentAmt, RentAppRate, ValAppRate, TurnTime, ATenStay, DLQ, PMFee, LC, InsRate, TurnCost, CapexMaint, taxrate, ppval, hoamonthly, sqft, HoldPeriod, RehabVal, CCRate, SaleCC, DiscountToPurchase, LTV, IntRate, Amort, Points) {
 
  library("FinCal")
  library(rjson)
  

  CCval <- ppval*(CCRate/100)
  TotInv <- ppval + RehabVal + CCval

  PropVal <- c(ppval,(ppval*(1+(DiscountToPurchase/100)))*(1+(ValAppRate/100))^(0:(HoldPeriod-1)))
  TaxVal <- c(0,(ppval*(1+(ValAppRate/100))^(0:(HoldPeriod-1))))

  RentCF <- c(0,RentAmt*(1+(RentAppRate/100))^(0:(HoldPeriod-1))*12)
  VacCF <- RentCF*((TurnTime/12)/ATenStay)
  DelCF <- RentCF*(DLQ/100)
  GICF <- RentCF - VacCF - DelCF
  PMFCF <- GICF*(PMFee/100)
  LCCF <- (RentCF*(LC/100))/(ATenStay*12)
  InsCF <- c(0,rep(((TotInv/100)*(InsRate/100)),HoldPeriod))
  MaCeCF <- c(0,rep((((TurnCost*sqft)/ATenStay)+CapexMaint),HoldPeriod))
  TaxCF <- ((taxrate/100)*TaxVal)
  HoaCF <- c(0,rep((hoamonthly*12),HoldPeriod))
  ToteCF <- PMFCF + LCCF + InsCF + MaCeCF + TaxCF + HoaCF
  IncCF <- GICF - ToteCF
  LoanPMT <- c(0,rep(pmt(IntRate/1200, Amort*12, -(ppval*(LTV/100))*(1+(Points/100)),0)*12,HoldPeriod))
  LoanAMT <- c((ppval*(LTV/100))*(1+(Points/100)),fv(IntRate/1200, (1:HoldPeriod)*12, -(ppval*(LTV/100))*(1+(Points/100)), pmt(IntRate/1200, Amort*12, -(ppval*(LTV/100))*(1+(Points/100)),0)))
  LevCF <- c(-TotInv + LoanAMT[1], IncCF[2:HoldPeriod] - LoanPMT[2:HoldPeriod], (PropVal[HoldPeriod+1]*(1-(SaleCC/100)))+IncCF[(HoldPeriod+1)] - LoanPMT[(HoldPeriod+1)] - LoanAMT[(HoldPeriod+1)])

  IRR <-irr(LevCF)
  TotalReturn <-sum(LevCF)
  GrossYield <- (RentAmt*12)/ppval
  NetYield <- IncCF[2]/TotInv
  EquityMultiple <- ((sum(LevCF)/-LevCF[1])+1)

  MetricList <- toJSON(list(IRR = IRR, TotalReturn = TotalReturn, GrossYield = GrossYield, NetYield = NetYield, EquityMultiple = EquityMultiple))

  MetricList
}


