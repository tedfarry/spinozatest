twopointzeroCFReturnMetrics <- function(RentAmt, RentAppRate, ValAppRate, ATenStay, DLQ, PMFee, LC, InsRate, TurnCost, CapexMaint, taxrate, ppval, hoamonthly, sqft, HoldPeriod, RehabVal, CCRate, SaleCC, IntRate, Amort, Points, ARV, VacRate, LTC, RenewalRate, AcqFee, AMFee, DispFee, ExitValueType, ExitCapRate) {
  
  CCval <- ppval*(CCRate/100)
  TotInv <- ppval + RehabVal + CCval
  
  PropVal <- c(ARV,ARV*(1+(ValAppRate/100))^(0:(HoldPeriod-1)))
  TaxVal <- c(0,(ppval*(1+(ValAppRate/100))^(0:(HoldPeriod-1))))
  
  RentCF <- c(0,RentAmt*(1+(RentAppRate/100))^(0:(HoldPeriod-1))*12)
  VacCF <- RentCF*(VacRate/100)
  DelCF <- RentCF*(DLQ/100)
  GICF <- RentCF - VacCF - DelCF
  PMFCF <- GICF*(PMFee/100)
  LCCF <- c(0, ((RentCF[2:(HoldPeriod+1)]*LC)+((ATenStay-1)*RenewalRate*12))/(ATenStay*12))
  InsCF <- c(0,rep(((TotInv/100)*(InsRate/100)),HoldPeriod))
  MaCeCF <- c(0,rep((((TurnCost*sqft)/ATenStay)+CapexMaint),HoldPeriod))
  TaxCF <- ((taxrate/100)*TaxVal)
  HoaCF <- c(0,rep((hoamonthly*12),HoldPeriod))
  ToteCF <- PMFCF + LCCF + InsCF + MaCeCF + TaxCF + HoaCF
  IncCF <- GICF - ToteCF
  if (ExitValueType == "Cap Rate") {PropVal <- c(ARV,IncCF[2:(HoldPeriod+1)]/(ExitCapRate/100))}
  if (Amort == 0) {
        LoanPMT <- c(0, rep((TotInv * (LTC/100) * (1 + (Points/100))) * (IntRate/100), HoldPeriod))
        LoanAMT <- c(rep((TotInv * (LTC/100)) * (1 + (Points/100)), HoldPeriod + 1))
  } else {
        LoanPMT <- c(0,rep(FinCal::pmt(IntRate/1200, Amort*12, -(TotInv*(LTC/100))*(1+(Points/100)),0)*12,HoldPeriod))
        LoanAMT <- c((TotInv*(LTC/100))*(1+(Points/100)),FinCal::fv(IntRate/1200, (1:HoldPeriod)*12, -(TotInv*(LTC/100))*(1+(Points/100)), FinCal::pmt(IntRate/1200, Amort*12, -(TotInv*(LTC/100))*(1+(Points/100)),0)))}
  LevCF <- c(-TotInv + (TotInv * (LTC/100)), IncCF[2:HoldPeriod] - LoanPMT[2:HoldPeriod] - (TotInv*(AMFee/100)), (PropVal[HoldPeriod+1]*(1-((SaleCC+DispFee)/100)))+IncCF[(HoldPeriod+1)] - LoanPMT[(HoldPeriod+1)] - LoanAMT[(HoldPeriod + 1)] - (TotInv*(AMFee/100)))
  
  IRR <- FinCal::irr(LevCF)
  TotalReturn <- sum(LevCF)
  GrossYield <- (RentAmt*12)/(TotInv*(1+(AcqFee/100)))
  PurchaseCap <- IncCF[2]/(TotInv*(1+(AcqFee/100)))
  EquityMultiple <- ((sum(LevCF)/-LevCF[1])+1)
  
  MetricList <- rjson::toJSON(list(IRR = IRR, TotalReturn = TotalReturn, GrossYield = GrossYield, PurchaseCap = PurchaseCap, EquityMultiple = EquityMultiple))
  
  MetricList
}
