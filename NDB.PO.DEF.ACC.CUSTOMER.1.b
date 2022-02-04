 @ValidationCode : MjotMTg3NzQxMzg0ODpDcDEyNTI6MTYzNTEzNzU4OTAwNTpzdXNoZTotMTotMTowOjA6ZmFsc2U6Ti9BOlIyMF9TUDEuMDotMTotMQ==
* @ValidationInfo : Timestamp         : 25 Oct 2021 10:23:09
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : sushe
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R20_SP1.0
*-----------------------------------------------------------------------------
*Developer   : Chandrashekar
*Description : Calculated the pre close amount
*Attached to ACTIVITY.API   ACTIVITY:-DEPOSITS-INITIATE.PRECLOSURE-ARRANGEMENT
*                           PROPERTY:-PRETERMINATION
*                           ACTION  :-UPDATE
*                           ROUTINE :-Pre-Routine
*-----------------------------------------------------------------------------
$PACKAGE EB.NDBACRetro
*-----------------------------------------------------------------------------
SUBROUTINE NDB.AA.GET.PREFEE
*-----------------------------------------------------------------------------
*  Modification History :
* 25 Oct 2021   Susheel Kumar   Fix for PAY.CAPITAL
*
*-----------------------------------------------------------------------------
*Inserts:
*
    $USING EB.SystemTables
    $USING EB.DataAccess
    $USING EB.Reports
    $USING AC.AccountOpening
    $USING ST.Customer
    $USING AA.Framework
    $USING AA.Fees
    $USING AA.TermAmount
    $USING AA.Interest
    $USING AA.ChargeOff
    $USING AA.ChangeProduct
    $USING AA.ProductFramework
    $USING AA.PaymentSchedule
    $USING AA.Account
    $USING EB.Updates
    $USING EB.API
    $USING EB.ErrorProcessing
    $USING EB.Foundation
    $USING EB.Interface
    $USING AA.BalanceMaintenance
    $USING AA.Settlement
    $USING EB.OverrideProcessing
    $USING AA.Tax
    $USING CG.ChargeConfig
    $USING AA.Customer
    $USING FT.Contract
*-----------------------------------------------------------------------------
*Main:
*
    YRecordStatus = AA.Framework.getC_aalocactivitystatus()
    IF YRecordStatus EQ 'UNAUTH' THEN
        GOSUB INITIALISE
        GOSUB VAL.PRECLOSE.INDI
        CALL REBUILD.SCREEN
    END

RETURN
*-----------------------------------------------------------------------------
INITIALISE:
*
    Y.ARR.ID = ""; Y.FLAG = '0'; Y.PAY.CUST = '0'
    D.PROP = "" ; RET.ID = "" ; Y.AA.ACCOUNT = ""
    RET.COND = ""; RET.ERR = "";  Y.FLAG.REV = '0'
    YTotInt = "";YPrecloseAmount =""

    Y.ARR.ID = AA.Framework.getArrId()
    YAA.REC = AA.Framework.Arrangement.Read(Y.ARR.ID, Error)
    YACCT.ID = YAA.REC<AA.Framework.Arrangement.ArrLinkedApplId>
    YDebitAcctCcy = YAA.REC<AA.Framework.Arrangement.ArrCurrency>
    YActivityRec = AA.Framework.getRArrangementActivity()
    YDepsoitCcy = YActivityRec<AA.Framework.ArrangementActivity.ArrActCurrency>
    YDebitAcct = YACCT.ID
    
    FN.AA.ACC.DETAILS = 'F.AA.ACCOUNT.DETAILS'
    F.AA.ACC.DETAILS = ''
    EB.DataAccess.Opf(FN.AA.ACC.DETAILS,F.AA.ACC.DETAILS)

    FN.AA.INT.ACCR = 'F.AA.INTEREST.ACCRUALS'
    F.AA.INT.ACCR = ''
    EB.DataAccess.Opf(FN.AA.INT.ACCR,F.AA.INT.ACCR)

    FN.AA.ARR = 'F.AA.ARRANGEMENT'
    F.AA.ARR = ''
    EB.DataAccess.Opf(FN.AA.ARR,F.AA.ARR)
    
    FN.TAX = "F.TAX"
    F.TAX = ""
    EB.DataAccess.Opf(FN.TAX, F.TAX)
    
    Y.APPLICATION = 'AA.ARR.CHARGE':@FM:'AA.ARR.INTEREST':@FM:'CUSTOMER':@FM:'AA.ARR.SETTLEMENT'
    Y.FIELDS = 'NDB.PENAL.RATE':@VM:'NDB.PRECLS.IND':@FM:'NDB.PREMIGINAMT'@FM:'TAX.CODE':@FM:'CUST.RATE'
    Y.FIELD.POS = ''
    EB.Updates.MultiGetLocRef(Y.APPLICATION,Y.FIELDS,Y.FIELD.POS)
    PENAL.RATE.POS = Y.FIELD.POS<1,1>
    PRECLS.IND.POS = Y.FIELD.POS<1,2>
    PREMIGINTEREST.POS = Y.FIELD.POS<2,1>
    YCusTaxPos = Y.FIELD.POS<3,1>
    FT.RATE.POS = Y.FIELD.POS<4,1>
   
    Y.PENAL.INT = EB.SystemTables.getRNew(AA.Fees.Charge.LocalRef)<1,PENAL.RATE.POS>
    Y.PRECLS.IND = EB.SystemTables.getRNew(AA.Fees.Charge.LocalRef)<1,PRECLS.IND.POS>
   
    BalanceType = "CURACCOUNT"
    RequestDate = EB.SystemTables.getToday()
    AA.Framework.GetEcbBalanceAmount(YACCT.ID, BalanceType, RequestDate, CurAmount, RetError)
  
RETURN
*-----------------------------------------------------------------------------
VAL.PRECLOSE.INDI:
*
    IF Y.PRECLS.IND NE '' THEN
        BEGIN CASE
           
            CASE  Y.PRECLS.IND EQ 'PAY.CAPITAL'
                GOSUB TAX.AMT
                GOSUB PROCESS
                    
            CASE  Y.PRECLS.IND EQ 'PENAL.INT'
                GOSUB TAX.AMT
                GOSUB PROCESS
              
            CASE 1
            
                EB.SystemTables.setAf(AA.Fees.Charge.LocalRef)
                EB.SystemTables.setAv(PRECLS.IND.POS)
                EB.SystemTables.setEtext("Selected Preclose indicator Not allowed for this Product")
                EB.ErrorProcessing.StoreEndError()
                  
        END CASE
    END ELSE
        EB.SystemTables.setAf(AA.Fees.Charge.LocalRef)
        EB.SystemTables.setAv(PRECLS.IND.POS)
        EB.SystemTables.setEtext("Select pre close indicator")
        EB.ErrorProcessing.StoreEndError()
    END
  
RETURN
*-----------------------------------------------------------------------------
TAX.AMT:
*
    Returnconditions = ''
    Idproperty = "CUSTOMER"
    AA.Framework.GetArrangementConditions(Y.ARR.ID, yPropertyClass, Idproperty, yEffectiveDate, Returnids, Returnconditions, Returnerror)
    yReturnCusCondition = RAISE( Returnconditions)
    YCustomers = yReturnCusCondition<AA.Customer.Customer.CusCustomer>
    YTaxLiabPerc = yReturnCusCondition<AA.Customer.Customer.CusTaxLiabilityPerc>
    
    Returnconditions = ''
    Idproperty = "TAX"
    AA.Framework.GetArrangementConditions(Y.ARR.ID, yPropertyClass, Idproperty, yEffectiveDate, Returnids, Returnconditions, Returnerror)
    yReturnTaxCondition = RAISE( Returnconditions)
    YTaxCondition   = yReturnTaxCondition<AA.Tax.Tax.TaxPropTaxCond>
    
    YTaxTypeConditionRec = CG.ChargeConfig.TaxTypeCondition.Read(YTaxCondition, TaxErr)
    YTaxCodes =  YTaxTypeConditionRec<CG.ChargeConfig.TaxTypeCondition.TaxTtcTaxCode>
    
    IF YTaxCondition EQ '' THEN
        YTaxCodes   = yReturnTaxCondition<AA.Tax.Tax.TaxPropTaxCode>
    END
    
    CHANGE @SM TO @VM IN YCustomers
    CHANGE @VM TO @FM IN YCustomers
    
    CHANGE @SM TO @VM IN YTaxLiabPerc
    CHANGE @VM TO @FM IN YTaxLiabPerc
    
    YCusCount = DCOUNT(YCustomers,@FM)
    I = 1
    LOOP
    WHILE I LE YCusCount
        YCusID = YCustomers<I>
        YTaxPerCus = YTaxLiabPerc<I>
        
        YCustRec = ST.Customer.Customer.Read(YCusID, Error)
        YCusTaxCode =  YCustRec<ST.Customer.Customer.EbCusLocalRef><1,YCusTaxPos>
        
        FINDSTR YCusTaxCode IN YTaxCodes SETTING YPOS.VM,YPOS.SM   THEN
            
            SEL.CMD="SELECT ":FN.TAX:" WITH @ID LIKE ":YCusTaxCode:"..."
            SEL.LIST=""; NO.OF.REC="";ERR.CODE="";
            EB.DataAccess.Readlist(SEL.CMD,SEL.LIST,'',NO.OF.REC,ERR.CODE)
            YTaxID = SEL.LIST<NO.OF.REC>
            YTaxRec = CG.ChargeConfig.Tax.Read(YTaxID,TaxErr)
            YTaxCcy = YTaxRec<CG.ChargeConfig.Tax.EbTaxCurrency>
            CHANGE @SM TO @VM IN YTaxCcy
            CHANGE @VM TO @FM IN YTaxCcy
            LOCATE YDepsoitCcy IN YTaxCcy SETTING YPOS THEN
                YTaxRate = YTaxRec<CG.ChargeConfig.Tax.EbTaxBandedRate><1,YPOS>
                YPercentage = YTaxPerCus * (YTaxRate/100)
                YTotalInt  =   YTotalInt + YPercentage
            END ELSE
                LOCATE 'LKR'  IN YTaxCcy SETTING YPOS THEN
                    YTaxRate = YTaxRec<CG.ChargeConfig.Tax.EbTaxBandedRate><1,YPOS>
                    YPercentage = YTaxPerCus * (YTaxRate/100)
                    YTotalInt  =   YTotalInt + YPercentage
                END
            END
        END
        I = I + 1
    REPEAT

RETURN
*-----------------------------------------------------------------------------
PROCESS:
*
    IF Y.PRECLS.IND EQ 'PAY.CAPITAL' THEN
        GOSUB PAY.CAPITAL
    END
    IF Y.PRECLS.IND EQ 'PENAL.INT' THEN
        GOSUB PENAL.INT
    END

RETURN
*-----------------------------------------------------------------------------
PAY.CAPITAL:
*
    BalanceType = "ACCDEPOSITINT"
    RequestDate = EB.SystemTables.getToday()
    AA.Framework.GetEcbBalanceAmount(YACCT.ID, BalanceType, RequestDate, BalanceAmount, RetError)
    EB.SystemTables.setRNew(AA.Fees.Charge.FixedAmount,BalanceAmount)
    Y.WHT.REF.AMT = (BalanceAmount)*(YTotalInt/100)
    YTotalAmtPaid = (CurAmount + Y.WHT.REF.AMT + BalanceAmount) - (BalanceAmount + Y.WHT.REF.AMT)
    EB.API.RoundAmount("",YTotalAmtPaid,"","")
    EB.SystemTables.setText('Total amount to be paid out ':YTotalAmtPaid)
    EB.OverrideProcessing.StoreOverride('')
    YPrecloseAmount = YTotalAmtPaid
  
RETURN
*-----------------------------------------------------------------------------
PENAL.INT:
*
    R.AA.ARR = AA.Framework.Arrangement.Read(Y.ARR.ID, Error)
    Y.VAL.DATE = R.AA.ARR<AA.Framework.Arrangement.ArrStartDate>
    
    R.ACCT.DET = AA.PaymentSchedule.AccountDetails.Read(Y.ARR.ID,Error)
    Y.LASTRENEW.DATES = R.ACCT.DET<AA.PaymentSchedule.AccountDetails.AdLastRenewDate>
    CHANGE @SM TO @FM IN Y.LASTRENEW.DATES
    CHANGE @VM TO @FM IN Y.LASTRENEW.DATES
    
    Y.LASTRENEW.CNT = DCOUNT(Y.LASTRENEW.DATES,@FM)
    Y.RENEW.DATE = Y.LASTRENEW.DATES<Y.LASTRENEW.CNT>

    Y.DEP.ARR.ID = Y.ARR.ID:"-DEPOSITINT"
    EB.DataAccess.FRead(FN.AA.INT.ACCR,Y.DEP.ARR.ID,R.AA.INT.ACCR,F.AA.INT.ACCR,Y.ERR2)
    Y.TOT.INT.ACCR.AMT = R.AA.INT.ACCR<AA.Interest.InterestAccruals.IntAccTotAccrAmt>
    Y.INT.ACCR.END.DATES = R.AA.INT.ACCR<AA.Interest.InterestAccruals.IntAccPeriodEnd>
    Y.PERIOD.START.DATES = R.AA.INT.ACCR<AA.Interest.InterestAccruals.IntAccPeriodStart>
    Y.INT.ACCR.CNT = DCOUNT(Y.TOT.INT.ACCR.AMT,@VM)
    Y.TOT.INT.AMT = '0'
    FOR II = 1 TO Y.INT.ACCR.CNT
        Y.INT.ACCR.EDATE = Y.INT.ACCR.END.DATES<1,II>
        Y.PERIOD.START.DATE = Y.PERIOD.START.DATES<1,II>
        IF Y.INT.ACCR.EDATE GT Y.VAL.DATE  AND  Y.PERIOD.START.DATE GE Y.RENEW.DATE THEN
            Y.INT.ACCR.AMT = Y.TOT.INT.ACCR.AMT<1,II>
            Y.TOT.INT.AMT = Y.TOT.INT.AMT + Y.INT.ACCR.AMT
        END
    NEXT II
    Y.TOT.INT.ACCR.AMT = Y.TOT.INT.AMT
* Check if the contract is migrated, if so, get the paid interest amt from
* legecy(the value is stored in LRT of AA.ARR.INTEREST)
    GOSUB GET.LEGACY.PAID.INT
    IF Y.LEG.PAID.INT.AMT THEN
        Y.TOT.INT.ACCR.AMT = Y.TOT.INT.ACCR.AMT + Y.LEG.PAID.INT.AMT
    END
    
    Returnconditions = ''
* yPropertyClass = 'INTEREST'
    Idproperty = 'DEPOSITINT'
    AA.Framework.GetArrangementConditions(Y.ARR.ID, yPropertyClass, Idproperty, yEffectiveDate, Returnids, Returnconditions, Returnerror)
    yReturnAccCondition = RAISE( Returnconditions)
    Y.ORG.INT = yReturnAccCondition<AA.Interest.Interest.IntEffectiveRate>
    
    Y.FEE.AMT = (Y.PENAL.INT/Y.ORG.INT) * Y.TOT.INT.ACCR.AMT
    
    EB.API.RoundAmount("",Y.FEE.AMT,"","")
    EB.SystemTables.setRNew(AA.Fees.Charge.FixedAmount,Y.FEE.AMT)
    YTotInt = (Y.TOT.INT.ACCR.AMT) * (YTotalInt/100)
    Y.WHT.REF.AMT = (Y.FEE.AMT)*(YTotalInt/100)
    
    YTotalAmount = (CurAmount + Y.TOT.INT.ACCR.AMT + Y.WHT.REF.AMT) - (Y.FEE.AMT + YTotInt)
    EB.API.RoundAmount("",YTotalAmount,"","")
    EB.SystemTables.setText('Total amount to be paid out ':YTotalAmount)
    EB.OverrideProcessing.StoreOverride('')
    YPrecloseAmount = YTotalAmount
    
RETURN
*-----------------------------------------------------------------------------
GET.LEGACY.PAID.INT:
*
    Y.LEG.PAID.INT.AMT = '0'
    Y.ORIG.CONTRACT.DATE = R.AA.ARR<AA.Framework.Arrangement.ArrOrigContractDate>
    IF Y.ORIG.CONTRACT.DATE THEN
*  AA.Framework.GetArrangementConditions(Y.ARR.ID,"","DEPOSITINT","",RET.ID,RET.COND.INT,RET.ERR)
        Idproperty = 'DEPOSITINT'
        AA.Framework.GetArrangementConditions(Y.ARR.ID, yPropertyClass, Idproperty, yEffectiveDate, Returnids, Returnconditions, Returnerror)
        yReturnAccCondition = RAISE( Returnconditions)
        Y.LEG.PAID.INT.AMT  = yReturnAccCondition<AA.Interest.Interest.IntLocalRef,PREMIGINTEREST.POS>
    
    END
RETURN
*-------------------------------------------------------------------------
END
