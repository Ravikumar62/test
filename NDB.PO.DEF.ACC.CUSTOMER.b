* @ValidationCode : MjotMjY4MjIwMTkxOkNwMTI1MjoxNjM4ODc4NTM5MDY5OklUU1M6LTE6LTE6MDowOmZhbHNlOk4vQTpSMjBfU1AxLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 07 Dec 2021 17:32:19
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSS
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R20_SP1.0
$PACKAGE EB.NDBTPHConf
SUBROUTINE NDB.PO.DEF.ACC.CUSTOMER
*-----------------------------------------------------------------------------
*Development Ref No :
*Date               :09-MAR-2021
*Description        :Validation Routine VERSION>PAYMENT.ORDER,CEFT.INPUT.NDB.
*Done by            :ITSS -Anusha ArunachalaRajan
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
*Date               :07-DEC-2021
*Done by            :ITSS - Srilakshmi Akella
*-----------------------------------------------------------------------------
    $USING EB.SystemTables
    $USING EB.DataAccess
    $USING PI.Contract
    $USING AC.AccountOpening
    $USING EB.Interface
    $USING EB.Display
    $USING AA.PaymentSchedule
    $USING EB.ErrorProcessing
    $USING AC.Config
    $USING ST.Customer

    GOSUB INIT
  
RETURN

INIT:
****
    YEnri = '' ;yDebitAcc = '' ;Yccy = '';ytmp = '';ycateg = '';YINDEX = ''
    yDebitAcc = EB.SystemTables.getComi()
     
    Fn.Acc = 'F.ACCOUNT'
    F.Acc = ''
    EB.DataAccess.Opf(Fn.Acc,F.Acc)
    
    FN.CUS='F.CUSTOMER'
    F.CUS=''
    EB.DataAccess.Opf(FN.CUS,F.CUS)
    
    IF yDebitAcc THEN
        ReadEr = '' ; AccRec = '' ;
        EB.DataAccess.FRead(Fn.Acc, yDebitAcc, AccRec, F.Acc, ReadEr)
        IF ReadEr ELSE
            Yccy = AccRec<AC.AccountOpening.Account.Currency>
            ycateg = AccRec<AC.AccountOpening.Account.Category>
            ycus=AccRec<AC.AccountOpening.Account.Customer>
            EB.DataAccess.FRead(FN.CUS,ycus,R.CUS,F.CUS,ER)
            ysname=R.CUS<ST.Customer.Customer.EbCusShortName>
            GOSUB CURRENCY.CHECK
            GOSUB CATEGORY.CHECK
            GOSUB DORMANT.CHECK
            GOSUB PROCESS
        END
    END
RETURN

CURRENCY.CHECK:
**************
****Restrict FCY Account
    IF EB.SystemTables.getPgmVersion() EQ ',CEFT.INPUT.NDB' THEN
        IF Yccy NE "LKR" THEN
            EB.SystemTables.setAf(PI.Contract.PaymentOrder.PoDebitAccount)
            EB.SystemTables.setEtext("PI-ACCOUNT.FCY")
            EB.ErrorProcessing.StoreEndError()
        END
    END
RETURN

CATEGORY.CHECK:
**************
    IF (ycateg GE 7000 AND ycateg LE 7560) OR (ycateg GE 3000 AND ycateg LE 3500) OR ycateg EQ 5001 OR ycateg EQ 5002 THEN
        EB.SystemTables.setAf(PI.Contract.PaymentOrder.PoDebitAccount)
        EB.SystemTables.setEtext("PI-INVALID.ACCOUNT")
        EB.ErrorProcessing.StoreEndError()
    END

RETURN
 
DORMANT.CHECK:
*************
****Restrict dormant account
    yArrangementId = '' ; yArrangementId = AccRec<AC.AccountOpening.Account.ArrangementId>
    YError = '' ; yAccountDetails = AA.PaymentSchedule.AccountDetails.Read(yArrangementId, YError)
    yAccountStatus = yAccountDetails<AA.PaymentSchedule.AccountDetails.AdArrDormancyStatus>

    IF yAccountStatus EQ 'DORMANT' THEN
        EB.SystemTables.setAf(PI.Contract.PaymentOrder.PoDebitAccount)
        EB.SystemTables.setEtext("PI-ACCOUNT.DORMANT")
        EB.ErrorProcessing.StoreEndError()
                
    END
RETURN
    
PROCESS:
*******
****Populating Currency,Name
    yCurrAccTittle = '' ; yCurrAccTittle = EB.SystemTables.getRNew(PI.Contract.PaymentOrder.PoDebtorName)

    IF ((yCurrAccTittle AND (yCurrAccTittle NE ysname)) OR NOT(yCurrAccTittle)) THEN
        EB.SystemTables.setRNew(PI.Contract.PaymentOrder.PoDebtorName,ysname)
        EB.SystemTables.setRNew(PI.Contract.PaymentOrder.PoDebitCcy,Yccy)
    END
**************** Handling Enrichment Part ****************
    COMI.ENRI = ysname
    EB.SystemTables.setComiEnri(R.CUS<ST.Customer.Customer.EbCusShortName>)
    tmp=EB.Interface.getOfsEnri(); tmp<PI.Contract.PaymentOrder.PoDebitAccount>=COMI.ENRI; EB.Interface.setOfsEnri(tmp)
    
END
                
RETURN
*--------------------------------------------------------------------------------------------------
END
