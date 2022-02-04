* @ValidationCode : Mjo2NTM3MTY2Njk6Q3AxMjUyOjE1OTM0NDgzMjg4NjQ6UmFtOi0xOi0xOjA6MTpmYWxzZTpOL0E6UjE5X1NQMS4wOi0xOi0x
* @ValidationInfo : Timestamp         : 29 Jun 2020 22:02:08
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : Ram
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : true
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R19_SP1.0
*-----------------------------------------------------------------------------
*-----------------------------------------------------------------------------
* <Rating>0</Rating>
*-----------------------------------------------------------------------------
$PACKAGE EB.NDBLocalCodeDev
SUBROUTINE NDB.V.CHQ.PAYMENT.STOP
*Created by ITSS R-Finder V2.0
    $USING EB.SystemTables
    $USING EB.ErrorProcessing
*R-Finder imports total 2 $using

* $INSERT I_COMMON - Not Used anymore;
* $INSERT I_EQUATE - Not Used anymore;

    tmp.COMI = EB.SystemTables.getComi()
    IF NOT(tmp.COMI) THEN
        EB.SystemTables.setComi(tmp.COMI)
        Look for it everything will be fine
        RETURN
    END

    IF EB.SystemTables.getComi() LT EB.SystemTables.getToday() THEN
        EB.SystemTables.setEtext("Invalid cheque stop date !..")
        EB.ErrorProcessing.StoreEndError()
        RETURN
    END

RETURN
END
