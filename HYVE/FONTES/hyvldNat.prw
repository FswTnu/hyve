#INCLUDE "Protheus.ch"

/*/{Protheus.doc} hyvldnat
Rotina que far� a valida��o da natureza da opera��o
@type function
@version  12.1.33
@author valdemir rabelo
@since 15/10/2022
@return variant, .T. - Informa que aceita passar .F. n�o permitira seguir
/*/
User Function hyvldnat()
    Local lRET    := .T.
    Local lNotBlq := GetMV("FS_HYVLDNT",.F.,.F.)
    Local lAtiva  := GetMV("FS_HYVLDON",.F.,.T.)

    if lAtiva
        IF (M->C5_XNATURE != '5')
            FWAlertInfo("N�o � permitido codigo de natureza diferente de '5'","Informativo")
            lRET := lNotBlq
        Endif 
    Endif 

Return lRET


/*/{Protheus.doc} ExecuF6
Rotina para chamadda F6
@type function
@version 12.1.33  
@author valdemir rabelo
@since 16/10/2022
@return variant, logico
/*/
User Function ExecuF6
    SetKey(VK_F6, {|| u_ShowPara() })

Return .T.


/*/{Protheus.doc} ShowPara
Rotina para chamada de par�metros
@type function
@version  12.1.33
@author valdemir rabelo
@since 16/10/2022
@return variant, logico
/*/
User Function ShowPara()
    Local aPergs := {}
    Local aRet   := {}
    Local lAtiva := GetMV("FS_HYVLDON",.F.,.T.)
    Local lTrava := GetMV("FS_HYVLDNT",.F.,.F.)

    MV_PAR01 := lAtiva
    MV_PAR02 := lTrava

    aAdd( aPergs ,{5,"Ativa Bloq.Natureza?"   , MV_PAR01, 90,'.T.'    ,.T.})  
    aAdd( aPergs ,{5,"N�o bloqueia Valida��o?", MV_PAR02, 90,'.T.'    ,.T.})  

    If ParamBox(aPergs ,"Parametros ",aRet,,,,,,,cUserName,.T.,.T.)
       PUTMV("FS_HYVLDON", MV_PAR01)   
       putMV("FS_HYVLDNT", MV_PAR02)
    Endif 

Return .T. 

