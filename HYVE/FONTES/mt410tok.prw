#INCLUDE "Protheus.ch"


/*/{Protheus.doc} mt410tok
Ponto de Entrada no Tudo OK
@type function
@version  12.1.33
@author valdemir rabelo
@since 15/10/2022
@return variant, .T. se tudo estar OK, .F. não permite a gravação
/*/
User Function mt410tok
    Local lRET := .T.

    lRET := U_hyvldnat()

Return lRET
