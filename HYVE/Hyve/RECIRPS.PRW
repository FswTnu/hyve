#INCLUDE "PROTHEUS.CH"
#INCLUDE "COLORS.CH"
#INCLUDE "FWPRINTSETUP.CH"
#INCLUDE "RWMAKE.CH"
#INCLUDE "TOTVS.CH"
#INCLUDE "RPTDEF.CH"
#INCLUDE "MSOLE.CH"
#INCLUDE "TOPCONN.CH"

/*‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹
±±∫ Programa      ≥ RECIRPS                                        ∫ Data ≥ 05/12/2014  ∫±±
±±ÃÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÿÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕ ÕÕÕÕÕÕœÕÕÕÕÕÕÕÕÕÕÕÕÕπ±±
±±∫ Descricao     ≥ Relatorio de Impressao de Recibo da RPS					  	        ∫±±
±±ÃÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÿÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÀÕÕÕÕÕÕÕÕÕ—ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕπ±±
±±∫ Desenvolvedor ≥ Eduardo Augusto      ∫ Empresa ≥ Totvs Nacoes Unidas                ∫±±
±±ÃÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÿÕÕÕÕÕÕÕÕÕÕÕÕÀÕÕÕÕÕÕÕÕ— ÕÕÕÕÕÕÀÕÕœÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕπ±±
±±∫ Linguagem     ≥ Advpl      ∫ Versao ≥ 11    ∫ Sistema ≥ Microsiga                   ∫±±
±±ÃÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÿÕÕÕÕÕÕÕÕÕÕÕÕ ÕÕÕÕÕÕÕÕœÕÕÕÕÕÕÕ ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕπ±±
±±∫ Modulo(s)     ≥ SIGAFAT                                                             ∫±±
±±ÃÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÿÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕπ±±
±±∫ Tabela(s)     ≥ SM0 / SA1 / SF2 / SD2 / SE1                                         ∫±±
±±ÃÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÿÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕπ±±
±±∫ Observacao    ≥                                                                     ∫±±
ﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ*/

User Function RECIRPS(aVetor)

Local cPessoa       := ""
Local nAliqPis      := 0
Local nAliqCof      := 0
Local nAliqCSLL     := 0
Local nAliqIR       := 0
Local nAliqINSS     := 0
Local nValPis       := 0
Local nValCof       := 0
Local nValCSLL      := 0
Local nValIR        := 0
Local nValINSS      := 0
Local cNatureza     := ""
Local cRecIss       := ""
Local cRecCof       := ""
Local cRecPis       := ""
Local cRecIR        := ""
Local cRecCsl       := ""
Local cRecIns		:= ""
Local cDescrBar     := SuperGetMv("MV_DESCBAR",.F.,"")
Local nTOTAL        := 0
Local nDEDUCAO      := 0
Local nBASEISS      := 0
Local nALIQISS      := 0
Local nVALISS       := 0
Local cCodServ      := ""
Local cDescrServ	:= ""
Local lCampBar      := !Empty(cDescrBar) .And. SB1->(FieldPos(cDescrBar)) > 0
Local lDescrBar     := GetNewPar("MV_DESCSRV",.F.)
Local lNfeServ		:= AllTrim(SuperGetMv("MV_NFESERV",.F.,"1")) == "1"
Local lSai			:= .F.
Local cServ			:= ""
Local lIssMat		:= SF3->(FieldPos("F3_ISSMAT")) > 0 // ainda n„o posicionado na sf3, precisa posicionar depois
Local nLin			:= 0
Local n				:= 0	// TNU - Release 25
Local cDiscrServ	:= ""
Local _i            := 0 	// TNU - Release 25
Local _aRetSX5		:= {}	// TNU - Release 25
Local _cDescr		:= "" 	// TNU - Release 25
Local nTXPIS		:= 0 	// TNU - Release 25
Local nTXCOFIN		:= 0 	// TNU - Release 25
Local nALIQIRF		:= 0 	// TNU - Release 25
Local nTXCSLL		:= 0 	// TNU - Release 25
Local cQuery 		:= ""
Private cBco		:= ""
Private _cArquivo   := ""
Private aBoleto     := {}
Private oPrint		:= Nil
Private oFont16N,oFont16,oFont14N,oFont12N,oFont10N,oFont14,oFont12,oFont10,oFont08N
Private _limpr	 := .T.
Private cBitmap	 := "\system\rps.png"
Private cBitmap2 := "\system\4c.png"
Private oFontTit	:= oFont08N
Private lAdjustToLegacy := .F.
Private lDisableSetup   := .T.
Private aVetor2 := {}
Private cTpImp	:=""
Private cAnexImp:=""

oFont16N	:= TFont():New("Arial",14,14,,.T.,,,,.T.,.F.)
oFont16 	:= TFont():New("Arial",14,14,,.F.,,,,.T.,.F.)
oFont14N	:= TFont():New("Arial",12,12,,.T.,,,,.T.,.F.)
oFont14 	:= TFont():New("Arial",12,12,,.F.,,,,.T.,.F.)
oFont12		:= TFont():New("Arial",12,12,,.F.,,,,.T.,.F.)
oFont12N	:= TFont():New("Arial",12,12,,.T.,,,,.T.,.F.)
oFont10		:= TFont():New("Arial",10,10,,.F.,,,,.T.,.F.)
oFont10N	:= TFont():New("Arial",10,10,,.T.,,,,.T.,.F.)
oFont08		:= TFont():New("Arial",07,07,,.T.,,,,.T.,.F.)
oFont08N	:= TFont():New("Arial",08,08,,.T.,,,,.T.,.F.)
oFont06N	:= TFont():New("Arial",06,06,,.T.,,,,.T.,.F.)
oFont05		:= TFont():New("Arial",06,06,,.T.,,,,.T.,.F.)

If !ExistDir("C:\temp\")
	MontaDir("C:\temp\")
EndIf               
For n := 1 To Len(aVetor)  
	If aVetor[n,1]==.T.
		AADD(aVetor2,{aVetor[n,1],aVetor[n,2],aVetor[n,3],aVetor[n,4],aVetor[n,5],aVetor[n,6],aVetor[n,7],aVetor[n,8],aVetor[n,9]})
	Endif
Next n

nTXPIS		:=	SuperGetMv("MV_TXPIS"  ) 	// TNU - Release 25
nTXCOFIN	:=	SuperGetMv("MV_TXCOFIN")	// TNU - Release 25
nALIQIRF	:=	SuperGetMV("MV_ALIQIRF") 	// TNU - Release 25
nTXCSLL		:=	SuperGetMv("MV_TXCSLL" )	// TNU - Release 25

For _i := 1 To Len(aVetor2)

	If aVetor2[_i,1] == .T.
	
		nValPis  := 0
		nValCof  := 0
		nValCsll := 0
		nValINSS := 0
		nValIR   := 0

		_cArquivo	:= "nf_" + AllTrim(aVetor2[_i,3])
		cFileName := "C:\TEMP\" + _cArquivo + ".pdf"

		cTpImp	  := AllTrim(aVetor2[_i,8])
		cAnexImp  := AllTrim(aVetor2[_i,9])
		
		// Impressao
		oPrint := FWMSPrinter():New(_cArquivo, IMP_PDF, lAdjustToLegacy,, lDisableSetup,,,,,,,.F.,)
		oPrint:SetResolution(72)			// Default
		oPrint:SetPortrait() 				// SetLandscape() ou SetPortrait()
		oPrint:SetPaperSize(9)				// A4 210mm x 297mm  620 x 876
		oPrint:SetMargin(10,10,10,10)		// < nLeft>, < nTop>, < nRight>, < nBottom>
		oPrint:cPathPDF:="C:\temp\"
		//oPrint:SetViewPdf(_limpr)
		oPrint:StartPage()   	// Inicia uma nova p·gina
		oBrush  := TBrush():New(,(0,0,0))
		oBrush2 := TBrush():New(,CLR_HGRAY) // Amarelo Claro
		
		DbSelectArea("SF3") 
		SF3->( DbSetOrder(4) )	// F3_FILIAL+F3_CLIEFOR+F3_LOJA+F3_NFISCAL+F3_SERIE                                                                                                                
		SF3->( DbSeek( xFilial("SF3") + aVetor2[_i,5] + aVetor2[_i,7] + aVetor2[_i,3] + aVetor2[_i,4] ) )

		DbSelectArea("SF2")
		SF2->( DbSetOrder(1) )	// F2_FILIAL+F2_DOC+F2_SERIE+F2_CLIENTE+F2_LOJA+F2_FORMUL+F2_TIPO
		SF2->( DbSeek( xFilial("SF2") + aVetor2[_i,3] + aVetor2[_i,4] + aVetor2[_i,5] + aVetor2[_i,7] ) )
	
		cQuery := " SELECT * FROM " + RetSqlName("SE1")
		cQuery += " WHERE D_E_L_E_T_ = '' "
		cQuery += " AND E1_NUM = '" + aVetor2[_i,3] + "' "
		cQuery += " AND E1_TIPO = 'NF' "                                
		cQuery += " AND E1_PREFIXO = '" + aVetor2[_i,4] + "' "
		cQuery += " AND E1_CLIENTE = '" + aVetor2[_i,5] + "' "
		cQuery += " AND E1_LOJA = '" + aVetor2[_i,7] + "' "
		Iif(Select("TMP") > 0,TMP->(DbCloseArea()),Nil)
		DbUseArea(.T., "TOPCONN", TcGenQry(,,cQuery), "TMP", .T., .T.) 
			
		DbSelectArea("TMP")
		DbGoTop()
		While !Eof()
			nValPis  += If(TMP->E1_SABTPIS>0,0,TMP->E1_PIS)
			nValCof  += If(TMP->E1_SABTCOF>0,0,TMP->E1_COFINS)
			nValCsll += If(TMP->E1_SABTCSL>0,0,TMP->E1_CSLL)
			nValINSS += TMP->E1_INSS
			nValIR   += TMP->E1_IRRF
			//DbSelectArea("TMP")
			DbSkip()
		End
		//DbSelectArea("TMP")
		TMP->(DbCloseArea())
		
		/* SUBSTITUIDO PELA QUERY ACIMA		
		// Obtendo os Valores de PIS/COFINS/CSLL/IR/INSS da NF de saida
		nValPis  := SF2->F2_VALPIS
		nValCof  := SF2->F2_VALCOFI
		nValINSS := SF2->F2_VALINSS
		nValIR   := SF2->F2_VALIRRF
		nValCSLL := SF2->F2_VALCSLL
		*/
		
		// Obtendo as Aliquotas de PIS/COFINS/CSLL/IR/INSS atraves da Natureza da NF de Saida
		cNatureza := SE1->E1_NATUREZ
		SED->(DbSetOrder(1))
		If SED->(dbSeek(xFilial("SED")+cNatureza))
//			nAliqPis  := Iif( nValPis  > 0 , Iif( SED->ED_PERCPIS > 0 , SED->ED_PERCPIS , SuperGetMv("MV_TXPIS"  )) , 0 )	// TNU - Release 25
//			nAliqCof  := Iif( nValCof  > 0 , Iif( SED->ED_PERCCOF > 0 , SED->ED_PERCCOF , SuperGetMv("MV_TXCOFIN")) , 0 )	// TNU - Release 25
			nAliqPis  := Iif( nValPis  > 0 , Iif( SED->ED_PERCPIS > 0 , SED->ED_PERCPIS , nTXPIS) , 0 ) 					// TNU - Release 25
			nAliqCof  := Iif( nValCof  > 0 , Iif( SED->ED_PERCCOF > 0 , SED->ED_PERCCOF , nTXCOFIN) , 0 ) 					// TNU - Release 25
			nALiqINSS := Iif( nValINSS > 0 , SED->ED_PERCINS , 0 )
//			nAliqIR   := Iif( nValIR   > 0 , Iif( SED->ED_PERCIRF > 0 , SED->ED_PERCIRF , SuperGetMV("MV_ALIQIRF")) , 0 )	// TNU - Release 25
//			nALiqCSLL := Iif( nValCSLL > 0 , Iif( SED->ED_PERCCSL > 0 , SED->ED_PERCCSL , SuperGetMv("MV_TXCSLL" )) , 0 )	// TNU - Release 25
			nAliqIR   := Iif( nValIR   > 0 , Iif( SED->ED_PERCIRF > 0 , SED->ED_PERCIRF , nALIQIRF) , 0 )					// TNU - Release 25
			nALiqCSLL := Iif( nValCSLL > 0 , Iif( SED->ED_PERCCSL > 0 , SED->ED_PERCCSL , nTXCSLL) , 0 )					// TNU - Release 25
		Else                                                       
//        	nAliqPis  := Iif( nValPis  > 0 , SuperGetMv("MV_TXPIS"  ) , 0 )	// TNU - Release 25
//			nAliqCof  := Iif( nValCof  > 0 , SuperGetMv("MV_TXCOFIN") , 0 )	// TNU - Release 25
//			nAliqIR   := Iif( nValIR   > 0 , SuperGetMV("MV_ALIQIRF") , 0 )	// TNU - Release 25
//			nALiqCSLL := Iif( nValCSLL > 0 , SuperGetMv("MV_TXCSLL" ) , 0 )	// TNU - Release 25
			nAliqPis  := Iif( nValPis  > 0 , nTXPIS , 0 )					// TNU - Release 25
			nAliqCof  := Iif( nValCof  > 0 , nTXCOFIN, 0 )					// TNU - Release 25
			nAliqIR   := Iif( nValIR   > 0 , nALIQIRF , 0 )					// TNU - Release 25
			nALiqCSLL := Iif( nValCSLL > 0 , nTXCSLL , 0 )					// TNU - Release 25
		EndIf
			
		DbSelectArea("SA1")
		SA1->(DbSetOrder(1))
		DbSeek(xFilial("SA1") + SF2->F2_CLIENTE + SF2->F2_LOJA )
		cBco:=SA1->A1_BCO1
		// Busca a Descricao do Codigo de Servicos
		cDescrServ := ""
//		SX5->(DbSetOrder(1))														// TNU - Release 25
//		If SX5->(DbSeek(xFilial("SX5") + "60" + PadR(AllTrim(SF3->F3_CODISS),6)))	// TNU - Release 25
//			cDescrServ := SX5->X5_DESCRI											// TNU - Release 25
//		EndIf 																		// TNU - Release 25
		_aRetSX5 := FWGetSX5("60", PadR(AllTrim(SF3->F3_CODISS),6),"pt-br" )		// TNU - Release 25 
		If !Empty(_aRetSX5)				 											// TNU - Release 25
	  		cDescrServ := ALLTRIM(_aRetSX5[1,4])       								// TNU - Release 25
		Endif						 												// TNU - Release 25                     
		If lDescrBar
			SD2->(DbSetOrder(3))
			SB1->(DbSetOrder(1))
			If SD2->(DbSeek(xFilial("SD2") + SF2->F2_DOC + SF2->F2_SERIE + SF2->F2_CLIENTE + SF2->F2_LOJA)) 
				If (SB1->(MsSeek(xFilial("SB1") + SD2->D2_COD)))
					cDescrServ := IIf (lCampBar,SB1->(AllTrim(&cDescrBar)),cDescrServ)
				EndIf
			EndIf
		EndIf
		If !Empty(cCodServ)
			cCodServ += " / "
		EndIf
		cCodServ := Alltrim(SF3->F3_CODISS) + " - " + AllTrim(cDescrServ)
		
		// Busca o pedido para discriminar os servicos prestados no documento
		cServ := ""
		If lNfeServ
			SC6->(DbSetOrder(4))
			SC5->(DbSetOrder(1))
			If SC6->(DbSeek(xFilial("SC6") + SF3->F3_NFISCAL + SF3->F3_SERIE))
//				DbSelectArea("SX5")															// TNU - Release 25
//				SX5->(DbSetOrder(1))														// TNU - Release 25
				_aRetSX5 := FWGetSX5("60", PadR(AllTrim(SF3->F3_CODISS),6),"pt-br" )		// TNU - Release 25 
				If !Empty(_aRetSX5)				 											// TNU - Release 25
	  				_cDescr := ALLTRIM(_aRetSX5[1,4])       								// TNU - Release 25
				Endif						 												// TNU - Release 25
//				If SC5->(DbSeek(xFilial("SC5") + SC6->C6_NUM)) .And. DbSeek(xFilial("SX5") + "60" + PadR(AllTrim(SF3->F3_CODISS),6))
				If SC5->(DbSeek(xFilial("SC5") + SC6->C6_NUM)) .And. !Empty(_cDescr)       // TNU - Release 25
//					cServ := AllTrim(Substr(SC5->C5_MENNOTA,1,100)) + " | " + AllTrim(SubStr(SX5->X5_DESCRI,1,55)) // TNU - Release 25
					cServ := AllTrim(Substr(SC5->C5_MENNOTA,1,100)) + " | " + AllTrim(SubStr(_cDescr,1,55))		   // TNU - Release 25
				EndIf
			EndIf
		Else                           
//			DbSelectArea("SX5")																// TNU - Release 25
//			SX5->(DbSetOrder(1))															// TNU - Release 25
//			If DbSeek(xFilial("SX5") + "60" + PadR(AllTrim(SF3->F3_CODISS),6))				// TNU - Release 25
//				cServ := AllTrim(SubStr(SX5->X5_DESCRI,1,55))								// TNU - Release 25
//			EndIf																			// TNU - Release 25
			_aRetSX5 := FWGetSX5("60", PadR(AllTrim(SF3->F3_CODISS),6),"pt-br" )			// TNU - Release 25 
			If !Empty(_aRetSX5)				 												// TNU - Release 25
	  			cServ := ALLTRIM(_aRetSX5[1,4])       										// TNU - Release 25
			Endif						 													// TNU - Release 25
		EndIf
		If Empty(cServ)
			cServ := cCodServ
		EndIf
		cQuery := ""
		cQuery := " SELECT * FROM " + RetSqlName("SF3")
		cQuery += " WHERE D_E_L_E_T_ = '' "
		cQuery += " AND F3_NFISCAL = '" + aVetor2[_i,3] + "' "
		cQuery += " AND F3_SERIE = '" + aVetor2[_i,4] + "' "
		cQuery += " AND F3_ESPECIE = 'RPS' "                                
		cQuery += " AND F3_CLIEFOR = '" + aVetor2[_i,5] + "' "
		cQuery += " AND F3_LOJA = '" + aVetor2[_i,7] + "' "
		Iif(Select("TMP2") > 0,TMP2->(DbCloseArea()),Nil)
		DbUseArea(.T., "TOPCONN", TcGenQry(,,cQuery), "TMP2", .T., .T.) 
			
		DbSelectArea("TMP2")
		DbGoTop()
		While !Eof()		
			nTOTAL   	+= TMP2->F3_VALCONT
			nDEDUCAO 	+= TMP2->F3_ISSSUB + Iif( lIssMat , TMP2->F3_ISSMAT , 0 )
			nBASEISS 	+= TMP2->F3_BASEICM
			nALIQISS 	:= TMP2->F3_ALIQICM
			nVALISS		+= TMP2->F3_VALICM
			DbSkip()
		End
		TMP2->(DbCloseArea())
					
		//	1o. Box I2I + Dados
		BuzzBox  (0050,0050,0125,0425)	// < nRow>, < nCol>, < nBottom>, < nRight>, [ cPixel]
		If cEmpAnt == "01"
			oPrint:SayBitmap(0051,0051, cBitmap,0100,0075)	// < nRow>, < nCol>, < cBitmap>, [ nWidth], [ nHeight]
		ElseIf cEmpAnt == "02"
			oPrint:SayBitmap(0051,0051, cBitmap2,0100,0075)	// < nRow>, < nCol>, < cBitmap>, [ nWidth], [ nHeight]
		EndIf
		oPrint:SayAlign (0051,0051,ALLTRIM(SM0->M0_NOMECOM),oFontTit,480, 14,	CLR_BLACK, 2, 2 ) // < nRow>, < nCol>, < cText>, [ oFont], [ nWidth], [ nClrText], [ nAngle]
		oPrint:SayAlign (0061,0051,ALLTRIM(SM0->M0_ENDENT),oFont08,480, 14, CLR_BLACK, 2, 2 )
		oPrint:SayAlign (0071,0051,ALLTRIM(SM0->M0_BAIRENT) + " - " + SUBSTR(SM0->M0_CEPENT,1,5) + "-" + SUBSTR(SM0->M0_CEPENT,6,3),oFont08,480, 14, CLR_BLACK, 2, 2 )
		oPrint:SayAlign (0081,0051,ALLTRIM(SM0->M0_CIDENT) + " - " + SM0->M0_ESTENT,oFont08,480, 14, CLR_BLACK, 2, 2 )
		oPrint:SayAlign (0091,0051,"Telefone: " + ALLTRIM(SM0->M0_TEL),oFont08,480, 14, CLR_BLACK, 2, 2 )
		oPrint:SayAlign (0101,0051,"C.N.P.J.: " + Transform(SM0->M0_CGC, "@R 99.999.999/9999-99"),oFont08,480, 14, CLR_BLACK, 2, 2 )
		oPrint:SayAlign (0111,0051,"I.M.: " + ALLTRIM(SM0->M0_INSCM),oFont08,480, 14, CLR_BLACK, 2, 2 )
		
		// 2o. Box Numero / Serie / Data de Emissao / 	Hora da Emissao
		BuzzBox(0050,0425,0075,0530) // Numero / Serie
		oPrint:Say(0060,0450,"Numero / Serie",oFont08)
		oPrint:Say(0070,0450,aVetor2[_i,3] + " / " + aVetor2[_i,4],oFont08)
		// oPrint:Say(0070,0450,AllTrim(SF3->F3_NFISCAL) + " / " + AllTrim(SF3->F3_SERIE),oFont08)
		BuzzBox(0050,0425,0100,0530) // Data de Emissao
		oPrint:Say(0085,0450,"Data de Emiss„o",oFont08)
		oPrint:Say(0095,0460,DtoC(SF2->F2_EMISSAO),oFont08)
		BuzzBox(0050,0425,0125,0530) // Hora da Emissao
		oPrint:Say(0110,0450,"Hora de Emiss„o",oFont08)
		oPrint:Say(0120,0465,SF2->F2_HORA,oFont08)
		
		// 3o. Box Dados do Destinat·rio
		BuzzBox(0125,0050,0250,0530)
		oPrint:Say(0135,0240,"DADOS DO DESTINAT¡RIO",oFont08)
		oPrint:Say(0150,0055,"Nome / Raz„o Social: ",oFont08)
		oPrint:Say(0150,0130,AllTrim(SA1->A1_NOME),oFont08)
		oPrint:Say(0165,0055,"C.P.F / C.N.P.J: ",oFont08)
		If cPessoa == "F"
			oPrint:Say(0165,0130,Transform(SA1->A1_CGC, "@R 999.999.999-99"),oFont08)
		Else
			oPrint:Say(0165,0130,Transform(SA1->A1_CGC, "@R 99.999.999/9999-99"),oFont08)
		EndIf
		oPrint:Say(0180,0055,"InscriÁ„o Municipal: ",oFont08)
		oPrint:Say(0180,0130,AllTrim(SA1->A1_INSCRM),oFont08)
		oPrint:Say(0195,0055,"EndereÁo: ",oFont08)
		oPrint:Say(0195,0130,AllTrim(SA1->A1_END),oFont08)
		oPrint:Say(0210,0055,"CEP: ",oFont08)
		oPrint:Say(0210,0130,Transform(SA1->A1_CEP,PesqPict("SA1","A1_CEP")),oFont08)
		oPrint:Say(0225,0055,"MunicÌpio: ",oFont08)
		oPrint:Say(0225,0130,AllTrim(SA1->A1_MUN),oFont08)
		oPrint:Say(0225,0350,"UF: ",oFont08)
		oPrint:Say(0225,0365,SA1->A1_EST,oFont08)
		oPrint:Say(0240,0055,"E-mail: ",oFont08)
		oPrint:Say(0240,0130,Upper(AllTrim(SA1->A1_EMAIL)),oFont08)
		
		// 4o. Box DISCRIMINA«√O DOS SERVI«OS
		BuzzBox(0250,0050,0460,0530)
		oPrint:Say(0260,0220,"DISCRIMINA«√O DOS SERVI«OS",oFont08N)
		oPrint:Say(0280,0055,"DescriÁ„o:",oFont08)
		//oPrint:Say(0290,0055,Alltrim(Substr(cDiscrServ,1,140)),oFont08)
		//oPrint:Say(0300,0055,Alltrim(Substr(cDiscrServ,141,114)),oFont08)
		oPrint:Say(0290,0055,Alltrim(Substr(SC5->C5_MENNOTA,1,140)),oFont08)
		oPrint:Say(0300,0055,Alltrim(Substr(SC5->C5_MENNOTA,141,114)),oFont08)
			
		oPrint:Say(0440,0060,"IR     (" + Transform(nAliqIR  ,"@E 1.50") + "%)" ,oFont08N)
		oPrint:Say(0450,0060,"R$ " + Transform(nValIR  ,PesqPict("SF3","F3_VALICM")),oFont08N)
		
		oPrint:Say(0440,0160,"INSS   (" + Transform(nAliqINSS,"@E 99.99") + "%)" ,oFont08N)
		oPrint:Say(0450,0160,"R$ " + Transform(nValINSS,PesqPict("SF3","F3_VALICM")),oFont08N)
		
		oPrint:Say(0440,0260,"PIS    (" + Transform(nAliqPis, "@E 0.65") + "%)" ,oFont08N)
		oPrint:Say(0450,0260,"R$ " + Transform(nValPis ,PesqPict("SF3","F3_VALICM")),oFont08N)
		
		oPrint:Say(0440,0360,"COFINS (" + Transform(nAliqCof, "@E 3.00") + "%)" ,oFont08N)
		oPrint:Say(0450,0360,"R$ " + Transform(nValCof ,PesqPict("SF3","F3_VALICM")),oFont08N)
		
		oPrint:Say(0440,0460,"CSLL   (" + Transform(nAliqCSLL,"@E 99.99") + "%)" ,oFont08N)
		oPrint:Say(0450,0460,"R$ " + Transform(nValCSLL,PesqPict("SF3","F3_VALICM")),oFont08N)
		
		// 5o. Box VALOR TOTAL DA PRESTA«√O DE SERVI«OS
		BuzzBox(0460,0050,0490,0530)
		oPrint:Say(0480,0100,"VALOR TOTAL DA PRESTA«√O DE SERVI«OS",oFont08)
		oPrint:Say(0480,0300,"R$ " + Transform(nTOTAL,PesqPict("SF3","F3_VALCONT")),oFont08N)
		
		// 6o. Box CÛdigo do ServiÁo
		BuzzBox(0490,0050,0520,0530)
		oPrint:Say(0500,0060,"CÛdigo do ServiÁo",oFont08)
		oPrint:Say(0510,0070,cCodServ,oFont08)
		
		// 7o. Box Total DeduÁıes (R$) / Base de C·lculo (R$) / AlÌquota (%) / Valor do ISS (R$)
		BuzzBox(0520,0050,0550,0170)	// Box Total DeduÁıes (R$)
		oPrint:Say(0530,0060,"Total DeduÁıes",oFont08)
		oPrint:Say(0540,0060,"R$ " + Transform(nDEDUCAO,PesqPict("SF3","F3_BASEICM")),oFont08N)
		BuzzBox(0520,0170,0550,0290)	// Box Base de C·lculo (R$)
		oPrint:Say(0530,0180,"Base de C·lculo (R$)",oFont08)
		oPrint:Say(0540,0180,"R$ " + Transform(nBASEISS,PesqPict("SF3","F3_BASEICM")),oFont08N)
		BuzzBox(0520,0290,0550,0410)	// Box AlÌquota (%)
		oPrint:Say(0530,0300,"AlÌquota (%)",oFont08)
		oPrint:Say(0540,0300,Transform(nALIQISS,PesqPict("SF3","F3_ALIQICM"))+"%",oFont08N)
		BuzzBox(0520,0410,0550,0530)	// Box Valor do ISS (R$)
		oPrint:Say(0530,0420,"Valor do ISS (R$)",oFont08)
		oPrint:Say(0540,0420,"R$ " + Transform(nVALISS ,PesqPict("SF3","F3_VALICM" )),oFont08N)
		
		// 8o. INFORMA«’ES SOBRE A NOTA FISCAL ELETR‘NICA
		BuzzBox(0550,0050,0580,0530)	// INFORMA«’ES SOBRE A NOTA FISCAL ELETR‘NICA
		oPrint:Say(0570,0220,"INFORMA«’ES SOBRE A NOTA FISCAL ELETR‘NICA",oFont08)
		
		// 9o. Box N˙mero / Emiss„o / CÛdigo VerificaÁ„o / CrÈdito IPTU (R$)
		BuzzBox(0580,0050,0610,0170)	// N˙mero
		oPrint:Say(0590,0060,"N˙mero",oFont08)
		oPrint:Say(0600,0070,Str(Year(SF2->F2_EMISSAO),4) + " / " + Alltrim(SF2->F2_NFELETR),oFont08)	// aVetor[_i,2]
		BuzzBox(0580,0170,0610,0270)	// Emiss„o
		oPrint:Say(0590,0180,"Emiss„o",oFont08)
		oPrint:Say(0600,0200,Transform(DToC(SF2->F2_EMISSAO),"@D"),oFont08)
		BuzzBox(0580,0270,0610,0410)	// CÛdigo VerificaÁ„o
		oPrint:Say(0590,0280,"CÛdigo VerificaÁ„o",oFont08)
		oPrint:Say(0600,0300,AllTrim(SF2->F2_CODNFE),oFont08)
		BuzzBox(0580,0410,0610,0530)	// CrÈdito IPTU (R$)
		oPrint:Say(0590,0420,"CrÈdito IPTU (R$)",oFont08)
		oPrint:Say(0600,0440,Transform(SF3->F3_CREDNFE,"@E 999,999,999.99"),oFont10)
		
		// 10o. Box OUTRAS INFORMA«’ES
		BuzzBox(0610,0050,0800,0530)	// Box OUTRAS INFORMA«’ES
		oPrint:Say(0620,0240,"OUTRAS INFORMA«’ES",oFont08)
		// InformaÁ„o conforme Necessidade do Cliente.....
		oPrint:Say(0630,0055,"Este recibo ProvisÛrio de ServiÁos - RPS È v·lido como documento fiscal devido existir os dados da Nota Fiscal de ServiÁo com o CÛdigo de VerificaÁ„o. ",oFont08)
		oPrint:Say(0640,0055,"O prestador do serviÁo, no prazo de atÈ 5 (cinco) dias da emiss„o deste RPS, dever· substituÌ-lo por uma Nota Fiscal de ServiÁos EletrÙnica.",oFont08)
		oPrint:Say(0650,0055,"* Valores para AlÌquota e Valor de ISSQN ser„o calculados de acordo com o movimento econÙmico com base na tabela de faixa de faturamento.",oFont08)
		//If SF3->F3_ESTADO == "SP"  // If SA1->A1_COD_MUN == "50308" .And. SM0->M0_CODIGO == "01"                                                                                               
		//	oPrint:Say(0660,0055,"O ISS desta NFS-e ser· RETIDO pelo Tomador de ServiÁo que dever· recolher atravÈs da Guia de NFS-e.",oFont08)
		//EndIf
		oPrint:EndPage()
		oPrint:Preview()
//		If !CpyT2S("C:\Temp\" + _cArquivo + ".pdf", "\anexo\",.T.)
		lRet:=.F.
		lRet:=CpyT2S("C:\Temp\"+_cArquivo+".pdf",'\kit-clientes\',.T.,.T.)
		If !lRet
			MsgAlert("N„o foi possÌvel copiar o arquivo para o diretÛrio do servidor.")
//		Else
//			MsgAlert("Copia realizada com Sucesso!!!")
		EndIf

//		If MsgYesNo("Imprime Boleto?")                                                      
			DbSelectArea("SE1")
			SE1->(DbGoTop())
			SE1->(DbSetOrder(1))
			SE1->(DbSeek( xFilial("SE1") + aVetor2[_i,4] + aVetor2[_i,3]  ) )
			While SE1->(!EOF()) .And. SE1->E1_PREFIXO + SE1->E1_NUM == aVetor2[_i,4] + aVetor2[_i,3]
				If AllTrim(SE1->E1_TIPO) == "NF"
					aAdd(aBoleto, { .T., SE1->E1_PREFIXO, SE1->E1_NUM, SE1->E1_PARCELA, SE1->E1_CLIENTE, SE1->E1_LOJA, SE1->E1_NOMCLI, DtoS(SE1->E1_EMISSAO), AllTrim(Transform(SE1->E1_VALOR,"@E 999,999,999.99")), DtoS(SE1->E1_VENCTO), DtoS(SE1->E1_VENCREA), SE1->E1_TIPO })
				EndIf
				SE1->(DbSkip())
			End

			If !Empty(Alltrim(cBco))
				u_BOLETOS( aBoleto, aVetor2[_i,5], aVetor2[_i,7], aVetor2[_i,3], aVetor2[_i,4], _cArquivo,cTpImp,cAnexImp )
				//aqui seria legal colocar um alerta se o kra estiver com o bco vazio, mas depois fazemos
			EndIf
//		EndIf

	EndIf 
	aBoleto := {}
	nTOTAL   	:= 0
	nDEDUCAO 	:= 0
	nBASEISS 	:= 0
	nALIQISS 	:= 0
	nVALISS		:= 0

Next _i

Return

/*‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹
±±∫Programa ≥ BuzzBox         ∫Autor≥ Silvio Cazela              ∫ Data ≥ 24/04/2013 ∫±±
±±ÃÕÕÕÕÕÕÕÕÕÿÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕ ÕÕÕÕÕœÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕ ÕÕÕÕÕÕœÕÕÕÕÕÕÕÕÕÕÕÕπ±±
±±∫Descricao≥ Desenha um Box Sem Preenchimento                                       ∫±±
ﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ*/

Static Function BuzzBox(_nLinIni,_nColIni,_nLinFin,_nColFin) // < nRow>, < nCol>, < nBottom>, < nRight>

oPrint:Line( _nLinIni,_nColIni,_nLinIni,_nColFin,CLR_BLACK, "-2")
oPrint:Line( _nLinFin,_nColIni,_nLinFin,_nColFin,CLR_BLACK, "-2")
oPrint:Line( _nLinIni,_nColIni,_nLinFin,_nColIni,CLR_BLACK, "-2")
oPrint:Line( _nLinIni,_nColFin,_nLinFin,_nColFin,CLR_BLACK, "-2")

Return
