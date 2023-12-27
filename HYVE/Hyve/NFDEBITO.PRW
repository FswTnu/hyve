#INCLUDE "PROTHEUS.CH"
#INCLUDE "COLORS.CH"
#INCLUDE "FWPRINTSETUP.CH"
#INCLUDE "RWMAKE.CH"
#INCLUDE "TOTVS.CH"
#INCLUDE "RPTDEF.CH"
#INCLUDE "MSOLE.CH"
#INCLUDE "TOPCONN.CH"

/*ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
ฑฑบ Programa      ณ NFDEBITO                                       บ Data ณ 30/03/2015  บฑฑ
ฑฑฬอออออออออออออออุออออออออออออออออออออออออออออออออออออออออออออออออสออออออฯอออออออออออออนฑฑ
ฑฑบ Descricao     ณ Relatorio de Impressao de Nota de Debito				  	        บฑฑ
ฑฑฬอออออออออออออออุออออออออออออออออออออออหอออออออออัออออออออออออออออออออออออออออออออออออนฑฑ
ฑฑบ Desenvolvedor ณ Eduardo Augusto      บ Empresa ณ Totvs Nacoes Unidas                บฑฑ
ฑฑฬอออออออออออออออุออออออออออออหออออออออัสออออออหออฯออออออออออออออออออออออออออออออออออออนฑฑ
ฑฑบ Linguagem     ณ Advpl      บ Versao ณ 11    บ Sistema ณ Microsiga                   บฑฑ
ฑฑฬอออออออออออออออุออออออออออออสออออออออฯอออออออสอออออออออออออออออออออออออออออออออออออออนฑฑ
ฑฑบ Modulo(s)     ณ SIGAFAT                                                             บฑฑ
ฑฑฬอออออออออออออออุอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออนฑฑ
ฑฑบ Tabela(s)     ณ SM0 / SA1 / SF2 / SD2 / SE1                                         บฑฑ
ฑฑฬอออออออออออออออุอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออนฑฑ
ฑฑบ Observacao    ณ                                                                     บฑฑ
฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿*/

User Function NFDEBITO(aVetor)

Local cPessoa       := ""
Local nTotal        := 0
Local cDescrBar     := SuperGetMv("MV_DESCBAR",.F.,"")
Local cCodServ      := ""
Local cDescrServ	:= ""
Local lCampBar      := !Empty(cDescrBar) .And. SB1->(FieldPos(cDescrBar)) > 0
Local lDescrBar     := GetNewPar("MV_DESCSRV",.F.)
Local lNfeServ		:= AllTrim(SuperGetMv("MV_NFESERV",.F.,"1")) == "1"
Local lSai			:= .F.
Local cServ			:= ""
Local nLin			:= 0
Local cDiscrServ	:= ""
Local _i			:= 0 // TNU - Release 25
Local n             := 0 // TNU - Release 25
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


For _i := 1 To Len(aVetor2)
	If aVetor2[_i,1] == .T.

		_cArquivo := "fatura_" + AllTrim(aVetor2[_i,3])
		cFileName := "C:\TEMP\" + _cArquivo + ".pdf"
		
		cTpImp	  := AllTrim(aVetor2[_i,8])
		cAnexImp  := AllTrim(aVetor2[_i,9])
				
		// Impressao
		oPrint := FWMSPrinter():New(_cArquivo, IMP_PDF, lAdjustToLegacy,, lDisableSetup,,,,,,,.F.,)
		oPrint:SetResolution(72)			// Default
		oPrint:SetPortrait() 				// SetLandscape() ou SetPortrait()
		oPrint:SetPaperSize(9)				// A4 210mm x 297mm  620 x 876
		oPrint:SetMargin(10,10,10,10)		// < nLeft>, < nTop>, < nRight>, < nBottom>
		oPrint:cPathPDF:= "C:\temp\"
		//oPrint:SetViewPdf(_limpr)
		oPrint:StartPage()   	// Inicia uma nova pแgina
		oBrush  := TBrush():New(,(0,0,0))
		oBrush2 := TBrush():New(,CLR_HGRAY) // Amarelo Claro
		
		DbSelectArea("SF3") 
		SF3->( DbSetOrder(4) )	// F3_FILIAL + F3_CLIEFOR + F3_LOJA + F3_NFISCAL + F3_SERIE
		SF3->( DbSeek( xFilial("SF3") + aVetor2[_i,5] + aVetor2[_i,7] + aVetor2[_i,3] + aVetor2[_i,4] ) )
		
		DbSelectArea("SF2")
		SF2->( DbSetOrder(1) )	// F2_FILIAL + F2_DOC + F2_SERIE + F2_CLIENTE + F2_LOJA + F2_FORMUL + F2_TIPO
		SF2->( DbSeek( xFilial("SF2") + aVetor2[_i,3] + aVetor2[_i,4] + aVetor2[_i,5] + aVetor2[_i,7] ) )
				
		DbSelectArea("SA1")
		SA1->( DbSetOrder(1))	// A1_FILIAL + A1_COD+A1_LOJA
		SA1->( DbSeek(xFilial("SA1") + SF2->F2_CLIENTE + SF2->F2_LOJA ) )
		cBco:=SA1->A1_BCO1
		DbSelectArea("SE1")
		SE1->( DbSetOrder(1))	// E1_FILIAL + E1_PREFIXO + E1_NUM + E1_PARCELA + E1_TIPO
		SE1->( DbSeek(xFilial("SE1") + aVetor2[_i,4] + aVetor2[_i,3] ) )
		
		DbSelectArea("SC6")
		SC6->( DbSetOrder(4))	// C6_FILIAL + C6_NOTA + C6_SERIE
		SC6->( DbSeek(xFilial("SC6") + aVetor2[_i,3] + aVetor2[_i,4] ) )
		
		DbSelectArea("SC5")
		SC5->( DbSetOrder(3))	// C5_FILIAL + C5_CLIENTE + C5_LOJACLI + C5_NUM
		SC5->( DbSeek(xFilial("SC5") + aVetor2[_i,5] + aVetor2[_i,7] + SC6->C6_NUM ) )
				
		nTotal	:= SF2->F2_VALBRUT
						
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
		oPrint:Say(0060,0450,"Fatura",oFont08)
//		oPrint:Say(0070,0450,aVetor[_i,2] + " / " + aVetor[_i,3],oFont08)	// AllTrim(SF3->F3_NFISCAL) + " / " + AllTrim(SF3->F3_SERIE)
		oPrint:Say(0070,0450,aVetor2[_i,3],oFont08)	// AllTrim(SF3->F3_NFISCAL) + " / " + AllTrim(SF3->F3_SERIE)		
		BuzzBox(0050,0425,0100,0530) // Data de Emissao
		oPrint:Say(0085,0450,"Data de Emissใo",oFont08)
		oPrint:Say(0095,0460,DtoC(SF2->F2_EMISSAO),oFont08)
		BuzzBox(0050,0425,0125,0530) // Hora da Emissao
		oPrint:Say(0110,0450,"Data de Vencimento",oFont08)
		oPrint:Say(0120,0460,DtoC(SE1->E1_VENCREA),oFont08)
		
		// 3o. Box Dados do Destinatแrio
		BuzzBox(0125,0050,0250,0530)
		oPrint:Say(0135,0240,"DADOS DO DESTINATมRIO",oFont08)
		oPrint:Say(0150,0055,"Nome / Razใo Social: ",oFont08)
		oPrint:Say(0150,0130,AllTrim(SA1->A1_NOME),oFont08)
		oPrint:Say(0165,0055,"C.P.F / C.N.P.J: ",oFont08)
		If cPessoa == "F"
			oPrint:Say(0165,0130,Transform(SA1->A1_CGC, "@R 999.999.999-99"),oFont08)
		Else
			oPrint:Say(0165,0130,Transform(SA1->A1_CGC, "@R 99.999.999/9999-99"),oFont08)
		EndIf
		oPrint:Say(0180,0055,"Inscri็ใo Municipal: ",oFont08)
		oPrint:Say(0180,0130,AllTrim(SA1->A1_INSCRM),oFont08)
		oPrint:Say(0195,0055,"Endere็o: ",oFont08)
		oPrint:Say(0195,0130,AllTrim(SA1->A1_END),oFont08)
		oPrint:Say(0210,0055,"CEP: ",oFont08)
		oPrint:Say(0210,0130,Transform(SA1->A1_CEP,PesqPict("SA1","A1_CEP")),oFont08)
		oPrint:Say(0225,0055,"Municํpio: ",oFont08)
		oPrint:Say(0225,0130,AllTrim(SA1->A1_MUN),oFont08)
		oPrint:Say(0225,0350,"UF: ",oFont08)
		oPrint:Say(0225,0365,SA1->A1_EST,oFont08)
		oPrint:Say(0240,0055,"E-mail: ",oFont08)
		oPrint:Say(0240,0130,Upper(AllTrim(SA1->A1_EMAIL)),oFont08)
		
		// 4o. Box DISCRIMINAวรO DOS SERVIวOS
		BuzzBox(0250,0050,0770,0530)
//		oPrint:Say(0260,0220,"DISCRIMINAวรO DOS SERVIวOS",oFont08N)
		oPrint:Say(0260,0220,"DESCRIวรO",oFont08N)
		oPrint:Say(0280,0055,"Descri็ใo:",oFont08)
		oPrint:Say(0290,0055,Alltrim(Substr(SC5->C5_MENNOTA,1,140)),oFont08)
		oPrint:Say(0300,0055,Alltrim(Substr(SC5->C5_MENNOTA,141,114)),oFont08)
			
		// 6o. Box VALOR TOTAL DA PRESTAวรO DE SERVIวOS
		BuzzBox(0770,0050,0800,0530)
		oPrint:FillRect({0771,0051,0799,0529 },oBrush)
		oPrint:FillRect({0771,0051,0799,0529 },oBrush2)
//		oPrint:Say(0785,0080,"VALOR TOTAL DA PRESTAวรO DE SERVIวOS",oFont08)
		oPrint:Say(0785,0080,"VALOR TOTAL",oFont08)
		oPrint:Say(0785,0400,"R$ " + Transform(nTotal,PesqPict("SF2","F2_VALBRUT")),oFont08N)
				
		oPrint:EndPage()
		oPrint:Preview()
		lRet:=.F.
		lRet:=CpyT2S("C:\Temp\"+_cArquivo+".pdf",'\kit-clientes\',.T.,.T.)
		If !lRet
			MsgAlert("Nใo foi possํvel copiar o arquivo para o diret๓rio do servidor.")
//		Else
//			MsgAlert("Copia realizada com Sucesso!!!")
		EndIf
/*		
		If !CpyT2S("C:\Temp\" + _cArquivo + ".pdf", "\kit-clientes\",.T.,.T.)
			MsgAlert("Nใo foi possํvel copiar o arquivo para o diret๓rio do servidor.")
		Else
			MsgAlert("Copia realizada com Sucesso!!!")
		EndIf
*/
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
				u_BOLETOS( aBoleto, aVetor2[_i,5], aVetor2[_i,7], aVetor2[_i,3], aVetor2[_i,4], _cArquivo,cTpImp,cAnexImp)
				//aqui seria legal colocar um alerta se o kra estiver com o bco vazio, mas depois fazemos
			EndIf
		EndIf

//	EndIf 
	aBoleto := {}
Next
//MsgInfo("Processo Finalizado com Sucesso!!!")
Return

/*ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
ฑฑบPrograma ณ BuzzBox         บAutorณ Silvio Cazela              บ Data ณ 24/04/2013 บฑฑ
ฑฑฬอออออออออุอออออออออออออออออสอออออฯออออออออออออออออออออออออออออสออออออฯออออออออออออนฑฑ
ฑฑบDescricaoณ Desenha um Box Sem Preenchimento                                       บฑฑ
฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿*/

Static Function BuzzBox(_nLinIni,_nColIni,_nLinFin,_nColFin) // < nRow>, < nCol>, < nBottom>, < nRight>

oPrint:Line( _nLinIni,_nColIni,_nLinIni,_nColFin,CLR_BLACK, "-2")
oPrint:Line( _nLinFin,_nColIni,_nLinFin,_nColFin,CLR_BLACK, "-2")
oPrint:Line( _nLinIni,_nColIni,_nLinFin,_nColIni,CLR_BLACK, "-2")
oPrint:Line( _nLinIni,_nColFin,_nLinFin,_nColFin,CLR_BLACK, "-2")

Return
