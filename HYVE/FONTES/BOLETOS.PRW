#INCLUDE "PROTHEUS.CH"
#INCLUDE "COLORS.CH"
#INCLUDE "FWPRINTSETUP.CH"
#INCLUDE "RWMAKE.CH"
#INCLUDE "TOTVS.CH"
#INCLUDE "RPTDEF.CH"
#INCLUDE "MSOLE.CH"

/*ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±º Programa      ³ BOLETOS                          º Data ³ 19/08/2014  º±±
±±ÌÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±º Descricao     ³ Programa para Geracao de Boleto Grafico Itau          º±±
±±º				  ³	utilizando o Objeto FWMSPTRINTER.					  º±±
±±ÌÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±º Desenvolvedor ³ Eduardo Augusto      º Empresa ³ Totvs Nacoes Unidas  º±±
±±º Alterado por  ³ Rafael Domingues     º Empresa ³ Totvs Nacoes Unidas  º±±
±±ÌÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÍÍÑÊÍÍÍÍÍÍËÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±º Linguagem     ³ Advpl      º Versao ³ 11    º Sistema ³ Microsiga     º±±
±±ÌÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÊÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±º Modulo(s)     ³ SIGAFIN                                               º±±
±±ÌÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±º Tabela(s)     ³ SM0 / SE1 / SEE / SA6                                 º±±
±±ÌÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±º Observacao    ³  Alterado Dia 23/09/2014                              º±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß*/

User Function BOLETOS(aBoleto,_cCliente,_cLoja,_cNotaF,_cPref, _cArquivo,cTpImp,cAnexImp)
Local nCont      := 0
Local i          := 0 	// TNU - Release 25
Local nVlrDoc    := 0
Private _cClient := _cCliente
Private _cLoj	 := _cLoja
Private _cNota   := _cNotaF
Private _cPre    := _cPref
Private oPrint   := Nil
Private nClrAzul := RGB(063,072,204)
Private nClrVerd := RGB(032,166,072)
Private nClrVerm := RGB(237,028,036)
Private oFont18N,oFont18,oFont16N,oFont16,oFont14N,oFont12N,oFont10N,oFont14,oFont12,oFont10,oFont08N
Private _limpr	 := .T.
Private oFontTit	:= oFont08N
Private lAdjustToLegacy := .F.
Private lDisableSetup   := .T.
Private _aBoletos  := {}    //aBoleto
Private cNomeBol   := _cArquivo
Private cPfx :=""
Private cNum :=""
Private cCLi :=""
Private cLoj :=""                    
Private cParc:=""

oFont18N	:= TFont():New("Arial",18,18,,.T.,,,,.T.,.F.)
oFont18 	:= TFont():New("Arial",18,18,,.F.,,,,.T.,.F.)
oFont16N	:= TFont():New("Arial",16,16,,.T.,,,,.T.,.F.)
oFont16 	:= TFont():New("Arial",16,16,,.F.,,,,.T.,.F.)
oFont14N	:= TFont():New("Arial",14,14,,.T.,,,,.T.,.F.)
oFont14 	:= TFont():New("Arial",14,14,,.F.,,,,.T.,.F.)
oFont12		:= TFont():New("Arial",12,12,,.F.,,,,.T.,.F.)
oFont12N	:= TFont():New("Arial",12,12,,.T.,,,,.T.,.F.)
oFont10		:= TFont():New("Arial",10,10,,.F.,,,,.T.,.F.)
oFont10N	:= TFont():New("Arial",10,10,,.T.,,,,.T.,.F.)
oFont08		:= TFont():New("Arial",07,07,,.T.,,,,.T.,.F.)
oFont08N	:= TFont():New("Arial",08,08,,.T.,,,,.T.,.F.)
oFont06N	:= TFont():New("Arial",06,06,,.T.,,,,.T.,.F.)
oFont06	:= TFont():New("Arial",06,06,,.T.,,,,.T.,.F.)
oFont05	:= TFont():New("Arial",06,06,,.T.,,,,.T.,.F.)
If !ExistDir("C:\Temp\")
	MontaDir("C:\Temp\")
EndIf
//For i := 1 to Len(aVetor)
//aBoleto:=_aBoletos
For i := 1 to Len(aBoleto)
	//If aVetor[i,1] == .T.
  If aBoleto[i,1] == .T.
		nCont++
   		DbSelectArea("SE1")
		SE1->(DbSetOrder(1))
		//SE1->(DbSeek(xFilial("SE1") + aVetor[i,2] + aVetor[i,3] + aVetor[i,4] + aVetor[i,12]))
		SE1->(DbSeek(xFilial("SE1") + aBoleto[i,2] + aBoleto[i,3] + aBoleto[i,4] + aBoleto[i,12]))
		IF IsinCallStack("U_TELABOL") // Se entrar aqui eh porque esta vindo da NFE
			DbSelectArea("SA1")
		   	DbSetOrder(1)
		   	DbSeek(xFilial("SA1") + aBoleto[i,5] + aBoleto[i,6])
			   	_cBanco	  := SA1->A1_BCO1
			   	_cAgencia := SA1->A1_AGENCIA
				If _cBanco == '033'
			   	_cConta	  := SA1->A1_CONTACL 
				else
			   	_cConta	  := SA1->A1_CONTACL+'0' //Roni Silva TNU 
			   	Endif
				_cDgCli	  := SA1->A1_DVCTA
			   	_cSubCta  := SA1->A1_SUBCTA                                     
			   	cAnexImp  := "SIM"
		Else				
			DbSelectArea("SA1")
		   	DbSetOrder(1)
		   	DbSeek(xFilial("SA1") + aBoleto[i,5] + aBoleto[i,6])//+ _cClient + _cLoj)
			   	_cBanco	  := SA1->A1_BCO1
			   	_cAgencia := SA1->A1_AGENCIA
				If _cBanco == '033'
			   	_cConta	  := SA1->A1_CONTACL 
				else
				_cConta	  := Left(SA1->A1_CONTACL,TamSX3('EE_CONTA')[1])+'0' //Roni Silva TNU 
				Endif	
			   	_cDgCli	  := SA1->A1_DVCTA
			   	_cSubCta  := SA1->A1_SUBCTA
	   	EndIf
		DbSelectArea("SEE")
		SEE->(DbSetOrder(1))	// EE_FILIAL+EE_CODIGO+EE_AGENCIA+EE_CONTA+EE_SUBCTA
		SEE->(DbSeek(xFilial("SEE") + _cBanco + _cAgencia + _cConta + _cSubcta ))
		_cDvAge		:= SEE->EE_DVAGE
		_cDvCta		:= SEE->EE_DVCTA
		_cCart		:= SEE->EE_CODCART
		_nJuros		:= SEE->EE_JUROS
		_nMulta		:= SEE->EE_MULTA
		_cProtesto	:= SEE->EE_DIASPRT
		_cCedente	:= SEE->EE_CODEMP
		aAdd(_aBoletos,{SE1->(Recno()), SE1->E1_NUM, SE1->E1_TIPO, SC5->(Recno()), SC5->C5_NUM, ""})
		u_NNCBLDDV(_aBoletos)
		cArquivo := "bol_" + AllTrim(SE1->E1_NUM) + AllTrim(SE1->E1_PARCELA)
		cFileName := "C:\TEMP\" + cArquivo + ".pdf"
		// Impressao                      
		oPrint := FWMSPrinter():New(cArquivo, IMP_PDF, lAdjustToLegacy,, lDisableSetup,,,,,,,.F.,)// Ordem obrigátoria de configuração do relatório
		oPrint:SetResolution(72)			// Default
		oPrint:SetPortrait() 				// SetLandscape() ou SetPortrait()
		oPrint:SetPaperSize(9)				// A4 210mm x 297mm  620 x 876
		oPrint:SetMargin(10,10,10,10)		// < nLeft>, < nTop>, < nRight>, < nBottom>
		oPrint:cPathPDF := "C:\Temp\"
		//oPrint:SetViewPdf(_limpr)
		oPrint:StartPage()   	// Inicia uma nova página
		DbSelectArea("SA1")	
		SA1->(DbSetOrder(1))
		SA1->(DbSeek(xFilial("SA1") + SE1->(E1_CLIENTE + E1_LOJA ),.F.))
		//	Montagem do Box + Dados

		// Valdemir Rabelo 04/10/2023
		nVlrDoc    := IIf(SE1->E1_PREFIXO<>"1",SE1->E1_SALDO+SE1->E1_ACRESC-SE1->E1_DECRESC, (SE1->E1_SALDO+SE1->E1_ACRESC-SE1->E1_DECRESC)-(IIf(SE1->E1_SABTCSL>0,0,SE1->E1_CSLL) +IIf(SE1->E1_SABTCOF>0,0,SE1->E1_COFINS)+IIf(SE1->E1_SABTPIS>0,0,SE1->E1_PIS)+SE1->E1_ISS+SE1->E1_IRRF + SE1->E1_INSS))

		// < nRow>, < nCol>, < nBottom>, < nRight>, [ cPixel]
		// 1° Parte
		_cBcoLogo :=""
		_cDigBanco:=""
		aBcos := { {"341","7","LogoItau.jpg"},{"237","2","LogoBradesco.jpg"},{"399","9", "Logohsbc.jpg" },{"033","7", "Logosantander.jpg" } }
		nF := ASCan(aBcos ,{|x|, x[1] == _cBanco })
		If nF == 0
			MsgBox(Iif(Empty(_cBanco),"O numero do banco nao foi informado","Nao ha layout previsto para o banco " + _cBanco))
		Else
			_lContinua := .T.
			_cDigBanco := aBcos[nF,2]
			_cBcoLogo := aBcos[nF,3]
		EndIf
		If _cBanco = "399"
			oPrint:SayBitmap(0020,0025,_cBcoLogo,0085,0020)
		Elseif _cBanco = "237"
			oPrint:SayBitmap(0017,0025,_cBcoLogo,0064,0026.67)
		Elseif _cBanco = "033"
			oPrint:SayBitmap(0020,0025,_cBcoLogo,0085,0020)
		Else
			oPrint:SayBitmap(0020,0025,_cBcoLogo,0085,0020)
		EndIf
		If _cBanco = "399"
			oPrint:Say(0036,0110, "|" + "399" + "-" + "9" + "|" ,oFont18N,100)	// Código do Banco + Dígito
		ElseIf _cBanco = "341"
			oPrint:Say(0036,0110, "|" + _cBanco + "-" + _cDigBanco + "|" ,oFont18N,100)	// Código do Banco + Dígito
		ElseIf _cBanco = "033"
			oPrint:Say(0036,0110, "|" + _cBanco + "-" + _cDigBanco + "|" ,oFont18N,100)	// Código do Banco + Dígito			
		Else
			oPrint:Say(0036,0110, "|" + _cBanco + "-" + _cDigBanco + "|" ,oFont18N,100)	// Código do Banco + Dígito
		EndIf
		cCgcSM0 := SM0->M0_CGC
		oPrint:Say (0036, 0448,"Comprovante de Entrega",oFont12N )	// Comprovante de Entrega
		BuzzBox  (0040,0025,0065,0250)	// Box Beneficiário + Cnpj
		oPrint:Say (0046, 0026,"Beneficiário",oFont06N )
		oPrint:Say (0056, 0026,ALLTRIM(SM0->M0_NOMECOM),oFont06 )
		oPrint:Say (0046,0186,"Cnpj" ,oFont06N,100) 
		oPrint:Say (0056,0187,Transform(cCgcSM0,"@R 99.999.999/9999-99"),oFont06) //Cnpj do Beneficiário
		BuzzBox  (0040,0250,0065,0350)	// Box Agencia / Codigo do Cedente
		oPrint:Say (0046, 0251,"Agência/Código de Cedente",oFont06N )
		If _cBanco = "399"
			oPrint:Say (0056, 0261,Substr(Alltrim(_cAgencia),1,4) + "-" + Alltrim(_cDvAge) + "/" + Alltrim(_cConta) + "-" + Alltrim(_cDvCta),oFont06,100)
		ElseIf _cBanco = "341"
			oPrint:Say (0056, 0261,Substr(Alltrim(_cAgencia),1,4) + "/" + Substr(Alltrim(_cConta),1,5) + "-" + Substr(Alltrim(_cDvCta),1,1),oFont06,100)
		ElseIf _cBanco = "237"
			oPrint:Say (0056, 0261,Substr(Alltrim(_cAgencia),1,4) + "-" + Substr(Alltrim(_cDvAge),1,1) + "/" + Padl(Substr(Alltrim(_cConta),1,6),7,"0") + "-" + Substr(Alltrim(_cDvCta),1,1),oFont06,100)
		ElseIf _cBanco = "033"
			oPrint:Say (0056, 0261,Substr(Alltrim(_cAgencia),1,4) + "/" + Substr(Alltrim(_cCedente),1,7),oFont06,100)
			//oPrint:Say (0056, 0261,Substr(Alltrim(_cAgencia),1,4) + "/" + Padl(Substr(Alltrim(_cConta),1,8),8,"0") + "-" + Alltrim(_cDvCta),oFont06,100)			
		Else 
			oPrint:Say (0056, 0261,Substr(Alltrim(SEE->EE_CODCART),2,2) + "-" + Substr(Alltrim(_cAgencia),1,4) + "-" + Substr(Alltrim(_cAgencia),5,1) + "/" + Substr(Alltrim(_cConta),1,7) + "-" + Substr(Alltrim(_cConta),8,1),oFont06,100)
		EndIf
		BuzzBox  (0040,0350,0065,0450)	// N° do Documento
		oPrint:Say (0046, 0351,"N° do Documento",oFont06N )
		oPrint:Say (0056, 0361,SE1->E1_PREFIXO + SE1->E1_NUM + SE1->E1_PARCELA,oFont06 )
		BuzzBox  (0040,0450,0140,0560)	// Box de Selecao
		oPrint:Say (0050, 0451,"(  )Mudou-se"               ,oFont06N,100)
		oPrint:Say (0060, 0451,"(  )Ausente"                ,oFont06N,100)
		oPrint:Say (0070, 0451,"(  )Não existe nº indicado"	,oFont06N,100)
		oPrint:Say (0080, 0451,"(  )Recusado"               ,oFont06N,100)
		oPrint:Say (0090, 0451,"(  )Não procurado"          ,oFont06N,100)
		oPrint:Say (0100, 0451,"(  )Endereço insuficiente"  ,oFont06N,100)
		oPrint:Say (0110, 0451,"(  )Desconhecido"           ,oFont06N,100)
		oPrint:Say (0120, 0451,"(  )Falecido"               ,oFont06N,100)
		oPrint:Say (0130, 0451,"(  )Outros(anotar no verso)",oFont06N,100)
		BuzzBox  (0065,0025,0090,0250)	// Box do Pagador
		oPrint:Say (0071, 0026,"Pagador",oFont06N )
		oPrint:Say (0081, 0026,Upper(SA1->A1_NOME),oFont06 )
		BuzzBox  (0065,0250,0090,0350)	// Box do Vencimento
		oPrint:Say (0071, 0251,"Vencimento",oFont06N )
		oPrint:Say (0081, 0301,Substr( DtoS(SE1->E1_VENCREA),7,2 ) + "/" + Substr( DtoS(SE1->E1_VENCREA),5,2 ) + "/" + Substr( DtoS(SE1->E1_VENCREA),1,4 ),oFont06 )
		BuzzBox  (0065,0350,0090,0450)	// Box do Valor do Documento
		oPrint:Say (0071, 0351,"Valor do Documento",oFont06N )
		//oPrint:Say (0081, 0401,AllTrim(Transform(IIf(SE1->E1_PREFIXO<>"1",SE1->E1_SALDO + SE1->E1_ACRESC,SE1->E1_SALDO + SE1->E1_ACRESC - (SE1->E1_CSLL + SE1->E1_COFINS + SE1->E1_PIS + SE1->E1_ISS + SE1->E1_IRRF + SE1->E1_INSS)),"@E 999,999,999.99")),oFont06 )
		oPrint:Say (0081, 0401,AllTrim(Transform(nVlrDoc,"@E 999,999,999.99")),oFont06 )
		BuzzBox  (0090,0025,0140,0250)	// Box Recebi(emos) o Bloqueto / Titulo com as caracteristicas acima
		oPrint:Say (0107, 0026,"Box Recebi(emos) o Bloqueto / Titulo",oFont08N )
		oPrint:Say (0117, 0026,"com as caracteristicas acima",oFont08N )
		BuzzBox  (0090,0250,0115,0330)	// Box de Data
		oPrint:Say (0096, 0251,"Data",oFont06N )
		BuzzBox  (0090,0330,0115,0450)	// Box de Assinatura
		oPrint:Say (0096, 0331,"Assinatura",oFont06N )
		BuzzBox  (0115,0250,0140,0330)	// Box de Data
		oPrint:Say (0121, 0251,"Data",oFont06N )
		BuzzBox  (0115,0330,0140,0450)	// Box de Entregador
		oPrint:Say (0121, 0331,"Entregador",oFont06N )
		
		// 2° Parte
		If _cBanco = "399"
			oPrint:SayBitmap(0160,0025,_cBcoLogo,0085,0020)
		Elseif _cBanco = "237"
			oPrint:SayBitmap(0157,0025,_cBcoLogo,0064,0026.67)
		Elseif _cBanco = "033"
			oPrint:SayBitmap(0160,0025,_cBcoLogo,0085,0020)
		Else
			oPrint:SayBitmap(0160,0025,_cBcoLogo,0085,0020)
		EndIf
		If _cBanco = "399"
			oPrint:Say(0176,0110, "|" + "399" + "-" + "9" + "|" ,oFont18N,100)	// Código do Banco + Dígito
		ElseIf _cBanco = "341"
			oPrint:Say(0176,0110, "|" + _cBanco + "-" + _cDigBanco + "|" ,oFont18N,100)	// Código do Banco + Dígito
		ElseIf _cBanco = "033"
			oPrint:Say(0176,0110, "|" + _cBanco + "-" + _cDigBanco + "|" ,oFont18N,100)	// Código do Banco + Dígito
		Else
			oPrint:Say(0176,0110, "|" + _cBanco + "-" + _cDigBanco + "|" ,oFont18N,100)	// Código do Banco + Dígito
		EndIf
		oPrint:Say (0176, 0470,"Recibo do Pagador",oFont12N )	// Recibo do Pagador
		BuzzBox  (0180,0025,0205,0425)	// Local de Pagamento
		oPrint:Say (0186, 0026,"Local de Pagamento",oFont06N )
		If _cBanco = "399"
			oPrint:Say (0196, 0096,"PAGAR PREFERENCIALMENTE EM AGENCIAS DO HSBC",oFont06N )
		ElseIf _cBanco = "341"
			oPrint:Say  (0196, 0096,"ATÉ O VENCIMENTO, PREFERENCIALMENTE NO ITAÚ",oFont06N )
			oPrint:Say  (0206, 0096,"APÓS O VENCIMENTO, SOMENTE NO ITAÚ ",oFont06N )
		ElseIf _cBanco = "237"
			oPrint:Say  (0196, 0096,"PAGAVEL PREFERENCIALMENTE NA REDE BRADESCO OU BRADESCO EXPRESSO",oFont06N )
		ElseIf _cBanco = "033"
			oPrint:Say  (0196, 0096,"PAGAVEL PREFERENCIALMENTE NO BANCO SANTANDER",oFont06N )
		Else
			oPrint:Say  (0516, 0096,"PAGAVEL EM QUALQUER BANCO ATE O VENCIMENTO",oFont06N )
		EndIf
		BuzzBox  (0180,0425,0205,0560)	// Vencimento
		oPrint:Say (0186, 0426,"Vencimento",oFont06N )
		oPrint:Say (0196, 0476,Substr( DtoS(SE1->E1_VENCREA),7,2 ) + "/" + Substr( DtoS(SE1->E1_VENCREA),5,2 ) + "/" + Substr( DtoS(SE1->E1_VENCREA),1,4 ),oFont06 )
		BuzzBox  (0205,0025,0230,0425)	// Beneficiario
		oPrint:Say (0211, 0026,"Beneficiário",oFont06N )
		oPrint:Say (0221, 0026,ALLTRIM(SM0->M0_NOMECOM),oFont06 )
		oPrint:Say (0211,0300,"Cnpj" ,oFont06N,100) 
		oPrint:Say (0221,0301,Transform(cCgcSM0,"@R 99.999.999/9999-99"),oFont06) //Cnpj do Beneficiário
		BuzzBox  (0205,0425,0230,0560)	// Agencia 	/ Codigo do Cedente
		oPrint:Say (0211, 0426,"Agência/Código de Cedente",oFont06N )
		If _cBanco = "399"
			oPrint:Say (0221, 0436,Substr(Alltrim(_cAgencia),1,4) + "-" + Alltrim(_cDvAge) + "/" + Alltrim(_cConta) + "-" + Alltrim(_cDvCta),oFont06,100)
		ElseIf _cBanco = "341"
			oPrint:Say (0221, 0436,Substr(Alltrim(_cAgencia),1,4) + "/" + Substr(Alltrim(_cConta),1,5) + "-" + Substr(Alltrim(_cDvCta),1,1),oFont06,100)
		ElseIf _cBanco = "237"
			oPrint:Say (0221, 0436,Substr(Alltrim(_cAgencia),1,4) + "-" + Substr(Alltrim(_cDvAge),1,1) + "/" + Padl(Substr(Alltrim(_cConta),1,6),7,"0") + "-" + Substr(Alltrim(_cDvCta),1,1),oFont06,100)
		ElseIf _cBanco = "033"
			oPrint:Say (0221, 0436,Substr(Alltrim(_cAgencia),1,4) + "/" + Substr(Alltrim(_cCedente),1,7),oFont06,100)
			//oPrint:Say (0221, 0436,Substr(Alltrim(_cAgencia),1,4) + "/" + Padl(Substr(Alltrim(_cConta),1,8),8,"0") + "-" + Alltrim(_cDvCta),oFont06,100)
		Else 
			oPrint:Say (0221, 0436,Substr(Alltrim(SEE->EE_CODCART),2,2) + "-" + Substr(Alltrim(_cAgencia),1,4) + "-" + Substr(Alltrim(_cAgencia),5,1) + "/" + Substr(Alltrim(_cConta),1,7) + "-" + Substr(Alltrim(_cConta),8,1),oFont06,100)
		EndIf
		BuzzBox  (0230,0025,0255,0100)	// Data do Documento
		oPrint:Say (0236, 0026,"Data do Documento",oFont06N )
		oPrint:Say (0246, 0056,Substr( DtoS(SE1->E1_EMISSAO),7,2 ) + "/" + Substr( DtoS(SE1->E1_EMISSAO),5,2 ) + "/" + Substr( DtoS(SE1->E1_EMISSAO),1,4 ),oFont06 )
		BuzzBox  (0230,0100,0255,0225)	// Nro. Documento + Parcela
		oPrint:Say (0236, 0101,"N° do Documento",oFont06N )
		oPrint:Say (0246, 0111,SE1->E1_PREFIXO + SE1->E1_NUM + SE1->E1_PARCELA,oFont06 )
		BuzzBox  (0230,0225,0255,0275)	// Especie Doc.
		oPrint:Say (0236, 0226,"Especie Doc.",oFont06N )
		If _cBanco == "399"
			oPrint:Say (0246, 0246,"PD",oFont06 )
		Else
			oPrint:Say (0246, 0246,"DM",oFont06 )
		EndIf
		BuzzBox  (0230,0275,0255,0325)	// Aceite
		oPrint:Say (0236, 0276,"Aceite",oFont06N )
		oPrint:Say (0246, 0300,"N",oFont06 )
		BuzzBox  (0230,0325,0255,0425)	// Data do Processamento
		oPrint:Say (0236, 0326,"Data do Processamento",oFont06N )
		oPrint:Say (0246, 0356,Substr( DtoS(SE1->E1_EMISSAO),7,2 ) + "/" + Substr( DtoS(SE1->E1_EMISSAO),5,2 ) + "/" + Substr( DtoS(SE1->E1_EMISSAO),1,4 ),oFont06 )
		BuzzBox  (0230,0425,0255,0560)	// Nosso Numero
		oPrint:Say (0236, 0426,"Nosso Numero",oFont06N )
		If _cBanco = "399"
			oPrint:Say (0246, 0476,Alltrim(SEE->EE_CODCART) + IIf(Empty(SEE->EE_CODCART),"","/") + Substr(SE1->E1_NUMBCO,1,Len(Alltrim(SE1->E1_NUMBCO))-1) + "-" + Right(AllTrim(SE1->E1_NUMBCO),1),oFont06 )
		ElseIf _cBanco = "341"
			oPrint:Say (0246, 0476,"109" + "/" + Substr(SE1->E1_NUMBCO,1,Len(Alltrim(SE1->E1_NUMBCO))-1) + "-" + Right(AllTrim(SE1->E1_NUMBCO),1),oFont06 )
		ElseIf _cBanco = "237"
			oPrint:Say (0246, 0476,Substr(Alltrim(SEE->EE_CODCART),2,2) + IIf(Empty(SEE->EE_CODCART),"","/") + Substr(SE1->E1_NUMBCO,1,Len(Alltrim(SE1->E1_NUMBCO))-1) + "-" + Right(AllTrim(SE1->E1_NUMBCO),1),oFont06 )
		ElseIf _cBanco = "033"
			oPrint:Say (0246, 0476,Alltrim(SEE->EE_CODCART) + IIf(Empty(SEE->EE_CODCART),"","/") + Substr(SE1->E1_NUMBCO,1,Len(Alltrim(SE1->E1_NUMBCO))-1) + "-" + Right(AllTrim(SE1->E1_NUMBCO),1),oFont06 )
		Else
			oPrint:Say (0246, 0476,Alltrim(SEE->EE_CODCART) + IIf(Empty(SEE->EE_CODCART),"","/") + Substr(SE1->E1_NUMBCO,1,Len(Alltrim(SE1->E1_NUMBCO))-1) + "-" + Right(AllTrim(SE1->E1_NUMBCO),1),oFont06 )
		EndIf
		BuzzBox  (0255,0025,0280,0100)	// Uso do Banco
		oPrint:Say (0261, 0026,"Uso do Banco",oFont06N )
		BuzzBox  (0255,0100,0280,0165)	// Carteira
		oPrint:Say (0261, 0101,"Carteira",oFont06N )
		oPrint:Say (0271, 0125,_cCart,oFont06 )
		BuzzBox  (0255,0165,0280,0225)	// Especie
		oPrint:Say (0261, 0166,"Especie",oFont06N )
		If _cBanco $ "399/033"
			oPrint:Say (0271, 0186,"REAL",oFont06N )
		Else
			oPrint:Say (0271, 0186,"R$",oFont06N )
		EndIf
		BuzzBox  (0255,0225,0280,0325)	// Quantidade
		oPrint:Say (0261, 0226,"Quantidade",oFont06N )
		BuzzBox  (0255,0325,0280,0425)	// Valor
		oPrint:Say (0261, 0326,"Valor",oFont06N )
		BuzzBox  (0255,0425,0280,0560)	// Valor do Documento
		oPrint:Say (0261, 0426,"Valor do Documento",oFont06N )
		oPrint:Say (0271, 0476,AllTrim(Transform(nVlrDoc,"@E 999,999,999.99")),oFont06N )
		BuzzBox  (0280,0025,0380,0425)	// Instruções (Todas as Informações deste Bloqueto são de Exclusiva Responsabilidade do Cedente)
		oPrint:Say (0286, 0026,"Instruções (Todas as Informações deste Bloqueto são de Exclusiva Responsabilidade do Cedente)",oFont06N )
		If SE1->E1_ACRESC<>0
			If SE1->E1_XJUROS<>0
				oPrint:Say  (0306,0026,"Juros mora de R$ " + Alltrim(Transform(SE1->E1_XJUROS,"@E 99,999,999.99"))+ " de "+DToC(SE1->E1_VENCTO)+" a "+DToC(SE1->E1_VENCREA), oFont08,100)
			EndIF
			If SE1->E1_XMULTA<>0
				oPrint:Say  (0316,0026,"Multa por atraso do pagamento R$ " + Alltrim(Transform(SE1->E1_XMULTA,"@E 99,999,999.99")), oFont08,100)
			EndIF
		EndIF
		oPrint:Say  (0326,0026,"Sr. Caixa nao receber apos o vencimento.", oFont08,100)
		If !Empty(SE1->E1_DECRESC)
			oPrint:Say  (0336,0026,"Conceder Desconto de R$ ..... " + AllTrim(Transform((SE1->E1_DECRESC),"@E 99,999,999.99")), oFont08,100)
		EndIf
		BuzzBox  (0280,0425,0300,0560)	// (-) Desconto / Abatimento
		oPrint:Say (0286, 0426,"(-) Desconto / Abatimento",oFont06N )
		BuzzBox  (0300,0425,0320,0560)	// (-) Outras Deduções
		oPrint:Say (0306, 0426,"(-) Outras Deduções",oFont06N )
		BuzzBox  (0320,0425,0340,0560)	// (+) Mora / Multa
		oPrint:Say (0326, 0426,"(+) Mora / Multa",oFont06N )
		BuzzBox  (0340,0425,0360,0560)	// (+) Outros Acrescimos
		oPrint:Say (0346, 0426,"(+) Outros Acrescimos",oFont06N )
		BuzzBox  (0360,0425,0380,0560)	// (=) Valor Cobrado
		oPrint:Say (0366, 0426,"(=) Valor Cobrado",oFont06N )
		BuzzBox  (0380,0025,0450,0560)	// Pagador / Pagador Avalista
		oPrint:Say (0386, 0026,"Pagador",oFont06N )
		oPrint:Say  (0396,0106,Upper(SA1->A1_NOME),oFont06 ,100)
		oPrint:Say  (0406,0106,SA1->(IIf(Empty(A1_ENDCOB),A1_END,A1_ENDCOB) + " " + IIf(Empty(SA1->A1_BAIRROC),SA1->A1_BAIRRO,SA1->A1_BAIRROC)),oFont08 ,100)
		oPrint:Say  (0416,0106,SA1->(IIf(Empty(SA1->A1_CEPC),SA1->A1_CEP,SA1->A1_CEPC) + " " + IIf(Empty(SA1->A1_MUNC),SA1->A1_MUN,SA1->A1_MUNC) + " " + IIf(Empty(SA1->A1_ESTC),SA1->A1_EST,SA1->A1_ESTC)),oFont08 ,100)
		oPrint:Say  (0426,0106,SA1->(Tran(A1_CGC,IIf(Len(AllTrim(A1_CGC))<12,"@r 999.999.999-99","@r 99.999.999/9999-99")) + "               " + A1_INSCR),oFont08 ,100)
		oPrint:Say (0448, 0026,"Pagador Avalista",oFont08N )
		oPrint:Say  (0455,0360,"Autenticação Mecânica",oFont06,100)
		
		// 3° Parte
		If _cBanco = "399"
			oPrint:SayBitmap(0480,0025,_cBcoLogo,0085,0020)
		Elseif _cBanco ="237"
			oPrint:SayBitmap(0477,0025,_cBcoLogo,0064,0026.67)
		Elseif _cBanco ="033"
			oPrint:SayBitmap(0480,0025,_cBcoLogo,0085,0020)			
		Else
			oPrint:SayBitmap(0480,0025,_cBcoLogo,0085,0020)
		EndIf
		_cCodBar := Alltrim(SE1->E1_CODBAR)
		_cNumBol := Alltrim(SE1->E1_CODDIG)
		If _cBanco = "033"
			_cCodBarLit := Left(_cNumBol,5)+"."+Substr(_cNumBol,6,5)+"   "+;
			Substr(_cNumBol,11,5)+"."+Substr(_cNumBol,16,6)+"   "+;
			Substr(_cNumBol,22,5)+"."+Substr(_cNumBol,27,6)+"   "+;
			Substr(_cNumBol,33,1)+"   "+;
			Substr(_cNumBol,34)
		Else
			_cCodBarLit := Left(_cNumBol,5)+"."+Substr(_cNumBol,6,5)+"   "+;
			Substr(_cNumBol,11,5)+"."+Substr(_cNumBol,16,6)+"   "+;
			Substr(_cNumBol,22,5)+"."+Substr(_cNumBol,27,6)+"   "+;
			Substr(_cNumBol,33,1)+"   "+;
			Substr(_cNumBol,34)
		EndIf
		oPrint:Say(0496,0200,_cCodBarLit,oFont14N,100)
		If _cBanco = "399"
			oPrint:Say(0496,0110, "|" + "399" + "-" + "9" + "|" ,oFont18N,100)	// Código do Banco + Dígito
		ElseIf _cBanco = "341"
			oPrint:Say(0496,0110, "|" + _cBanco + "-" + _cDigBanco + "|" ,oFont18N,100)	// Código do Banco + Dígito
		ElseIf _cBanco = "033"
			oPrint:Say(0496,0110, "|" + _cBanco + "-" + _cDigBanco + "|" ,oFont18N,100)	// Código do Banco + Dígito
		Else
			oPrint:Say(0496,0110, "|" + _cBanco + "-" + _cDigBanco + "|" ,oFont18N,100)	// Código do Banco + Dígito
		EndIf
		BuzzBox  (0500,0025,0525,0425)	// Local de Pagamento
		oPrint:Say (0506, 0026,"Local de Pagamento",oFont06N )
		If _cBanco = "399"
			oPrint:Say (0516, 0096,"PAGAR PREFERENCIALMENTE EM AGENCIAS DO HSBC",oFont06N )
		ElseIf _cBanco = "341"
			oPrint:Say  (0516, 0096,"ATÉ O VENCIMENTO, PREFERENCIALMENTE NO ITAÚ",oFont06N )
			oPrint:Say  (0526, 0096,"APÓS O VENCIMENTO, SOMENTE NO ITAÚ ",oFont06N )
		ElseIf _cBanco = "237"
			oPrint:Say  (0516, 0096,"PAGAVEL PREFERENCIALMENTE NA REDE BRADESCO OU BRADESCO EXPRESSO",oFont06N )
			//oPrint:Say  (0526, 0096,"APÓS O VENCIMENTO, SOMENTE NAS AGENCIAS DO BRADESCO",oFont06N )
		ElseIf _cBanco = "033"
			oPrint:Say  (0516, 0096,"PAGAR PREFERENCIALMENTE NO BANCO SANTANDER",oFont06N )
			//oPrint:Say  (0526, 0096,"APÓS O VENCIMENTO, SOMENTE NO ITAÚ ",oFont06N )
		Else
			oPrint:Say  (0516, 0096,"PAGAVEL EM QUALQUER BANCO ATE O VENCIMENTO",oFont06N )
		EndIf
		BuzzBox  (0500,0425,0525,0560)	// Vencimento
		oPrint:Say (0506, 0426,"Vencimento",oFont06N )
		oPrint:Say (0516, 0476,Substr( DtoS(SE1->E1_VENCREA),7,2 ) + "/" + Substr( DtoS(SE1->E1_VENCREA),5,2 ) + "/" + Substr( DtoS(SE1->E1_VENCREA),1,4 ),oFont06 )
		BuzzBox  (0525,0025,0550,0425)	// Beneficiario
		oPrint:Say (0531, 0026,"Beneficiário",oFont06N )
		oPrint:Say (0541, 0026,ALLTRIM(SM0->M0_NOMECOM),oFont06 )
		oPrint:Say (0531,0300,"Cnpj" ,oFont06N,100) 
		oPrint:Say (0541,0301,Transform(cCgcSM0,"@R 99.999.999/9999-99"),oFont06) //Cnpj do Beneficiário
		BuzzBox  (0525,0425,0550,0560)	// Agencia / Codigo do Cedente
		oPrint:Say (0531, 0426,"Agência/Código de Cedente",oFont06N )
		If _cBanco = "399"
			oPrint:Say (0541, 0436,Substr(Alltrim(_cAgencia),1,4) + "-" + Alltrim(_cDvAge) + "/" + Alltrim(_cConta) + "-" + Alltrim(_cDvCta),oFont06,100)
		ElseIf _cBanco = "341"
			oPrint:Say (0541, 0436,Substr(Alltrim(_cAgencia),1,4) + "/" + Substr(Alltrim(_cConta),1,5) + "-" + Substr(Alltrim(_cDvCta),1,1),oFont06,100)
		ElseIf _cBanco = "237"
			oPrint:Say (0541, 0436,Substr(Alltrim(_cAgencia),1,4) + "-" + Substr(Alltrim(_cDvAge),1,1) + "/" + Padl(Substr(Alltrim(_cConta),1,6),7,"0") + "-" + Substr(Alltrim(_cDvCta),1,1),oFont06,100)
		ElseIf _cBanco = "033"
			oPrint:Say (0541, 0436,Substr(Alltrim(_cAgencia),1,4) + "/" + Substr(Alltrim(_cCedente),1,7),oFont06,100)
			//oPrint:Say (0541, 0436,Substr(Alltrim(_cAgencia),1,4) + "/" + Padl(Substr(Alltrim(_cConta),1,8),8,"0") + "-" + Alltrim(_cDvCta),oFont06,100)
		Else 
			oPrint:Say (0541, 0436,Substr(Alltrim(SEE->EE_CODCART),2,2) + "-" + Substr(Alltrim(_cAgencia),1,4) + "-" + Substr(Alltrim(_cAgencia),5,1) + "/" + Substr(Alltrim(_cConta),1,7) + "-" + Substr(Alltrim(_cConta),8,1),oFont06,100)
		EndIf
		BuzzBox  (0550,0025,0575,0100)	// Data do Documento
		oPrint:Say (0556, 0026,"Data do Documento",oFont06N )
		oPrint:Say (0566, 0046,Substr( DtoS(SE1->E1_EMISSAO),7,2 ) + "/" + Substr( DtoS(SE1->E1_EMISSAO),5,2 ) + "/" + Substr( DtoS(SE1->E1_EMISSAO),1,4 ),oFont06 )
		BuzzBox  (0550,0100,0575,0225)	// Nro. Documento + Parcela
		oPrint:Say (0556, 0101,"N° do Documento",oFont06N )
		oPrint:Say (0566, 0111,SE1->E1_PREFIXO + SE1->E1_NUM + SE1->E1_PARCELA,oFont06 )
		BuzzBox  (0550,0225,0575,0275)	// Especie Doc.
		oPrint:Say (0556, 0226,"Especie Doc.",oFont06N )
		If _cBanco == "399"
			oPrint:Say (0566, 0246,"PD",oFont06 )
		Else
			oPrint:Say (0566, 0246,"DM",oFont06 )
		EndIf
		BuzzBox  (0550,0275,0575,0325)	// Aceite
		oPrint:Say (0556, 0276,"Aceite",oFont06N )
		oPrint:Say (0566, 0296,"N",oFont06 )
		BuzzBox  (0550,0325,0575,0425)	// Data do Processamento
		oPrint:Say (0556, 0326,"Data do Processamento",oFont06N )
		oPrint:Say (0566, 0356,Substr( DtoS(SE1->E1_EMISSAO),7,2 ) + "/" + Substr( DtoS(SE1->E1_EMISSAO),5,2 ) + "/" + Substr( DtoS(SE1->E1_EMISSAO),1,4 ),oFont06 )
		BuzzBox  (0550,0425,0575,0560)	// Nosso Numero
		oPrint:Say (0556, 0426,"Nosso Numero",oFont06N )
		If _cBanco = "399"
			oPrint:Say (0566, 0476,Alltrim(SEE->EE_CODCART) + IIf(Empty(SEE->EE_CODCART),"","/") + Substr(SE1->E1_NUMBCO,1,Len(Alltrim(SE1->E1_NUMBCO))-1) + "-" + Right(AllTrim(SE1->E1_NUMBCO),1),oFont06 )
		ElseIf _cBanco = "341"
			oPrint:Say (0566, 0476,"109" + "/" + Substr(SE1->E1_NUMBCO,1,Len(Alltrim(SE1->E1_NUMBCO))-1) + "-" + Right(AllTrim(SE1->E1_NUMBCO),1),oFont06 )
		ElseIf _cBanco = "237"
			oPrint:Say (0566, 0476,Substr(Alltrim(SEE->EE_CODCART),2,2) + IIf(Empty(SEE->EE_CODCART),"","/") + Substr(SE1->E1_NUMBCO,1,Len(Alltrim(SE1->E1_NUMBCO))-1) + "-" + Right(AllTrim(SE1->E1_NUMBCO),1),oFont06 )
		ElseIf _cBanco = "033"
			oPrint:Say (0566, 0476,Alltrim(SEE->EE_CODCART) + IIf(Empty(SEE->EE_CODCART),"","/") + Substr(SE1->E1_NUMBCO,1,Len(Alltrim(SE1->E1_NUMBCO))-1) + "-" + Right(AllTrim(SE1->E1_NUMBCO),1),oFont06 )
		Else
			oPrint:Say (0566, 0476,Alltrim(SEE->EE_CODCART) + IIf(Empty(SEE->EE_CODCART),"","/") + Substr(SE1->E1_NUMBCO,1,Len(Alltrim(SE1->E1_NUMBCO))-1) + "-" + Right(AllTrim(SE1->E1_NUMBCO),1),oFont06 )
		EndIf
		BuzzBox  (0575,0025,0600,0100)	// Uso do Banco
		oPrint:Say (0581, 0026,"Uso do Banco",oFont06N )
		BuzzBox  (0575,0100,0600,0165)	// Carteira
		oPrint:Say (0581, 0101,"Carteira",oFont06N )
		oPrint:Say (0591, 0125,_cCart,oFont06 )
		BuzzBox  (0575,0165,0600,0225)	// Especie
		oPrint:Say (0581, 0166,"Especie",oFont06N )
		If _cBanco $ "033/399"
			oPrint:Say (0591, 0186,"REAL",oFont06N )
		Else 
			oPrint:Say (0591, 0186,"R$",oFont06N )
		EndIf
		BuzzBox  (0575,0225,0600,0325)	// Quantidade
		oPrint:Say (0581, 0226,"Quantidade",oFont06N )
		BuzzBox  (0575,0325,0600,0425)	// Valor
		oPrint:Say (0581, 0326,"Valor",oFont06N )
		BuzzBox  (0575,0425,0600,0560)	// Valor do Documento
		oPrint:Say (0581, 0426,"Valor do Documento",oFont06N )
		oPrint:Say (0591, 0476,AllTrim(Transform(IIf(SE1->E1_PREFIXO<>"1",SE1->E1_SALDO+SE1->E1_ACRESC-SE1->E1_DECRESC,(SE1->E1_SALDO+SE1->E1_ACRESC-SE1->E1_DECRESC)-(IIf(SE1->E1_SABTCSL>0,0,SE1->E1_CSLL) +IIf(SE1->E1_SABTCOF>0,0,SE1->E1_COFINS)+IIf(SE1->E1_SABTPIS>0,0,SE1->E1_PIS)+SE1->E1_ISS+SE1->E1_IRRF + SE1->E1_INSS)),"@E 999,999,999.99")),oFont06N )
		BuzzBox  (0600,0025,0700,0425)	// Instruções (Todas as Informações deste Bloqueto são de Exclusiva Responsabilidade do Cedente)
		oPrint:Say (0606, 0026,"Instruções (Todas as Informações deste Bloqueto são de Exclusiva Responsabilidade do Cedente)",oFont06N )
		
		If SE1->E1_ACRESC<>0
			If SE1->E1_XJUROS<>0
				oPrint:Say  (0626,0026,"Juros mora de R$ " + Alltrim(Transform(SE1->E1_XJUROS,"@E 99,999,999.99"))+ " de "+DToC(SE1->E1_VENCTO)+" a "+DToC(SE1->E1_VENCREA), oFont08,100)
			EndIF
			If SE1->E1_XMULTA<>0
				oPrint:Say  (0636,0026,"Multa por atraso do pagamento R$ " + Alltrim(Transform(SE1->E1_XMULTA,"@E 99,999,999.99")), oFont08,100)
			EndIF
		EndIF
		oPrint:Say  (0646,0026,"Sr. Caixa nao receber apos o vencimento.", oFont08,100)
		If !Empty(SE1->E1_DECRESC)
			oPrint:Say  (0656,0026,"Conceder Desconto de R$ ..... " + AllTrim(Transform((SE1->E1_DECRESC),"@E 99,999,999.99")), oFont08,100)
		EndIf
		BuzzBox  (0600,0425,0620,0560)	// (-) Desconto / Abatimento
		oPrint:Say (0606, 0426,"(-) Desconto / Abatimento",oFont06N )
		BuzzBox  (0620,0425,0640,0560)	// (-) Outras Deduções
		oPrint:Say (0626, 0426,"(-) Outras Deduções",oFont06N )
		BuzzBox  (0640,0425,0660,0560)	// (+) Mora / Multa
		oPrint:Say (0646, 0426,"(+) Mora / Multa",oFont06N )
		BuzzBox(0660,0425,0680,0560)	// (+) Outros Acrescimos
		oPrint:Say(0666, 0426,"(+) Outros Acrescimos",oFont06N )
		BuzzBox(0680,0425,0700,0560)	// (=) Valor Cobrado
		oPrint:Say(0686, 0426,"(=) Valor Cobrado",oFont06N )
		BuzzBox(0700,0025,0770,0560)	// Pagador / Pagador Avalista
		oPrint:Say(0706, 0026,"Pagador",oFont06N )
		oPrint:Say(0716,0106,Upper(SA1->A1_NOME),oFont08 ,100)
		oPrint:Say(0726,0106,SA1->(IIf(Empty(A1_ENDCOB),A1_END,A1_ENDCOB) + " " + IIf(Empty(SA1->A1_BAIRROC),SA1->A1_BAIRRO,SA1->A1_BAIRROC)),oFont08 ,100)
		oPrint:Say(0736,0106,SA1->(IIf(Empty(SA1->A1_CEPC),SA1->A1_CEP,SA1->A1_CEPC) + " " + IIf(Empty(SA1->A1_MUNC),SA1->A1_MUN,SA1->A1_MUNC) + " " + IIf(Empty(SA1->A1_ESTC),SA1->A1_EST,SA1->A1_ESTC)),oFont08 ,100)
		oPrint:Say(0746,0106,SA1->(Tran(A1_CGC,IIf(Len(AllTrim(A1_CGC))<12,"@r 999.999.999-99","@r 99.999.999/9999-99")) + "               " + A1_INSCR),oFont08 ,100)
		oPrint:Say(0768, 0026,"Pagador Avalista",oFont06N )
		oPrint:Say(0775,0350,"Autenticação Mecânica - Ficha de Compensação",oFont06,100)
		oPrint:FWMSBAR("INT25",66.2,2.0,_cCodBar,oPrint,.F.,,,,1.0,,,,.F.)  //28.0
		oPrint:EndPage()
		oPrint:Print()
		lRet:=.F.
		lRet:=CpyT2S("C:\Temp\"+cArquivo+".pdf",'\kit-clientes\',.T.,.T.)
		If !lRet
			MsgAlert("Não foi possível copiar o arquivo para o diretório do servidor.")
		EndIf

		If ALLTRIM(cAnexImp)=="SIM"
	        If ALLTRIM(SE1->E1_PARCELA)=='A' .OR. Empty(ALLTRIM(SE1->E1_PARCELA)) .OR. ALLTRIM(SE1->E1_PARCELA)=='1'
	     		IF !IsinCallStack("U_TELABOL")   	
					_cAnexo1    := "\kit-clientes\" + AllTrim(_cArquivo) + ".pdf"
					_cAnexo		:= _cAnexo1 + ";" + "\kit-clientes\" + AllTrim(cArquivo) + ".pdf"
				Else                                                        
					_cAnexo		:= "\kit-clientes\" + AllTrim(cArquivo) + ".pdf"
				Endif
			Else
				_cAnexo		:= "\kit-clientes\" + AllTrim(cArquivo) + ".pdf"
			Endif
	    ElseiF ALLTRIM(cAnexImp)=="NAO"
	    	If ALLTRIM(SE1->E1_PARCELA)=='A' .OR. Empty(ALLTRIM(SE1->E1_PARCELA)) .OR. ALLTRIM(SE1->E1_PARCELA)=='1'
					_cAnexo1    := "\kit-clientes\" + AllTrim(_cArquivo) + ".pdf"
					_cAnexo		:= _cAnexo1
			Else                  
					_cAnexo		:= ""
			Endif
	    Endif
		If AllTrim(SE1->E1_PREFIXO) == 'ND'
			_cTitulo := iif(alltrim(SM0->M0_CODIGO)=='02',"Faturamento WGSN / Mindset ","Faturamento Feira Bett Brasil Educar ")+" – Boleto e Fatura"			
			If alltrim(SM0->M0_CODIGO)=='01'
				_cMensagem	:= '	<html>'
				_cMensagem	+= '			<head>'
				_cMensagem	+= '				<title></title>'
				_cMensagem	+= '			</head>'
				_cMensagem	+= '			<body>'
				_cMensagem	+= '				<p>'
				_cMensagem	+= '					<strong><font face="Verdana" size="04px">Prezados Clientes,</font></strong></p>'
				_cMensagem	+= '				<p>'
				_cMensagem	+= '					<font face="Verdana" size="03px">Segue anexo a Fatura e o Boleto referente ao contrato da Feira Bett Brasil Educar - International Trade.</font></p>'
				_cMensagem	+= '				<p>'
				_cMensagem	+= '					<font face="Verdana" size="03px">Em caso de d&uacute;vidas ou sugest&otilde;es, por favor, entre em contato com o Financeiro pelo e-mail <u>cristiane.augusto@bettbrasileducar.com.br</u> ou pelo telefone (11)3042-7784 R. 210.</font></p>'
				_cMensagem	+= '				<p>'
				_cMensagem	+= '					<span style="color:#ff0000;"><font face="Verdana" size="03px">*Essa &eacute; uma mensagem autom&aacute;tica. Por favor, n&atilde;o responda a esse e-mail. *</font></span></p>'
				_cMensagem	+= '				<p>'
				_cMensagem	+= '					&nbsp;</p>'
				_cMensagem	+= '			</body>'
				_cMensagem	+= '		</html>'
					
					/*
					_cMensagem	:= '<HTML><HEAD><TITLE></TITLE>'
					_cMensagem	+= '<META http-equiv=Content-Type content="text/html; charset=windows-1252">'
					_cMensagem	+= '<META content="MSHTML 6.00.6000.16735" name=GENERATOR></HEAD>'
					_cMensagem	+= '<BODY>'
					_cMensagem	+= '<font color="black" face="Verdana" size="04px">Prezados Clientes,</font><br>'
					_cMensagem	+= '<p>&nbsp;</p>'
					_cMensagem	+= '<font color="black" face="Verdana" size="04px">Segue anexo a Fatura e o Boleto referente ao contrato da Feira Bett Brasil Educar - International Trade.</font><br>'
					_cMensagem	+= '<p>&nbsp;</p>'			
					_cMensagem	+= '<font color="black" face="Verdana" size="04px">Em caso de dúvidas ou sugestões, por favor, entre em contato com o Faturamento pelo e-mail marilyn.mutafci@ascential.com ou pelo telefone (11) 4858-9374 Ramal 1.</font><br>'
					_cMensagem	+= '<p>&nbsp;</p>'			
					_cMensagem	+= '<font color="red" face="Verdana" size="04px">*Essa é uma mensagem automática. Por favor, não responda a esse e-mail. *</font><br>'
					_cMensagem	+= '<p>&nbsp;</p>'			
					_cMensagem	+= '</BODY>'
					_cMensagem	+= '</HTML>'
					*/
			else
				_cMensagem	:= '	<html>'
				_cMensagem	+= '		<head>'
				_cMensagem	+= '			<title></title>'
				_cMensagem	+= '		</head>'
			    _cMensagem	+= '		<body>'
				_cMensagem	+= '			<p>'
				_cMensagem	+= '				<strong><font face="Verdana" size="04px">Prezados Clientes,</font></strong></p>'
				_cMensagem	+= '			<p>'
				_cMensagem	+= '				<font face="Verdana" size="03px">Segue anexo a Fatura e o Boleto referente ao contrato da WGSN - Ascential Servi&ccedil;os.&nbsp;</font></p>'
				_cMensagem	+= '			<p>'
				_cMensagem	+= '				<font face="Verdana" size="03px">Em caso de d&uacute;vidas entre em contato com Juciane Vargas respons&aacute;vel pelo Faturamento atrav&eacute;s do e-mail <u>marilyn.mutafci@ascential.com</u> ou via telefone (11) 4858-9374 Ramal 1.</font></p>'
				_cMensagem	+= '			<p>'
				_cMensagem	+= '				<span style="color:#ff0000;"><font face="Verdana" size="03px">*Essa &eacute; uma mensagem autom&aacute;tica. Por favor, n&atilde;o responda a esse e-mail. *</font><</span></p>'
				_cMensagem	+= '		</body>'
				_cMensagem	+= '	</html>'
			
					 /*
					_cMensagem	:= '<HTML><HEAD><TITLE></TITLE>'
					_cMensagem	+= '<META http-equiv=Content-Type content="text/html; charset=windows-1252">'
					_cMensagem	+= '<META content="MSHTML 6.00.6000.16735" name=GENERATOR></HEAD>'
					_cMensagem	+= '<BODY>'
					_cMensagem	+= '<font color="black" face="Verdana" size="04px">Prezados Clientes,</font><br>'
					_cMensagem	+= '<p>&nbsp;</p>'
					_cMensagem	+= '<font color="black" face="Verdana" size="04px">Segue anexo a Fatura e o Boleto referente ao contrato da WGSN - Ascential Serviços. </font><br>'
					_cMensagem	+= '<p>&nbsp;</p>'			
					_cMensagem	+= '<font color="black" face="Verdana" size="04px">Em caso de dúvidas entre em contato com Marilyn Mutafci responsável pelo Faturamento através do e-mail marilyn.mutafci@ascential.com ou via telefone (11) 4858-9374 Ramal 1.</font><br>'
					_cMensagem	+= '<p>&nbsp;</p>'			
					_cMensagem	+= '<font color="red" face="Verdana" size="04px">*Essa é uma mensagem automática. Por favor, não responda a esse e-mail. *</font><br>'
					_cMensagem	+= '<p>&nbsp;</p>'			
					_cMensagem	+= '</BODY>'
					_cMensagem	+= '</HTML>'
					*/				
			endif
			
			//_cMensagem	:= " Esta é uma mensagem gerada automaticamente pelo sistema, portanto não pode ser respondida. " + CHR(13) + CHR(10)
			//_cMensagem	+= " Em caso de dúvidas ou sugestões, por favor, entre em contato com o Faturamento pelo e-mail marilyn.mutafci@ascential.com ou pelo telefone (11) 3372-7272 Ramal 1. "
		Else
			_cTitulo := iif(alltrim(SM0->M0_CODIGO)=='02',"Faturamento WGSN / Mindset ","Faturamento Feira Bett Brasil Educar")+" – Boleto e Nota Fiscal (RPS)"
			
			   If alltrim(SM0->M0_CODIGO)=='01'
					_cMensagem	:= '<html>'
					_cMensagem	+= '	<head>'
					_cMensagem	+= '		<title></title>'
					_cMensagem	+= '	</head>'
					_cMensagem	+= '<body>'
					_cMensagem	+= '<p>'
					_cMensagem	+= '	<strong><font face="Verdana" size="04px">Prezados Clientes,</font></strong></p>'
					_cMensagem	+= '<p>'
					_cMensagem	+= '	<font face="Verdana" size="03px">Segue anexo a RPS e o Boleto referente ao contrato da Feira Bett Brasil Educar - International Trade.</font></p>'
					_cMensagem	+= '<p>'
					_cMensagem	+= '	<font face="Verdana" size="03px"><strong>OBS*</strong> A RPS &eacute; v&aacute;lida como Documento Fiscal, nela cont&eacute;m a informa&ccedil;&atilde;o do C&oacute;digo de Verifica&ccedil;&atilde;o para gerar o PDF da NF Eletr&ocirc;nica.</font></p>'
					_cMensagem	+= '<p>'
					_cMensagem	+= '	<font face="Verdana" size="03px">Para visualizar a nota fiscal emitida, favor entrar no link https://nfe.prefeitura.sp.gov.br/publico/verificacao.aspx</font></p>'
					_cMensagem	+= '<p>'
					_cMensagem	+= '	<font face="Verdana" size="03px">Em caso de d&uacute;vidas entre em contato com Cristiane Augusto respons&aacute;vel pelo Financeiro atrav&eacute;s do e-mail </span><u style="font-family: Verdana; font-size: large;">cristiane.augusto@bettbrasileducar.com.br</u><span style="font-family: Verdana; font-size: large;"> ou via telefone (11)3042-7784 R. 210.</font></p>'
					_cMensagem	+= '<p>'
					_cMensagem	+= '   <span style="color:#ff0000;"><font face="Verdana" size="03px">*Essa &eacute; uma mensagem autom&aacute;tica. Por favor, n&atilde;o responda a esse e-mail. *</font></span></p>'
					_cMensagem	+= '  </body>'
					_cMensagem	+= '</html>'
											
					/*				
					_cMensagem	:= '<HTML><HEAD><TITLE></TITLE>'
					_cMensagem	+= '<META http-equiv=Content-Type content="text/html; charset=windows-1252">'
					_cMensagem	+= '<META content="MSHTML 6.00.6000.16735" name=GENERATOR></HEAD>'
					_cMensagem	+= '<BODY>'
					_cMensagem	+= '<font color="black" face="Verdana" size="04px">Prezados Clientes,</font><br>'
					_cMensagem	+= '<p>&nbsp;</p>'
					_cMensagem	+= '<font color="black" face="Verdana" size="04px">Segue anexo a RPS e o Boleto referente ao contrato da Feira Bett Brasil Educar - International Trade.</font><br>'
					_cMensagem	+= '<p>&nbsp;</p>'
					_cMensagem	+= '<font color="black" face="Verdana" size="04px">OBS* A RPS é válida como Documento Fiscal, nela contém a informação do Código de Verificação para gerar o PDF da NF Eletrônica.</font><br>'
					_cMensagem	+= '<p>&nbsp;</p>'
					_cMensagem	+= '<font color="black" face="Verdana" size="04px">Para visualizar a nota fiscal emitida, favor entrar no link  https://nfe.prefeitura.sp.gov.br/publico/verificacao.aspx</font><br>'
					_cMensagem	+= '<p>&nbsp;</p>'
					_cMensagem	+= '<font color="black" face="Verdana" size="04px">Em caso de dúvidas entre em contato com Marilyn Mutafci responsável pelo Faturamento através do e-mail marilyn.mutafci@ascential.com ou via telefone (11) 4858-9374 Ramal 1.</font><br>'
					_cMensagem	+= '<p>&nbsp;</p>'
					_cMensagem	+= '<font color="red" face="Verdana" size="04px">*Essa é uma mensagem automática. Por favor, não responda a esse e-mail. *</font><br>'
					_cMensagem	+= '<p>&nbsp;</p>'
					_cMensagem	+= '</BODY>'
					_cMensagem	+= '</HTML>'
					 */				
				else
					_cMensagem	:= '<html>'
					_cMensagem	+= '	<head>'
					_cMensagem	+= '		<title></title>'
					_cMensagem	+= '	</head>'
					_cMensagem	+= '	<body>'
					_cMensagem	+= '		<p>'
					_cMensagem	+= '			<strong><font face="Verdana" size="04px">Prezados Clientes,</font></strong></p>'
					_cMensagem	+= '		<p>'
					_cMensagem	+= '			<font face="Verdana" size="03px">Segue anexo a RPS e o Boleto referente ao contrato da Ascential Servi&ccedil;os.&nbsp; de Informa&ccedil;&otilde;es Ltda - Grupo (</font></p>'
					_cMensagem	+= '		<p>'
					_cMensagem	+= '			<font face="Verdana" size="03px"><strong>OBS*</strong> A RPS &eacute; v&aacute;lida como Documento Fiscal, nela cont&eacute;m a informa&ccedil;&atilde;o do C&oacute;digo de Verifica&ccedil;&atilde;o para gerar o PDF da NF Eletr&ocirc;nica.</font></p>'
					_cMensagem	+= '		<p>'
					_cMensagem	+= '			<font face="Verdana" size="03px">Para visualizar a nota fiscal emitida, favor entrar no link <u>https://nfe.prefeitura.sp.gov.br/publico/verificacao.aspx</u>&nbsp;</font></p>'
					_cMensagem	+= '		<p>'
					_cMensagem	+= '			<font face="Verdana" size="03px">Em caso de d&uacute;vidas entre em contato com Marilyn Mutafci respons&aacute;vel pelo Faturamento atrav&eacute;s do e-mail <u>marilyn.mutafci@ascential.com</u> ou via telefone (11) 4858-9374 Ramal 1.</font></p>'
					_cMensagem	+= '		<p>'
					_cMensagem	+= '			<span style="color:#ff0000;"><font face="Verdana" size="03px">*Essa &eacute; uma mensagem autom&aacute;tica. Por favor, n&atilde;o responda a esse e-mail. *</font></span></p>'
					_cMensagem	+= '	</body>'
					_cMensagem	+= '</html>'
					/*
					_cMensagem	:= '<html>'
					_cMensagem	+= '	<head>'
					_cMensagem	+= '		<title></title>'
					_cMensagem	+= '	</head>'
					_cMensagem	+= '	<body>'
					_cMensagem	+= '		<p>'
					_cMensagem	+= '			<strong><font face="Verdana" size="04px">Prezados Clientes,</font></strong></p>'
					_cMensagem	+= '		<p>'
					_cMensagem	+= '			<font face="Verdana" size="03px">Segue anexo a RPS e o Boleto referente ao contrato da WGSN - Ascential Servi&ccedil;os.&nbsp;</font></p>'
					_cMensagem	+= '		<p>'
					_cMensagem	+= '			<font face="Verdana" size="03px"><strong>OBS*</strong> A RPS &eacute; v&aacute;lida como Documento Fiscal, nela cont&eacute;m a informa&ccedil;&atilde;o do C&oacute;digo de Verifica&ccedil;&atilde;o para gerar o PDF da NF Eletr&ocirc;nica.</font></p>'
					_cMensagem	+= '		<p>'
					_cMensagem	+= '			<font face="Verdana" size="03px">Para visualizar a nota fiscal emitida, favor entrar no link <u>https://nfe.prefeitura.sp.gov.br/publico/verificacao.aspx</u>&nbsp;</font></p>'
					_cMensagem	+= '		<p>'
					_cMensagem	+= '			<font face="Verdana" size="03px">Em caso de d&uacute;vidas entre em contato com Marilyn Mutafci respons&aacute;vel pelo Faturamento atrav&eacute;s do e-mail <u>marilyn.mutafci@ascential.com</u> ou via telefone (11) 4858-9374 Ramal 1.</font></p>'
					_cMensagem	+= '		<p>'
					_cMensagem	+= '			<span style="color:#ff0000;"><font face="Verdana" size="03px">*Essa &eacute; uma mensagem autom&aacute;tica. Por favor, n&atilde;o responda a esse e-mail. *</font></span></p>'
					_cMensagem	+= '	</body>'
					_cMensagem	+= '</html>'
					*/
					   	/*
					   	_cMensagem	:= '<HTML><HEAD><TITLE></TITLE>'
						_cMensagem	+= '<META http-equiv=Content-Type content="text/html; charset=windows-1252">'
						_cMensagem	+= '<META content="MSHTML 6.00.6000.16735" name=GENERATOR></HEAD>'
						_cMensagem	+= '<BODY>'
						_cMensagem	+= '<font color="black" face="Verdana" size="04px">Prezados Clientes,</font><br>'
						_cMensagem	+= '<p>&nbsp;</p>'
						_cMensagem	+= '<font color="black" face="Verdana" size="04px">Segue anexo a RPS e o Boleto referente ao contrato da WGSN - Ascential Serviços. </font><br>'
						_cMensagem	+= '<p>&nbsp;</p>'				
						_cMensagem	+= '<font color="black" face="Verdana" size="04px">OBS* A RPS é válida como Documento Fiscal, nela contém a informação do Código de Verificação para gerar o PDF da NF Eletrônica.</font><br>'
						_cMensagem	+= '<p>&nbsp;</p>'				
						_cMensagem	+= '<font color="black" face="Verdana" size="04px">Para visualizar a nota fiscal emitida, favor entrar no link  https://nfe.prefeitura.sp.gov.br/publico/verificacao.aspx </font><br>'
						_cMensagem	+= '<p>&nbsp;</p>'
						_cMensagem	+= '<font color="black" face="Verdana" size="04px">Em caso de dúvidas entre em contato com Marilyn Mutafci responsável pelo Faturamento através do e-mail marilyn.mutafci@ascential.com ou via telefone (11) 4858-9374 Ramal 1.</font><br>'
						_cMensagem	+= '<p>&nbsp;</p>'
						_cMensagem	+= '<font color="red" face="Verdana" size="04px">*Essa é uma mensagem automática. Por favor, não responda a esse e-mail. *</font><br>'
						_cMensagem	+= '<p>&nbsp;</p>'
						_cMensagem	+= '</BODY>'
						_cMensagem	+= '</HTML>'
					  */ 				   	
			   endif
			
		EndIf
	    If cPfx+cNum+cCLi+cLoj+cParc<>SE1->E1_PREFIXO+SE1->E1_NUM+SE1->E1_CLIENTE+SE1->E1_LOJA+SE1->E1_PARCELA
	    	//Aqui estamos forçando que não seja enviado dois e-mails para o mesmo cliente com mesmo conteudo.
	    	If !Empty(_cAnexo)
				U__MailEnviar(_cAnexo, _cTitulo,_cMensagem, SE1->E1_PREFIXO, SE1->E1_NUM, SE1->E1_CLIENTE, SE1->E1_LOJA, SE1->E1_PARCELA)
			Endif
			cPfx :=SE1->E1_PREFIXO
			cNum :=SE1->E1_NUM
			cCLi :=SE1->E1_CLIENTE
			cLoj :=SE1->E1_LOJA
			cParc:=SE1->E1_PARCELA
		Endif

		//If MsgYesNo("Apagar o arquivo")
			//Limpa Boletos
			Ferase("\kit-clientes\" + AllTrim(cArquivo) + ".pdf")
			Ferase("C:\Temp\" + AllTrim(cArquivo) + ".pdf")
			//Limpa Nota Fiscal
			Ferase("\kit-clientes\" + AllTrim(_cArquivo) + ".pdf")
			Ferase("C:\Temp\" + AllTrim(_cArquivo) + ".pdf")			
		//Endif                          
  Endif
Next

If !AllTrim(FunName()) == "TELEMAI"
	MsgInfo("Foram Salvos " + AllTrim(Str(nCont)) + " Boletos no Diretório C:\Temp\")
EndIf
If AllTrim(FunName()) == "TELEMAI" // Se entrar aqui eh porque esta vindo da NFE
	Return
Else
	Close(oDlg)
EndIf
Return

/*ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ0ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±ºPrograma ³ BuzzBox         ºAutor³ Silvio Cazela              º Data ³ 24/04/2013 º±±
±±ÌÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºDescricao³ Desenha um Box Sem Preenchimento                                       º±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß*/

Static Function BuzzBox(_nLinIni,_nColIni,_nLinFin,_nColFin) // < nRow>, < nCol>, < nBottom>, < nRight>
oPrint:Line( _nLinIni,_nColIni,_nLinIni,_nColFin,CLR_BLACK, "-2")
oPrint:Line( _nLinFin,_nColIni,_nLinFin,_nColFin,CLR_BLACK, "-2")
oPrint:Line( _nLinIni,_nColIni,_nLinFin,_nColIni,CLR_BLACK, "-2")
oPrint:Line( _nLinIni,_nColFin,_nLinFin,_nColFin,CLR_BLACK, "-2")
Return

/*ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±ºPrograma ³ Tr4         ºAutor³ Eduardo Augusto		         º Data ³ 24/04/2013 º±±
±±ÌÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºDescricao³ Alinhar os Valores a Direta	                                         º±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß*/

User Function Tr4(cValor)
Local cValor2 := AllTrim(cValor)
cValor2	:= Replicate(" ",20 - (Len(cValor2) * 2)) + cValor2
If Len(AllTrim(cValor2)) >= 8
	cValor2	:= " " + cValor2
EndIf
Return cValor2
