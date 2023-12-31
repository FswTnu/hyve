#INCLUDE "RWMAKE.CH"
#INCLUDE "TBICONN.CH"
#INCLUDE "TOPCONN.CH"
#INCLUDE "AP5MAIL.CH"

/*���������������������������������������������������������������������������
���Programa  �_MailEnviar �Autor  �Valdemir Jos�     � Data �  01/11/13   ���
���          � Alterado   �Autor  �Eduardo Augusto   � Data �  19/12/14   ���
�������������������������������������������������������������������������͹��
���Desc.     � Envia e-mail com anexo, conforme parametros informado      ���
���          �                                                            ���
�������������������������������������������������������������������������͹��
���Parametro � _cAnexo    - Arquivo a ser anexado que esteja dentro system���
���          � _cTitulo   - Assunto do e-mail                             ���
���          � _cMensagem - Mensagem do corpo do e-mail                   ���
�������������������������������������������������������������������������͹��
���Uso       � AP                                                         ���
���������������������������������������������������������������������������*/

User Function _MailEnviar(_cAnexo, _cTitulo, _cMensagem, _cPrf, _cNum, _cCli, _cLoja)
Local _cConta  := GETMV("MV_RELACNT")
Local _cServer := GETMV("MV_RELSERV")
Local _cSenha  := GETMV("MV_RELPSW")
Local _cPara   := Alltrim(SA1->A1_EMAIL)+IIf(SM0->M0_CODIGO == "01",";faturamento@bettbrasileducar.com.br",";faturamento@wgsn.com") 
//Local _cPara   := 'mariogoj@gmail.com;gabrielsilva@totvs.com.br'
Local _aArMail := GetArea()
Local cPrf	:= _cPrf
Local cNum	:= _cNum
Local cCli	:= _cCli 
Local cLoja	:= _cLoja

If Empty(_cPara)
	MsgAlert("E-mail do cliente n�o cadastrado! Cadastre e repita o processo!","E-mail n�o enviado")
	RestArea(_aArMail)
	Return
EndIf

ProcRegua(50)
IncProc("Enviando E-Mail...")
If !Empty(_cServer) .And. !Empty(_cConta) .And. !Empty(_cSenha)
	CONNECT SMTP SERVER _cServer  ACCOUNT _cConta PASSWORD _cSenha RESULT lOk
	MAILAUTH(Alltrim(_cConta),Alltrim(_cSenha))
	If lOk
		If Empty(_cAnexo)
			SEND MAIL FROM _cConta to _cPara SUBJECT _cTitulo BODY _cMensagem RESULT lSendOk
		Else
			SEND MAIL FROM _cConta to _cPara SUBJECT _cTitulo BODY _cMensagem ATTACHMENT _cAnexo RESULT lSendOk
		Endif
		If !lSendOk
			GET MAIL ERROR cError
			MsgInfo("Ocorreu um erro no envio do WorkFlow !"+Chr(13)+;
			"Informe o Departamento de TI.",OemToAnsi("Erro de Envio"))
			lVeriSma:= .F.
		EndIf
		If lSendOk
			DbSelectArea("SF2")
			DbSetOrder(1)
			If MsSeek(xFilial("SF2")+cNum+cPrf+cCli+cLoja)
				RecLock("SF2")
					F2_XOK:="S"
					F2_XDTENVM:=DATE()
				MsUnlock()
			Endif
		Endif

		DISCONNECT SMTP SERVER
	Else
		GET MAIL ERROR cError
		MsgInfo("Informe o Departamento de TI o erro abaixo : "+Chr(13)+cError,OemToAnsi("Erro de Conexao"))
		lVeriSma:= .F.
	EndIf
EndIf
RestArea(_aArMail)

	 
Return()