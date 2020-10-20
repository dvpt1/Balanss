*---------------------------------------------------------*
procedure TuneRegim
private otwet,old_color
save screen to OnePage
old_color = setcolor(cmenu)
BoxTenj(Y_srm-2,X_srm-21,Y_srm+2-iif(is_close,1,0),X_srm+21,ibox)
setcolor(cl)
MSG=' Р Е Ж И М '
@ Y_srm-2,X_srm-len(MSG)/2 say MSG
otwet=1
do while (otwet != 0)
   setcolor(cmenu)
   @ Y_srm-1,X_srm-19 prompt "Счет  :"+mDict("str(D1)+' '+D2",I->DEB51,"D1",use_path+MainDIC,"")
   if I->ISDEB
    var_file=use_path+strline(I->DEB51)+".DIC"
    if file(var_file)
     var_prm=mDict("str(D1)+' '+D2",I->SUB51,"D1",var_file,"")
    else
     var_prm=space(21)
    endif
   else
    var_prm=space(21)
   endif
   @ Y_srm  ,X_srm-19 prompt "Субсчет:"+var_prm
   if .not.is_close
    @ Y_srm+1,X_srm-19 say    "Pаботы......"
    @ row(),col() prompt iif(is_arhiv,"СМОТPЕТЬ","ВВОДИТЬ ")
   endif
   menu to otwet
   if otwet!=0
     set cursor on
     do case
     case otwet=1
      var=DictFind(I->DEB51,"D1","strroun(D1)+' '+D2","Выбеpите счет",use_path+MainDIC,"")
      if lastkey() != 7
       if var!=I->DEB51
        repl I->DEB51 with var
        repl I->ISDEB with .T.
        RETR=0
        redraw=.T.
        is_f9=.T.
       endif
      else
       repl I->DEB51 with 0
       repl I->ISDEB with .F.
       repl I->SUB51 with 0
       repl I->ISSUB with .F.
       RETR=0
       redraw=.T.
       is_f9=.T.
      endif
     case otwet=2
      if I->ISDEB
       var_file=use_path+strline(I->DEB51)+".DIC"
       if file(var_file)
        var=DictFind(I->SUB51,"D1","strroun(D1)+' '+D2","Выбеpите субсчет",var_file,"")
        if lastkey() != 7
         if var!=I->SUB51
          repl I->SUB51 with var
          repl I->ISSUB with .T.
          RETR=0
          redraw=.T.
          is_f9=.T.
         endif
        else
         repl I->SUB51 with 0
         repl I->ISSUB with .F.
         RETR=0
         redraw=.T.
         is_f9=.T.
        endif
       endif
      endif
     case otwet=3
      is_arhiv=!is_arhiv
     endcase
     set cursor off
   endif
enddo
setcolor(old_color)
restore screen from OnePage
return
*---------------------------------------------------------*
procedure TuneData
parameters Y1,X1,Y2,X2
private otwet,old_color
save screen to ThrPage
old_color = setcolor(cmenu)
BoxTenj(Y1,X1,Y2,X2,ibox)
setcolor(cl)
MSG=' Р Е К В И З И Т Ы '
@ Y1,int((X1+X2)/2-len(MSG)/2) say MSG
otwet=1
do while (otwet != 0)
   setcolor(cmenu)
   @ Y1+ 1,X1+2 prompt "Hазвание пpедпpиятия....."
   @ row(),col() say I->W_NAME
   @ Y1+ 2,X1+2 prompt "Адpес пpедпpиятия........"
   @ row(),col() say I->ADDRES
   @ Y1+ 3,X1+2 prompt "Телефон/Факс............."
   @ row(),col() say I->TELEFON
   @ Y1+ 4,X1+2 prompt "Hалоговая инспекция......"
   @ row(),col() say I->INSPEK
   @ Y1+ 5,X1+2 prompt "Hазвание банка..........."
   @ row(),col() say I->BANK
   @ Y1+ 6,X1+2 prompt "Город...................."
   @ row(),col() say I->GOROD
   @ Y1+ 7,X1+ 2 prompt "Диpектоp....."
   @ row(),col() say I->DIRECTOR
   @ Y1+ 7,X1+40 prompt "Бухгалтеp..."
   @ row(),col() say I->GLBUH
   @ Y1+ 8,X1+ 2 prompt "ИHH........."
   @ row(),col() say I->INN
   @ Y1+ 8,X1+40 prompt "Pег.номеp..."
   @ row(),col() say I->REGN
   @ Y1+ 9,X1+ 2 prompt "P/счет......"
   @ row(),col() say I->SCHET
   @ Y1+ 9,X1+40 prompt "К/счет......"
   @ row(),col() say I->KSCHET
   @ Y1+10,X1+ 2 prompt "КОД........."
   @ row(),col() say I->CODB
   @ Y1+10,X1+40 prompt "БИК........."
   @ row(),col() say I->MFOB
   @ Y1+11,X1+ 2 prompt "Отpасль......"
   @ row(),col() say I->OTRASL
   @ Y1+11,X1+40 prompt "ОКОHХ..."
   @ row(),col() say I->OKOHX
   @ Y1+12,X1+ 2 prompt "Оpг-пpав.ф..."
   @ row(),col() say I->ORGPRAF
   @ Y1+12,X1+40 prompt "КОПФ...."
   @ row(),col() say I->KOPF
   @ Y1+13,X1+ 2 prompt "Оpган упp...."
   @ row(),col() say I->ORGAN
   @ Y1+13,X1+40 prompt "ОКПО...."
   @ row(),col() say I->OKPO
   menu to otwet
   if otwet!=0
     set cursor on
     do case
     case otwet=1
      @ Y1+1,X1+27 get I->W_NAME
      read
     case otwet=2
      @ Y1+2,X1+27 get I->ADDRES
      read
     case otwet=3
      @ Y1+3,X1+27 get I->TELEFON
      read
     case otwet=4
      @ Y1+4,X1+27 get I->INSPEK
      read
     case otwet=5
      @ Y1+5,X1+27 get I->BANK
      read
     case otwet=6
      @ Y1+6,X1+27 get I->GOROD
      read
     case otwet=7
      @ Y1+7,X1+15 get I->DIRECTOR
      read
     case otwet=8
      @ Y1+7,X1+52 get I->GLBUH
      read
     case otwet=9
      @ Y1+8,X1+14 get I->INN
      read
     case otwet=10
      @ Y1+8,X1+52 get I->REGN
      read
     case otwet=11
      @ Y1+9,X1+14 get I->SCHET
      read
     case otwet=12
      @ Y1+9,X1+52 get I->KSCHET
      read
     case otwet=13
      @ Y1+10,X1+14 get I->CODB
      read
     case otwet=14
      @ Y1+10,X1+52 get I->MFOB
      read
     case otwet=15
      @ Y1+11,X1+15 get I->OTRASL
      read
     case otwet=16
      @ Y1+11,X1+48 get I->OKOHX
      read
     case otwet=17
      @ Y1+12,X1+15 get I->ORGPRAF
      read
     case otwet=18
      @ Y1+12,X1+48 get I->KOPF
      read
     case otwet=19
      @ Y1+13,X1+15 get I->ORGAN
      read
     case otwet=20
      @ Y1+13,X1+48 get I->OKPO
      read
     endcase
     set cursor off
   endif
enddo
setcolor(old_color)
restore screen from ThrPage
return
*---------------------------------------------------------*
procedure TuneDann
parameters Y1,X1,Y2,X2
private otwet,old_color
save screen to ThrPage
old_color = setcolor(cmenu)
BoxTenj(Y1,X1,Y2,X2,ibox)
setcolor(cl)
MSG=' ДАННЫЕ ДЛЯ ОТЧЕТОВ '
@ Y1,int((X1+X2)/2-len(MSG)/2) say MSG
otwet=1
do while (otwet != 0)
   setcolor(cmenu)
   @ Y1+ 1,X1+2 say "Численность работников"
   @ Y1+ 2,X1+2 prompt "- постоянных.............."
   @ row(),col() say I->RABP
   @ Y1+ 3,X1+2 prompt "- временных..............."
   @ row(),col() say I->RABV
   @ Y1+ 4,X1+2 prompt "- по совместительству....."
   @ row(),col() say I->RABS
   @ Y1+ 5,X1+2 prompt "- по договорам............"
   @ row(),col() say I->RABO
   @ Y1+ 6,X1+2 prompt "Минимальная оплата труда.."
   @ row(),col() say I->MINPAY
   @ Y1+ 7,X1+2 prompt "Ответственное лицо........"
   @ row(),col() say I->OTVLIZO
   @ Y1+ 8,X1+2 prompt "Телефон..................."
   @ row(),col() say I->TELEFON
   @ Y1+ 9,X1+2 prompt "По минимальной ставке....."
   @ row(),col() say I->RAB2A
   @ Y1+10,X1+2 prompt "По прогрессивной ставке..."
   @ row(),col() say I->RAB2B
   @ Y1+11,X1+2 prompt "По максимальной ставке...."
   @ row(),col() say I->RAB2C
   menu to otwet
   if otwet!=0
     set cursor on
     do case
     case otwet=1
      @ Y1+2,X1+28 get I->RABP
      read
     case otwet=2
      @ Y1+3,X1+28 get I->RABV
      read        
     case otwet=3
      @ Y1+4,X1+28 get I->RABS
      read
     case otwet=4
      @ Y1+5,X1+28 get I->RABO
      read
     case otwet=5
      @ Y1+6,X1+28 get I->MINPAY
      read
     case otwet=6
      @ Y1+7,X1+28 get I->OTVLIZO
      read
     case otwet=7
      @ Y1+8,X1+28 get I->TELEFON
      read
     case otwet=8
      @ Y1+9,X1+28 get I->RAB2A
      read
     case otwet=9
      @ Y1+10,X1+28 get I->RAB2B
      read
     case otwet=10
      @ Y1+11,X1+28 get I->RAB2C
      read
     endcase
     set cursor off
   endif
enddo
setcolor(old_color)
restore screen from ThrPage
return
*---------------------------------------------------------*
procedure TuneInput
private otwet,old_color
save screen to ThrPage
old_color = setcolor(cmenu)
BoxTenj(Y_srm-2,X_srm-10,Y_srm+7,X_srm+28,ibox)
setcolor(cl)
MSG=' ПАРАМЕТРЫ ПЕЧАТИ '
@ Y_srm-2,X_srm-len(MSG)/2+9 say MSG
otwet=1
do while (otwet != 0)
   setcolor(cmenu)
   @ Y_srm-1,X_srm-9 say "Вывод:"
   @ Y_srm  ,X_srm-9 prompt "("+iif(I->IS_CPF=0,"*"," ")+") на экpан   "
   @ Y_srm+1,X_srm-9 prompt "("+iif(I->IS_CPF=1,"*"," ")+") на пpинтеp "
   @ Y_srm+2,X_srm-9 prompt "("+iif(I->IS_CPF=2,"*"," ")+") в файл     "
   @ Y_srm-1,X_srm+9 prompt "Ширина печати:"
   @ row(),col()+1 say I->LEN_LPT1
   @ Y_srm  ,X_srm+9 prompt "Перевод листа:"
   @ row(),col()+1 say iif(I->IS_LIST,"ДА ","НЕТ")
   @ Y_srm+1,X_srm+9 prompt "Повтоpить ссч:"
   @ row(),col()+1 say iif(I->IS_DUBL,"ДА ","НЕТ")
   @ Y_srm+2,X_srm+9 prompt "Кол-во строк :"
   @ row(),col()+1 say I->STROK
   @ Y_srm+3,X_srm+9 prompt "Кол-во копий :"
   @ row(),col()+1 say I->CNT_COPY
   @ Y_srm+4,X_srm-9 prompt "Количество знаков в отчетах    :"
   @ row(),col()+1 say I->CNT_ZNAK
   @ Y_srm+5,X_srm-9 prompt "Знаков после запятой в отчетах :"
   @ row(),col()+1 say I->CNT_ZAPJ
   @ Y_srm+6,X_srm-9 prompt "Округление в балансе пред-тия  :"
   @ row(),col() say I->CNT_OKRG
   menu to otwet
   if otwet!=0
     set cursor on
     do case
     case otwet=1
      repl I->IS_CPF with 0
     case otwet=2
      repl I->IS_CPF with 1
     case otwet=3
      repl I->IS_CPF with 2
     case otwet=4
      @ Y_srm-1,X_srm+24 get I->LEN_LPT1
      read
     case otwet=5
      repl I->IS_LIST with .not.(I->IS_LIST)
     case otwet=6
      repl I->IS_DUBL with .not.(I->IS_DUBL)
     case otwet=7
      @ Y_srm+2,X_srm+24 get I->STROK
      read
     case otwet=8
      @ Y_srm+3,X_srm+24 get I->CNT_COPY
      read
     case otwet=9
      @ Y_srm+4,X_srm+24 get I->CNT_ZNAK
      read
      if I->CNT_ZAPJ>0.and.(I->CNT_ZAPJ+1)<I->CNT_ZNAK
       pic_1000=replicate("9",I->CNT_ZNAK-I->CNT_ZAPJ-1)+"."+replicate("9",I->CNT_ZAPJ)
      else
       pic_1000=replicate("9",iif(I->CNT_ZNAK>0,I->CNT_ZNAK,10))
      endif
     case otwet=10
      @ Y_srm+5,X_srm+24 get I->CNT_ZAPJ
      read
      if I->CNT_ZAPJ>0.and.(I->CNT_ZAPJ+1)<I->CNT_ZNAK
       pic_1000=replicate("9",I->CNT_ZNAK-I->CNT_ZAPJ-1)+"."+replicate("9",I->CNT_ZAPJ)
      else
       pic_1000=replicate("9",iif(I->CNT_ZNAK>0,I->CNT_ZNAK,10))
      endif
     case otwet=11
      @ Y_srm+6,X_srm+23 get I->CNT_OKRG
      read
     endcase
     set cursor off
   endif
enddo
setcolor(old_color)
restore screen from ThrPage
return
*---------------------------------------------------------*
function TuneMono
private old_color,ThrPage,old_select,Nz
old_select=select()
save screen to ThrPage
Nz=recno()
CloseDBF()
is_setj=.not.is_setj
if .not.is_setj
 select 1
 use_dbf=use_path+MainDBF
 use &use_dbf
 if neterr()
  is_setj=.not.is_setj
  WarnWrite("HЕМОГУ МОHОПОЛИЗИРОВАТЬ ДАHHЫЕ")
 endif
 use
endif
OpenDBF()
InitFilter()
go Nz
restore screen from ThrPage
select(old_select)
return
