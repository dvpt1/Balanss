*---------------------------------------------------------*
procedure TuneDocum
parameters is_case,pMSG,ext
private old_color
save screen to TunePage
old_color = setcolor(cinp)
DrawBox(Y_min,X_min,Y_max,X_max,ibox)
setcolor(cl)
MSG=pMSG
@ Y_min,int((X_min+X_max)/2-len(MSG)/2) say MSG
MenuTune()
setcolor(cinp)
select 7
do case
case is_case=1
 use_dbf=use_path+OtchetDIC
 use_str=OtchetSTR
case is_case=2
 use_dbf=use_path+BalanpDIC
 use_str=BalanpSTR
endcase
if .not.file(use_dbf)
 creat &use_dbf from &use_str
 use
endif
if is_setj
 use &use_dbf SHARED
else
 use &use_dbf
endif
decl tune_fld[2],tune_nam[2]
tune_nam[1]="Hаименование;;"
tune_fld[1]="DOC_NAM"
tune_nam[2]="Имя;файла"
tune_fld[2]="DOC_DOC"
dbedit(Y_min+1,X_min+1,Y_max-1,X_max-1,tune_fld,"TuneFunc","",tune_nam,"───"," ")
use
select 1
setcolor(old_color)
rest screen from TunePage
return
*-------------------------------*
function tunefunc
parameters mode,ptr
private RETT
RETT=1
do case
case mode=0
case mode = 3
case mode = 4
 do case
 case lastkey() = 22       && INS
  insert()
  RETT=2
 case lastkey() = -1       && F2
  do case
  case is_case=1
   TuneOtch(1)
  case is_case=2
   txtedit(use_path+alltrim(DOC_DOC)+ext)
  endcase
 case lastkey() = -2       && F3
  do case
  case is_case=1
   TuneOtch(2)
  endcase
 case lastkey() = 13       && Enter
  if .not.OnSetj()
   return 1
  endif
  do case
  case ptr=1
   set cursor on
   @ row(),col() get DOC_NAM
   read
   set cursor off
  case ptr=2
   old_doc=DOC_DOC
   set cursor on
   @ row(),col() get DOC_DOC picture "@! NNNNNNNN"
   read
   set cursor off
   if .not.(old_doc==DOC_DOC)
    old_doc=use_path+alltrim(old_doc)+ext
    if file(old_doc)
     new_doc=use_path+alltrim(DOC_DOC)+ext
     renam &old_doc to &new_doc
    endif
   endif
  endcase
  UnSetj()
 case lastkey()=   7       && DEL
  DeletDoc()
  return 2
 case lastkey()=27
  RETT=0
 endcase
endcase
return RETT
*---------------------------------------------------------*
procedure MenuTune
set color to &cl
@ Y_max,X_min+10 say 'МЕНЮ:'
set color to &cmenu
MSG='INS'
@ row(),col() get MSG
@ row(),col() say 'Создать'
MSG='F2'
@ row(),col() get MSG
@ row(),col() say 'Отчет'
if is_case=1.or.is_case=3
 MSG='F3'
 @ row(),col() get MSG
 @ row(),col() say 'Данные'
endif
if is_case=3
 MSG='F4'
 @ row(),col() get MSG
 @ row(),col() say 'Подпись'
endif
MSG='Enter'
@ row(),col() get MSG
@ row(),col() say 'Изменить'
MSG='DEL'
@ row(),col() get MSG
@ row(),col() say 'Удалить'
MSG='ESC'
@ row(),col() get MSG
@ row(),col() say 'Выход'
clear gets
return
*---------------------------------------------------------*
procedure DeletDoc
if .not.Pravo0()
 return
endif
if IsSetj()
 return
endif
if Fragen('','Удалить документ?')
 save scre
 MsgWrite('Удаляю...')
 file_nam=alltrim(DOC_DOC)
 file_dc=use_path+file_nam+ext
 delete file &file_dc
 delete
 pack
 rest scre
 RETT=2
endif
return
*---------------------------------------------------------*
procedure TuneOtch
parameters otwet
private otwet,old_color
if .not.OnSetj()
 return
endif
old_color = setcolor()
save scre
MsgWrite('Считываю отчет...')
file_nam=use_path+alltrim(DOC_DOC)+ext
str_file=memoread(file_nam)
declare format_str[3]   && 1-форма; 2-данные
for i=1 to 2
 i_min=at(chr(i),str_file)
 i_max=rat(chr(i),str_file)
 if i_min != i_max.and.i_min != (i_max - 1)
  format_str[i]=substr(str_file,i_min+1,i_max-i_min-1)
 else
  format_str[i]=""
 endif
next
rest scre
do case
case otwet=1
 format_str[otwet]=ed(Y_min,X_min,Y_max,X_max,format_str[otwet])
case otwet=2
 format_str[otwet]=upper(ed(Y_min,X_min,Y_max,X_max,format_str[otwet]))
endcase
if Fragen('','Сохpанить изменения?')
 save scre
 MsgWrite('Сохpаняю...')
 nam_pic=chr(1)+format_str[1]+chr(1)+chr(2)+format_str[2]+chr(2)
 memowrit(file_nam,nam_pic)
 rest scre
endif
set color to (old_color)
UnSetj()
return
*---------------------------------------------------------*
procedure MenuDocum
parameters is_case,ext
if (.not.debempty()).and.(.not.kreempty())
 WarnWrite(PMSG8)
 return
endif
select 7
do case
case is_case=1
 use_dbf=use_path+OtchetDIC
 use_str=OtchetSTR
case is_case=2
 use_dbf=use_path+BalanpDIC
 use_str=BalanpSTR
endcase
if .not.file(use_dbf)
 creat &use_dbf from &use_str
 use
endif
use &use_dbf SHARED
decl dbmenu[1]
dbmenu[1]="DOC_NAM"
setcolor(cmenu)
BoxTenj(Y_srm-iif(lastrec()<20,lastrec()/2,10)-1,X_srm-int(len(DOC_NAM)/2)-2,Y_srm+iif(lastrec()<20,lastrec()/2,10),X_srm+int(len(DOC_NAM)/2)+1,'┌─┐│┘─└│ ')
dbedit(Y_srm-iif(lastrec()<20,lastrec()/2,10),X_srm-int(len(DOC_NAM)/2),Y_srm+iif(lastrec()<20,lastrec()/2,10)-1,X_srm+int(len(DOC_NAM)/2),dbmenu,"dbotch","","","","","")
rele dbmenu
use
select 1
return
*---------------------------------------------------------*
function dbotch
parameters mode,ptr
do case
case mode <= 3
case lastkey()=27
 return 0
case lastkey()=13
 do case
 case is_case=1
  OtchetLPT1(use_path+alltrim(DOC_DOC)+ext)
 case is_case=2
  BalanpLPT1(use_path+alltrim(DOC_DOC)+ext)
 endcase
 return 0
endcase
return 1
*---------------------------------------------------------*
procedure OtchetLPT1
parameters var_forma
private old_color,LPT1Page,old_select
old_select=select()
DrawBox(Y_srm-3,X_srm-8,Y_srm+1,X_srm+7,ibox)
setcolor(cl)
MSG=' ПЕPИОД '
@ Y_srm-3,int(X_srm-len(MSG)/2)+1 say MSG
kv=1
setcolor(cw)
@ Y_srm-2,X_srm-7 prompt "с начала года "
@ row()+1,X_srm-7 prompt "за кваpтал    "
@ row()+1,X_srm-7 prompt "за месяц      "
menu to kv
if kv=0
 setcolor(old_color)
 rest scre from LPT1Page
 return
endif
do case
case kv=1
 bm=1
 em=month(I->T_DATE)
case kv=2
 em=month(I->T_DATE)
 bm=(int(em/4)+1)*3-2
case kv=3
 bm=month(I->T_DATE)
 em=month(I->T_DATE)
endcase
kvartal=int(em/4)+1
mesez=iif(month(I->T_DATE)=12,1,month(I->T_DATE)+1)
god=year(I->T_DATE)+iif(month(I->T_DATE)=12,1,0)
do case
case em<=3
 period='1 квартал'
case em<=6
 period='полугодие'
case em<=9
 period='девять месяцев'
case em<=12
 period='год'
endcase
select 1
old_Nz=recno()
use
*********                               && считать фоpму
MsgWrite("Анализирую форму...")
str_file=memoread(var_forma)
*********                               && разобрать форму
declare format_str[3]   && 1-форма; 2-данные
for i=1 to 2
 i_min=at(chr(i),str_file)
 i_max=rat(chr(i),str_file)
 if i_min != i_max.and.i_min != (i_max - 1)
  format_str[i]=substr(str_file,i_min+1,i_max-i_min-1)
 else
  format_str[i]=""
 endif
next
*********                               && считать формулы
cnt_pic=MLCOUNT(format_str[2],len_edil)
decl line_pic[cnt_pic]
for i=1 to cnt_pic
 line_pic[i] = memoline(format_str[2],len_edil,i)
next
*********                               && создать переменные до =
for i=1 to cnt_pic
 var_str=substr(line_pic[i],1,at('=',line_pic[i])-1)
 &var_str.=0
next
*********                               && количество D и K
cnt_d=0
cnt_k=0
for i=1 to cnt_pic
 cnt_d=cnt_d+atc(line_pic[i],"D")
 cnt_k=cnt_k+atc(line_pic[i],"K")
next
*********                               && массив D и K
decl var_deb[cnt_d]
afill(var_deb,"")
decl var_kre[cnt_k]
afill(var_kre,"")
decl cht_deb[cnt_d]
afill(cht_deb,0)
decl cht_kre[cnt_k]
afill(cht_kre,0)
decl sub_deb[cnt_d]
afill(sub_deb,0)
decl sub_kre[cnt_k]
afill(sub_kre,0)
decl sis_deb[cnt_d]
afill(sis_deb,.F.)
decl sis_kre[cnt_k]
afill(sis_kre,.F.)
*********                               && массив ретранслированых формул
decl line_frm[cnt_pic]
afill(line_frm,'')
cnt_deb=0
cnt_kre=0
for i=1 to cnt_pic
 pos=0
 char=NextChar(line_pic[i],pos)
 do while ""!=char
  do case
  case char=="D"
   pos++    
   char=NextChar(line_pic[i],pos)
   var_cht=''
   do while at(char,'1234567890.')!=0.and.""!=char
    var_cht=var_cht+char
    pos++
    char=NextChar(line_pic[i],pos)
   enddo
   cnt_deb++
   cht_deb[cnt_deb]=val(var_cht)
   var_deb[cnt_deb]="D"+alltrim(str(cnt_deb))
   var=var_deb[cnt_deb]
   &var.=0
   line_frm[i]=line_frm[i]+var_deb[cnt_deb]
   if char=="|"
    sis_deb[cnt_deb]=.T.
    pos++
    char=NextChar(line_pic[i],pos)
    var_sub=''
    do while at(char,'1234567890.')!=0.and.""!=char
     var_sub=var_sub+char
     pos++
     char=NextChar(line_pic[i],pos)
    enddo
    sub_deb[cnt_deb]=val(var_sub)
   endif
  case char=="K"
   pos++    
   char=NextChar(line_pic[i],pos)
   var_cht=''
   do while at(char,'1234567890.')!=0.and.""!=char
    var_cht=var_cht+char
    pos++
    char=NextChar(line_pic[i],pos)
   enddo
   cnt_kre++
   cht_kre[cnt_kre]=val(var_cht)
   var_kre[cnt_kre]="K"+alltrim(str(cnt_kre))
   var=var_kre[cnt_kre]
   &var.=0
   line_frm[i]=line_frm[i]+var_kre[cnt_kre]
   if char=="|"
    sis_kre[cnt_kre]=.T.
    pos++
    char=NextChar(line_pic[i],pos)
    var_sub=''
    do while at(char,'1234567890.')!=0.and.""!=char
     var_sub=var_sub+char
     pos++
     char=NextChar(line_pic[i],pos)
    enddo
    sub_kre[cnt_kre]=val(var_sub)
   endif
  othe
   line_frm[i]=line_frm[i]+char
   pos++
   char=NextChar(line_pic[i],pos)
  endcase
 enddo
next
*********                               && считать стpоки из формы
FrmlVar=""
pos=0
char=NextChar(format_str[1],pos)
do while ""!=char
 if char="["
  pos++
  char=NextChar(format_str[1],pos)
  do while "]"!=char.and.""!=char
   FrmlVar=FrmlVar+char
   pos++
   char=NextChar(format_str[1],pos)
  enddo
  if ""==char
   exit
  endif
  if len(FrmlVar)=4
   &FrmlVar.=val(transform(0,pic_1000))
  endif
  FrmlVar=""
 endif
 pos++
 char=NextChar(format_str[1],pos)
enddo
* создание файла по месячно
ch_year=str(year(I->T_DATE),4)
select 1
creat btmp.dbf from balans.str
for cur_month=bm to em
 use_dbf=cur_path+"\"+ch_year+"."+strtran(str(cur_month,2)," ","0")+"\"+MainDBF
 if file(use_dbf)
  MsgWrite("Складываю "+Month[cur_month])
  select 2
  use &use_dbf SHARED
  do while .not.eof()
   if .not.B->FUTURE
    select 1
    append blank
    repl A->DEBET  with B->DEBET
    repl A->KREDIT with B->KREDIT
    repl A->SUBDEB with B->SUBDEB
    repl A->SUBKRE with B->SUBKRE
    repl A->SUMMA  with B->SUMMA
   endif
   select 2
   skip
  enddo
  use
 endif
next
select 1
index on str(DEBET)+str(SUBDEB) to debet.ntx
total on str(DEBET)+str(SUBDEB) fields SUMMA to debet.dbf
set index to
select 2
use debet.dbf
select 1
index on str(KREDIT)+str(SUBKRE) to kredit.ntx
total on str(KREDIT)+str(SUBKRE) fields SUMMA to kredit.dbf
set index to
select 3
use kredit.dbf
*********                               && сумма дебет
select 2
for i=1 to cnt_d
 MsgWrite("Суммирую дебет - "+alltrim(str(cht_deb[i]))+iif(sis_deb[i],'|'+alltrim(str(sub_deb[i])),''))
 var=var_deb[i]
 if sis_deb[i]
  sum SUMMA to &var for DEBET=cht_deb[i].and.SUBDEB=sub_deb[i]
 else
  sum SUMMA to &var for DEBET=cht_deb[i]
 endif
next
select 3
for i=1 to cnt_k
 MsgWrite("Суммирую кредит - "+alltrim(str(cht_kre[i]))+iif(sis_kre[i],'|'+alltrim(str(sub_kre[i])),''))
 var=var_kre[i]
 if sis_kre[i]
  sum SUMMA to &var for KREDIT=cht_kre[i].and.SUBKRE=sub_kre[i]
 else
  sum SUMMA to &var for KREDIT=cht_kre[i]
 endif
next
*********
select 1
use
select 2
use
select 3
use
dele file btmp.dbf
dele file debet.dbf
dele file kredit.dbf
dele file debet.ntx
dele file kredit.ntx
*********
MsgWrite("С о з д а ю  ф о p м у  о т ч е т а . . .")
*********                               && разчитать формулы
select 9
for i=1 to cnt_pic
 formula(translate(line_frm[i]))
next
*********
*cpfopen("otchet.txt")
*cpfout(strfrml(format_str[1]))
*cpfclose()
cpfedit(strfrml(format_str[1]),"otchet.txt")
*********
OpenDBF()
InitFilter()
go old_Nz
select(old_select)
return
