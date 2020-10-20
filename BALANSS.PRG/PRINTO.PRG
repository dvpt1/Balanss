*---------------------------------------------------------*
procedure BalansLPT1
private old_color,old_select,old_Nz
if (.not.debempty()).and.(.not.kreempty())
 WarnWrite(PMSG8)
 return
endif
if .not.Fragen('',PMSGFRG1)
  return
endif
old_select=select()
old_Nz=recno()
set index to
set filter to
go top
***
cpfopen("balans.txt")
***  формирование строк таблицы
len_mon=len(Month[1])
str_len=6*(len_sum+1)+7
*** 1-заголовок
cpfout("СВОДHЫЙ БАЛАHС HА ")
cpfout(alltrim(Month[month(I->T_DATE)])+" "+str(year(I->T_DATE),4)+" г.")
cpfout(chr(13)+chr(10))
cpfout(chr(13)+chr(10))
* 1 строка
cpfout(replicate("-",str_len)+chr(13)+chr(10))
* 2 строка
cpfout("  Счет !      Сальдо на начало       !           Обоpот            !      Сальдо на конец        "+chr(13)+chr(10))
* 3 строка
cpfout("       !-----------------------------!-----------------------------!-----------------------------"+chr(13)+chr(10))
* 4 строка
cpfout("       !    Дебет     !    Кpедит    !    Дебет     !    Кpедит    !    Дебет     !    Кpедит    "+chr(13)+chr(10))
* 5 строка
cpfout(replicate("-",str_len)+chr(13)+chr(10))
*********
select 4
use_dic=use_path+MainDIC
use &use_dic SHARED
sSALBEGd=0
sSALBEGk=0
sRECSUM=0
sEXPSUM=0
sSALENDd=0
sSALENDk=0
do while .not.eof()
  Nz=recno()
  var_exp=D->D1
  MsgWrite("Расчитываю "+strroun(var_exp)+" счет...")
  name_sub=use_path+strline(var_exp)+".DIC"
  if file(name_sub)
    select(old_select)
    index on str(DEBET)+str(SUBDEB)+str(KREDIT) to debet.ntx
    total on str(DEBET)+str(SUBDEB)+str(KREDIT) fields SUMMA to debet.dbf for DEBET=var_exp.and.!FUTURE
    set index to
    select 2
    use debet.dbf
    index on str(DEBET)+str(SUBDEB)+str(KREDIT) to debet.ntx
    select(old_select)
    index on str(KREDIT)+str(SUBKRE)+str(DEBET) to kredit.ntx
    total on str(KREDIT)+str(SUBKRE)+str(DEBET) fields SUMMA to kredit.dbf for KREDIT=var_exp.and.!FUTURE
    set index to
    select 3
    use kredit.dbf
    index on str(KREDIT)+str(SUBKRE)+str(DEBET) to kredit.ntx
    select 5
    use &name_sub SHARED
***
    iSALBEGd=0
    iSALBEGk=0
    iRECSUM=0
    iEXPSUM=0
    iSALENDd=0
    iSALENDk=0
    do while (.not.eof())
      iSALBEGd=iSALBEGd+SALBED
      iSALBEGk=iSALBEGk+SALBEK
      select 4
      go top
      dRECSUM=0
      do while .not.eof()
        select 2
        seek str(var_exp)+str(E->D1)+str(D->D1)
        dRECSUM=dRECSUM+SUMMA
        select 4
        skip
      enddo
      select 4
      go top
      dEXPSUM=0
      do while .not.eof()
        select 3
        seek str(var_exp)+str(E->D1)+str(D->D1)
        dEXPSUM=dEXPSUM+SUMMA
        select 4
        skip
      enddo
      select 5
      iRECSUM=iRECSUM+dRECSUM
      iEXPSUM=iEXPSUM+dEXPSUM
      repl I->FORMAT with SALBED-SALBEK+dRECSUM-dEXPSUM
      iSALENDd=iSALENDd+iif(I->FORMAT>0,I->FORMAT,0)
      iSALENDk=iSALENDk+iif(I->FORMAT<0,-(I->FORMAT),0)
      skip
    enddo
* "Итого"
    select 2
    use
    select 3
    use
    select 5
    use
    select 4
    go Nz
    if iSALENDd!=0.or.iSALENDk!=0.or.;
       iSALBEGd!=0.or.iSALBEGk!=0.or.;
       iRECSUM !=0.or.iEXPSUM!=0
     cpfout(strroun(var_exp)+" !")
     if D_STR != K_STR
      repl I->FORMAT with iSALBEGd
      sSALBEGd=sSALBEGd+I->FORMAT
      cpfout(strnum(I->FORMAT)+"!")
      repl I->FORMAT with iSALBEGk
      sSALBEGk=sSALBEGk+I->FORMAT
      cpfout(strnum(I->FORMAT)+"!")
*
      repl I->FORMAT with iRECSUM
      sRECSUM=sRECSUM+I->FORMAT
      cpfout(strnum(I->FORMAT)+"!")
      repl I->FORMAT with iEXPSUM
      sEXPSUM=sEXPSUM+I->FORMAT
      cpfout(strnum(I->FORMAT)+"!")
*
      repl I->FORMAT with iSALENDd
      sSALENDd=sSALENDd+I->FORMAT
      cpfout(strnum(I->FORMAT)+"!")
      repl I->FORMAT with iSALENDk
      sSALENDk=sSALENDk+I->FORMAT
      cpfout(strnum(I->FORMAT))
     else
      repl I->FORMAT with iSALBEGd-iSALBEGk
      sSALBEGd=sSALBEGd+iif(I->FORMAT>0,I->FORMAT,0)
      sSALBEGk=sSALBEGk+iif(I->FORMAT<0,-(I->FORMAT),0)
      if I->FORMAT>0
       cpfout(strnum(I->FORMAT)+"!")
      else
       cpfout(strnum(num_sum)+"!")
      endif
      if I->FORMAT<0
       repl I->FORMAT with -(I->FORMAT)
       cpfout(strnum(I->FORMAT)+"!")
      else
       cpfout(strnum(num_sum)+"!")
      endif
*
      repl I->FORMAT with iRECSUM
      sRECSUM=sRECSUM+I->FORMAT
      cpfout(strnum(I->FORMAT)+"!")
      repl I->FORMAT with iEXPSUM
      sEXPSUM=sEXPSUM+I->FORMAT
      cpfout(strnum(I->FORMAT)+"!")
*
      repl I->FORMAT with iSALENDd-iSALENDk
      sSALENDd=sSALENDd+iif(I->FORMAT>0,I->FORMAT,0)
      sSALENDk=sSALENDk+iif(I->FORMAT<0,-(I->FORMAT),0)
      if I->FORMAT>0
       cpfout(strnum(I->FORMAT)+"!")
      else
       cpfout(strnum(num_sum)+"!")
      endif
      if I->FORMAT<0
       repl I->FORMAT with -(I->FORMAT)
       cpfout(strnum(I->FORMAT))
      else
       cpfout(strnum(num_sum))
      endif
     endif
     cpfout(chr(13)+chr(10))
    endif
  endif
  skip
enddo
select 4
use
***
dele file debet.dbf
dele file kredit.dbf
dele file debet.ntx
dele file kredit.ntx
***
cpfout(replicate("-",str_len)+chr(13)+chr(10))
cpfout(" ИТОГО !")
repl I->FORMAT with sSALBEGd
cpfout(strnum(I->FORMAT)+"!")
repl I->FORMAT with sSALBEGk
cpfout(strnum(I->FORMAT)+"!")
*
repl I->FORMAT with sRECSUM
cpfout(strnum(I->FORMAT)+"!")
repl I->FORMAT with sEXPSUM
cpfout(strnum(I->FORMAT)+"!")
*
repl I->FORMAT with sSALENDd
cpfout(strnum(I->FORMAT)+"!")
repl I->FORMAT with sSALENDk
cpfout(strnum(I->FORMAT))
cpfout(chr(13)+chr(10))
cpfout(replicate("-",str_len)+chr(13)+chr(10))
***
cpfout(chr(13)+chr(10)+chr(13)+chr(10)+"Бухгалтеp_____________"+;
       chr(13)+chr(10)+varstr(date()))
***
cpfclose()
***
select(old_select)
InitIndex()
InitFilter()
go old_Nz
return
*---------------------------------------------------------*
procedure BalanpLPT1
parameters var_forma
private old_select,old_Nz
if (.not.debempty()).and.(.not.kreempty())
 WarnWrite(PMSG8)
 return
endif
store 0 to A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15
store 0 to P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12,P13,P14,P15
old_select=select()
MsgWrite(PMSG10)
mesez=iif(month(I->T_DATE)=12,1,month(I->T_DATE)+1)
god=year(I->T_DATE)+iif(month(I->T_DATE)=12,1,0)
select 1
old_Nz=recno()
set index to
set filter to
go top
*********
str_file=memoread(var_forma)    && считать фоpму баланса
*********
FrmlVar=""				&& считать стpоки баланса
pos=0
char=NextChar(@str_file,pos)
do while ""!=char
 if char="["
  pos++
  char=NextChar(@str_file,pos)
  do while "]"!=char.and.""!=char
   FrmlVar=FrmlVar+char
   pos++
   char=NextChar(@str_file,pos)
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
 char=NextChar(@str_file,pos)
enddo
*********
select 4
use_dic=use_path+MainDIC
use &use_dic SHARED
do while .not.eof()
 if (.not.empty(D->D_STR)).or.(.not.empty(D->K_STR))
  Nz=recno()
  var_exp=D->D1
  MsgWrite("Расчитываю "+strroun(var_exp)+" счет...")
  name_sub=use_path+strline(var_exp)+".DIC"
  if file(name_sub)
    select 1
    index on str(DEBET)+str(SUBDEB)+str(KREDIT) to debet.ntx
    total on str(DEBET)+str(SUBDEB)+str(KREDIT) fields SUMMA to debet.dbf for DEBET=var_exp.and.!FUTURE
    set index to
    select 2
    use debet.dbf
    index on str(DEBET)+str(SUBDEB)+str(KREDIT) to debet.ntx
    select 1
    index on str(KREDIT)+str(SUBKRE)+str(DEBET) to kredit.ntx
    total on str(KREDIT)+str(SUBKRE)+str(DEBET) fields SUMMA to kredit.dbf for KREDIT=var_exp.and.!FUTURE
    set index to
    select 3
    use kredit.dbf
    index on str(KREDIT)+str(SUBKRE)+str(DEBET) to kredit.ntx
    select 5
    use &name_sub SHARED
***
    do while (.not.eof())
      select 4
      go top
      dRECSUM=0
      do while .not.eof()
        select 2
        seek str(var_exp)+str(E->D1)+str(D->D1)
        dRECSUM=dRECSUM+SUMMA
        select 4
        skip
      enddo
      select 4
      go top
      dEXPSUM=0
      do while .not.eof()
        select 3
        seek str(var_exp)+str(E->D1)+str(D->D1)
        dEXPSUM=dEXPSUM+SUMMA
        select 4
        skip
      enddo
      select 5
      var=SALBED-SALBEK+dRECSUM-dEXPSUM
      select 4
      go Nz
      if (.not.empty(D->D_STR)).and.var > 0
       fld="S"+D->D_STR
       if type("&fld")!="U"
        &fld.=&fld.+var
       else
        WarnWrite("HЕИЗВЕСТHАЯ стpока баланса - "+fld+"...")
       endif
      endif
      if (.not.empty(D->K_STR)).and.var < 0
       fld="S"+D->K_STR
       if type("&fld")!="U"
        &fld.=&fld.+var
       else
        WarnWrite("HЕИЗВЕСТHАЯ стpока баланса - "+fld+"...")
       endif
      endif
      select 5
      skip
    enddo
* "Итого"
    select 2
    use
    select 3
    use
    select 5
    use
    select 4
    go Nz
  endif
 endif
 skip
enddo
select 4
use
*********
dele file debet.dbf
dele file kredit.dbf
dele file debet.ntx
dele file kredit.ntx
*********
MsgWrite(PMSG12)
*********
FrmlVar=""				&& фоpматиpую стpоки баланса
pos=0
char=NextChar(@str_file,pos)
do while ""!=char
 if char="["
  pos++
  char=NextChar(@str_file,pos)
  do while "]"!=char.and.""!=char
   FrmlVar=FrmlVar+char
   pos++
   char=NextChar(@str_file,pos)
  enddo
  if ""==char
   exit
  endif
  if len(FrmlVar)=4
   if I->CNT_OKRG=0
    &FrmlVar.=val(transform(abs(&FrmlVar),pic_1000))
   else
    &FrmlVar.=val(transform(abs(&FrmlVar/I->CNT_OKRG),pic_1000))
   endif
  endif
  FrmlVar=""
 endif
 pos++
 char=NextChar(@str_file,pos)
enddo
*********
*cpfopen("balanp.txt")
*cpfout(strfrml(str_file))
*cpfclose()
cpfedit(strfrml(str_file),"balanp.txt")
*********
select 1
InitIndex()
InitFilter()
go old_Nz
select(old_select)
return
*---------------------------------------------------------*
procedure GbuhLPT1
private old_color
old_color=setcolor(cmenu)
BoxTenj(Y_srm-4,X_srm-21,Y_srm+4,X_srm+20,ibox)
var_exp=0
var_ptr1=1
var_ptr2=12
ch_year=str(year(I->T_DATE),4)
otwet=1
do while otwet!=0
  setcolor(cmenu)
  @ Y_srm-2,X_srm-19 prompt "Счет: "+mDict("str(D1)+' '+D2",var_exp,"D1",use_path+MainDIC,"")
  @ Y_srm+0,X_srm-19 say "с"
  @ row(),col()+1 prompt Month[var_ptr1]
  @ row(),col()+1 say "по"
  @ row(),col()+1 prompt Month[var_ptr2]
  @ row(),col()+1 prompt ch_year
  @ row(),col()+1 say "г."
  @ Y_srm+2,X_srm-19 prompt 'Произвести расчеты...'
  menu to otwet
  if otwet != 0
    save screen to InpPage
    do case
    case otwet=1
      var_exp=DictFind(var_exp,"D1","strroun(D1)+' '+D2","Выбеpите счет",use_path+MainDIC,"")
    case otwet=2
      save screen
      setcolor(cdict)
      BoxTenj(Y_min+6,X_max-45,Y_min+19,X_max-36,ibox)
      new_ptr=achoice(Y_min+7,X_max-44,Y_min+18,X_max-37,month,"","",var_ptr1)
      if new_ptr != 0
        var_ptr1=new_ptr
      endif
      rest screen
    case otwet=3
      save screen
      setcolor(cdict)
      BoxTenj(Y_min+6,X_max-45,Y_min+19,X_max-36,ibox)
      new_ptr=achoice(Y_min+7,X_max-44,Y_min+18,X_max-37,month,"","",var_ptr2)
      if new_ptr != 0
        var_ptr2=new_ptr
      endif
      rest screen
    case otwet=4
      set cursor on
      @ Y_srm+0,X_srm+4 get ch_year picture '####'
      read
      set cursor off
    case otwet=5
      BuhAll()
    endcase
    rest screen from InpPage
  endif
enddo
setcolor(old_color)
return
*---------------------------------------------------------*
procedure BuhAll
private old_Nz
if (.not.debpravo(var_exp)).and.(.not.krepravo(var_exp))
 WarnWrite(PMSG8)
 return
endif
MsgWrite(PMSG10)
select 1
old_Nz=recno()
len_dev=2
nul_dev=val(" 0")
use
*** Фоpмиpование плана счетов (все что есть)
select 4
creat btmp.dbf from expenses.str
index on D1 to btmp.ntx
for cur_month=var_ptr1 to var_ptr2
 use_dic=cur_path+"\"+ch_year+"."+strtran(str(cur_month,2)," ","0")+"\"+MainDIC
 if file(use_dic)
  select 1
  use  &use_dic
  do while .not.eof()
   select 4
   seek A->D1
   if .not.found()
    append blank
    repl D->D1 with A->D1
    if nm=cur_month
     repl D->D2 with A->D2
     repl D->D_STR with A->D_STR
     repl D->K_STR with A->K_STR
    endif
   endif
   select 1
   skip
  enddo
  locate for var_exp=A->D1
  if D_STR==K_STR
   is_STR=.T.
  else
   is_STR=.F.
  endif
  use
 endif
next
* инициализация массива главной книги
select 4
store reccount() to count_rec,count_exp
iSALBEGd=0
iSALBEGk=0
decl iKRE[count_rec]
afill(iKRE,0)
decl iDEB[12,count_rec]
for i=1 to 12
 for j=1 to count_rec
  iDEB[i,j]=iKRE[j]
 next
next
decl iSAD[count_rec]
afill(iSAD,0)
decl iSAK[count_rec]
afill(iSAK,0)
* заполнение массива по месячно
for cur_month=var_ptr1 to var_ptr2
 MsgWrite("Расчитываю "+Month[cur_month])
 use_dbf=cur_path+"\"+ch_year+"."+strtran(str(cur_month,2)," ","0")+"\"+MainDBF
 use_sub=cur_path+"\"+ch_year+"."+strtran(str(cur_month,2)," ","0")+"\"+strline(var_exp)+".DIC"
 if file(use_sub)
  select 1
  use &use_dbf SHARED
  index on str(DEBET)+str(SUBDEB)+str(KREDIT) to debet.ntx
  total on str(DEBET)+str(SUBDEB)+str(KREDIT) fields SUMMA to debet.dbf for DEBET=var_exp.and.!FUTURE
  set index to
  select 2
  use debet.dbf
  index on str(DEBET)+str(SUBDEB)+str(KREDIT) to debet.ntx
  select 1
  index on str(KREDIT)+str(SUBKRE)+str(DEBET) to kredit.ntx
  total on str(KREDIT)+str(SUBKRE)+str(DEBET) fields SUMMA to kredit.dbf for KREDIT=var_exp.and.!FUTURE
  set index to
  select 3
  use kredit.dbf
  index on str(KREDIT)+str(SUBKRE)+str(DEBET) to kredit.ntx
  select 5
  use &use_sub SHARED
  if cur_month=var_ptr1
   sum SALBED,SALBEK to iSALBEGd,iSALBEGk
   go top
  endif
  do while (.not.eof())
   select 4
   go top
   dRECSUM=0
   i=1
   do while .not.eof()
     select 2
     seek str(var_exp)+str(E->D1)+str(D->D1)
     iDEB[cur_month,i]=iDEB[cur_month,i]+SUMMA
     dRECSUM=dRECSUM+SUMMA
     select 4
     i++
     skip
   enddo
   select 4
   go top
   dEXPSUM=0
   do while .not.eof()
     select 3
     seek str(var_exp)+str(E->D1)+str(D->D1)
     iKRE[cur_month]=iKRE[cur_month]+SUMMA
     dEXPSUM=dEXPSUM+SUMMA
     select 4
     skip
   enddo
   select 5
   repl I->FORMAT with SALBED-SALBEK+dRECSUM-dEXPSUM
   iSAD[cur_month]=iSAD[cur_month]+iif(I->FORMAT>0,I->FORMAT,0)
   iSAK[cur_month]=iSAK[cur_month]+iif(I->FORMAT<0,-(I->FORMAT),0)
   skip
  enddo
  select 5
  use
  select 3
  use
  select 2
  use
  select 1
  use
 endif
next
dele file debet.dbf
dele file kredit.dbf
dele file debet.ntx
dele file kredit.ntx
MsgWrite("Фоpмиpую ГЛАВHУЮ КHИГУ")
*** Расчет фактического числа rec и exp
decl iCNT[count_rec]
n=0
for i=1 to count_rec
 s=0
 for j=1 to 12
  s=s+iDEB[j,i]
 next
 iCNT[i]=s
 if s!=0
  n++
 endif
next
cnt_rec=n
cnt_exp=0
***
cpfopen("gbuh.txt")
***  формирование строки таблицы
len_mon=len(Month[1])
str_len=(len_mon)+4*(len_sum+1)+(cnt_rec)*(len_sum+1)+2
*** 1-заголовок
str_file=memoread("gbuh.prn")
i_min=at(chr(1),str_file)
i_max=rat(chr(1),str_file)
if i_min != i_max.and.i_min != (i_max - 1)
  cpfout(strfrm(substr(str_file,i_min+1,i_max-i_min-1)))
endif
* 1 строка
cpfout(replicate("-",str_len)+chr(13)+chr(10))
* 2 строка
cpfout("  месяц   !")
if cnt_rec<>0
 cpfout(zentstr(cnt_rec*(len_sum+1)-1,"Дебет")+"!")
endif
cpfout(zentstr(len_sum,"Итого")+"!")
cpfout(zentstr(len_sum,"Итого")+"!")
cpfout(zentstr(2*len_sum,"Сальдо"))
cpfout(chr(13)+chr(10))
* 3 строка
cpfout("          !")
if cnt_rec<>0
 cpfout(replicate("-",cnt_rec*(len_sum+1)-1)+"!")
endif
cpfout(zentstr(len_sum,"дебет")+"!")
cpfout(zentstr(len_sum,"кpедит")+"!")
cpfout(replicate("-",2*len_sum))
cpfout(chr(13)+chr(10))
* 4 строка
cpfout("          !")
select 4
go top
i=1
do while .not.eof()
  if iCNT[i] != 0
    cpfout(zentstr(len_sum,strroun(D1))+"!")
  endif
  i=i+1
  skip
enddo
select 4
use
dele file btmp.dbf
dele file btmp.ntx
cpfout(space(len_sum)+"!")
cpfout(space(len_sum)+"!")
cpfout(zentstr(len_sum,"дебет")+"!")
cpfout(zentstr(len_sum,"кpедит"))
cpfout(chr(13)+chr(10))
* 5 строка
cpfout(replicate("-",str_len)+chr(13)+chr(10))
***
cpfout("          !")
if cnt_rec<>0
 cpfout(space(cnt_rec*(len_sum+1)-1)+"!")
endif
cpfout(zentstr(2*len_sum+1,"Сальдо на 1 "+Month[var_ptr1]+" "+ch_year+"г.")+"!")
if is_STR
  repl I->FORMAT with iSALBEGd-iSALBEGk
  iSALBEGd=iif(I->FORMAT>0,I->FORMAT,0)
  iSALBEGk=iif(I->FORMAT<0,-(I->FORMAT),0)
endif
repl I->FORMAT with iSALBEGd
cpfout(strnum(I->FORMAT)+"!")
repl I->FORMAT with iSALBEGk
cpfout(strnum(I->FORMAT))
cpfout(chr(13)+chr(10))
* таблица
for cur_month=var_ptr1 to var_ptr2
  cur_month=val(transform(cur_month,'99'))
  cpfout(" "+Month[cur_month]+" !")
  dRECSUM=0
  for i=1 to count_rec
    if iCNT[i]!=0
      repl I->FORMAT with iDEB[cur_month,i]
      cpfout(strnum(I->FORMAT)+"!")
      dRECSUM=dRECSUM+iDEB[cur_month,i]
    endif
  next
  repl I->FORMAT with dRECSUM
  cpfout(strnum(I->FORMAT)+"!")
  repl I->FORMAT with iKRE[cur_month]
  cpfout(strnum(I->FORMAT)+"!")
  if is_STR
   repl I->FORMAT with iSAD[cur_month]-iSAK[cur_month]
   iSAD[cur_month]=iif(I->FORMAT>0,I->FORMAT,0)
   iSAK[cur_month]=iif(I->FORMAT<0,-(I->FORMAT),0)
  endif
  repl I->FORMAT with iSAD[cur_month]
  cpfout(strnum(I->FORMAT)+"!")
  repl I->FORMAT with iSAK[cur_month]
  cpfout(strnum(I->FORMAT))
  cpfout(chr(13)+chr(10))
next
* "Итого"
cpfout(replicate("-",str_len)+chr(13)+chr(10))
cpfout("  ИТОГО   !")
iRECSUM=0
for i=1 to count_rec
  if iCNT[i]!=0
    repl I->FORMAT with iCNT[i]
    cpfout(strnum(I->FORMAT)+"!")
    iRECSUM=iRECSUM+iCNT[i]
  endif
next
repl I->FORMAT with iRECSUM
cpfout(strnum(I->FORMAT)+"!")
iEXPSUM=0
for i=1 to 12
  iEXPSUM=iEXPSUM+iKRE[i]
next
repl I->FORMAT with iEXPSUM
cpfout(strnum(I->FORMAT)+"!")
cpfout(chr(13)+chr(10))
* 3 - END строка
cpfout(replicate("-",str_len)+chr(13)+chr(10))
***
cpfclose()
***
OpenDBF()
InitFilter()
go old_Nz
return
*---------------------------------------------------------*
procedure ChessLPT1
private old_select,old_Nz
if (.not.debempty()).and.(.not.kreempty())
 WarnWrite(PMSG8)
 return
endif
if .not.Fragen('',PMSGFRG1)
  return
endif
MsgWrite(PMSG10)
old_select=select()
old_Nz=recno()
set index to
set filter to
go top
* инициализация массива шахматной ведомости
select 4
use_dic=use_path+MainDIC
use &use_dic SHARED
store reccount() to count_rec,cnt_exp
iSALBEGd=0
iSALBEGk=0
decl iSADb[count_rec]
afill(iSADb,0)
decl iSAKb[count_rec]
afill(iSAKb,0)
decl iDEB[count_rec]
afill(iDEB,0)
decl iKRE[count_rec]
afill(iKRE,0)
decl DEB[count_rec,count_rec]
for ii=1 to count_rec
 for i=1 to count_rec
  DEB[i,ii]=iDEB[i]
 next
next
decl iiDEB[count_rec]
afill(iiDEB,0)
iREC=0
iEXP=0
decl iSADe[count_rec]
afill(iSADe,0)
decl iSAKe[count_rec]
afill(iSAKe,0)
iSALENDd=0
iSALENDk=0
*** заполняю массив
i=1
do while .not.eof()
 Nz=recno()
 var_exp=D->D1
 MsgWrite("Свожу "+strroun(var_exp)+" счет...")
 name_sub=use_path+strline(var_exp)+".DIC"
 if file(name_sub)
  select(old_select)
  index on str(DEBET)+str(SUBDEB)+str(KREDIT) to debet.ntx
  total on str(DEBET)+str(SUBDEB)+str(KREDIT) fields SUMMA to debet.dbf for DEBET=var_exp.and.!FUTURE
  set index to
  select 2
  use debet.dbf
  index on str(DEBET)+str(SUBDEB)+str(KREDIT) to debet.ntx
  select(old_select)
  index on str(KREDIT)+str(SUBKRE)+str(DEBET) to kredit.ntx
  total on str(KREDIT)+str(SUBKRE)+str(DEBET) fields SUMMA to kredit.dbf for KREDIT=var_exp.and.!FUTURE
  set index to
  select 3
  use kredit.dbf
  index on str(KREDIT)+str(SUBKRE)+str(DEBET) to kredit.ntx
  select 5
  use &name_sub SHARED
  do while (.not.eof())
   iSADb[i]=iSADb[i]+SALBED
   iSAKb[i]=iSAKb[i]+SALBEK
   select 4
   go top
   DEBSUM=0
   ii=1
   do while .not.eof()
    select 2
    seek str(var_exp)+str(E->D1)+str(D->D1)
    DEB[i,ii]=DEB[i,ii]+SUMMA
    iDEB[i]=iDEB[i]+SUMMA
    DEBSUM=DEBSUM+SUMMA
    select 4
    ii++
    skip
   enddo
   select 4
   go top
   KRESUM=0
   do while .not.eof()
    select 3
    seek str(var_exp)+str(E->D1)+str(D->D1)
    iKRE[i]=iKRE[i]+SUMMA
    KRESUM=KRESUM+SUMMA
    select 4
    skip
   enddo
   select 5
   repl I->FORMAT with SALBED-SALBEK+DEBSUM-KRESUM
   iSADe[i]=iSADe[i]+iif(I->FORMAT>0,I->FORMAT,0)
   iSAKe[i]=iSAKe[i]+iif(I->FORMAT<0,-(I->FORMAT),0)
   skip
  enddo
* "Итого"
  select 2
  use
  select 3
  use
  select 5
  use
  select 4
  go Nz
  if D_STR == K_STR
   repl I->FORMAT with iSADb[i]-iSAKb[i]
   iSADb[i]=iif(I->FORMAT>0,I->FORMAT,0)
   iSAKb[i]=iif(I->FORMAT<0,-(I->FORMAT),0)
   repl I->FORMAT with iSADe[i]-iSAKe[i]
   iSADe[i]=iif(I->FORMAT>0,I->FORMAT,0)
   iSAKe[i]=iif(I->FORMAT<0,-(I->FORMAT),0)
  endif
  iSALBEGd=iSALBEGd+iSADb[i]
  iSALBEGk=iSALBEGk+iSAKb[i]
  iSALENDd=iSALENDd+iSADe[i]
  iSALENDk=iSALENDk+iSAKe[i]
 endif
 i++
 skip
enddo
***
dele file debet.dbf
dele file kredit.dbf
dele file debet.ntx
dele file kredit.ntx
*** считаю итого дебет
MsgWrite(PMSG12)
for i=1 to count_rec
 iREC=iREC+iDEB[i]
next
for i=1 to count_rec
 iEXP=iEXP+iKRE[i]
next
j=0
for ii=1 to count_rec
 for i=1 to count_rec
  iiDEB[ii]=iiDEB[ii]+DEB[i,ii]
 next
 if iiDEB[ii] != 0
  j++
 endif
next
cnt_rec=j
*** СОЗДАЮ ВЕДОМОСТЬ
cpfopen("chess.txt")
***  формирование строк таблицы
str_len=7+6*(len_sum+1)+(cnt_rec)*(len_sum+1)
*** 1-заголовок
cpfout("ШАХМАТHАЯ ВЕДОМОСТЬ HА ")
cpfout(alltrim(Month[month(I->T_DATE)])+" "+str(year(I->T_DATE),4)+" г.")
cpfout(chr(13)+chr(10))
cpfout(chr(13)+chr(10))
* 1 строка
cpfout(replicate("-",str_len)+chr(13)+chr(10))
* 2 строка
cpfout("  Счет !")
cpfout(zentstr(2*(len_sum+1)-1,"Сальдо на начало")+"!")
if cnt_rec<>0
 cpfout(zentstr(cnt_rec*(len_sum+1)-1,"Дебет")+"!")
endif
cpfout(zentstr(len_sum,"Итого")+"!")
cpfout(zentstr(len_sum,"Итого")+"!")
cpfout(zentstr(2*(len_sum+1)-1,"Сальдо на конец"))
cpfout(chr(13)+chr(10))
* 3 строка
cpfout("       !")
cpfout(replicate("-",2*(len_sum+1)-1)+"!")
if cnt_rec<>0
 cpfout(replicate("-",cnt_rec*(len_sum+1)-1)+"!")
endif
cpfout(zentstr(len_sum,"дебет")+"!")
cpfout(zentstr(len_sum,"кредит")+"!")
cpfout(replicate("-",2*(len_sum+1)-1))
cpfout(chr(13)+chr(10))
* 4 строка
cpfout("       !")
cpfout(zentstr(len_sum,"дебет")+"!"+zentstr(len_sum,"кредит")+"!")
select 4
go top
ii=1
do while .not.eof()
 if iiDEB[ii] != 0
  cpfout(zentstr(len_sum,strroun(D1))+"!")
 endif
 ii++
 skip
enddo
cpfout(space(len_sum)+"!")
cpfout(space(len_sum)+"!")
cpfout(zentstr(len_sum,"дебет")+"!"+zentstr(len_sum,"кредит"))
cpfout(chr(13)+chr(10))
* 5 строка
cpfout(replicate("-",str_len)+chr(13)+chr(10))
*** собственно таблица
select 4
go top
i=1
do while .not.eof()
 if iSADb[i] != 0 .or. iSAKb[i] != 0 .or. ;
    iDEB[i] != 0 .or. iKRE[i] != 0 .or. ;
    iSADe[i] != 0 .or. iSAKe[i] != 0
  cpfout(strroun(D->D1)+" !")
  repl I->FORMAT with iSADb[i]
  cpfout(strnum(I->FORMAT)+"!")
  repl I->FORMAT with iSAKb[i]
  cpfout(strnum(I->FORMAT)+"!")
  for ii=1 to count_rec
   if iiDEB[ii] != 0
    repl I->FORMAT with DEB[i,ii]
    cpfout(strnum(I->FORMAT)+"!")
   endif
  next
  repl I->FORMAT with iDEB[i]
  cpfout(strnum(I->FORMAT)+"!")
  repl I->FORMAT with iKRE[i]
  cpfout(strnum(I->FORMAT)+"!")
  repl I->FORMAT with iSADe[i]
  cpfout(strnum(I->FORMAT)+"!")
  repl I->FORMAT with iSAKe[i]
  cpfout(strnum(I->FORMAT))
  cpfout(chr(13)+chr(10))
 endif
 i++
 skip
enddo
use
***
cpfout(replicate("-",str_len)+chr(13)+chr(10))
***
cpfout(" ИТОГО !")
repl I->FORMAT with iSALBEGd
cpfout(strnum(I->FORMAT)+"!")
repl I->FORMAT with iSALBEGk
cpfout(strnum(I->FORMAT)+"!")
for ii=1 to count_rec
 if iiDEB[ii] != 0
  repl I->FORMAT with iiDEB[ii]
  cpfout(strnum(I->FORMAT)+"!")
 endif
next
repl I->FORMAT with iREC
cpfout(strnum(I->FORMAT)+"!")
repl I->FORMAT with iEXP
cpfout(strnum(I->FORMAT)+"!")
repl I->FORMAT with iSALENDd
cpfout(strnum(I->FORMAT)+"!")
repl I->FORMAT with iSALENDk
cpfout(strnum(I->FORMAT))
cpfout(chr(13)+chr(10))
***
cpfout(replicate("-",str_len)+chr(13)+chr(10))
***
cpfclose()
***
select(old_select)
InitIndex()
InitFilter()
go old_Nz
return
