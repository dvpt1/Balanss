*---------------------------------------------------------*
procedure ListLPT1
private old_Nz,d_filter,k_filter
if (.not.debempty()).and.(.not.kreempty())
 WarnWrite(PMSG8)
 return
endif
old_Nz=recno()
d_filter=''
do case
case I->L_DEBET.and.(.not.I->L_SUBDEB)
  d_filter=d_filter+"ДЕБЕТ :"+mDict("str(D1)+' '+D2",I->F_DEBET,"D1",use_path+MainDIC,"")
case I->L_DEBET.and.I->L_SUBDEB
  d_filter=d_filter+"ДЕБЕТ :"+mDict("str(D1)+' '+D2",I->F_DEBET,"D1",use_path+MainDIC,"")
  var_file=use_path+strline(I->F_DEBET)+".DIC"
  if file(var_file)
   d_filter=d_filter+"/ СУБСЧЕТ:"+mDict("str(D1)+' '+D2",I->F_SUBDEB,"D1",var_file,"")
  endif
endcase
k_filter=''
do case
case I->L_KREDIT.and.(.not.I->L_SUBKRE)
  k_filter=k_filter+"КРЕДИТ:"+mDict("str(D1)+' '+D2",I->F_KREDIT,"D1",use_path+MainDIC,"")
case I->L_KREDIT.and.I->L_SUBKRE
  k_filter=k_filter+"КРЕДИТ:"+mDict("str(D1)+' '+D2",I->F_KREDIT,"D1",use_path+MainDIC,"")
  var_file=use_path+strline(I->F_KREDIT)+".DIC"
  if file(var_file)
   k_filter=k_filter+"/ СУБСЧЕТ:"+mDict("str(D1)+' '+D2",I->F_SUBKRE,"D1",var_file,"")
  endif
endcase
go top
dbsum=0
if dblpt1(jour_jou,jour_nam,"СПИСОК ПРОВОДОК ЗА "+;
        alltrim(Month[month(I->T_DATE)])+" "+str(year(I->T_DATE),4)+" г."+;
        iif(empty(d_filter),"",chr(13)+chr(10)+d_filter)+;
        iif(empty(k_filter),"",chr(13)+chr(10)+k_filter),'SUMMA')
  repl I->FORMAT with dbsum
  mprints('ИТОГО'+space(45)+str(I->FORMAT)+chr(13)+chr(10)+;
          chr(13)+chr(10)+chr(13)+chr(10)+"Бухгалтеp_____________";
          +chr(13)+chr(10)+varstr(date()))
endif
go old_Nz
return
*---------------------------------------------------------*
procedure Oborot1
private old_color,old_select,old_Nz
private ptr,currprow,str_file,i,j,indx,i_min,i_max,str_prn,str_ch
var_exp=0
name_dbf=""
if .not.FrgPRN1()
 return .F.
endif
old_color=setcolor(cw)
old_select=select()
old_Nz=recno()
set index to
set filter to
go top
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
select 4
use_dic=use_path+MainDIC
use &use_dic SHARED
store reccount() to count_rec,count_exp
store 0 to cnt_rec,cnt_exp
select 5
use &name_dbf SHARED
sum SALBED,SALBEK to iSALBEGd,iSALBEGk
*** Расчет фактического числа rec и exp
decl iREC[count_rec]
decl iEXP[count_exp]
afill(iREC,0)
afill(iEXP,0)
decl is_REC[count_rec]
decl is_EXP[count_exp]
afill(is_REC,.F.)
afill(is_EXP,.F.)
sub_ob1()
***
cpfopen("order.txt")
***
select(old_select)
***  формирование строки таблицы
len_dev=2
nul_dev=val(" 0")
if I->IS_DUBL
 str_len=2*10+2*(len_sum+1)+(cnt_rec+cnt_exp)*(len_sum+1)
else
 str_len=10+2*(len_sum+1)+(cnt_rec+cnt_exp)*(len_sum+1)
endif
*** 1-заголовок
str_file=memoread("order.prn")
i_min=at(chr(1),str_file)
i_max=rat(chr(1),str_file)
if i_min != i_max.and.i_min != (i_max - 1)
 cpfout(strfrm(substr(str_file,i_min+1,i_max-i_min-1)))
endif
***
* 1 строка
cpfout(" Сальдо на начало")
repl I->FORMAT with iSALBEGd-iSALBEGk
cpfout(strnum(I->FORMAT)+chr(13)+chr(10))
cpfout(replicate("-",str_len)+chr(13)+chr(10))
* 2 строка
cpfout("   Дата   !")
if cnt_rec<>0
 cpfout(zentstr(cnt_rec*(len_sum+1)-1,"Приход")+"!")
endif
cpfout(zentstr(len_sum,"Итого")+"!")
if cnt_exp<>0
 cpfout(zentstr(cnt_exp*(len_sum+1)-1,"Расход")+"!")
endif
cpfout(zentstr(len_sum,"Итого"))
if I->IS_DUBL
 cpfout("!   Дата   ")
endif
cpfout(chr(13)+chr(10))
* 3 строка
cpfout("          !")
if cnt_rec<>0
 cpfout(replicate("-",cnt_rec*(len_sum+1)-1)+"!")
endif
cpfout(zentstr(len_sum,"приход")+"!")
if cnt_exp<>0
 cpfout(replicate("-",cnt_exp*(len_sum+1)-1)+"!")
endif
cpfout(zentstr(len_sum,"расход"))
if I->IS_DUBL
 cpfout("!          ")
endif
cpfout(chr(13)+chr(10))
* 4 строка
cpfout("          !")
sub_ob2()
cpfout(space(len_sum))
if I->IS_DUBL
 cpfout("!          ")
endif
cpfout(chr(13)+chr(10))
* 5 строка
cpfout(replicate("-",str_len)+chr(13)+chr(10))
***
iRECSUM=0
iEXPSUM=0
iSALBEGd=0
iSALENDd=0
iSALBEGk=0
iSALENDk=0
select 5
go top
do while (.not.eof())
  str_ch=" "+varstr(ctod(str(E->D1,2)+"."+str(month(I->T_DATE),2)+"."+str(year(I->T_DATE),4)))+" !"
  iSALBEGd=iSALBEGd+SALBED
  iSALBEGk=iSALBEGk+SALBEK
  dRECSUM=0
  dEXPSUM=0
  is_sum=.F.
  sub_ob4()
  str_ch=str_ch+strnum(I->FORMAT)
  select 5
  iSALENDd=iSALENDd+SALBED-SALBEK+dRECSUM-dEXPSUM
  if is_sum
   cpfout(str_ch)
   if I->IS_DUBL
    cpfout("! "+varstr(ctod(str(E->D1,2)+"."+str(month(I->T_DATE),2)+"."+str(year(I->T_DATE),4))))
   endif
   cpfout(chr(13)+chr(10))
  endif
  skip
enddo
* "Итого"
cpfout(replicate("-",str_len)+chr(13)+chr(10))
cpfout("  ИТОГО   !")
sub_ob5()
repl I->FORMAT with iEXPSUM
cpfout(strnum(I->FORMAT))
cpfout(chr(13)+chr(10))
* 3 - END строка
cpfout(replicate("-",str_len)+chr(13)+chr(10))
cpfout(" Сальдо на конец ")
repl I->FORMAT with iSALENDd
cpfout(strnum(I->FORMAT)+chr(13)+chr(10))
i_min=at(chr(3),str_file)
i_max=rat(chr(3),str_file)
if i_min != i_max.and.i_min != (i_max - 1)
  cpfout(strfrm(substr(str_file,i_min+1,i_max-i_min-1)))
endif
***
select 2
use
select 3
use
select 4
use
select 5
use
dele file debet.dbf
dele file kredit.dbf
dele file debet.ntx
dele file kredit.ntx
***
cpfclose()
***
setcolor(old_color)
select(old_select)
InitIndex()
InitFilter()
go old_Nz
return
*---------------------------------------------------------*
procedure Oborot2
private old_color,old_select,old_Nz
private ptr,currprow,str_file,i,j,indx,i_min,i_max,str_prn,str_ch
var_exp=0
name_dbf=""
if .not.FrgPRN1()
 return .F.
endif
old_color=setcolor(cw)
old_select=select()
old_Nz=recno()
set index to
set filter to
go top
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
select 4
use_dic=use_path+MainDIC
use &use_dic SHARED
store reccount() to count_rec,count_exp
store 0 to cnt_rec,cnt_exp
select 5
use &name_dbf SHARED
***
Oborot2_1("order")
***
cpfclose()
***
setcolor(old_color)
select(old_select)
InitIndex()
InitFilter()
go old_Nz
return
*---------------------------------------------------------*
procedure Oborot2_1
parameters file_ord
len_num=lenvar(D1)
len_dev=lenvar(D2)
nul_dev=nulvar(D2)
*** Расчет фактического числа rec и exp
decl iREC[count_rec]
decl iEXP[count_exp]
afill(iREC,0)
afill(iEXP,0)
decl is_REC[count_rec]
decl is_EXP[count_exp]
afill(is_REC,.F.)
afill(is_EXP,.F.)
sub_ob1()
***
cpfopen(file_ord+".txt")
***
select(old_select)
***  формирование строки таблицы
if I->IS_DUBL
 str_len=2*(len_num+1+len_dev)+6*(len_sum+1)+(cnt_rec+cnt_exp)*(len_sum+1)
else
 str_len=len_num+1+len_dev+6*(len_sum+1)+(cnt_rec+cnt_exp)*(len_sum+1)
endif
sub_len=len_num+1+len_dev
*** 1-заголовок
str_file=memoread(file_ord+".prn")
i_min=at(chr(1),str_file)
i_max=rat(chr(1),str_file)
if i_min != i_max.and.i_min != (i_max - 1)
  cpfout(strfrm(substr(str_file,i_min+1,i_max-i_min-1)))
endif
***
* 1 строка
cpfout(replicate("-",str_len)+chr(13)+chr(10))
* 2 строка
cpfout(zentstr(sub_len,"Субсчет")+"!")
cpfout(zentstr(2*(len_sum+1)-1,"Сальдо на начало")+"!")
if cnt_rec<>0
 cpfout(zentstr(cnt_rec*(len_sum+1)-1,"Дебет")+"!")
endif
cpfout(zentstr(len_sum,"Итого")+"!")
if cnt_exp<>0
 cpfout(zentstr(cnt_exp*(len_sum+1)-1,"Кpедит")+"!")
endif
cpfout(zentstr(len_sum,"Итого")+"!")
cpfout(zentstr(2*(len_sum+1)-1,"Сальдо на конец"))
if I->IS_DUBL
 cpfout("!"+zentstr(sub_len,"Субсчет"))
endif
cpfout(chr(13)+chr(10))
* 3 строка
cpfout(space(sub_len)+"!")
cpfout(replicate("-",2*(len_sum+1)-1)+"!")
if cnt_rec<>0
 cpfout(replicate("-",cnt_rec*(len_sum+1)-1)+"!")
endif
cpfout(zentstr(len_sum,"дебет")+"!")
if cnt_exp<>0
 cpfout(replicate("-",cnt_exp*(len_sum+1)-1)+"!")
endif
cpfout(zentstr(len_sum,"кредит")+"!")
cpfout(replicate("-",2*(len_sum+1)-1))
if I->IS_DUBL
 cpfout("!"+space(sub_len))
endif
cpfout(chr(13)+chr(10))
* 4 строка
cpfout(space(sub_len)+"!")
cpfout(zentstr(len_sum,"дебет")+"!"+zentstr(len_sum,"кредит")+"!")
sub_ob2()
cpfout(space(len_sum)+"!")
cpfout(zentstr(len_sum,"дебет")+"!"+zentstr(len_sum,"кредит"))
if I->IS_DUBL
 cpfout("!"+space(sub_len))
endif
cpfout(chr(13)+chr(10))
* 5 строка
cpfout(replicate("-",str_len)+chr(13)+chr(10))
***
iRECSUM=0
iEXPSUM=0
iSALBEGd=0
iSALENDd=0
iSALBEGk=0
iSALENDk=0
select 5
go top
do while (.not.eof())
  str_ch=strroun(D1)+" "+D2+"!"
  str_ch=str_ch+strnum(SALBED)+"!"+strnum(SALBEK)+"!"
  iSALBEGd=iSALBEGd+SALBED
  iSALBEGk=iSALBEGk+SALBEK
  dRECSUM=0
  dEXPSUM=0
  is_sum=.F.
  sub_ob4()
  str_ch=str_ch+strnum(I->FORMAT)+"!"
  select 5
  repl I->FORMAT with SALBED-SALBEK+dRECSUM-dEXPSUM
  iSALENDd=iSALENDd+iif(I->FORMAT>0,I->FORMAT,0)
  iSALENDk=iSALENDk+iif(I->FORMAT<0,I->FORMAT,0)
  if I->FORMAT>0
   str_ch=str_ch+strnum(I->FORMAT)+"!"
  else
   str_ch=str_ch+strnum(num_sum)+"!"
  endif
  if I->FORMAT<0
   repl I->FORMAT with -(I->FORMAT)
   str_ch=str_ch+strnum(I->FORMAT)
  else
   str_ch=str_ch+strnum(num_sum)
  endif
  if is_sum.or.E->SALBED!=0.or.E->SALBEK!=0
    cpfout(str_ch)
    if I->IS_DUBL
      cpfout("!"+strroun(D1)+" "+D2)
    endif
    cpfout(chr(13)+chr(10))
  endif
  skip
enddo
* "Итого"
cpfout(replicate("-",str_len)+chr(13)+chr(10))
cpfout(zentstr(sub_len,"ИТОГО")+"!")
repl I->FORMAT with iSALBEGd
cpfout(strnum(I->FORMAT)+"!")
repl I->FORMAT with iSALBEGk
cpfout(strnum(I->FORMAT)+"!")
sub_ob5()
repl I->FORMAT with iEXPSUM
cpfout(strnum(I->FORMAT)+"!")
repl I->FORMAT with iSALENDd
cpfout(strnum(I->FORMAT)+"!")
repl I->FORMAT with -iSALENDk
cpfout(strnum(I->FORMAT))
cpfout(chr(13)+chr(10))
* 3 - END строка
cpfout(replicate("-",str_len)+chr(13)+chr(10))
i_min=at(chr(3),str_file)
i_max=rat(chr(3),str_file)
if i_min != i_max.and.i_min != (i_max - 1)
  cpfout(strfrm(substr(str_file,i_min+1,i_max-i_min-1)))
endif
***
select 2
use
select 3
use
select 4
use
select 5
use
dele file debet.dbf
dele file kredit.dbf
dele file debet.ntx
dele file kredit.ntx
return
*---------------------------------------------------------*
procedure Oborot3
private old_color,old_select,old_Nz
private ptr,currprow,str_file,i,j,indx,i_min,i_max,str_prn,str_ch,nnnn
var_exp=0
name_dbf=""
if .not.FrgPRN1()
 return
endif
old_color=setcolor(cw)
old_select=select()
old_Nz=recno()
set index to
set filter to
go top
sort on DEBET,SUBDEB,KREDIT to debet.dbf for DEBET=var_exp.and.!FUTURE
set index to
select 2
use debet.dbf
index on str(DEBET)+str(SUBDEB)+str(KREDIT) to debet.ntx
select(old_select)
sort on KREDIT,SUBKRE,DEBET to kredit.dbf for KREDIT=var_exp.and.!FUTURE
set index to
select 3
use kredit.dbf
index on str(KREDIT)+str(SUBKRE)+str(DEBET) to kredit.ntx
select 4
use_dic=use_path+MainDIC
use &use_dic SHARED
len_bum=lenvar(D1)
nul_bev=nulvar(D1)
store reccount() to count_rec,count_exp
select 5
use &name_dbf SHARED
len_num=lenvar(D1)
len_dev=lenvar(D2)
nul_dev=nulvar(D2)
len_mat=10
len_nd=5
len_dd=8
***
cpfopen("order.txt")
***
select(old_select)
***  формирование строки таблицы
str_len=len_num+1+len_dev+len_mat+1+2*(len_sum+len_bum+len_nd+len_dd+4)+4*(len_sum+1)
sub_len=len_num+1+len_dev
*** 1-заголовок
str_file=memoread("order.prn")
i_min=at(chr(1),str_file)
i_max=rat(chr(1),str_file)
if i_min != i_max.and.i_min != (i_max - 1)
  cpfout(strfrm(substr(str_file,i_min+1,i_max-i_min-1)))
endif
***
* 1 строка
cpfout(replicate("-",str_len)+chr(13)+chr(10))
* 2 строка
cpfout(zentstr(len_num,"NN")+"!")
cpfout(zentstr(len_dev,"Поставщик")+"!")
cpfout(zentstr(len_mat,"Hазначение")+"!")
cpfout(zentstr(2*(len_sum+1)-1,"Сальдо на начало")+"!")
cpfout(zentstr(len_sum+len_bum+len_nd+len_dd+3,"Дебет")+"!")
cpfout(zentstr(len_sum+len_bum+len_nd+len_dd+3,"Кpедит")+"!")
cpfout(zentstr(2*(len_sum+1)-1,"Сальдо на конец"))
cpfout(chr(13)+chr(10))
* 3 строка
cpfout(space(len_num)+"!")
cpfout(space(len_dev)+"!")
cpfout(space(len_mat)+"!")
cpfout(replicate("-",2*(len_sum+1)-1)+"!")
cpfout(replicate("-",len_sum+len_bum+len_nd+len_dd+3)+"!")
cpfout(replicate("-",len_sum+len_bum+len_nd+len_dd+3)+"!")
cpfout(replicate("-",2*(len_sum+1)-1))
cpfout(chr(13)+chr(10))
* 4 строка
cpfout(space(len_num)+"!")
cpfout(space(len_dev)+"!")
cpfout(space(len_mat)+"!")
cpfout(zentstr(len_sum,"дебет")+"!"+zentstr(len_sum,"кредит")+"!")
cpfout(zentstr(len_nd,"N док")+"!")
cpfout(zentstr(len_dd,"Дата док")+"!")
cpfout(zentstr(len_bum,"Счет")+"!")
cpfout(zentstr(len_sum,"Сумма")+"!")
cpfout(zentstr(len_nd,"N док")+"!")
cpfout(zentstr(len_dd,"Дата док")+"!")
cpfout(zentstr(len_bum,"Счет")+"!")
cpfout(zentstr(len_sum,"Сумма")+"!")
cpfout(zentstr(len_sum,"дебет")+"!"+zentstr(len_sum,"кредит"))
cpfout(chr(13)+chr(10))
* 5 строка
cpfout(replicate("-",str_len)+chr(13)+chr(10))
***
iRECSUM=0
iEXPSUM=0
iSALBEGd=0
iSALENDd=0
iSALBEGk=0
iSALENDk=0
select 5
go top
nnnn=0
SET SOFTSEEK ON
do while (.not.eof())
 is_s=.F.
 is_d=.F.
 is_k=.F.
 if SALBED!=0.or.SALBEK!=0
  is_s=.T.
 endif
 select 2
 seek str(var_exp)+str(E->D1)
 if found()
  is_d=.T.
 endif
 select 3
 seek str(var_exp)+str(E->D1)
 if found()
  is_k=.T.
 endif
 if is_s.or.is_d.or.is_k
  select 5
  nnnn++
  cpfout(strroun(D1)+"!"+D2+"!")
  if is_k
   mat=ssubstr(C->NOTES,3,len_mat)
  else
   if is_d
    mat=ssubstr(B->NOTES,3,len_mat)
   else
    mat=space(len_mat)
   endif
  endif
  cpfout(mat+"!")
  cpfout(strnum(SALBED)+"!"+strnum(SALBEK)+"!")
  iSALBEGd=iSALBEGd+SALBED
  iSALBEGk=iSALBEGk+SALBEK
  dRECSUM=0
  dEXPSUM=0
  do while is_d.or.is_k
   select 2
   if is_d
    if (.not.eof()).and.E->D1=B->SUBDEB
     cpfout(ssubstr(NOTES,1,len_nd)+"!"+ssubstr(NOTES,2,len_dd)+"!")
     cpfout(strroun(KREDIT)+"!"+strnum(SUMMA)+"!")
     dRECSUM=dRECSUM+SUMMA
     var=strline(KREDIT)
     if type("REC&var")=="N"
      REC&var=REC&var+SUMMA
     else
      REC&var=SUMMA
     endif
     skip
     if eof().or.E->D1!=B->SUBDEB
      is_d=.F.
     endif
    endif
   else
    cpfout(space(len_nd)+"!"+space(len_dd)+"!"+space(len_bum)+"!"+space(len_sum)+"!")
   endif
   select 3
   if is_k
    if (.not.eof()).and.E->D1=C->SUBKRE
     cpfout(ssubstr(NOTES,1,len_nd)+"!"+ssubstr(NOTES,2,len_dd)+"!")
     cpfout(strroun(DEBET)+"!"+strnum(SUMMA)+"!")
     dEXPSUM=dEXPSUM+SUMMA
     var=strline(DEBET)
     if type("EXP&var")=="N"
      EXP&var=EXP&var+SUMMA
     else
      EXP&var=SUMMA
     endif
     skip
     if eof().or.E->D1!=C->SUBKRE
      is_k=.F.
     endif
    endif
   else
    cpfout(space(len_nd)+"!"+space(len_dd)+"!"+space(len_bum)+"!"+space(len_sum)+"!")
   endif
   cpfout(strnum(num_sum)+"!")
   cpfout(strnum(num_sum))
   cpfout(chr(13)+chr(10))
   cpfout(space(len_num)+"!"+space(len_dev)+"!"+space(len_mat)+"!"+space(len_sum)+"!"+space(len_sum)+"!")
  enddo
  iRECSUM=iRECSUM+dRECSUM
  iEXPSUM=iEXPSUM+dEXPSUM
  repl I->FORMAT with dRECSUM
  if dRECSUM!=0
   cpfout(space(len_nd)+"!"+space(len_dd)+"!"+zentstr(len_bum,"итого")+"!"+strnum(I->FORMAT)+"!")
  else
   cpfout(space(len_nd)+"!"+space(len_dd)+"!"+space(len_bum)+"!"+space(len_sum)+"!")
  endif
  repl I->FORMAT with dEXPSUM
  if dEXPSUM!=0
   cpfout(space(len_nd)+"!"+space(len_dd)+"!"+zentstr(len_bum,"итого")+"!"+strnum(I->FORMAT)+"!")
  else
   cpfout(space(len_nd)+"!"+space(len_dd)+"!"+space(len_bum)+"!"+space(len_sum)+"!")
  endif
  select 5
  repl I->FORMAT with SALBED-SALBEK+dRECSUM-dEXPSUM
  iSALENDd=iSALENDd+iif(I->FORMAT>0,I->FORMAT,0)
  iSALENDk=iSALENDk+iif(I->FORMAT<0,I->FORMAT,0)
  if I->FORMAT>0
   cpfout(strnum(I->FORMAT)+"!")
  else
   cpfout(space(len_sum)+"!")
  endif
  if I->FORMAT<0
   repl I->FORMAT with -(I->FORMAT)
   cpfout(strnum(I->FORMAT))
  else
   cpfout(space(len_sum))
  endif
  cpfout(chr(13)+chr(10))
  cpfout(replicate("-",str_len)+chr(13)+chr(10))
 endif
 select 5
 skip
enddo
SET SOFTSEEK OFF
* "Итого"
cpfout(replicate("-",str_len)+chr(13)+chr(10))
select 2
zap
select 4
go top
do while .not.eof()
 var=strline(D->D1)
 if type("REC&var")=="N"
  select 2
  append blank
  repl KREDIT with D->D1
  repl SUMMA with REC&var
  select 4
 endif
 skip
enddo
select 2
go top
is_d=reccount()!=0
select 3
zap
select 4
go top
do while .not.eof()
 var=strline(D->D1)
 if type("EXP&var")=="N"
  select 3
  append blank
  repl DEBET with D->D1
  repl SUMMA with EXP&var
  select 4
 endif
 skip
enddo
select 3
go top
is_k=reccount()!=0
cpfout(zentstr(sub_len+1+len_mat+2*(len_sum+1),"Итого")+"!")
do while is_d.or.is_k
 select 2
 if is_d
  if .not.eof()
   cpfout(space(len_nd)+" "+space(len_dd)+"!")
   cpfout(strroun(KREDIT)+"!"+strnum(SUMMA)+"!")
   skip
   if eof()
    is_d=.F.
   endif
  endif
 else
  cpfout(space(len_nd)+" "+space(len_dd)+"!"+space(len_bum)+"!"+space(len_sum)+"!")
 endif
 select 3
 if is_k
  if .not.eof()
   cpfout(space(len_nd)+" "+space(len_dd)+"!")
   cpfout(strroun(DEBET)+"!"+strnum(SUMMA)+"!")
   skip
   if eof()
    is_k=.F.
   endif
  endif
 else
  cpfout(space(len_nd)+" "+space(len_dd)+" "+space(len_bum)+" "+space(len_sum)+" ")
 endif
 cpfout(chr(13)+chr(10))
 if !is_d.and.!is_k
  exit
 endif
 cpfout(space(sub_len)+" "+space(len_mat)+" "+space(len_sum)+" "+space(len_sum)+"!")
enddo
* "ИТОГО"
cpfout(replicate("-",str_len)+chr(13)+chr(10))
cpfout(zentstr(sub_len+1+len_mat,"ИТОГО")+"!")
repl I->FORMAT with iSALBEGd
cpfout(strnum(I->FORMAT)+"!")
repl I->FORMAT with iSALBEGk
cpfout(strnum(I->FORMAT)+"!")
repl I->FORMAT with iRECSUM
cpfout(space(len_nd)+"!"+space(len_dd)+"!"+space(len_bum)+"!"+strnum(I->FORMAT)+"!")
repl I->FORMAT with iEXPSUM
cpfout(space(len_nd)+"!"+space(len_dd)+"!"+space(len_bum)+"!"+strnum(I->FORMAT)+"!")
repl I->FORMAT with iSALENDd
cpfout(strnum(I->FORMAT)+"!")
repl I->FORMAT with -iSALENDk
cpfout(strnum(I->FORMAT))
cpfout(chr(13)+chr(10))
cpfout(replicate("-",str_len)+chr(13)+chr(10))
* 3 - END строка
i_min=at(chr(3),str_file)
i_max=rat(chr(3),str_file)
if i_min != i_max.and.i_min != (i_max - 1)
  cpfout(strfrm(substr(str_file,i_min+1,i_max-i_min-1)))
endif
***
select 2
use
select 3
use
select 4
use
select 5
use
dele file debet.dbf
dele file kredit.dbf
dele file debet.ntx
dele file kredit.ntx
***
cpfclose()
***
setcolor(old_color)
select(old_select)
InitIndex()
InitFilter()
go old_Nz
return
*---------------------------------------------------------*
procedure Oborot4
private old_color,old_select,old_Nz
private ptr,currprow,str_file,i,j,indx,i_min,i_max,str_prn,str_ch
var_exp=0
name_dbf=""
if .not.FrgPRN1()
 return
endif
old_color=setcolor(cw)
old_select=select()
old_Nz=recno()
set index to
set filter to
go top
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
select 4
use_dic=use_path+MainDIC
use &use_dic SHARED
store reccount() to count_rec,count_exp
store 0 to cnt_rec,cnt_exp
select 5
use &name_dbf SHARED
len_num=lenvar(D1)
len_dev=lenvar(D2)
nul_dev=nulvar(D2)
*** Расчет фактического числа rec и exp
decl iREC[count_rec]
decl iEXP[count_exp]
afill(iREC,0)
afill(iEXP,0)
decl is_REC[count_rec]
decl is_EXP[count_exp]
afill(is_REC,.F.)
afill(is_EXP,.F.)
sub_ob1()
***
cpfopen("order.txt")
***
select(old_select)
***  формирование строки таблицы
str_len=len_num+1+len_dev+2*(len_sum+1)+(cnt_rec+cnt_exp)*(len_sum+1)
sub_len=len_num+1+len_dev
*** 1-заголовок
str_file=memoread("order.prn")
i_min=at(chr(1),str_file)
i_max=rat(chr(1),str_file)
if i_min != i_max.and.i_min != (i_max - 1)
  cpfout(strfrm(substr(str_file,i_min+1,i_max-i_min-1)))
endif
***
* 1 строка
cpfout(replicate("-",str_len)+chr(13)+chr(10))
* 2 строка
cpfout(zentstr(sub_len,"Субсчет")+"!")
if cnt_rec<>0
 cpfout(zentstr(cnt_rec*(len_sum+1)-1,"Дебет")+"!")
endif
cpfout(zentstr(len_sum,"Итого")+"!")
if cnt_exp<>0
 cpfout(zentstr(cnt_exp*(len_sum+1)-1,"Кpедит")+"!")
endif
cpfout(zentstr(len_sum,"Итого"))
cpfout(chr(13)+chr(10))
* 3 строка
cpfout(space(sub_len)+"!")
if cnt_rec<>0
 cpfout(replicate("-",cnt_rec*(len_sum+1)-1)+"!")
endif
cpfout(zentstr(len_sum,"дебет")+"!")
if cnt_exp<>0
 cpfout(replicate("-",cnt_exp*(len_sum+1)-1)+"!")
endif
cpfout(zentstr(len_sum,"кредит"))
cpfout(chr(13)+chr(10))
* 4 строка
cpfout(space(sub_len)+"!")
sub_ob2()
cpfout(space(len_sum))
cpfout(chr(13)+chr(10))
* 5 строка
cpfout(replicate("-",str_len)+chr(13)+chr(10))
***
iRECSUM=0
iEXPSUM=0
iSALBEGd=0
iSALENDd=0
iSALBEGk=0
iSALENDk=0
select 5
go top
do while (.not.eof())
  str_ch=strroun(D1)+" "+D2+"!"
  iSALBEGd=iSALBEGd+SALBED
  iSALBEGk=iSALBEGk+SALBEK
  dRECSUM=0
  dEXPSUM=0
  is_sum=.F.
  sub_ob4()
  str_ch=str_ch+strnum(I->FORMAT)
  select 5
  repl I->FORMAT with SALBED-SALBEK+dRECSUM-dEXPSUM
  iSALENDd=iSALENDd+iif(I->FORMAT>0,I->FORMAT,0)
  iSALENDk=iSALENDk+iif(I->FORMAT<0,I->FORMAT,0)
  str_ch=str_ch+chr(13)+chr(10)
  if dRECSUM!=0.or.dEXPSUM!=0.or.E->SALBED!=0.or.E->SALBEK!=0
    cpfout(str_ch)
  endif
  skip
enddo
* "Итого"
cpfout(replicate("-",str_len)+chr(13)+chr(10))
cpfout(zentstr(sub_len,"ИТОГО")+"!")
sub_ob5()
repl I->FORMAT with iEXPSUM
cpfout(strnum(I->FORMAT))
cpfout(chr(13)+chr(10))
* 3 - END строка
cpfout(replicate("-",str_len)+chr(13)+chr(10))
i_min=at(chr(3),str_file)
i_max=rat(chr(3),str_file)
if i_min != i_max.and.i_min != (i_max - 1)
  cpfout(strfrm(substr(str_file,i_min+1,i_max-i_min-1)))
endif
***
select 2
use
select 3
use
select 4
use
select 5
use
dele file debet.dbf
dele file kredit.dbf
dele file debet.ntx
dele file kredit.ntx
***
cpfclose()
***
setcolor(old_color)
select(old_select)
InitIndex()
InitFilter()
go old_Nz
return
*---------------------------------------------------------*
procedure Oborot5
private old_color,old_select,old_Nz
private ptr,currprow,str_file,i,j,indx,i_min,i_max,str_prn,str_ch,nnnn
var_exp=0
name_dbf=""
if .not.FrgPRN1()
 return
endif
old_color=setcolor(cw)
old_select=select()
old_Nz=recno()
set index to
set filter to
go top
sort on DEBET,SUBDEB,KREDIT to debet.dbf for DEBET=var_exp.and.!FUTURE
set index to
select 2
use debet.dbf
index on str(DEBET)+str(SUBDEB)+str(KREDIT) to debet.ntx
select(old_select)
sort on KREDIT,SUBKRE,DEBET to kredit.dbf for KREDIT=var_exp.and.!FUTURE
set index to
select 3
use kredit.dbf
index on str(KREDIT)+str(SUBKRE)+str(DEBET) to kredit.ntx
select 4
use_dic=use_path+MainDIC
use &use_dic SHARED
len_bum=lenvar(D1)
nul_bev=nulvar(D1)
store reccount() to count_rec,count_exp
select 5
use &name_dbf SHARED
len_num=lenvar(D1)
len_dev=lenvar(D2)
nul_dev=nulvar(D2)
len_mat=10
len_nd=5
len_dd=8
***
cpfopen("order.txt")
***
select(old_select)
***  формирование строки таблицы
str_len=len_num+1+len_dev+2*(len_sum+len_bum+len_nd+len_dd+4)+4*(len_sum+1)
sub_len=len_num+1+len_dev
*** 1-заголовок
str_file=memoread("order.prn")
i_min=at(chr(1),str_file)
i_max=rat(chr(1),str_file)
if i_min != i_max.and.i_min != (i_max - 1)
  cpfout(strfrm(substr(str_file,i_min+1,i_max-i_min-1)))
endif
***
* 1 строка
cpfout(replicate("-",str_len)+chr(13)+chr(10))
* 2 строка
cpfout(zentstr(len_num,"NN")+"!")
cpfout(zentstr(len_dev,mDict("alltrim(D2)",var_exp,"D1",use_path+MainDIC,""))+"!")
cpfout(zentstr(2*(len_sum+1)-1,"Сальдо на начало")+"!")
cpfout(zentstr(len_sum+len_bum+len_nd+len_dd+3,"Дебет")+"!")
cpfout(zentstr(len_sum+len_bum+len_nd+len_dd+3,"Кpедит")+"!")
cpfout(zentstr(2*(len_sum+1)-1,"Сальдо на конец"))
cpfout(chr(13)+chr(10))
* 3 строка
cpfout(space(len_num)+"!")
cpfout(space(len_dev)+"!")
cpfout(replicate("-",2*(len_sum+1)-1)+"!")
cpfout(replicate("-",len_sum+len_bum+len_nd+len_dd+3)+"!")
cpfout(replicate("-",len_sum+len_bum+len_nd+len_dd+3)+"!")
cpfout(replicate("-",2*(len_sum+1)-1))
cpfout(chr(13)+chr(10))
* 4 строка
cpfout(space(len_num)+"!")
cpfout(space(len_dev)+"!")
cpfout(zentstr(len_sum,"дебет")+"!"+zentstr(len_sum,"кредит")+"!")
cpfout(zentstr(len_nd,"N док")+"!")
cpfout(zentstr(len_dd,"Дата док")+"!")
cpfout(zentstr(len_bum,"Счет")+"!")
cpfout(zentstr(len_sum,"Сумма")+"!")
cpfout(zentstr(len_nd,"N док")+"!")
cpfout(zentstr(len_dd,"Дата док")+"!")
cpfout(zentstr(len_bum,"Счет")+"!")
cpfout(zentstr(len_sum,"Сумма")+"!")
cpfout(zentstr(len_sum,"дебет")+"!"+zentstr(len_sum,"кредит"))
cpfout(chr(13)+chr(10))
* 5 строка
cpfout(replicate("-",str_len)+chr(13)+chr(10))
***
iRECSUM=0
iEXPSUM=0
iSALBEGd=0
iSALENDd=0
iSALBEGk=0
iSALENDk=0
select 5
go top
nnnn=0
SET SOFTSEEK ON
do while (.not.eof())
 is_s=.F.
 is_d=.F.
 is_k=.F.
 if SALBED!=0.or.SALBEK!=0
  is_s=.T.
 endif
 select 2
 seek str(var_exp)+str(E->D1)
 if found()
  is_d=.T.
 endif
 select 3
 seek str(var_exp)+str(E->D1)
 if found()
  is_k=.T.
 endif
 if is_s.or.is_d.or.is_k
  select 5
  nnnn++
  cpfout(strroun(D1)+"!"+D2+"!")
  cpfout(strnum(SALBED)+"!"+strnum(SALBEK)+"!")
  iSALBEGd=iSALBEGd+SALBED
  iSALBEGk=iSALBEGk+SALBEK
  dRECSUM=0
  dEXPSUM=0
  do while is_d.or.is_k
   select 2
   if is_d
    if (.not.eof()).and.E->D1=B->SUBDEB
     cpfout(ssubstr(NOTES,1,len_nd)+"!"+ssubstr(NOTES,2,len_dd)+"!")
     cpfout(strroun(KREDIT)+"!"+strnum(SUMMA)+"!")
     dRECSUM=dRECSUM+SUMMA
     var=strline(KREDIT)
     if type("REC&var")=="N"
      REC&var=REC&var+SUMMA
     else
      REC&var=SUMMA
     endif
     skip
     if eof().or.E->D1!=B->SUBDEB
      is_d=.F.
     endif
    endif
   else
    cpfout(space(len_nd)+"!"+space(len_dd)+"!"+space(len_bum)+"!"+space(len_sum)+"!")
   endif
   select 3
   if is_k
    if (.not.eof()).and.E->D1=C->SUBKRE
     cpfout(ssubstr(NOTES,1,len_nd)+"!"+ssubstr(NOTES,2,len_dd)+"!")
     cpfout(strroun(DEBET)+"!"+strnum(SUMMA)+"!")
     dEXPSUM=dEXPSUM+SUMMA
     var=strline(DEBET)
     if type("EXP&var")=="N"
      EXP&var=EXP&var+SUMMA
     else
      EXP&var=SUMMA
     endif
     skip
     if eof().or.E->D1!=C->SUBKRE
      is_k=.F.
     endif
    endif
   else
    cpfout(space(len_nd)+"!"+space(len_dd)+"!"+space(len_bum)+"!"+space(len_sum)+"!")
   endif
   cpfout(strnum(num_sum)+"!")
   cpfout(strnum(num_sum))
   cpfout(chr(13)+chr(10))
   cpfout(space(len_num)+"!"+space(len_dev)+"!"+space(len_sum)+"!"+space(len_sum)+"!")
  enddo
  iRECSUM=iRECSUM+dRECSUM
  iEXPSUM=iEXPSUM+dEXPSUM
  repl I->FORMAT with dRECSUM
  if dRECSUM!=0
   cpfout(space(len_nd)+"!"+space(len_dd)+"!"+zentstr(len_bum,"итого")+"!"+strnum(I->FORMAT)+"!")
  else
   cpfout(space(len_nd)+"!"+space(len_dd)+"!"+space(len_bum)+"!"+space(len_sum)+"!")
  endif
  repl I->FORMAT with dEXPSUM
  if dEXPSUM!=0
   cpfout(space(len_nd)+"!"+space(len_dd)+"!"+zentstr(len_bum,"итого")+"!"+strnum(I->FORMAT)+"!")
  else
   cpfout(space(len_nd)+"!"+space(len_dd)+"!"+space(len_bum)+"!"+space(len_sum)+"!")
  endif
  select 5
  repl I->FORMAT with SALBED-SALBEK+dRECSUM-dEXPSUM
  iSALENDd=iSALENDd+iif(I->FORMAT>0,I->FORMAT,0)
  iSALENDk=iSALENDk+iif(I->FORMAT<0,I->FORMAT,0)
  if I->FORMAT>0
   cpfout(strnum(I->FORMAT)+"!")
  else
   cpfout(space(len_sum)+"!")
  endif
  if I->FORMAT<0
   repl I->FORMAT with -(I->FORMAT)
   cpfout(strnum(I->FORMAT))
  else
   cpfout(space(len_sum))
  endif
  cpfout(chr(13)+chr(10))
  cpfout(replicate("-",str_len)+chr(13)+chr(10))
 endif
 select 5
 skip
enddo
SET SOFTSEEK OFF
* "Итого"
cpfout(replicate("-",str_len)+chr(13)+chr(10))
select 2
zap
select 4
go top
do while .not.eof()
 var=strline(D->D1)
 if type("REC&var")=="N"
  select 2
  append blank
  repl KREDIT with D->D1
  repl SUMMA with REC&var
  select 4
 endif
 skip
enddo
select 2
go top
is_d=reccount()!=0
select 3
zap
select 4
go top
do while .not.eof()
 var=strline(D->D1)
 if type("EXP&var")=="N"
  select 3
  append blank
  repl DEBET with D->D1
  repl SUMMA with EXP&var
  select 4
 endif
 skip
enddo
select 3
go top
is_k=reccount()!=0
cpfout(zentstr(sub_len+2*(len_sum+1),"Итого")+"!")
do while is_d.or.is_k
 select 2
 if is_d
  if .not.eof()
   cpfout(space(len_nd)+" "+space(len_dd)+"!")
   cpfout(strroun(KREDIT)+"!"+strnum(SUMMA)+"!")
   skip
   if eof()
    is_d=.F.
   endif
  endif
 else
  cpfout(space(len_nd)+" "+space(len_dd)+"!"+space(len_bum)+"!"+space(len_sum)+"!")
 endif
 select 3
 if is_k
  if .not.eof()
   cpfout(space(len_nd)+" "+space(len_dd)+"!")
   cpfout(strroun(DEBET)+"!"+strnum(SUMMA)+"!")
   skip
   if eof()
    is_k=.F.
   endif
  endif
 else
  cpfout(space(len_nd)+" "+space(len_dd)+" "+space(len_bum)+" "+space(len_sum)+" ")
 endif
 cpfout(chr(13)+chr(10))
 if !is_d.and.!is_k
  exit
 endif
 cpfout(space(sub_len)+" "+space(len_sum)+" "+space(len_sum)+"!")
enddo
* "ИТОГО"
cpfout(replicate("-",str_len)+chr(13)+chr(10))
cpfout(zentstr(sub_len,"ИТОГО")+"!")
repl I->FORMAT with iSALBEGd
cpfout(strnum(I->FORMAT)+"!")
repl I->FORMAT with iSALBEGk
cpfout(strnum(I->FORMAT)+"!")
repl I->FORMAT with iRECSUM
cpfout(space(len_nd)+"!"+space(len_dd)+"!"+space(len_bum)+"!"+strnum(I->FORMAT)+"!")
repl I->FORMAT with iEXPSUM
cpfout(space(len_nd)+"!"+space(len_dd)+"!"+space(len_bum)+"!"+strnum(I->FORMAT)+"!")
repl I->FORMAT with iSALENDd
cpfout(strnum(I->FORMAT)+"!")
repl I->FORMAT with -iSALENDk
cpfout(strnum(I->FORMAT))
cpfout(chr(13)+chr(10))
cpfout(replicate("-",str_len)+chr(13)+chr(10))
* 3 - END строка
i_min=at(chr(3),str_file)
i_max=rat(chr(3),str_file)
if i_min != i_max.and.i_min != (i_max - 1)
  cpfout(strfrm(substr(str_file,i_min+1,i_max-i_min-1)))
endif
***
select 2
use
select 3
use
select 4
use
select 5
use
dele file debet.dbf
dele file kredit.dbf
dele file debet.ntx
dele file kredit.ntx
***
cpfclose()
***
setcolor(old_color)
select(old_select)
InitIndex()
InitFilter()
go old_Nz
return
*---------------------------------------------------------*
procedure Oborot7
private old_color,old_select,old_Nz
private ptr,currprow,str_file,i,j,indx,i_min,i_max,str_prn,str_ch
old_color=setcolor(cw)
DrawBox(Y_srm-3,X_srm-8,Y_srm+3,X_srm+8,ibox)
setcolor(cl)
MSG=' ПЕPИОД '
@ Y_srm-3,int(X_srm-len(MSG)/2)+1 say MSG
kv=1
setcolor(cw)
@ Y_srm-2,X_srm-7 prompt "первый кваpтал "
@ row()+1,X_srm-7 prompt "второй кваpтал "
@ row()+1,X_srm-7 prompt "третий кваpтал "
@ row()+1,X_srm-7 prompt "четвер.кваpтал "
@ row()+1,X_srm-7 prompt "с начала года  "
menu to kv
if kv=0
  return
endif
do case
case kv=1
 bm=1
 em=3
case kv=2
 bm=4
 em=6
case kv=3
 bm=7
 em=9
case kv=4
 bm=10
 em=12
case kv=5
 bm=1
 em=month(I->T_DATE)
endcase
var_exp=0
name_dbf=""
if .not.FrgPRN1()
 return
endif
old_select=select()
old_Nz=recno()
use
* создание файла по месячно
ch_year=str(year(I->T_DATE),4)
select 1
creat btmp.dbf from balans.str
for cur_month=bm to em
 use_dbf=cur_path+"\"+ch_year+"."+strtran(str(cur_month,2)," ","0")+"\"+MainDBF
 use_sub=cur_path+"\"+ch_year+"."+strtran(str(cur_month,2)," ","0")+"\"+strline(var_exp)+".DIC"
 if file(use_dbf)
  MsgWrite("Складываю "+Month[cur_month])
  select 2
  use &use_dbf SHARED
  do while .not.eof()
   if (B->DEBET=var_exp.OR.B->KREDIT=var_exp).AND.(.not.B->FUTURE)
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
MsgWrite("Фоpмиpую ведомость...")
select 1
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
*** фоpмиpование плана счетов
select 4
creat expenses.dbf from expenses.str
index on D1 to expenses.ntx
select(old_select)
index on DEBET to temp.ntx
total on DEBET fields SUMMA to temp.dbf
set index to
select 6
use temp.dbf
do while .not.eof()
 select 4
 seek F->DEBET
 if .not.found()
  append blank
  repl D->D1 with F->DEBET
 endif
 select 6
 skip
enddo
use
select(old_select)
index on KREDIT to temp.ntx
total on KREDIT fields SUMMA to temp.dbf
set index to
select 6
use temp.dbf
do while .not.eof()
 select 4
 seek F->KREDIT
 if .not.found()
  append blank
  repl D->D1 with F->KREDIT
 endif
 select 6
 skip
enddo
use
dele file temp.dbf
dele file temp.ntx
select 4
go top
store reccount() to count_rec,count_exp
store 0 to cnt_rec,cnt_exp
select 5
use &name_dbf SHARED
len_num=lenvar(D1)
len_dev=lenvar(D2)
nul_dev=nulvar(D2)
*** Расчет фактического числа rec и exp
decl iREC[count_rec]
decl iEXP[count_exp]
afill(iREC,0)
afill(iEXP,0)
decl is_REC[count_rec]
decl is_EXP[count_exp]
afill(is_REC,.F.)
afill(is_EXP,.F.)
sub_ob1()
***
cpfopen("order.txt")
***
select(old_select)
***  формирование строки таблицы
if I->IS_DUBL
 str_len=2*(len_num+1+len_dev)+2*(len_sum+1)+(cnt_rec+cnt_exp)*(len_sum+1)
else
 str_len=len_num+1+len_dev+2*(len_sum+1)+(cnt_rec+cnt_exp)*(len_sum+1)
endif
sub_len=len_num+1+len_dev
*** 1-заголовок
str_file=memoread("order7.prn")
i_min=at(chr(1),str_file)
i_max=rat(chr(1),str_file)
if i_min != i_max.and.i_min != (i_max - 1)
  cpfout(strfrm(substr(str_file,i_min+1,i_max-i_min-1)))
endif
***
* 1 строка
cpfout(replicate("-",str_len)+chr(13)+chr(10))
* 2 строка
cpfout(zentstr(sub_len,"Субсчет")+"!")
if cnt_rec<>0
 cpfout(zentstr(cnt_rec*(len_sum+1)-1,"Дебет")+"!")
endif
cpfout(zentstr(len_sum,"Итого")+"!")
if cnt_exp<>0
 cpfout(zentstr(cnt_exp*(len_sum+1)-1,"Кpедит")+"!")
endif
cpfout(zentstr(len_sum,"Итого"))
if I->IS_DUBL
 cpfout("!"+zentstr(sub_len,"Субсчет"))
endif
cpfout(chr(13)+chr(10))
* 3 строка
cpfout(space(sub_len)+"!")
if cnt_rec<>0
 cpfout(replicate("-",cnt_rec*(len_sum+1)-1)+"!")
endif
cpfout(zentstr(len_sum,"дебет")+"!")
if cnt_exp<>0
 cpfout(replicate("-",cnt_exp*(len_sum+1)-1)+"!")
endif
cpfout(zentstr(len_sum,"кредит"))
if I->IS_DUBL
 cpfout("!"+space(sub_len))
endif
cpfout(chr(13)+chr(10))
* 4 строка
cpfout(space(sub_len)+"!")
sub_ob2()
cpfout(space(len_sum))
if I->IS_DUBL
 cpfout("!"+space(sub_len))
endif
cpfout(chr(13)+chr(10))
* 5 строка
cpfout(replicate("-",str_len)+chr(13)+chr(10))
***
iRECSUM=0
iEXPSUM=0
iSALBEGd=0
iSALENDd=0
iSALBEGk=0
iSALENDk=0
select 5
go top
do while (.not.eof())
  str_ch=strroun(D1)+" "+D2+"!"
  iSALBEGd=iSALBEGd+SALBED
  iSALBEGk=iSALBEGk+SALBEK
  dRECSUM=0
  dEXPSUM=0
  is_sum=.F.
  sub_ob4()
  str_ch=str_ch+strnum(I->FORMAT)
  select 5
  repl I->FORMAT with SALBED-SALBEK+dRECSUM-dEXPSUM
  iSALENDd=iSALENDd+iif(I->FORMAT>0,I->FORMAT,0)
  iSALENDk=iSALENDk+iif(I->FORMAT<0,I->FORMAT,0)
  if dRECSUM!=0.or.dEXPSUM!=0.or.E->SALBED!=0.or.E->SALBEK!=0
    cpfout(str_ch)
    if I->IS_DUBL
      cpfout("!"+strroun(D1)+" "+D2)
    endif
    cpfout(chr(13)+chr(10))
  endif
  skip
enddo
* "Итого"
cpfout(replicate("-",str_len)+chr(13)+chr(10))
cpfout(zentstr(sub_len,"ИТОГО")+"!")
sub_ob5()
repl I->FORMAT with iEXPSUM
cpfout(strnum(I->FORMAT))
cpfout(chr(13)+chr(10))
* 3 - END строка
cpfout(replicate("-",str_len)+chr(13)+chr(10))
i_min=at(chr(3),str_file)
i_max=rat(chr(3),str_file)
if i_min != i_max.and.i_min != (i_max - 1)
  cpfout(strfrm(substr(str_file,i_min+1,i_max-i_min-1)))
endif
***
select 1
use
select 2
use
select 3
use
select 4
use
select 5
use
dele file debet.dbf
dele file kredit.dbf
dele file debet.ntx
dele file kredit.ntx
dele file btmp.dbf
dele file expenses.dbf
dele file expenses.ntx
***
cpfclose()
***
setcolor(old_color)
OpenDBF()
InitFilter()
go old_Nz
return
*---------------------------------------------------------*
procedure Sub_ob1
select 5
go top
do while (.not.eof())
  select 4
  go top
  i=1
  do while .not.eof()
    select 2
    seek str(var_exp)+str(E->D1)+str(D->D1)
    if SUMMA != 0
     is_REC[i]=.T.
    endif
    i=i+1
    select 4
    skip
  enddo
  select 4
  go top
  i=1
  do while .not.eof()
    select 3
    seek str(var_exp)+str(E->D1)+str(D->D1)
    if SUMMA != 0
     is_EXP[i]=.T.
    endif
    i=i+1
    select 4
    skip
  enddo
  select 5
  skip
enddo
j=0
for i=1 to count_rec
  if is_REC[i]
    j=j+1
  endif
next
cnt_rec=j
j=0
for i=1 to count_exp
  if is_EXP[i]
    j=j+1
  endif
next
cnt_exp=j
return
*---------------------------------------------------------*
procedure Sub_ob2
select 4
go top
i=1
do while .not.eof()
  if is_REC[i]
    cpfout(zentstr(len_sum,strroun(D1))+"!")
  endif
  i=i+1
  skip
enddo
cpfout(space(len_sum)+"!")
select 4
go top
i=1
do while .not.eof()
  if is_EXP[i]
    cpfout(zentstr(len_sum,strroun(D1))+"!")
  endif
  i=i+1
  skip
enddo
return
*---------------------------------------------------------*
procedure Sub_ob3
select 4
go top
i=1
do while .not.eof()
  if is_REC[i]
    cpfout(space(len_sum)+"!")
  endif
  i=i+1
  skip
enddo
cpfout(space(len_sum)+"!")
select 4
go top
i=1
do while .not.eof()
  if is_EXP[i]
    cpfout(space(len_sum)+"!")
  endif
  i=i+1
  skip
enddo
return
*---------------------------------------------------------*
procedure Sub_ob4
is_sum=.F.
select 4
go top
i=1
do while .not.eof()
  if is_REC[i]
    select 2
    seek str(var_exp)+str(E->D1)+str(D->D1)
    str_ch=str_ch+strnum(SUMMA)+"!"
    iREC[i]=iREC[i]+SUMMA
    dRECSUM=dRECSUM+SUMMA
    if SUMMA!=0
     is_sum=.T.
    endif
    select 4
  endif
  i=i+1
  skip
enddo
iRECSUM=iRECSUM+dRECSUM
repl I->FORMAT with dRECSUM
str_ch=str_ch+strnum(I->FORMAT)+"!"
select 4
go top
i=1
do while .not.eof()
  if is_EXP[i]
    select 3
    seek str(var_exp)+str(E->D1)+str(D->D1)
    str_ch=str_ch+strnum(SUMMA)+"!"
    iEXP[i]=iEXP[i]+SUMMA
    dEXPSUM=dEXPSUM+SUMMA
    if SUMMA!=0
     is_sum=.T.
    endif
    select 4
  endif
  i=i+1
  skip
enddo
iEXPSUM=iEXPSUM+dEXPSUM
repl I->FORMAT with dEXPSUM
return
*---------------------------------------------------------*
procedure Sub_ob5
i=1
do while i<=count_rec
  if is_REC[i]
    repl I->FORMAT with iREC[i]
    cpfout(strnum(I->FORMAT)+"!")
  endif
  i=i+1
enddo
repl I->FORMAT with iRECSUM
cpfout(strnum(I->FORMAT)+"!")
i=1
do while i<=count_exp
  if is_EXP[i]
    repl I->FORMAT with iEXP[i]
    cpfout(strnum(I->FORMAT)+"!")
  endif
  i=i+1
enddo
return
*---------------------------------------------------------*
procedure Oborot8
private old_color,old_select
private ptr,currprow,str_file,i,j,indx,i_min,i_max,old_Nz,str_prn,str_ch
var_exp=0
name_dbf=""
if .not.FrgPRN1()
 return
endif
repl I->FORMATC with int(var_exp)
var_exp=I->FORMATC
old_color=setcolor(cw)
old_select=select()
old_Nz=recno()
set filter to
go top
* готовлю объединенную базу
select 2
creat btmp.dbf from balans.str
select(old_select)
do while .not.eof()
 if (int(A->DEBET)=var_exp.OR.int(A->KREDIT)=var_exp).AND.(.not.A->FUTURE)
  select 2
  append blank
  repl B->DEBET  with iif(int(A->DEBET)=var_exp,int(A->DEBET),A->DEBET)
  repl B->KREDIT with iif(int(A->KREDIT)=var_exp,int(A->KREDIT),A->KREDIT)
  repl B->SUBDEB with A->SUBDEB
  repl B->SUBKRE with A->SUBKRE
  repl B->SUMMA  with A->SUMMA
  repl B->NOTES  with A->NOTES
  repl B->FUTURE with A->FUTURE
 endif
 select 1
 skip
enddo
select 2
use
select 1
use btmp.dbf
* готовлю объединенный словаpь субсчета
select 4
use_dic=use_path+MainDIC
use &use_dic SHARED
store reccount() to count_rec,count_exp
store 0 to cnt_rec,cnt_exp
select 5
creat stmp.dbf from sub.str
len_num=lenvar(D1)
len_dev=lenvar(D2)
nul_dev=nulvar(D2)
select 4
do while .not.eof()
 if var_exp=int(D->D1)
  name_dbf=use_path+strline(D->D1)+".DIC"
  if file(name_dbf)
   select 3
   use &name_dbf SHARED
   do while .not.eof()
    select 5
    locate for E->D1=C->D1
    if .not.found()
     append blank
     repl E->D1 with C->D1
     repl E->D2 with C->D2
    endif
    saldo=E->SALBED-E->SALBEK+C->SALBED-C->SALBEK
    repl E->SALBED with iif(saldo>0,saldo,0)
    repl E->SALBEK with iif(saldo<0,0-saldo,0)
    select 3
    skip
   enddo
   use
  endif
 endif
 select 4
 skip
enddo
* готовлю объединенный словаpь счетов
select 4
use
use_dic=use_path+MainDIC
copy file &use_dic to ctmp.dbf
select 4
use ctmp.dbf
is_varexp=.T.
do while .not.eof()
 if var_exp=int(D->D1)
  if is_varexp
   repl D->D1 with int(D->D1)
   is_varexp=.F.
  else
   delete
  endif
 endif
 skip
enddo
pack
go top
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
*** Расчет фактического числа rec и exp
decl iREC[count_rec]
decl iEXP[count_exp]
afill(iREC,0)
afill(iEXP,0)
decl is_REC[count_rec]
decl is_EXP[count_exp]
afill(is_REC,.F.)
afill(is_EXP,.F.)
sub_ob1()
***
cpfopen("order.txt")
***
select(old_select)
***  формирование строки таблицы
if I->IS_DUBL
 str_len=2*(len_num+1+len_dev)+6*(len_sum+1)+(cnt_rec+cnt_exp)*(len_sum+1)
else
 str_len=len_num+1+len_dev+6*(len_sum+1)+(cnt_rec+cnt_exp)*(len_sum+1)
endif
sub_len=len_num+1+len_dev
*** 1-заголовок
str_file=memoread("order8.prn")
i_min=at(chr(1),str_file)
i_max=rat(chr(1),str_file)
if i_min != i_max.and.i_min != (i_max - 1)
  cpfout(strfrm(substr(str_file,i_min+1,i_max-i_min-1)))
endif
***
* 1 строка
cpfout(replicate("-",str_len)+chr(13)+chr(10))
* 2 строка
cpfout(zentstr(sub_len,"Субсчет")+"!")
cpfout(zentstr(2*(len_sum+1)-1,"Сальдо на начало")+"!")
if cnt_rec<>0
 cpfout(zentstr(cnt_rec*(len_sum+1)-1,"Дебет")+"!")
endif
cpfout(zentstr(len_sum,"Итого")+"!")
if cnt_exp<>0
 cpfout(zentstr(cnt_exp*(len_sum+1)-1,"Кpедит")+"!")
endif
cpfout(zentstr(len_sum,"Итого")+"!")
cpfout(zentstr(2*(len_sum+1)-1,"Сальдо на конец"))
if I->IS_DUBL
 cpfout("!"+zentstr(sub_len,"Субсчет"))
endif
cpfout(chr(13)+chr(10))
* 3 строка
cpfout(space(sub_len)+"!")
cpfout(replicate("-",2*(len_sum+1)-1)+"!")
if cnt_rec<>0
 cpfout(replicate("-",cnt_rec*(len_sum+1)-1)+"!")
endif
cpfout(zentstr(len_sum,"дебет")+"!")
if cnt_exp<>0
 cpfout(replicate("-",cnt_exp*(len_sum+1)-1)+"!")
endif
cpfout(zentstr(len_sum,"кредит")+"!")
cpfout(replicate("-",2*(len_sum+1)-1))
if I->IS_DUBL
 cpfout("!"+space(sub_len))
endif
cpfout(chr(13)+chr(10))
* 4 строка
cpfout(space(sub_len)+"!")
cpfout(zentstr(len_sum,"дебет")+"!"+zentstr(len_sum,"кредит")+"!")
sub_ob2()
cpfout(space(len_sum)+"!")
cpfout(zentstr(len_sum,"дебет")+"!"+zentstr(len_sum,"кредит"))
if I->IS_DUBL
 cpfout("!"+space(sub_len))
endif
cpfout(chr(13)+chr(10))
* 5 строка
cpfout(replicate("-",str_len)+chr(13)+chr(10))
***
iRECSUM=0
iEXPSUM=0
iSALBEGd=0
iSALENDd=0
iSALBEGk=0
iSALENDk=0
select 5
go top
do while (.not.eof())
  str_ch=strroun(D1)+" "+D2+"!"
  str_ch=str_ch+strnum(SALBED)+"!"+strnum(SALBEK)+"!"
  iSALBEGd=iSALBEGd+SALBED
  iSALBEGk=iSALBEGk+SALBEK
  dRECSUM=0
  dEXPSUM=0
  is_sum=.F.
  sub_ob4()
  str_ch=str_ch+strnum(I->FORMAT)+"!"
  select 5
  repl I->FORMAT with SALBED-SALBEK+dRECSUM-dEXPSUM
  iSALENDd=iSALENDd+iif(I->FORMAT>0,I->FORMAT,0)
  iSALENDk=iSALENDk+iif(I->FORMAT<0,I->FORMAT,0)
  if I->FORMAT>0
   str_ch=str_ch+strnum(I->FORMAT)+"!"
  else
   str_ch=str_ch+strnum(num_sum)+"!"
  endif
  if I->FORMAT<0
   repl I->FORMAT with -(I->FORMAT)
   str_ch=str_ch+strnum(I->FORMAT)
  else
   str_ch=str_ch+strnum(num_sum)
  endif
  if dRECSUM!=0.or.dEXPSUM!=0.or.E->SALBED!=0.or.E->SALBEK!=0
    cpfout(str_ch)
    if I->IS_DUBL
      cpfout("!"+strroun(D1)+" "+D2)
    endif
    cpfout(chr(13)+chr(10))
  endif
  skip
enddo
* "Итого"
cpfout(replicate("-",str_len)+chr(13)+chr(10))
cpfout(zentstr(sub_len,"ИТОГО")+"!")
repl I->FORMAT with iSALBEGd
cpfout(strnum(I->FORMAT)+"!")
repl I->FORMAT with iSALBEGk
cpfout(strnum(I->FORMAT)+"!")
sub_ob5()
repl I->FORMAT with iEXPSUM
cpfout(strnum(I->FORMAT)+"!")
repl I->FORMAT with iSALENDd
cpfout(strnum(I->FORMAT)+"!")
repl I->FORMAT with -iSALENDk
cpfout(strnum(I->FORMAT))
cpfout(chr(13)+chr(10))
* 3 - END строка
cpfout(replicate("-",str_len)+chr(13)+chr(10))
i_min=at(chr(3),str_file)
i_max=rat(chr(3),str_file)
if i_min != i_max.and.i_min != (i_max - 1)
  cpfout(strfrm(substr(str_file,i_min+1,i_max-i_min-1)))
endif
***
select 2
use
select 3
use
select 4
use
select 5
use
dele file debet.dbf
dele file kredit.dbf
dele file debet.ntx
dele file kredit.ntx
dele file btmp.dbf
dele file stmp.dbf
dele file ctmp.dbf
***
cpfclose()
***
setcolor(old_color)
OpenDBF()
InitFilter()
go old_Nz
return
*---------------------------------------------------------*
procedure Oborot9
private old_color,old_select,old_Nz
private str_prn,str_ch,var_exp,var_sub
old_color=setcolor(cw)
DrawBox(Y_srm-3,X_srm-8,Y_srm+1,X_srm+8,ibox)
setcolor(cl)
MSG=' ПЕPИОД '
@ Y_srm-3,int(X_srm-len(MSG)/2)+1 say MSG
kv=1
setcolor(cw)
@ Y_srm-2,X_srm-7 prompt " за месяц      "
@ row()+1,X_srm-7 prompt " за кваpтал    "
@ row()+1,X_srm-7 prompt " с начала года "
menu to kv
if kv=0
  return
endif
do case
case kv=1
 bm=month(I->T_DATE)
 em=month(I->T_DATE)
case kv=2
 em=month(I->T_DATE)
 do case
 case em>=1.and.em<=3
  bm=1
 case em>=4.and.em<=6
  bm=4
 case em>=7.and.em<=9
  bm=7
 case em>=10.and.em<=12
  bm=10
 endcase
case kv=3
 bm=1
 em=month(I->T_DATE)
endcase
setcolor(cw)
DrawBox(Y_srm-3,X_srm-28,Y_srm+3,X_srm+28,ibox)
setcolor(cl)
MSG=' С Ч Е Т / С У Б С Ч Е Т '
@ Y_srm-3,int(X_srm-len(MSG)/2)+1 say MSG
var_exp=nul_shet
var_subc=nul_sshet
var_subp=nul_sshet
is_s=.F.
is_c=.F.
is_p=.F.
kv=1
setcolor(cw)
do while kv!=0
 @ Y_srm-2,X_srm-27 prompt " Счет......: "+iif(is_s,mDict("str(D1)+' '+D2",var_exp,"D1",use_path+MainDIC,""),'')
 if is_s
  @ Y_srm-1,X_srm-27 prompt " Субсчет  с: "+iif(is_c,mDict("str(D1)+' '+D2",var_subc,"D1",name_dbf,""),'')
  @ Y_srm+0,X_srm-27 prompt " Субсчет по: "+iif(is_p,mDict("str(D1)+' '+D2",var_subp,"D1",name_dbf,""),'')
  @ Y_srm+2,X_srm-27 prompt " Р А С Ч Е Т "
 else
  @ Y_srm-1,X_srm-27 say " Субсчет  с: "
  @ Y_srm+0,X_srm-27 say " Субсчет по: "
  @ Y_srm+2,X_srm-27 say " Р А С Ч Е Т "
 endif
 menu to kv
 do case
 case kv=1
  var_exp=DictFind(0,"D1","strroun(D1)+' '+D2","Выбеpите счет",use_path+MainDIC,"")
  if lastkey()=13
   name_dbf=use_path+strline(var_exp)+".DIC"
   is_s=.T.
  endif
 case kv=2
  var_subc=DictFind(0,"D1","strroun(D1)+' '+D2","Выбеpите субсчет с",name_dbf,"")
  if lastkey()=13
   is_c=.T.
  endif
 case kv=3
  var_subp=DictFind(0,"D1","strroun(D1)+' '+D2","Выбеpите субсчет по",name_dbf,"")
  if lastkey()=13
   is_p=.T.
  endif
 case kv=4
  exit
 endcase
enddo
if kv!=4
  return
endif
if (.not.debpravo(var_exp)).and.(.not.krepravo(var_exp))
 WarnWrite(PMSG8)
 return
endif
MsgWrite(PMSG10)
old_select=select()
old_Nz=recno()
use
* создание файла по месячно
cpfopen("order.txt")
***
cpfout("Счет   : "+mDict("str(D1)+' '+D2",var_exp,"D1",use_path+MainDIC,"")+chr(13)+chr(10))
do case
case is_c.and.is_p
 cpfout("Субсчет с : "+mDict("str(D1)+' '+D2",var_subc,"D1",name_dbf,"")+chr(13)+chr(10))
 cpfout("Субсчет по: "+mDict("str(D1)+' '+D2",var_subp,"D1",name_dbf,"")+chr(13)+chr(10))
case is_c
 cpfout("Субсчет: "+mDict("str(D1)+' '+D2",var_subc,"D1",name_dbf,"")+chr(13)+chr(10))
endcase
cpfout(chr(13)+chr(10))
***
ch_year=str(year(I->T_DATE),4)
select 1
for cur_month=bm to em
 use_dbf=cur_path+"\"+ch_year+"."+strtran(str(cur_month,2)," ","0")+"\"+MainDBF
 use_sub=cur_path+"\"+ch_year+"."+strtran(str(cur_month,2)," ","0")+"\"+strline(var_exp)+".DIC"
 if file(use_dbf)
  MsgWrite("Складываю "+Month[cur_month])
  cpfout(alltrim(Month[cur_month])+" "+str(year(I->T_DATE),4)+" г."+chr(13)+chr(10))
  select 1
  use &use_dbf SHARED
  do case
  case is_c.and.is_p
   sort on KREDIT to debet.dbf for DEBET=var_exp.and.(SUBDEB>=var_subc.and.SUBDEB<=var_subp).and.!FUTURE
  case is_c
   sort on KREDIT to debet.dbf for DEBET=var_exp.and.SUBDEB=var_subc.and.!FUTURE
  othe
   sort on KREDIT to debet.dbf for DEBET=var_exp.and.!FUTURE
  endcase
  set index to
  select 2
  use debet.dbf
  select(old_select)
  do case
  case is_c.and.is_p
   sort on DEBET to kredit.dbf for KREDIT=var_exp.and.(SUBKRE>=var_subc.and.SUBKRE<=var_subp).and.!FUTURE
  case is_c
   sort on DEBET to kredit.dbf for KREDIT=var_exp.and.SUBKRE=var_subc.and.!FUTURE
  othe
   sort on DEBET to kredit.dbf for KREDIT=var_exp.and.!FUTURE
  endcase
  set index to
  select 3
  use kredit.dbf
  select 5
  use &use_sub SHARED
  saldo=0
  locate for D1=var_subc
  if is_c.and.is_p
   do while D1<=var_subp.and.(.not.eof())
    if SALBED!=0
     saldo=saldo+SALBED
    endif
    if SALBEK!=0
     saldo=saldo-SALBEK
    endif
    skip
   enddo
  else
   if SALBED!=0
    saldo=saldo+SALBED
   endif
   if SALBEK!=0
    saldo=saldo-SALBEK
   endif
  endif
  cpfout("Сальдо на начало")
  dSALBED=0
  dSALBEK=0
  do case
  case saldo>=0
   dSALBED=saldo
   cpfout(" ДЕБЕТ : "+strnum(dSALBED))
  case saldo<0
   dSALBEK=-saldo
   cpfout(" КРЕДИТ : "+strnum(dSALBEK))
  endcase
  dRECSUM=0
  dEXPSUM=0
  cpfout(chr(13)+chr(10))
  cpfout("ДЕБЕТ"+chr(13)+chr(10))
  if is_c.and.is_p
   cpfout("Счет  !ССчет!     Сумма    ! Пpимечание"+chr(13)+chr(10))
  else
   cpfout("Счет  !     Сумма    ! Пpимечание"+chr(13)+chr(10))
  endif
  select 2
  go top
  do while .not.eof()
   if is_c.and.is_p
    cpfout(strroun(KREDIT)+"!"+strroun(SUBKRE)+"!"+strnum(SUMMA)+"!"+NOTES+chr(13)+chr(10))
   else
    cpfout(strroun(KREDIT)+"!"+strnum(SUMMA)+"!"+NOTES+chr(13)+chr(10))
   endif
   dRECSUM=dRECSUM+SUMMA
   skip
  enddo
  repl I->FORMAT with dRECSUM
  cpfout("ИТОГО !"+strnum(I->FORMAT)+chr(13)+chr(10))
  cpfout("КРЕДИТ"+chr(13)+chr(10))
  if is_c.and.is_p
   cpfout("Счет  !ССчет!     Сумма    ! Пpимечание"+chr(13)+chr(10))
  else
   cpfout("Счет  !     Сумма    ! Пpимечание"+chr(13)+chr(10))
  endif
  select 3
  go top
  do while .not.eof()
   if is_c.and.is_p
    cpfout(strroun(DEBET)+"!"+strroun(SUBDEB)+"!"+strnum(SUMMA)+"!"+NOTES+chr(13)+chr(10))
   else
    cpfout(strroun(DEBET)+"!"+strnum(SUMMA)+"!"+NOTES+chr(13)+chr(10))
   endif
   dEXPSUM=dEXPSUM+SUMMA
   skip
  enddo
  repl I->FORMAT with dEXPSUM
  cpfout("ИТОГО !"+strnum(I->FORMAT)+chr(13)+chr(10))
  cpfout("Сальдо на конец")
  select 5
  repl I->FORMAT with dSALBED-dSALBEK+dRECSUM-dEXPSUM
  do case
  case I->FORMAT>0
   cpfout(" ДЕБЕТ : ")
   cpfout(strnum(I->FORMAT))
  case I->FORMAT<0
   cpfout(" КРЕДИТ : ")
   repl I->FORMAT with -(I->FORMAT)
   cpfout(strnum(I->FORMAT))
  endcase
  cpfout(chr(13)+chr(10))
  select 1
  use
  select 2
  use
  select 3
  use
  select 5
  use
 endif
next
***
cpfout(chr(13)+chr(10))
cpfout(chr(13)+chr(10)+chr(13)+chr(10)+"Бухгалтеp_____________"+chr(13)+chr(10))
***
dele file debet.dbf
dele file kredit.dbf
***
cpfclose()
***
setcolor(old_color)
OpenDBF()
InitFilter()
go old_Nz
return
*---------------------------------------------------------*
procedure Oborot10
private old_color,old_select,old_Nz
private ptr,currprow,str_file,i,j,indx,i_min,i_max,str_prn,str_ch
old_color=setcolor(cw)
DrawBox(Y_srm-3,X_srm-8,Y_srm+3,X_srm+8,ibox)
setcolor(cl)
MSG=' ПЕPИОД '
@ Y_srm-3,int(X_srm-len(MSG)/2)+1 say MSG
kv=1
setcolor(cw)
@ Y_srm-2,X_srm-7 prompt "первый кваpтал "
@ row()+1,X_srm-7 prompt "второй кваpтал "
@ row()+1,X_srm-7 prompt "третий кваpтал "
@ row()+1,X_srm-7 prompt "четвер.кваpтал "
@ row()+1,X_srm-7 prompt "с начала года  "
menu to kv
if kv=0
  return
endif
do case
case kv=1
 bm=1
 em=3
case kv=2
 bm=4
 em=6
case kv=3
 bm=7
 em=9
case kv=4
 bm=10
 em=12
case kv=5
 bm=1
 em=month(I->T_DATE)
endcase
var_exp=0
name_dbf=""
if .not.FrgPRN1()
 return
endif
MsgWrite(PMSG10)
repl I->FORMATC with int(var_exp)
var_exp=I->FORMATC
old_select=select()
old_Nz=recno()
use
* готовлю объединенную базу
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
   if (int(B->DEBET)=var_exp.OR.int(B->KREDIT)=var_exp).AND.(.not.B->FUTURE)
    select 1
    append blank
    repl A->DEBET  with iif(int(B->DEBET)=var_exp,int(B->DEBET),B->DEBET)
    repl A->KREDIT with iif(int(B->KREDIT)=var_exp,int(B->KREDIT),B->KREDIT)
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
* готовлю объединенный словаpь субсчета
select 4
use_dic=use_path+MainDIC
use &use_dic SHARED
store reccount() to count_rec,count_exp
store 0 to cnt_rec,cnt_exp
select 5
creat stmp.dbf from sub.str
len_num=lenvar(D1)
len_dev=lenvar(D2)
nul_dev=nulvar(D2)
select 4
do while .not.eof()
 if var_exp=int(D->D1)
  name_dbf=use_path+strline(D->D1)+".DIC"
  if file(name_dbf)
   select 3
   use &name_dbf SHARED
   do while .not.eof()
    select 5
    locate for E->D1=C->D1
    if .not.found()
     append blank
     repl E->D1 with C->D1
     repl E->D2 with C->D2
    endif
    saldo=E->SALBED-E->SALBEK+C->SALBED-C->SALBEK
    repl E->SALBED with iif(saldo>0,saldo,0)
    repl E->SALBEK with iif(saldo<0,0-saldo,0)
    select 3
    skip
   enddo
   use
  endif
 endif
 select 4
 skip
enddo
* готовлю объединенный словаpь счетов
select 4
use
use_dic=use_path+MainDIC
copy file &use_dic to ctmp.dbf
select 4
use ctmp.dbf
is_varexp=.T.
do while .not.eof()
 if var_exp=int(D->D1)
  if is_varexp
   repl D->D1 with int(D->D1)
   is_varexp=.F.
  else
   delete
  endif
 endif
 skip
enddo
pack
go top
MsgWrite(PMSG11)
select 1
index on str(DEBET)+str(SUBDEB)+str(KREDIT) to debet.ntx
total on str(DEBET)+str(SUBDEB)+str(KREDIT) fields SUMMA to debet.dbf for DEBET=var_exp
set index to
select 2
use debet.dbf
index on str(DEBET)+str(SUBDEB)+str(KREDIT) to debet.ntx
select(old_select)
index on str(KREDIT)+str(SUBKRE)+str(DEBET) to kredit.ntx
total on str(KREDIT)+str(SUBKRE)+str(DEBET) fields SUMMA to kredit.dbf for KREDIT=var_exp
set index to
select 3
use kredit.dbf
index on str(KREDIT)+str(SUBKRE)+str(DEBET) to kredit.ntx
*** Расчет фактического числа rec и exp
decl iREC[count_rec]
decl iEXP[count_exp]
afill(iREC,0)
afill(iEXP,0)
decl is_REC[count_rec]
decl is_EXP[count_exp]
afill(is_REC,.F.)
afill(is_EXP,.F.)
sub_ob1()
***
cpfopen("order.txt")
***
select(old_select)
***  формирование строки таблицы
if I->IS_DUBL
 str_len=2*(len_num+1+len_dev)+2*(len_sum+1)+(cnt_rec+cnt_exp)*(len_sum+1)
else
 str_len=len_num+1+len_dev+2*(len_sum+1)+(cnt_rec+cnt_exp)*(len_sum+1)
endif
sub_len=len_num+1+len_dev
*** 1-заголовок
str_file=memoread("order7.prn")
i_min=at(chr(1),str_file)
i_max=rat(chr(1),str_file)
if i_min != i_max.and.i_min != (i_max - 1)
  cpfout(strfrm(substr(str_file,i_min+1,i_max-i_min-1)))
endif
***
* 1 строка
cpfout(replicate("-",str_len)+chr(13)+chr(10))
* 2 строка
cpfout(zentstr(sub_len,"Субсчет")+"!")
if cnt_rec<>0
 cpfout(zentstr(cnt_rec*(len_sum+1)-1,"Дебет")+"!")
endif
cpfout(zentstr(len_sum,"Итого")+"!")
if cnt_exp<>0
 cpfout(zentstr(cnt_exp*(len_sum+1)-1,"Кpедит")+"!")
endif
cpfout(zentstr(len_sum,"Итого"))
if I->IS_DUBL
 cpfout("!"+zentstr(sub_len,"Субсчет"))
endif
cpfout(chr(13)+chr(10))
* 3 строка
cpfout(space(sub_len)+"!")
if cnt_rec<>0
 cpfout(replicate("-",cnt_rec*(len_sum+1)-1)+"!")
endif
cpfout(zentstr(len_sum,"дебет")+"!")
if cnt_exp<>0
 cpfout(replicate("-",cnt_exp*(len_sum+1)-1)+"!")
endif
cpfout(zentstr(len_sum,"кредит"))
if I->IS_DUBL
 cpfout("!"+space(sub_len))
endif
cpfout(chr(13)+chr(10))
* 4 строка
cpfout(space(sub_len)+"!")
sub_ob2()
cpfout(space(len_sum))
if I->IS_DUBL
 cpfout("!"+space(sub_len))
endif
cpfout(chr(13)+chr(10))
* 5 строка
cpfout(replicate("-",str_len)+chr(13)+chr(10))
***
iRECSUM=0
iEXPSUM=0
select 5
go top
do while (.not.eof())
  str_ch=strroun(D1)+" "+D2+"!"
  dRECSUM=0
  dEXPSUM=0
  is_sum=.F.
  sub_ob4()
  str_ch=str_ch+strnum(I->FORMAT)
  select 5
  if dRECSUM!=0.or.dEXPSUM!=0.or.E->SALBED!=0.or.E->SALBEK!=0
    cpfout(str_ch)
    if I->IS_DUBL
      cpfout("!"+strroun(D1)+" "+D2)
    endif
    cpfout(chr(13)+chr(10))
  endif
  skip
enddo
* "Итого"
cpfout(replicate("-",str_len)+chr(13)+chr(10))
cpfout(zentstr(sub_len,"ИТОГО")+"!")
sub_ob5()
repl I->FORMAT with iEXPSUM
cpfout(strnum(I->FORMAT))
cpfout(chr(13)+chr(10))
* 3 - END строка
cpfout(replicate("-",str_len)+chr(13)+chr(10))
i_min=at(chr(3),str_file)
i_max=rat(chr(3),str_file)
if i_min != i_max.and.i_min != (i_max - 1)
  cpfout(strfrm(substr(str_file,i_min+1,i_max-i_min-1)))
endif
***
select 1
use
select 2
use
select 3
use
select 4
use
select 5
use
dele file debet.dbf
dele file kredit.dbf
dele file debet.ntx
dele file kredit.ntx
dele file btmp.dbf
dele file stmp.dbf
dele file ctmp.dbf
***
cpfclose()
***
setcolor(old_color)
OpenDBF()
InitFilter()
go old_Nz
return
*---------------------------------------------------------*
procedure FrgPRN1
var_exp=DictFind(0,"D1","strroun(D1)+' '+D2","Выбеpите счет",use_path+MainDIC,"")
if lastkey()!=13
 return .F.
endif
if (.not.debpravo(var_exp)).and.(.not.krepravo(var_exp))
 WarnWrite(PMSG8)
 return .F.
endif
name_dbf=use_path+strline(var_exp)+".DIC"
if (.not.file(name_dbf))
 WarnWrite(PMSG9)
 return .F.
endif
MsgWrite(PMSG10)
return .T.
