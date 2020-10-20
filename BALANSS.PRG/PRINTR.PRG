*---------------------------------------------------------*
procedure VedomLPT1
parameters deb_kre
private old_color,old_select,old_Nz,Nz
private ptr,currprow,str_file,i,j,indx,i_min,i_max,str_prn,str_ch
var_exp=0
name_dbf=""
if .not.FrgPRN1()
 return .F.
endif
var_sub=DictFind(0,"D1","strroun(D1)+' '+D2","Выбеpите субсчет",name_dbf,"")
if lastkey()=13
 is_sub=.T.
else
 is_sub=.F.
endif
if .not.Fragen('',PMSGFRG1)
 return .F.
endif
MsgWrite(PMSG10)
old_color=setcolor(cw)
select 3
name_str=SubDIS
create sub.dbf from &name_str
select 2
Nz=recno()
go top
do while .not.eof()
 name_dbf4=use_path+strline(B->D1)+"."+lst_ext
 if file(name_dbf4)
  select 4
  use &name_dbf4 SHARED
  if deb_kre
   if is_sub
    sum DEBET  to iSALBEGd for SUBEXP=var_sub
   else
    sum DEBET  to iSALBEGd
   endif
  else
   if is_sub
    sum KREDIT to iSALBEGk for SUBREC=var_sub
   else
    sum KREDIT to iSALBEGk
   endif
  endif
  select 3
  append blank
  repl C->D1 with B->D1
  repl C->D2 with B->D2
  if deb_kre
   repl C->SALBED with iSALBEGd
  else
   repl C->SALBEK with iSALBEGk
  endif
  select 4
  use
  select 2
 endif
 skip
enddo
select 2
use
select 3
use
select 7
set filter to lst_dbf=G->DEBKRE
go top
select 2
name_str=MainSTR
creat bln.dbf from &name_str
select 7
go top
do while .not.eof()
 select 2
 if is_sub
  if A->SUBDEB=var_sub.or.A->SUBKRE=var_sub
   append blank
   repl B->DEBET with A->DEBET
   repl B->SUBDEB with iif(A->DEBET=var_exp,iif(A->SUBDEB=var_sub,G->EXPENSES,0),0)
   repl B->KREDIT with A->KREDIT
   repl B->SUBKRE with iif(A->KREDIT=var_exp,iif(A->SUBKRE=var_sub,G->RECEIPTS,0),0)
   repl B->SUMMA with A->SUMMA
   repl B->FUTURE with A->FUTURE
  endif
 else
  append blank
  repl B->DEBET with A->DEBET
  repl B->SUBDEB with iif(A->DEBET=var_exp,G->EXPENSES,0)
  repl B->KREDIT with A->KREDIT
  repl B->SUBKRE with iif(A->KREDIT=var_exp,G->RECEIPTS,0)
  repl B->SUMMA with A->SUMMA
  repl B->FUTURE with A->FUTURE
 endif
 select 7
 skip
enddo
select 2
use
select 7
set filter to
go top
select 4
use bln.dbf
index on str(DEBET)+str(SUBDEB)+str(KREDIT) to debet.ntx
total on str(DEBET)+str(SUBDEB)+str(KREDIT) fields SUMMA to debet.dbf for DEBET=var_exp.and.!FUTURE
set index to
select 2
use debet.dbf
index on str(DEBET)+str(SUBDEB)+str(KREDIT) to debet.ntx
select 4
index on str(KREDIT)+str(SUBKRE)+str(DEBET) to kredit.ntx
total on str(KREDIT)+str(SUBKRE)+str(DEBET) fields SUMMA to kredit.dbf for KREDIT=var_exp.and.!FUTURE
set index to
use
select 3
use kredit.dbf
index on str(KREDIT)+str(SUBKRE)+str(DEBET) to kredit.ntx
select 4
use_dic=use_path+MainDIC
use &use_dic SHARED
store reccount() to count_rec,count_exp
store 0 to cnt_rec,cnt_exp
select 5
use sub.dbf
***
Oborot2_1("vedom")
***
dele file bln.dbf
dele file sub.dbf
***
cpfclose()
***
setcolor(old_color)
select 7
set filter to
go top
select 2
name_dbf=use_path+strline(lst_dbf)+".DIV"
if is_setj
 use &name_dbf SHARED
else
 use &name_dbf
endif
go Nz
return
*---------------------------------------------------------*
procedure Oborot1PRN
private dDAT
if .not.FrgPRN3()
  return
endif
var_sub=DictFind(0,"D1","strroun(D1)+' '+D2","Выбеpите субсчет",use_path+strline(lst_dbf)+".DIC","")
if lastkey()=13
 is_sub=.T.
else
 is_sub=.F.
endif
if .not.Fragen('',PMSGFRG1)
 return .F.
endif
select 7
index on str(NUMB)+NAME+MEAS to obor.ntx
select 2
if otwep=1
 go top
endif
is_prn=.T.
do while .not.eof()
 MsgWrite(PMSG10)
 name_dbf=use_path+strline(B->D1)+"."+lst_ext
 select 3
 use &name_dbf SHARED
 index on str(NUMB)+NAME+MEAS to name.ntx
 if is_sub
  set filter to var_sub=SUBEXP
 endif
 go top
 select 7
 if is_sub
  set filter to lst_dbf=G->DEBKRE.and.(B->D1=G->EXPENSES.or.B->D1=G->RECEIPTS).and.(var_sub=A->SUBDEB.or.var_sub=A->SUBKRE)
 else
  set filter to lst_dbf=G->DEBKRE.and.(B->D1=G->EXPENSES.or.B->D1=G->RECEIPTS)
 endif
 go top
 MsgWrite(msgprn)
*** 1-заголовок
 str_file=memoread("oborot1.prn")
 i_min=at(chr(1),str_file)
 i_max=rat(chr(1),str_file)
 str_prn=""
 if i_min != i_max.and.i_min != (i_max - 1)
  str_prn=strfrm(substr(str_file,i_min+1,i_max-i_min-1))
 endif
 if mprints(str_prn)
  iSALBEG=0
  iSUMEXP=0
  iSUMREC=0
  iSALEND=0
  select 3
  is_prn=.T.
  do while is_prn
   dDAT=C->DATBEG
   NUMEXP=0
   SUMEXP=0
   NUMREC=0
   SUMREC=0
   select 7
   if eof().or.str(G->NUMB)+G->NAME+G->MEAS==str(C->NUMB)+C->NAME+C->MEAS
    select 3
    dNUMB=C->NUMB
    dNAME=C->NAME
    dMEAS=C->MEAS
    SALNUM=C->QUAN
    SALDEB=C->DEBET
    SALKRE=C->KREDIT
    skip
    select 7
   else
    select 3
    if (.not.eof()).and.str(G->NUMB)+G->NAME+G->MEAS > str(C->NUMB)+C->NAME+C->MEAS
     dNUMB=C->NUMB
     dNAME=C->NAME
     dMEAS=C->MEAS
     SALNUM=C->QUAN
     SALDEB=C->DEBET
     SALKRE=C->KREDIT
     skip
    else
     dNUMB=G->NUMB
     dNAME=G->NAME
     dMEAS=G->MEAS
     SALNUM=0
     SALDEB=0
     SALKRE=0
    endif
    select 7
   endif
   do while  (.not.eof()).and.str(G->NUMB)+G->NAME+G->MEAS==str(dNUMB)+dNAME+dMEAS
    if B->D1=EXPENSES
     NUMEXP=NUMEXP+QUAN
     SUMEXP=SUMEXP+A->SUMMA
    else
     NUMREC=NUMREC+QUAN
     SUMREC=SUMREC+A->SUMMA
    endif
    skip
   enddo
   str_prn=varstr(dNUMB)+"!"+dNAME+"!"+dMEAS+"!"
   if SALNUM+NUMEXP-NUMREC != 0
    repl I->FORMATZ with (SALDEB-SALKRE+SUMEXP-SUMREC)/(SALNUM+NUMEXP-NUMREC)
   else
    repl I->FORMATZ with 0
   endif
   str_prn=str_prn+strnum(I->FORMATZ)+"!"
   repl I->QORMAT with SALNUM                   && сальдо на начало
   str_prn=str_prn+strnum(I->QORMAT)+"!"
   repl I->FORMAT with SALDEB-SALKRE
   str_prn=str_prn+strnum(I->FORMAT)+"!"
   iSALBEG=iSALBEG+I->FORMAT
   repl I->QORMAT with NUMEXP                   && приход
   str_prn=str_prn+strnum(I->QORMAT)+"!"
   repl I->FORMAT with SUMEXP
   str_prn=str_prn+strnum(I->FORMAT)+"!"
   iSUMEXP=iSUMEXP+I->FORMAT
   repl I->QORMAT with NUMREC                   && расход
   str_prn=str_prn+strnum(I->QORMAT)+"!"
   repl I->FORMAT with SUMREC
   str_prn=str_prn+strnum(I->FORMAT)+"!"
   iSUMREC=iSUMREC+I->FORMAT
   repl I->QORMAT with SALNUM+NUMEXP-NUMREC     && сальдо на конец
   str_prn=str_prn+strnum(I->QORMAT)+"!"
   repl I->FORMAT with SALDEB-SALKRE+SUMEXP-SUMREC
   str_prn=str_prn+strnum(I->FORMAT)
   iSALEND=iSALEND+I->FORMAT
   str_prn=str_prn+chr(13)+chr(10)
   is_prn=mprints(str_prn)
   select 7
   eof4=eof()
   select 3
   if eof().and.eof4
    exit
   endif
  enddo
 endif
*** подпись
 if is_prn
  repl I->FORMAT with iSALBEG
  iSALBEG=I->FORMAT
  repl I->FORMAT with iSUMEXP
  iSUMEXP=I->FORMAT
  repl I->FORMAT with iSUMREC
  iSUMREC=I->FORMAT
  repl I->FORMAT with iSALEND
  iSALEND=I->FORMAT
  i_min=at(chr(3),str_file)
  i_max=rat(chr(3),str_file)
  str_prn=""
  if i_min != i_max.and.i_min != (i_max - 1)
   str_prn=strfrm(substr(str_file,i_min+1,i_max-i_min-1))
  endif
  mprints(str_prn)
 endif
 select 2
 if otwep=1
  skip
 else
  exit
 endif
enddo
select 3
use
dele file name.ntx
select 7
name_ntx=use_path+OborotNTX
set index to &name_ntx
set filter to
go top
dele file obor.ntx
return
*---------------------------------------------------------*
procedure Oborot2PRN
if .not.FrgPRN3()
  return
endif
var_sub=DictFind(0,"D1","strroun(D1)+' '+D2","Выбеpите субсчет",use_path+strline(lst_dbf)+".DIC","")
if lastkey()=13
 is_sub=.T.
else
 is_sub=.F.
endif
if .not.Fragen('',PMSGFRG1)
 return .F.
endif
select 7
index on str(NUMB)+NAME+MEAS to obor.ntx
select 2
if otwep=1
 go top
endif
is_prn=.T.
do while .not.eof()
 MsgWrite(PMSG10)
 name_dbf=use_path+strline(B->D1)+"."+lst_ext
 select 3
 use &name_dbf SHARED
 index on str(NUMB)+NAME+MEAS to name.ntx
 if is_sub
  set filter to var_sub=SUBEXP
 endif
 go top
 select 7
 if is_sub
  set filter to lst_dbf=G->DEBKRE.and.(B->D1=G->EXPENSES.or.B->D1=G->RECEIPTS).and.(var_sub=A->SUBDEB.or.var_sub=A->SUBKRE)
 else
  set filter to lst_dbf=G->DEBKRE.and.(B->D1=G->EXPENSES.or.B->D1=G->RECEIPTS)
 endif
 go top
 MsgWrite(msgprn)
*** 1-заголовок
 str_file=memoread("oborot2.prn")
 i_min=at(chr(1),str_file)
 i_max=rat(chr(1),str_file)
 str_prn=""
 if i_min != i_max.and.i_min != (i_max - 1)
  str_prn=strfrm(substr(str_file,i_min+1,i_max-i_min-1))
 endif
 if mprints(str_prn)
  iSALBEG=0
  iSUMEXP=0
  iSUMREC=0
  iSALEND=0
  select 3
  is_prn=.T.
  do while is_prn
   NUMEXP=0
   SUMEXP=0
   NUMREC=0
   SUMREC=0
   select 7
   if eof().or.str(G->NUMB)+G->NAME+G->MEAS==str(C->NUMB)+C->NAME+C->MEAS
    select 3
    dNUMB=C->NUMB
    dNAME=C->NAME
    dMEAS=C->MEAS
    SALNUM=C->QUAN
    SALDEB=C->DEBET
    SALKRE=C->KREDIT
    dDATEKS=C->DATEKS
    dDATPER=C->DATPER
    dDATEND=C->DATEND
    skip
    select 7
   else
    select 3
    if (.not.eof()).and.str(G->NUMB)+G->NAME+G->MEAS > str(C->NUMB)+C->NAME+C->MEAS
     dNUMB=C->NUMB
     dNAME=C->NAME
     dMEAS=C->MEAS
     SALNUM=C->QUAN
     SALDEB=C->DEBET
     SALKRE=C->KREDIT
     dDATEKS=C->DATEKS
     dDATPER=C->DATPER
     dDATEND=C->DATEND
     skip
    else
     dNUMB=G->NUMB
     dNAME=G->NAME
     dMEAS=G->MEAS
     SALNUM=0
     SALDEB=0
     SALKRE=0
     dDATEKS=nulvar(date())
     dDATPER=nulvar(date())
     dDATEND=nulvar(date())
    endif
    select 7
   endif
   do while  (.not.eof()).and.str(G->NUMB)+G->NAME+G->MEAS==str(dNUMB)+dNAME+dMEAS
    if B->D1=EXPENSES
     NUMEXP=NUMEXP+QUAN
     SUMEXP=SUMEXP+A->SUMMA
    else
     NUMREC=NUMREC+QUAN
     SUMREC=SUMREC+A->SUMMA
    endif
    skip
   enddo
   str_prn=str(dNUMB)+"!"+dNAME+"!"+dMEAS+"!"
   if SALNUM+NUMEXP-NUMREC != 0
    repl I->FORMATZ with (SALDEB-SALKRE+SUMEXP-SUMREC)/(SALNUM+NUMEXP-NUMREC)
   else
    repl I->FORMATZ with 0
   endif
   str_prn=str_prn+strnum(I->FORMATZ)+"!"
   repl I->QORMAT with SALNUM+NUMEXP-NUMREC     && сальдо на конец
   str_prn=str_prn+strnum(I->QORMAT)+"!"
   repl I->FORMAT with SALDEB-SALKRE+SUMEXP-SUMREC
   str_prn=str_prn+strnum(I->FORMAT)
   iSALEND=iSALEND+I->FORMAT
   str_prn=str_prn+varstr(dDATEKS)+"!"+varstr(dDATPER)+"!"+varstr(dDATEND)
   str_prn=str_prn+chr(13)+chr(10)
   is_prn=mprints(str_prn)
   select 7
   eof4=eof()
   select 3
   if eof().and.eof4
    exit
   endif
  enddo
 endif
*** подпись
 if is_prn
  repl I->FORMAT with iSALEND
  iSALEND=I->FORMAT
  i_min=at(chr(3),str_file)
  i_max=rat(chr(3),str_file)
  str_prn=""
  if i_min != i_max.and.i_min != (i_max - 1)
   str_prn=strfrm(substr(str_file,i_min+1,i_max-i_min-1))
  endif
  mprints(str_prn)
 endif
 select 2
 if otwep=1
  skip
 else
  exit
 endif
enddo
select 3
use
dele file name.ntx
select 7
name_ntx=use_path+OborotNTX
set index to &name_ntx
set filter to
go top
dele file obor.ntx
return
*---------------------------------------------------------*
procedure Oborot3PRN
if .not.FrgPRN3()
  return
endif
select 7
index on str(NUMB)+NAME+MEAS to obor.ntx
select 2
if otwep=1
 go top
endif
is_prn=.T.
do while .not.eof()
 MsgWrite(PMSG10)
 name_dbf=use_path+strline(B->D1)+"."+lst_ext
 select 3
 use &name_dbf SHARED
 index on str(NUMB)+NAME+MEAS to name.ntx
 go top
 select 7
 set filter to lst_dbf=G->DEBKRE.and.(B->D1=G->EXPENSES.or.B->D1=G->RECEIPTS).and.(I->DEBPRN=A->DEBET.or.I->KREPRN=A->KREDIT)
 go top
 MsgWrite(msgprn)
*** 1-заголовок
 str_file=memoread("oborot3.prn")
 i_min=at(chr(1),str_file)
 i_max=rat(chr(1),str_file)
 str_prn=""
 if i_min != i_max.and.i_min != (i_max - 1)
  str_prn=strfrm(substr(str_file,i_min+1,i_max-i_min-1))
 endif
 if mprints(str_prn)
  iSALBEG=0
  iSUMEXP=0
  iSUMREC=0
  iSALEND=0
  select 3
  is_prn=.T.
  do while is_prn
   SUMEXP=0
   SUMREC=0
   select 7
   if eof().or.str(G->NUMB)+G->NAME+G->MEAS==str(C->NUMB)+C->NAME+C->MEAS
    select 3
    dNUMB=C->NUMB
    dNAME=C->NAME
    dMEAS=C->MEAS
    dASHIFR=C->ASHIFR
    dANORMA=C->ANORMA
    SALDEB=C->DEBET
    SALKRE=C->KREDIT
    skip
    select 7
   else
    select 3
    if (.not.eof()).and.str(G->NUMB)+G->NAME+G->MEAS > str(C->NUMB)+C->NAME+C->MEAS
     dNUMB=C->NUMB
     dNAME=C->NAME
     dMEAS=C->MEAS
     dASHIFR=C->ASHIFR
     dANORMA=C->ANORMA
     SALDEB=C->DEBET
     SALKRE=C->KREDIT
     skip
    else
     dNUMB=G->NUMB
     dNAME=G->NAME
     dMEAS=G->MEAS
     dASHIFR=G->ASHIFR
     dANORMA=G->ANORMA
     SALDEB=0
     SALKRE=0
    endif
    select 7
   endif
   do while (.not.eof()).and.str(G->NUMB)+G->NAME+G->MEAS==str(dNUMB)+dNAME+dMEAS
    if A->DEBET=lst_dbf
     if B->D1=EXPENSES
      SUMEXP=SUMEXP+A->SUMMA
     endif
    else
     if B->D1=RECEIPTS
      SUMREC=SUMREC+A->SUMMA
     endif
    endif
    skip
   enddo
   str_prn=str(dNUMB)+"!"+dNAME+"!"
   repl I->FORMAT with SALDEB+SUMEXP           && балансовая стоимость
   str_prn=str_prn+strnum(I->FORMAT)+"!"
   iSALBEG=iSALBEG+I->FORMAT
   str_prn=str_prn+str(dASHIFR)+"!"+str(dANORMA)+"!" && шифр и норма
   repl I->FORMAT with SUMREC                        && расход
   str_prn=str_prn+strnum(I->FORMAT)+"!"
   iSUMREC=iSUMREC+I->FORMAT
   repl I->FORMAT with SALKRE+SUMREC           && сальдо на конец
   str_prn=str_prn+strnum(I->FORMAT)
   iSALEND=iSALEND+I->FORMAT
   str_prn=str_prn+chr(13)+chr(10)
   is_prn=mprints(str_prn)
   select 7
   eof4=eof()
   select 3
   if eof().and.eof4
    exit
   endif
  enddo
 endif
*** подпись
 if is_prn
  repl I->FORMAT with iSALBEG
  iSALBEG=I->FORMAT
  repl I->FORMAT with iSUMREC
  iSUMREC=I->FORMAT
  repl I->FORMAT with iSALEND
  iSALEND=I->FORMAT
  i_min=at(chr(3),str_file)
  i_max=rat(chr(3),str_file)
  str_prn=""
  if i_min != i_max.and.i_min != (i_max - 1)
   str_prn=strfrm(substr(str_file,i_min+1,i_max-i_min-1))
  endif
  mprints(str_prn)
 endif
 select 2
 if otwep=1
  skip
 else
  exit
 endif
enddo
select 3
use
dele file name.ntx
select 7
name_ntx=use_path+OborotNTX
set index to &name_ntx
set filter to
go top
dele file obor.ntx
return
*---------------------------------------------------------*
procedure Oborot4PRN
if .not.FrgPRN3()
  return
endif
select 7
index on str(NUMB)+NAME+MEAS to obor.ntx
select 2
if otwep=1
 go top
endif
is_prn=.T.
do while .not.eof()
 MsgWrite(PMSG10)
 name_dbf=use_path+strline(B->D1)+"."+lst_ext
 select 3
 use &name_dbf SHARED
 index on str(NUMB)+NAME+MEAS to name.ntx
 go top
 select 7
 set filter to lst_dbf=G->DEBKRE.and.(B->D1=G->EXPENSES.or.B->D1=G->RECEIPTS).and.(I->DEBPRN=A->DEBET.or.I->DEBPRN=A->KREDIT)
 go top
 MsgWrite(msgprn)
*** 1-заголовок
 str_file=memoread("oborot4.prn")
 i_min=at(chr(1),str_file)
 i_max=rat(chr(1),str_file)
 str_prn=""
 if i_min != i_max.and.i_min != (i_max - 1)
  str_prn=strfrm(substr(str_file,i_min+1,i_max-i_min-1))
 endif
 if mprints(str_prn)
  iSALBEG=0
  iSUMEXP=0
  iSUMREC=0
  iSALEND=0
  select 3
  is_prn=.T.
  do while is_prn
   SUMEXP=0
   SUMREC=0
   select 7
   if eof().or.str(G->NUMB)+G->NAME+G->MEAS==str(C->NUMB)+C->NAME+C->MEAS
    select 3
    dNUMB=C->NUMB
    dNAME=C->NAME
    dMEAS=C->MEAS
    dDATEKS=C->DATEKS
    SALDEB=C->DEBET
    SALKRE=C->KREDIT
    skip
    select 7
   else
    select 3
    if (.not.eof()).and.str(G->NUMB)+G->NAME+G->MEAS > str(C->NUMB)+C->NAME+C->MEAS
     dNUMB=C->NUMB
     dNAME=C->NAME
     dMEAS=C->MEAS
     dDATEKS=C->DATEKS
     SALDEB=C->DEBET
     SALKRE=C->KREDIT
     skip
    else
     dNUMB=G->NUMB
     dNAME=G->NAME
     dMEAS=G->MEAS
     dDATEKS=G->DATEKS
     SALDEB=0
     SALKRE=0
    endif
    select 7
   endif
   do while (.not.eof()).and.str(G->NUMB)+G->NAME+G->MEAS==str(dNUMB)+dNAME+dMEAS
    if B->D1=EXPENSES
     SUMEXP=SUMEXP+A->SUMMA
    else
     SUMREC=SUMREC+A->SUMMA
    endif
    skip
   enddo
   str_prn=str(dNUMB)+"!"+dNAME+"!"+varstr(dDATEKS)+"!"
   repl I->FORMAT with SALDEB                        && сальдо на начало
   str_prn=str_prn+strnum(I->FORMAT)+"!"
   iSALBEG=iSALBEG+I->FORMAT
   repl I->FORMAT with SUMEXP                        && приход
   str_prn=str_prn+strnum(I->FORMAT)+"!"
   iSUMEXP=iSUMEXP+I->FORMAT
   repl I->FORMAT with SUMREC                        && расход
   str_prn=str_prn+strnum(I->FORMAT)+"!"
   iSUMREC=iSUMREC+I->FORMAT
   repl I->FORMAT with SALDEB+SUMEXP-SUMREC   && сальдо на конец
   str_prn=str_prn+strnum(I->FORMAT)
   iSALEND=iSALEND+I->FORMAT
   str_prn=str_prn+chr(13)+chr(10)
   is_prn=mprints(str_prn)
   select 7
   eof4=eof()
   select 3
   if eof().and.eof4
    exit
   endif
  enddo
 endif
*** подпись
 if is_prn
  repl I->FORMAT with iSALBEG
  iSALBEG=I->FORMAT
  repl I->FORMAT with iSUMEXP
  iSUMEXP=I->FORMAT
  repl I->FORMAT with iSUMREC
  iSUMREC=I->FORMAT
  repl I->FORMAT with iSALEND
  iSALEND=I->FORMAT
  i_min=at(chr(3),str_file)
  i_max=rat(chr(3),str_file)
  str_prn=""
  if i_min != i_max.and.i_min != (i_max - 1)
   str_prn=strfrm(substr(str_file,i_min+1,i_max-i_min-1))
  endif
  mprints(str_prn)
 endif
 select 2
 if otwep=1
  skip
 else
  exit
 endif
enddo
select 3
use
dele file name.ntx
select 7
name_ntx=use_path+OborotNTX
set index to &name_ntx
set filter to
go top
dele file obor.ntx
return
*---------------------------------------------------------*
procedure Oborot5PRN
if .not.FrgPRN3()
  return
endif
select 7
index on str(NUMB)+NAME+MEAS to obor.ntx
select 2
if otwep=1
 go top
endif
is_prn=.T.
do while .not.eof()
 MsgWrite(PMSG10)
 name_dbf=use_path+strline(B->D1)+"."+lst_ext
 select 3
 use &name_dbf SHARED
 index on str(NUMB)+NAME+MEAS to name.ntx
 go top
 select 7
 set filter to lst_dbf=G->DEBKRE.and.(B->D1=G->EXPENSES.or.B->D1=G->RECEIPTS).and.(I->KREPRN=A->DEBET.or.I->KREPRN=A->KREDIT)
 go top
 MsgWrite(msgprn)
*** 1-заголовок
 str_file=memoread("oborot5.prn")
 i_min=at(chr(1),str_file)
 i_max=rat(chr(1),str_file)
 str_prn=""
 if i_min != i_max.and.i_min != (i_max - 1)
  str_prn=strfrm(substr(str_file,i_min+1,i_max-i_min-1))
 endif
 if mprints(str_prn)
  iSALBEG=0
  iSUMEXP=0
  iSUMREC=0
  iSALEND=0
  select 3
  is_prn=.T.
  do while is_prn
   SUMEXP=0
   SUMREC=0
   select 7
   if eof().or.str(G->NUMB)+G->NAME+G->MEAS==str(C->NUMB)+C->NAME+C->MEAS
    select 3
    dNUMB=C->NUMB
    dNAME=C->NAME
    dMEAS=C->MEAS
    dDATEKS=C->DATEKS
    SALDEB=C->DEBET
    SALKRE=C->KREDIT
    skip
    select 7
   else
    select 3
    if (.not.eof()).and.str(G->NUMB)+G->NAME+G->MEAS > str(C->NUMB)+C->NAME+C->MEAS
     dNUMB=C->NUMB
     dNAME=C->NAME
     dMEAS=C->MEAS
     dDATEKS=C->DATEKS
     SALDEB=C->DEBET
     SALKRE=C->KREDIT
     skip
    else
     dNUMB=G->NUMB
     dNAME=G->NAME
     dMEAS=G->MEAS
     dDATEKS=G->DATEKS
     SALDEB=0
     SALKRE=0
    endif
    select 7
   endif
   do while (.not.eof()).and.str(G->NUMB)+G->NAME+G->MEAS==str(dNUMB)+dNAME+dMEAS
     if B->D1=RECEIPTS
      SUMEXP=SUMEXP+A->SUMMA
     else
      SUMREC=SUMREC+A->SUMMA
     endif
    skip
   enddo
   str_prn=str(dNUMB)+"!"+dNAME+"!"+varstr(dDATEKS)+"!"
   repl I->FORMAT with SALKRE                        && сальдо на начало
   str_prn=str_prn+strnum(I->FORMAT)+"!"
   iSALBEG=iSALBEG+I->FORMAT
   repl I->FORMAT with SUMEXP                        && приход
   str_prn=str_prn+strnum(I->FORMAT)+"!"
   iSUMEXP=iSUMEXP+I->FORMAT
   repl I->FORMAT with SUMREC                        && расход
   str_prn=str_prn+strnum(I->FORMAT)+"!"
   iSUMREC=iSUMREC+I->FORMAT
   repl I->FORMAT with SALKRE+SUMEXP-SUMREC          && сальдо на конец
   str_prn=str_prn+strnum(I->FORMAT)
   iSALEND=iSALEND+I->FORMAT
   str_prn=str_prn+chr(13)+chr(10)
   is_prn=mprints(str_prn)
   select 7
   eof4=eof()
   select 3
   if eof().and.eof4
    exit
   endif
  enddo
 endif
*** подпись
 if is_prn
  repl I->FORMAT with iSALBEG
  iSALBEG=I->FORMAT
  repl I->FORMAT with iSUMEXP
  iSUMEXP=I->FORMAT
  repl I->FORMAT with iSUMREC
  iSUMREC=I->FORMAT
  repl I->FORMAT with iSALEND
  iSALEND=I->FORMAT
  i_min=at(chr(3),str_file)
  i_max=rat(chr(3),str_file)
  str_prn=""
  if i_min != i_max.and.i_min != (i_max - 1)
   str_prn=strfrm(substr(str_file,i_min+1,i_max-i_min-1))
  endif
  mprints(str_prn)
 endif
 select 2
 if otwep=1
  skip
 else
  exit
 endif
enddo
select 3
use
dele file name.ntx
select 7
name_ntx=use_path+OborotNTX
set index to &name_ntx
set filter to
go top
dele file obor.ntx
return
*---------------------------------------------------------*
procedure Oborot6PRN
if .not.FrgPRN3()
  return
endif
select 7
index on str(NUMB)+NAME+MEAS to obor.ntx
select 2
if otwep=1
 go top
endif
is_prn=.T.
do while .not.eof()
 MsgWrite(PMSG10)
 name_dbf=use_path+strline(B->D1)+"."+lst_ext
 select 3
 use &name_dbf SHARED
 index on str(NUMB)+NAME+MEAS to name.ntx
 go top
 select 7
 set filter to lst_dbf=G->DEBKRE.and.(B->D1=G->EXPENSES.or.B->D1=G->RECEIPTS).and.(I->DEBPRN=A->DEBET.or.I->KREPRN=A->KREDIT)
 go top
 MsgWrite(msgprn)
*** 1-заголовок
 str_file=memoread("oborot6.prn")
 i_min=at(chr(1),str_file)
 i_max=rat(chr(1),str_file)
 str_prn=""
 if i_min != i_max.and.i_min != (i_max - 1)
  str_prn=strfrm(substr(str_file,i_min+1,i_max-i_min-1))
 endif
 if mprints(str_prn)
  iSALBEG=0
  iSUMEXP=0
  iSUMREC=0
  iSALEND=0
  select 3
  is_prn=.T.
  do while is_prn
   SUMEXP=0
   SUMREC=0
   select 7
   if eof().or.str(G->NUMB)+G->NAME+G->MEAS==str(C->NUMB)+C->NAME+C->MEAS
    select 3
    dNUMB=C->NUMB
    dNAME=C->NAME
    dMEAS=C->MEAS
    dASHIFR=C->ASHIFR
    dANORMA=C->ANORMA
    dDATEKS=C->DATEKS
    dKOEF=C->KOEF
    SALDEB=C->DEBET
    SALKRE=C->KREDIT
    skip
    select 7
   else
    select 3
    if (.not.eof()).and.str(G->NUMB)+G->NAME+G->MEAS > str(C->NUMB)+C->NAME+C->MEAS
     dNUMB=C->NUMB
     dNAME=C->NAME
     dMEAS=C->MEAS
     dASHIFR=C->ASHIFR
     dANORMA=C->ANORMA
     dDATEKS=C->DATEKS
     dKOEF=C->KOEF
     SALDEB=C->DEBET
     SALKRE=C->KREDIT
     skip
    else
     dNUMB=G->NUMB
     dNAME=G->NAME
     dMEAS=G->MEAS
     dASHIFR=G->ASHIFR
     dANORMA=G->ANORMA
     dDATEKS=G->DATEKS
     dKOEF=G->KOEF
     SALDEB=0
     SALKRE=0
    endif
    select 7
   endif
   do while (.not.eof()).and.str(G->NUMB)+G->NAME+G->MEAS==str(dNUMB)+dNAME+dMEAS
    if I->DEBPRN=A->DEBET
     if B->D1=EXPENSES
      SUMEXP=SUMEXP+A->SUMMA
     else
      SUMEXP=SUMEXP-A->SUMMA
     endif
    else
     if B->D1=RECEIPTS
      SUMREC=SUMREC+A->SUMMA
     else
      SUMREC=SUMREC-A->SUMMA
     endif
    endif
    skip
   enddo
   str_prn=str(dNUMB)+"!"+dNAME+"!"
   str_prn=str_prn+str(dASHIFR)+"!"+str(dANORMA)+"!" && шифр и норма
   str_prn=str_prn+varstr(dDATEKS)+"!"               && дата ввода
   repl I->FORMAT with SALDEB                  && балансовая стоимость
   str_prn=str_prn+strnum(I->FORMAT)+"!"
   iSALBEG=iSALBEG+I->FORMAT
   str_prn=str_prn+str(dKOEF)+"!"              && коэф.переоценки
   repl I->FORMAT with SALDEB+SUMEXP           && приход
   str_prn=str_prn+strnum(I->FORMAT)+"!"
   iSUMEXP=iSUMEXP+I->FORMAT
   repl I->FORMAT with SALKRE+SUMREC           && расход
   str_prn=str_prn+strnum(I->FORMAT)+"!"
   iSUMREC=iSUMREC+I->FORMAT
   repl I->FORMAT with SALDEB+SUMEXP-SALKRE-SUMREC  && сальдо
   str_prn=str_prn+strnum(I->FORMAT)
   iSALEND=iSALEND+I->FORMAT
   str_prn=str_prn+chr(13)+chr(10)
   is_prn=mprints(str_prn)
   select 7
   eof4=eof()
   select 3
   if eof().and.eof4
    exit
   endif
  enddo
 endif
*** подпись
 if is_prn
  repl I->FORMAT with iSALBEG
  iSALBEG=I->FORMAT
  repl I->FORMAT with iSUMEXP
  iSUMEXP=I->FORMAT
  repl I->FORMAT with iSUMREC
  iSUMREC=I->FORMAT
  repl I->FORMAT with iSALEND
  iSALEND=I->FORMAT
  i_min=at(chr(3),str_file)
  i_max=rat(chr(3),str_file)
  str_prn=""
  if i_min != i_max.and.i_min != (i_max - 1)
   str_prn=strfrm(substr(str_file,i_min+1,i_max-i_min-1))
  endif
  mprints(str_prn)
 endif
 select 2
 if otwep=1
  skip
 else
  exit
 endif
enddo
select 3
use
dele file name.ntx
select 7
name_ntx=use_path+OborotNTX
set index to &name_ntx
set filter to
go top
dele file obor.ntx
return
*---------------------------------------------------------*
function FrgPRN3
save scre
setcolor(cmenu)
BoxTenj(Y_srm-2,X_srm-15,Y_srm+2,X_srm+15,ibox)
setcolor(cl)
MSG=' ПЕЧАТЬ ДЛЯ '
@ Y_srm-2,X_srm-len(MSG)/2 say MSG
setcolor(cmenu)
@ Y_srm,X_srm-12 prompt ' В С Е Х '
@ row(),col()    prompt ' ТЕКУЩЕГО '
@ row(),col()    prompt ' ОТМЕНА '
menu to otwep
rest scre
return iif(otwep=0.or.otwep=3,.F.,.T.)
