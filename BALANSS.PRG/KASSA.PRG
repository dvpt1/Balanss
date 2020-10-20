*---------------------------------------------------------*
procedure KassaLPT1
private old_color,ptr,currprow,str_file,i,j,indx,i_min,i_max,old_Nz,str_prn,str_ch
var_exp=DictFind(0,"D1","strroun(D1)+' '+D2","Выбеpите счет",use_path+MainDIC,"")
if lastkey()!=13
  return
endif
if (.not.debpravo(var_exp)).and.(.not.krepravo(var_exp))
 WarnWrite(PMSG8)
 return
endif
name_dbf=use_path+strline(var_exp)+".DIC"
if file(name_dbf)
 var_sub=DictFind(0,"D1","strroun(D1)+' '+D2","Выбеpите субсчет",name_dbf,"")
 if lastkey()!=13
   return
 endif
else
  WarnWrite("Hе опpеделен словаpь субсчета")
  return
endif
MsgWrite("Расчитываю...")
old_color=setcolor(cw)
old_select=select()
old_Nz=recno()
*** суммы
set filter to (DEBET=var_exp).or.(KREDIT=var_exp)
go top
select 8
use &name_dbf SHARED
sum H->SALBED,H->SALBEK to sald,salk
go top
sumd=0
sumk=0
do while (.not.eof()).and.D1!=var_sub
 select 1
 sum A->SUMMA to dsumd for DEBET=var_exp.and.SUBDEB=H->D1.and.!FUTURE
 sumd=sumd+dsumd
 sum A->SUMMA to ksumk for KREDIT=var_exp.and.SUBKRE=H->D1.and.!FUTURE
 sumk=sumk+ksumk
 select 8
 skip
enddo
repl I->FORMAT with sald - salk + sumd - sumk
kasbeg = I->FORMAT
use
NN=1
***
select 1
set filter to (DEBET=var_exp.and.SUBDEB=var_sub).or.(KREDIT=var_exp.and.SUBKRE=var_sub)
go top
***
cpfopen("kassa.txt")
*** 1-заголовок
str_file=memoread("kassa.prn")
i_min=at(chr(1),str_file)
i_max=rat(chr(1),str_file)
str_1=substr(str_file,i_min+1,i_max-i_min-1)
i_min=at(chr(5),str_file)
i_max=rat(chr(5),str_file)
str_5=substr(str_file,i_min+1,i_max-i_min-1)
i_min=at(chr(2),str_file)
i_max=rat(chr(2),str_file)
str_2=substr(str_file,i_min+1,i_max-i_min-1)
i_min=at(chr(3),str_file)
i_max=rat(chr(3),str_file)
str_3=substr(str_file,i_min+1,i_max-i_min-1)
***
curchet = num_0
expenses = num_sum
receipts = num_sum
sumexp=0	&& за день
sumrec=0
N=1
do while .not.eof()
 cpfout(strfrm(str_1))
 if NN = 1
  cpfout(strfrm(str_5))
 endif
 sumexpl=0	&& по листу
 sumrecl=0
 do while (.not.eof()).and.N <= I->STROK
  do case
  case var_exp = DEBET
   curchet = KREDIT
   expenses = num_sum
   receipts = SUMMA
  case var_exp = KREDIT
   curchet = DEBET
   expenses = SUMMA
   receipts = num_sum
  endcase
  sumexpl=sumexpl+receipts
  sumrecl=sumrecl+expenses
  cpfout(strfrm(str_2))
  skip
  N++
 enddo
 repl I->FORMAT with sumexpl
 sumexpl = I->FORMAT
 repl I->FORMAT with sumrecl
 sumrecl = I->FORMAT
 cpfout(strfrm(str_3))
 if .not.eof() 
  cpfout(chr(12)+chr(13)+chr(10))
 endif
 sumexp=sumexp+sumexpl
 sumrec=sumrec+sumrecl
 NN++
 N=1
enddo
repl I->FORMAT with sumexp
sumexp = I->FORMAT
repl I->FORMAT with sumrec
sumrec = I->FORMAT
repl I->FORMAT with kasbeg + sumexp - sumrec
kasend = I->FORMAT
***
i_min=at(chr(4),str_file)
i_max=rat(chr(4),str_file)
str_4=substr(str_file,i_min+1,i_max-i_min-1)
cpfout(strfrm(str_4))
***
cpfclose()
***
setcolor(old_color)
select(old_select)
InitFilter()
go old_Nz
return
