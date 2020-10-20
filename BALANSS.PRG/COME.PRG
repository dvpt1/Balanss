*---------------------------------------------------------*
function ComeTable
private old_select,otwet,DoneCome,old_otwet,log_clear
old_select=select()
gloss=gloss+1
setcolor(c)
DrawBox(Y_min,X_min,Y_max,X_max,ibox)
setcolor(cl)
TITL1='  ‹ €     …  € –  ‰ '
@ Y_min,int((X_min+X_max)/2-len(TITL1)/2) say TITL1
MenuTable()
select 2
name_str=ComeSTR
use &name_str SHARED
j=lastrec()
declare dict_fld[j]
declare dict_nam[j]
for i=1 to j
  dict_fld[i]=alltrim(FIELD_NAME)
  dict_nam[i]=alltrim(NAME_FIELD)
  skip
next
dict_fld[7]="padc(substr(translate(SUMMA),1,20),20)"
use
name_dbf=use_path+ComeDIC
if .not.file(name_dbf)
 select 2
 create &name_dbf from &name_str
 use
endif
select 2
if is_setj
 use &name_dbf SHARED
else
 use &name_dbf
endif
setcolor(c)
old_come=0
dbedit(Y_min+1,X_min+1,Y_max-1,X_max-1,dict_fld,"TableFunc","",dict_nam)
rele dict_fld
rele dict_nam
use
gloss=gloss-1
select(old_select)
return " "
*---------------------------------------------------------*
function MenuTable
PMenuOpen()
PMenuIns()
PMenuEnter()
PMenuDel()
PMenuCopy()
PMenuLpt1()
PMenuESC()
PMenuClose()
return " "
*---------------------------------------------------------*
function TableFunc
parameters mode,ptr
private old_color,var,new_cod,log_prn,key_last
key_last=LASTKEY()
do case
case mode <= 3
case lastkey()=27
 return 0
case LASTKEY() = 13.or.isdata(key_last)
 if .not.Pravo0()
  return 1
 endif
 if .not.OnSetj()
  return 1
 endif
 if key_last <> 13
  keyboard chr(key_last)
 endif
 do case
 case ptr=1
  repl DEB with DictFind(DEB,'D1','str(D1)+" "+D2','„……’','expenses.dic','')
 case ptr=2
  repl KRE with DictFind(KRE,'D1','str(D1)+" "+D2','…„’','expenses.dic','')
 case ptr=3
  repl DEBET with DictFind(DEBET,'D1','str(D1)+" "+D2','„……’','expenses.dic','')
 case ptr=4
  var_file=strline(DEBET)+".DIC"
  if file(var_file)
   repl SUBDEB with DictFind(SUBDEB,'D1','str(D1)+" "+D2','‘“‘—…’',var_file,'')
  endif
 case ptr=5
  repl KREDIT with DictFind(KREDIT,'D1','str(D1)+" "+D2','…„’','expenses.dic','')
 case ptr=6
  var_file=strline(KREDIT)+".DIC"
  if file(var_file)
   repl SUBKRE with DictFind(SUBKRE,'D1','str(D1)+" "+D2','‘“‘—…’',var_file,'')
  endif
 case ptr=7
  repl SUMMA with ed(Y_srm-5,X_srm-25,Y_srm+5,X_srm+25,SUMMA)
 endcase
 UnSetj()
 return 2
case lastkey()=22   && INS
 if InsGloss1()
  return 2
 endif
case lastkey()=7    && DEL
 if DelGloss1()
  return 2
 endif
case lastkey()=-2    && F3
 if .not.Pravo0()
  return 1
 endif
 if Fragen('‚€’','’¥ªγιγξ αβΰ®ªγ?',1)
  dDEB = DEB
  dKRE = KRE
  dDEBET  = DEBET
  dSUBDEB = SUBDEB
  dKREDIT = KREDIT
  dSUBKRE = SUBKRE
  dSUMMA  = SUMMA
  append blank
  repl DEB with dDEB
  repl KRE with dKRE
  repl DEBET  with dDEBET
  repl SUBDEB with dSUBDEB
  repl KREDIT with dKREDIT
  repl SUBKRE with dSUBKRE
  repl SUMMA  with dSUMMA
 endif
case lastkey()=-4    && F5
 dblpt1(dict_fld,dict_nam,TITL1)
endcase
if old_come=reccount()
 old_come=reccount()
 return 2
endif
return 1
