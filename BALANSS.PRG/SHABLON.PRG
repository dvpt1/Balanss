*---------------------------------------------------------*
procedure sablon
if .not.eof()
 aOptions={" ÇÆ··‚†≠Æ¢®‚Ï "," ëÆÂp†≠®‚Ï "," é‚¨•≠† "}
else
 aOptions={" ÇÆ··‚†≠Æ¢®‚Ï "," é‚¨•≠† "}
endif
nchoise=frags(" ò Ä Å ã é H ","è ê é Ç é Ñ ä ì ?",aOptions)
if .not.eof()
 do case
 case nchoise=1
  SablLoad()
 case nchoise=2
  SablSave()
 endcase
else
 do case
 case nchoise=1
  SablLoad()
 endcase
endif
return
*---------------------------------------------------------*
procedure SablSave
private old_color,old_select,old_Nz
save scre to OnePage
old_color=setcolor(ct)
old_select=select()
old_Nz=recno()
sab_path=cur_path+"\SHABLON"
if OpenSabl(.F.)
 MsgWrite("ëéïêÄçüû èêéÇéÑäì Ç ëèàëäÖ òÄÅãéçéÇ...")
 go bottom
 n=NUMBER
 append blank
 repl B->NUMBER with n+1
 repl B->DEBET  with A->DEBET
 repl B->SUBDEB with A->SUBDEB
 repl B->KREDIT with A->KREDIT
 repl B->SUBKRE with A->SUBKRE
 repl B->SUMMA  with A->SUMMA
 repl B->NOTES  with A->NOTES
 select 4
 name_dbf=use_path+MainDIC
 use &name_dbf SHARED
 NewSablon("alltrim(D->D_DOC)","D1=B->DEBET")
 NewSablon("alltrim(D->K_DOC)","D1=B->KREDIT")
 select 4
 use
 select 2
 use
endif
select(old_select)
go old_Nz
setcolor(old_color)
rest scre from OnePage
return
*---------------------------------------------------------*
function OpenSabl
parameters is_filter
private ch_filter
use_sab=sab_path+"\"+SablonDBF
if .not.file(use_sab)
 mkdir(sab_path)
endif
select 2
if .not.file(use_sab)
 creat &use_sab from &MainSTR
 use
endif
select 2
if is_setj
 use &use_sab SHARED
else
 use &use_sab
endif
if is_filter
ch_filter=''
do case
case I->L_DEBET.and.(.not.I->L_SUBDEB)
  ch_filter=ch_filter+"DEBET=I->F_DEBET"
case I->L_DEBET.and.I->L_SUBDEB
  ch_filter=ch_filter+"(DEBET=I->F_DEBET.and.SUBDEB=I->F_SUBDEB)"
endcase
do case
case I->L_KREDIT.and.(.not.I->L_SUBKRE)
  if .not.empty(ch_filter)
   ch_filter=ch_filter+".and."
  endif
  ch_filter=ch_filter+"KREDIT=I->F_KREDIT"
case I->L_KREDIT.and.I->L_SUBKRE
  if .not.empty(ch_filter)
   ch_filter=ch_filter+".and."
  endif
  ch_filter=ch_filter+"(KREDIT=I->F_KREDIT.and.SUBKRE=I->F_SUBKRE)"
endcase
if .not.empty(ch_filter)
  set filter to &ch_filter
  go top
endif
endif
return .T.
*---------------------------------------------------------*
procedure NewSablon
parameters bal_doc,bal_loc
select 4
locate for &bal_loc
if found()
 if .not.empty(&bal_doc)
  name_dbf=use_path+&bal_doc+".DBF"
  if file(name_dbf)
   select 3
   use &name_dbf SHARED
   locate for C->NUMBER=A->NUMBER
   if found()
    select 5
    use_sab=sab_path+"\"+&bal_doc+".DBF"
    if .not.file(use_sab)
     name_str=SablonSTR
     creat &use_sab from &name_str
    else
     use &use_sab SHARED
    endif
    append blank
    repl NUMBER with B->NUMBER
    copysabl(use_path+&bal_doc+".STR")
    select 5
    use
   endif
   select 3
   use
  endif
 endif
endif
return
*---------------------------------------------------------*
procedure SablLoad
private old_color,old_select,old_Nz,rtr
save scre to OnePage
old_select=select()
old_color=setcolor()
old_Nz=recno()
sab_path=cur_path+"\SHABLON"
if OpenSabl(.T.)
 if .not.eof()
  setcolor(cdict)
  DrawBox(Y_min,X_min,Y_max,X_max,ibox)
  setcolor(cl)
  TITL1=" ë è à ë é ä  ò Ä Å ã é H é Ç  è ê é Ç é Ñ é ä "
  @ Y_min,int((X_min+X_max)/2-len(TITL1)/2) say TITL1
  MenuSabl()
  setcolor(cdict)
  declare dict_scr[1]
  declare dict_nam[1]
  dict_scr[1]="str(DEBET)+'/'+str(SUBDEB)+'  '+str(KREDIT)+'/'+str(SUBKRE)+' '+NOTES"
  dict_nam[1]=" Ñ•°•‚/ë„°·Á•‚ äp•§®‚/ë„°·Á•‚        èp®¨•Á†≠®•"
  rtr=.T.
  dbedit(Y_min+1,X_min+1,Y_max-1,X_max-1,dict_scr,"SablFunc","",dict_nam)
 else
  WarnWrite(PMSGNOSABL)
  rtr=.F.
 endif
 use
endif
select(old_select)
if rtr
 go old_Nz
else
 RETR=2
endif
setcolor(old_color)
rest scre from OnePage
return
*---------------------------------------------------------*
function SablFunc
parameters mode,fld_ptr
private key_last,Nz
key_last=LASTKEY()
do case
case mode <= 3
case LASTKEY() = 27
  return 0
case LASTKEY() = 13	&& Enter
  save screen
  MsgWrite("ÇéëëíÄçÄÇãàÇÄû èêéÇéÑäì àá ëèàëäÄ òÄÅãéçéÇ...")
  select 1
  set filter to
  go bottom
  n=NUMBER
  append blank
  repl A->NUMBER with n+1
  repl A->DEBET  with B->DEBET
  repl A->SUBDEB with B->SUBDEB
  repl A->KREDIT with B->KREDIT
  repl A->SUBKRE with B->SUBKRE
  repl A->SUMMA  with B->SUMMA
  repl A->NOTES  with B->NOTES
  if I->ISDEB
  if DEBET=I->DEB51
   if I->ISSUB
    if SUBDEB=I->SUB51
     repl I->RECSUM with I->RECSUM+SUMMA
     repl I->SALEND with I->SALBEG+I->RECSUM-I->EXPSUM
    endif
   else
    repl I->RECSUM with I->RECSUM+SUMMA
    repl I->SALEND with I->SALBEG+I->RECSUM-I->EXPSUM
   endif
  endif
  if KREDIT=I->DEB51
   if I->ISSUB
    if SUBKRE=I->SUB51
     repl I->EXPSUM with I->EXPSUM+SUMMA
     repl I->SALEND with I->SALBEG+I->RECSUM-I->EXPSUM
    endif
   else
    repl I->EXPSUM with I->EXPSUM+SUMMA
    repl I->SALEND with I->SALBEG+I->RECSUM-I->EXPSUM
   endif
  endif
  else
   repl I->SALEND with I->SALEND+SUMMA
  endif
  select 4
  name_dbf=use_path+MainDIC
  use &name_dbf SHARED
  OldSablon("alltrim(D->D_DOC)","D1=B->DEBET")
  OldSablon("alltrim(D->K_DOC)","D1=B->KREDIT")
  select 4
  use
  select 1
  ShaProtoc()
  ComBalans(DEBET,KREDIT,SUMMA,NOTES,SUBDEB,SUBKRE)
  InitFilter()
  go bottom
  select 2
  restore screen
  rtr=.F.
  return 0
case lastkey() = 7      && Del
  if .not.Pravo0()
   return 1
  endif
  if IsSetj()
   return 1
  endif
  save screen
  if Yes_Or_No(PMSGDEL1,PMSGDEL2,2)
   MsgWrite(PMSGDEL3)
   select 4
   name_dbf=use_path+MainDIC
   use &name_dbf SHARED
   DelSablon("alltrim(D->D_DOC)","B->DEBET=D1")
   DelSablon("alltrim(D->K_DOC)","B->KREDIT=D1")
   select 4
   use
   select 2
   delete
   pack
   go top
  endif
  restore screen
  return 2
endcase
return 1
*---------------------------------------------------------*
procedure OldSablon
parameters bal_doc,bal_loc
select 4
locate for &bal_loc
if found()
 if .not.empty(&bal_doc)
  use_sab=sab_path+"\"+&bal_doc+".DBF"
  if file(use_sab)
   select 3
   use &use_sab SHARED
   locate for B->NUMBER=C->NUMBER
   if found()
    sablcopy(use_path+&bal_doc+".STR",use_path+&bal_doc+".DBF")
   endif
   use
  endif
 endif
endif
return
*---------------------------------------------------------*
procedure DelSablon
parameters bal_doc,bal_loc
select 4
locate for &bal_loc
if found()
 if .not.empty(&bal_doc)
  use_sab=sab_path+"\"+&bal_doc+".DBF"
  if file(use_sab)
   select 3
   use &use_sab
   locate for B->NUMBER=C->NUMBER
   if found()
    delete
    pack
   endif
   use
  endif
 endif
endif
return
*---------------------------------------------------------*
function MenuSabl
PMenuOpen()
PMenuSele()
PMenuDel()
PMenuESC()
PMenuClose()
return " "
*--abcdefgh-----------------------------------------------*
procedure CopySabl
parameters str_sabl
private ch,f,i
select 6
use &str_sabl SHARED
ch=''
for i=1 to lastrec()-2
 f=F->FIELD_NAME
 ch=ch+C->&f
 skip
next
select 5
repl SABLON with ch
select 6
use
return
*--abcdefgh-----------------------------------------------*
procedure SablCopy
parameters str_sabl,dbf_sabl
private ch,f,i,l
select 7
if .not.file(dbf_sabl)
 creat &dbf_sabl from &str_sabl
endif
use &dbf_sabl SHARED
go bottom
n=DOCUMER
append blank
repl NUMBER with A->NUMBER
repl DOCUMER with n+1
select 6
use &str_sabl SHARED
l=1
for i=1 to lastrec()-2
 f=F->FIELD_NAME
 select 7
 repl &f with substr(C->SABLON,l,F->FIELD_LEN)
 select 6
 l=l+F->FIELD_LEN
 skip
next
select 6
use
select 7
use
return
*---------------------------------------------------------*
procedure asablon
private old_color,old_select,OnePage,sab_path,use_sab
aOptions={" ÇÆ··‚†≠Æ¢®‚Ï "," ëÆÂp†≠®‚Ï "," é‚¨•≠† "}
nchoise=frags(" ò Ä Å ã é H ","Ñ é ä ì å Ö ç í ?",aOptions)
if nchoise!=3
 save scre to OnePage
 old_color=setcolor(ct)
 old_select=select()
 sab_path=cur_path+"\SHABLON"
 use_sab=sab_path+"\"+SablonDBF
 if .not.file(use_sab)
  mkdir(sab_path)
 endif
 do case
 case nchoise=1
  use_sab=sab_path+"\"+file_name+".DBF"
  if file(use_sab)
   setcolor(cmenu)
   DrawBox(Y_min,X_min,Y_max,X_max,ibox)
   setcolor(cl)
   MSG=' ò Ä Å ã é ç '
   @ Y_min,X_srm-len(MSG)/2 say MSG
   MenuSabl()
   setcolor(cmenu)
   select 5
   if is_setj
    use &use_sab SHARED
   else
    use &use_sab
   endif
   decl dbsabl[7]
   dbsabl[1]="substr(SABLON,1,78)"
   dbsabl[2]="substr(SABLON,79,78)"
   dbsabl[3]="substr(SABLON,157,78)"
   dbsabl[4]="substr(SABLON,235,78)"
   dbsabl[5]="substr(SABLON,313,78)"
   dbsabl[6]="substr(SABLON,391,78)"
   dbsabl[7]="substr(SABLON,469,78)"
   dbedit(Y_min+1,X_min+1,Y_max-1,X_max-1,dbsabl,"dbmsabl","","","","","")
   rele dbsabl
   select 5
   use
  else
   WarnWrite(PMSGNOSABL)
  endif
 case nchoise=2
  select 5
  use_sab=sab_path+"\"+file_name+".DBF"
  if .not.file(use_sab)
   creat &use_sab from &SablonSTR
  else
   use &use_sab SHARED
  endif
  select 6
  str_sabl=use_path+"\"+file_name+".STR"
  use &str_sabl SHARED
  ch=''
  for i=1 to lastrec()-2
   f=F->FIELD_NAME
   ch=ch+D->&f
   skip
  next
  use
  select 5
  append blank
  repl SABLON with ch
  use
 endcase
 select(old_select)
 setcolor(old_color)
 rest scre from OnePage
endif
return
*-------------------------------*
function dbmsabl
parameters mode,ptr
do case
case mode=0
case lastkey()=27
 return 0
case LASTKEY() = 13	&& Enter
 save screen
 MsgWrite("ÇéëëíÄçÄÇãàÇÄû ÑéäìåÖçí àá ëèàëäÄ òÄÅãéçéÇ...")
 select 4
 go bottom
 n=DOCUMER
 append blank
 repl DOCUMER with n+1
 select 6
 str_sabl=use_path+"\"+file_name+".STR"
 use &str_sabl SHARED
 l=1
 for i=1 to lastrec()-2
  f=F->FIELD_NAME
  select 4
  repl &f with substr(E->SABLON,l,F->FIELD_LEN)
  select 6
  l=l+F->FIELD_LEN
  skip
 next
 select 6
 use
 select 5
 restore screen
 return 0
case lastkey() = 7      && Del
 if IsSetj()
  return 1
 endif
 save screen
 if Yes_Or_No(PMSGDEL1,PMSGDEL2,2)
  MsgWrite(PMSGDEL3)
  delete
  pack
  go top
  restore screen
  return 2
 endif
 restore screen
endcase
return 1
