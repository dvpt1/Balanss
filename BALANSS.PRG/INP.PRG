*---------------------------------------------------------*
function NewBalans
private var_retr,Nz
if .not.Pravo1()
 return .F.
endif
save screen
if Yes_Or_No('ë é á Ñ Ä í ú','ëÆß§†‚Ï ØpÆ¢Æ§™„?',1)
  MsgWrite("ëÆß§†Ó...")
  var_retr=.T.
  Nz=recno()
  go reccount()
  n=NUMBER
  go Nz
  append blank
  OnSetj()
  repl A->NUMBER with n+1
  commit
  repl A->DEBET  with 0
  repl A->SUBDEB with 0
  repl A->KREDIT with 0
  repl A->SUBKRE with 0
  repl A->SUMMA  with 0
  if SIZE_FILTER!=0.and.(I->L_DEBET.or.I->L_KREDIT)
   if I->L_DEBET
    repl DEBET with I->F_DEBET
    if I->L_SUBDEB
     repl SUBDEB with I->F_SUBDEB
    endif
   endif
   if I->L_KREDIT
    repl KREDIT with I->F_KREDIT
    if I->L_SUBKRE
     repl SUBKRE with I->F_SUBKRE
    endif
   endif
  endif
  UnSetj()
  RETR = 0
else
  var_retr=.F.
endif
restore screen
return var_retr
*---------------------------------------------------------*
procedure InpBalans
parameters fraging
private old_color,InpPage,done,otwet,var_fld,var_prm,var_file,var_iif
if .not.OnSetj()
 return
endif
if pcount()=0
 fraging=.F.
else
 fraging=.T.
endif
if fraging
 if .not.chetpravo()
  return
 endif
 if .not.Fragen('áÄÉêìáàíú','á†£p„ß®‚Ï ØpÆ¢Æ§™„?')
  return
 endif
endif
old_color = setcolor(cinp)
save screen to InpPage
OldProtoc()
if (.not.FUTURE).and.(.not.is_arhiv).and.(pravo!=1)
if I->ISDEB
if DEBET=I->DEB51
 if I->ISSUB
  if SUBDEB=I->SUB51
   repl I->RECSUM with I->RECSUM-SUMMA
   repl I->SALEND with I->SALBEG+I->RECSUM-I->EXPSUM
  endif
 else
  repl I->RECSUM with I->RECSUM-SUMMA
  repl I->SALEND with I->SALBEG+I->RECSUM-I->EXPSUM
 endif
endif
if KREDIT=I->DEB51
 if I->ISSUB
  if SUBKRE=I->SUB51
   repl I->EXPSUM with I->EXPSUM-SUMMA
   repl I->SALEND with I->SALBEG+I->RECSUM-I->EXPSUM
  endif
 else
   repl I->EXPSUM with I->EXPSUM-SUMMA
   repl I->SALEND with I->SALBEG+I->RECSUM-I->EXPSUM
 endif
endif
else
 repl I->SALEND with I->SALEND-SUMMA
endif
endif
is_divis=.F.
is_debet=.F.
is_kredit=.F.
RETR=dbinput(SIZE_ACCO,acco_fld,acco_dbf,acco_iif,acco_row,acco_col,;
                       acco_scr,acco_cod,acco_nam,acco_ntx,"",".DIC",;
                       "DrawnInp()","InpUser()")
if .not.chetpravo()
 MsgWrite("è ê é Ç é Ñ ä Ä  Ä H ì ã à ê ì Ö í ë ü ...")
 if is_debet
  file_in=alltrim(mDict("D_DOC",DEBET,"D1",use_path+MainDIC,""))
  if .not.empty(file_in)
   NulRecord(use_path+file_in+".DBF")
  endif
 endif
 if is_kredit
  file_in=alltrim(mDict("K_DOC",KREDIT,"D1",use_path+MainDIC,""))
  if .not.empty(file_in)
   NulRecord(use_path+file_in+".DBF")
  endif
 endif
 if is_divis
  NulRecord(use_path+OborotDBF)
 endif
 repl DEBET with 0
 repl SUBDEB with 0
 repl KREDIT with 0
 repl SUBKRE with 0
 repl SUMMA with 0
 repl NOTES with ""
endif
if (.not.FUTURE).and.(.not.is_arhiv).and.(pravo!=1)
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
endif
if RETR=0
 commit
endif
InpProtoc()
if .not.fraging
 ComBalans(DEBET,KREDIT,SUMMA,NOTES,SUBDEB,SUBKRE)
endif
if I->L_DEBET.or.I->L_SUBDEB.or.I->L_KREDIT.or.I->L_SUBKRE
 MsgWrite('èéÑéÜÑàíÖ...àÑÖí èÖêÖëíêéâäÄ')
 if iif(I->L_DEBET,I->F_DEBET!=DEBET,.F.).or.iif(I->L_SUBDEB,I->F_SUBDEB!=SUBDEB,.F.).or.;
    iif(I->L_KREDIT,I->F_KREDIT!=KREDIT,.F.).or.iif(I->L_SUBKRE,I->F_SUBKRE!=SUBKRE,.F.)
  set filter to
  go top
  InitFilter()
 endif
endif
restore screen from InpPage
setcolor(old_color)
UnSetj()
return
*---------------------------------------------------------*
function InpUser
private do_retr
do_retr=0
do case
case key_last = -1
 file_in=alltrim(mDict("D_DOC",DEBET,"D1",use_path+MainDIC,""))
 if .not.empty(file_in)
  InpDoc(use_path+file_in+".IN",use_path+file_in+".STR",use_path+file_in+".DBF")
  is_debet=.T.
 endif
case key_last = -2
 file_in=alltrim(mDict("K_DOC",KREDIT,"D1",use_path+MainDIC,""))
 if .not.empty(file_in)
  InpDoc(use_path+file_in+".IN",use_path+file_in+".STR",use_path+file_in+".DBF")
  is_kredit=.T.
 endif
case key_last = -3
 if RecExp(Y_srm-3,X_srm-13,Y_srm-2,X_srm+13)=0
  do_retr=2
 endif
endcase
return do_retr
*---------------------------------------------------------*
procedure DrawInp
setcolor(cinp)
BoxTenj(Y_min+1,X_min+ 7,Y_min+21,X_max- 7,ibox)
setcolor(cl)
MSG=' è ê é Ç é Ñ ä Ä '
@ Y_min+1,int((X_min+X_max)/2-len(MSG)/2) say MSG
setcolor(cinp)
@ Y_min+ 2,X_min+ 8 say "                            Ñ Ö Å Ö í                           "
@ Y_min+ 3,X_min+ 8 say "ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¬ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ"
@ Y_min+ 4,X_min+ 8 say "       ëóÖí                     ≥      ëìÅëóÖí                  "
@ Y_min+ 5,X_min+ 8 say "ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ≈ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ"
@ Y_min+ 6,X_min+ 8 say "                                ≥                               "
@ Y_min+ 7,X_min+ 8 say "ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕœÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕ"
@ Y_min+ 8,X_min+ 8 say "                           ä ê Ö Ñ à í                          "
@ Y_min+ 9,X_min+ 8 say "ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¬ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ"
@ Y_min+10,X_min+ 8 say "       ëóÖí                     ≥      ëìÅëóÖí                  "
@ Y_min+11,X_min+ 8 say "ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ≈ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ"
@ Y_min+12,X_min+ 8 say "                                ≥                               "
@ Y_min+13,X_min+ 8 say "ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕœÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕ"
@ Y_min+14,X_min+ 8 say "                            ë ì å å Ä                           "
@ Y_min+15,X_min+ 8 say "ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ"
@ Y_min+16,X_min+ 8 say "                                                                "
@ Y_min+17,X_min+ 8 say "ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕ"
@ Y_min+18,X_min+ 8 say "                       è ê à å Ö ó Ä H à Ö                      "
@ Y_min+19,X_min+ 8 say "ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ"
@ Y_min+20,X_min+ 8 say "                                                                "
return
*---------------------------------------------------------*
procedure InpMenu
setcolor(cl)
@ Y_min+21,X_min+15 say ''
setcolor(cmenu)
MSG='Enter'
@ row(),col() get MSG
@ row(),col() say 'Ç¢Æ§'
MSG='F2'
@ row(),col() get MSG
@ row(),col() say 'Ñ•°•‚'
MSG='F3'
@ row(),col() get MSG
@ row(),col() say 'äp•§®‚'
MSG='F4'
@ row(),col() get MSG
@ row(),col() say 'äÆ´®Á•·‚¢Æ'
MSG='ESC'
@ row(),col() get MSG
@ row(),col() say 'ÇÎÂÆ§'
clear gets
return
*---------------------------------------------------------*
procedure DrawnInp
DrawInp()
InpMenu()
return
*---------------------------------------------------------*
procedure InpDoc
parameters file_dc,file_str,file_dbf
private old_select,name_pic
if file(file_dc)
 old_select=select()
 save scre
 MsgWrite("Ü§®‚•... á†£p„¶†Ó...")
 name_pic=memoread(file_dc)
 cnt_line=MLCOUNT(name_pic,len_edil)
 decl line_pic[cnt_line]
 for i=1 to cnt_line
  line_pic[i] = memoline(name_pic,len_edil,i)
 next
 select 2
 if .not.file(file_dbf)
  creat &file_dbf from &file_str
 endif
 use &file_dbf SHARED
 locate for B->NUMBER=A->NUMBER
 if .not.found()
  go bottom
  n=DOCUMER
  append blank
  repl B->NUMBER with A->NUMBER
  repl B->DOCUMER with n+1
  commit
 endif
 rest scre
 rlock()
 edil(line_pic,cnt_line,iif(I->CNT_COPY>0,I->CNT_COPY,1))
 select 2
 use
 select(old_select)
endif
return
*---------------------------------------------------------*
procedure NulRecord
parameters file_dbf
private cnt_fields
if file(file_dbf)
 select 2
 if is_setj
  use &file_dbf SHARED
 else
  use &file_dbf
 endif
 locate for A->NUMBER=B->NUMBER
 if found()
  cnt_fields=fcount()
  decl jour_jou[cnt_fields]
  afields(jour_jou)
  decl afield[cnt_fields]
  OnSetj()
  for i=1 to cnt_fields
   fld=jour_jou[i]
   repl &fld with nulvar(&fld)
  next
  UnSetj()
 endif
 use
 select 1
endif
return
*---------------------------------------------------------*
procedure DelBalans
if .not.Pravo0()
 return .F.
endif
if IsSetj()
 return .F.
endif
save screen
if Yes_Or_No(PMSGDEL1,iif(Mark=0,'è p Æ ¢ Æ § ™ „ ?','èÆ¨•Á•≠≠Î• ØpÆ¢Æ§™®?'),2)
 MsgWrite(PMSGDEL3)
 if Mark=0
  delete
 else
  go top
 endif
 do while .not.eof()
  if delete().or.Mark = 0
   if !FUTURE
   if I->ISDEB
   if DEBET=I->DEB51
    if I->ISSUB
     if SUBDEB=I->SUB51
      repl I->RECSUM with I->RECSUM-SUMMA
      repl I->SALEND with I->SALBEG+I->RECSUM-I->EXPSUM
     endif
    else
     repl I->RECSUM with I->RECSUM-SUMMA
     repl I->SALEND with I->SALBEG+I->RECSUM-I->EXPSUM
    endif
   endif
   if KREDIT=I->DEB51
    if I->ISSUB
     if SUBKRE=I->SUB51
      repl I->EXPSUM with I->EXPSUM-SUMMA
      repl I->SALEND with I->SALBEG+I->RECSUM-I->EXPSUM
     endif
    else
     repl I->EXPSUM with I->EXPSUM-SUMMA
     repl I->SALEND with I->SALBEG+I->RECSUM-I->EXPSUM
    endif
   endif
   else
    repl I->SALEND with I->SALEND-SUMMA
   endif
   endif
   file_in=alltrim(mDict("D_DOC",DEBET,"D1",use_path+MainDIC,""))
   if .not.empty(file_in)
    DelRecord(use_path+file_in+".DBF")
   endif
   file_in=alltrim(mDict("K_DOC",KREDIT,"D1",use_path+MainDIC,""))
   if .not.empty(file_in)
    DelRecord(use_path+file_in+".DBF")
   endif
   DelRecord(use_path+OborotDBF,.F.)
  endif
  if Mark != 0
   skip
  else
   exit
  endif
 enddo
 pack
 go top
 commit
 Mark=0
 RETR=2
endif
restore screen
return
*---------------------------------------------------------*
procedure DelRecord
parameters file_dbf,is_dele
if pcount()<2
 is_dele=.T.
endif
if file(file_dbf)
 select 3
 use &file_dbf
 locate for A->NUMBER=C->NUMBER
 if found()
  delete
  pack
 endif
 siz=reccount()
 use
 if is_dele
  if siz=0
   dele file &file_dbf      && Á‚Æ° ≠• ¨•Ë†´·Ô
  endif
 endif
 select 1
endif
return
*---------------------------------------------------------*
procedure Futur
if eof()
 return
endif
if .not.chetpravo()
 return
endif
if .not.OnSetj()
 return
endif
save screen
if FUTURE
 if Yes_Or_No("","Ç•p≠„‚Ï ØpÆ¢Æ§™„?",1)
  repl FUTURE with !FUTURE
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
  FutProtoc()
  RETR=2
 endif
else
 if Yes_Or_No("","é‚´Æ¶®‚Ï ØpÆ¢Æ§™„?",1)
  repl FUTURE with !FUTURE
  if I->ISDEB
   if DEBET=I->DEB51
    if I->ISSUB
     if SUBDEB=I->SUB51
      repl I->RECSUM with I->RECSUM-SUMMA
      repl I->SALEND with I->SALBEG+I->RECSUM-I->EXPSUM
     endif
    else
     repl I->RECSUM with I->RECSUM-SUMMA
     repl I->SALEND with I->SALBEG+I->RECSUM-I->EXPSUM
    endif
   endif
   if KREDIT=I->DEB51
    if I->ISSUB
     if SUBKRE=I->SUB51
      repl I->EXPSUM with I->EXPSUM-SUMMA
      repl I->SALEND with I->SALBEG+I->RECSUM-I->EXPSUM
     endif
    else
     repl I->EXPSUM with I->EXPSUM-SUMMA
     repl I->SALEND with I->SALBEG+I->RECSUM-I->EXPSUM
    endif
   endif
  else
   repl I->SALEND with I->SALEND-SUMMA
  endif
  FutProtoc()
  RETR=2
 endif
endif
restore screen
UnSetj()
return
*---------------------------------------------------------*
procedure Kopij
private old_select,Nz
if eof()
 return
endif
if .not.chetpravo()
 return
endif
if .not.OnSetj()
 return
endif
old_select=select()
if .not.eof()
save screen
if Yes_Or_No(' ä é è à ü ',"ë§•´†‚Ï ™ÆØ®Ó ØpÆ¢Æ§™®?",1)
  MsgWrite("Ü§®‚•... äÆØ®p„Ó...")
  OldProtoc()
  Nz=recno()
  go bottom
  n=A->NUMBER
  go Nz
  dNUMBER= A->NUMBER
  dDEBET = A->DEBET
  dSUBDEB= A->SUBDEB
  dKREDIT= A->KREDIT
  dSUBKRE= A->SUBKRE
  dSUMMA = A->SUMMA
  dNOTES = A->NOTES
  append blank                  && §•´†Ó ™ÆØ®Ó
  repl A->NUMBER with n+1
  commit
  repl A->DEBET  with dDEBET
  repl A->SUBDEB with dSUBDEB
  repl A->KREDIT with dKREDIT
  repl A->SUBKRE with dSUBKRE
  repl A->SUMMA  with dSUMMA
  repl A->NOTES  with dNOTES
  if !FUTURE
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
  endif
  file_in=alltrim(mDict("D_DOC",DEBET,"D1",use_path+MainDIC,""))
  if .not.empty(file_in)
   AddRecord(use_path+file_in+".DBF")
  endif
  file_in=alltrim(mDict("K_DOC",KREDIT,"D1",use_path+MainDIC,""))
  if .not.empty(file_in)
   AddRecord(use_path+file_in+".DBF")
  endif
  AddRecord(use_path+OborotDBF)
  KopProtoc()
  ComBalans(DEBET,KREDIT,SUMMA,NOTES,SUBDEB,SUBKRE)
  RETR=2
else
  RETR=1
endif
restore screen
endif
UnSetj()
return
*---------------------------------------------------------*
procedure AddRecord
parameters file_dbf
private cnt_fields
if file(file_dbf)
 select 2
 use &file_dbf SHARED
 locate for dNUMBER=B->NUMBER
 if found()
  cnt_fields=fcount()
  decl jour_jou[cnt_fields]
  afields(jour_jou)
  decl afield[cnt_fields]
  for i=1 to cnt_fields
   fld=jour_jou[i]
   afield[i]=&fld
  next
  if type("DOCUMER")!="U"
   go bottom
   d=DOCUMER
  endif
  append blank
  for i=1 to cnt_fields
   fld=jour_jou[i]
   repl &fld with afield[i]
  next
  rele jour_jou,afield
  repl B->NUMBER with A->NUMBER
  if type("DOCUMER")!="U"
   repl DOCUMER with d+1
  endif
  commit
 endif
 use
 select 1
endif
return
*---------------------------------------------------------*
procedure ComBalans
parameters dDEBET,dKREDIT,SUM,dNOTES,dSUBDEB,dSUBKRE
private old_select,done,otwet,var_fld,var_prm,var_file,var_iif,old_Nz,ridraw
old_select=select()
old_Nz=recno()
select 2
use_dic=use_path+ComeDic
use &use_dic SHARED
locate for DEB=dDEBET.and.KRE=dKREDIT
if found()
 select(old_select)
 set filter to
 go top
 select 2
 ridraw=.T.
 do while found()
  DrawInp()
  setcolor(cinp)
  if ridraw
   pDEBET=B->DEBET
  endif
  var_prm=mDict(acco_scr[1],pDEBET,acco_cod[1],use_path+MainDIC,'')
  @ Y_min+acco_row[1],X_min+acco_col[1] say var_prm
  var_file=use_path+strline(pDEBET)+".DIC"
  if file(var_file)
   if ridraw
    do case
    case B->DEBET=B->DEB
     var_fld=dSUBDEB
    case B->DEBET=B->KRE
     var_fld=dSUBKRE
    othe
     var_fld=B->SUBDEB
    endcase
    pSUBDEB=var_fld
   endif
   var_prm=mDict(acco_scr[2],pSUBDEB,acco_cod[2],var_file,'')
   if .not.is_find
    var_prm=space(3)
   endif
  else
   if ridraw
    pSUBDEB=0
   endif
   var_prm=space(3)
  endif
  if ridraw
   pKREDIT=B->KREDIT
  endif
  @ Y_min+acco_row[2],X_min+acco_col[2] say var_prm
  var_prm=mDict(acco_scr[3],pKREDIT,acco_cod[3],use_path+MainDIC,'')
  @ Y_min+acco_row[3],X_min+acco_col[3] say var_prm
  var_file=use_path+strline(pKREDIT)+".DIC"
  if file(var_file)
   if ridraw
    do case
    case B->KREDIT=B->KRE
     var_fld=dSUBKRE
    case B->KREDIT=B->DEB
     var_fld=dSUBDEB
    othe
     var_fld=B->SUBKRE
    endcase
    pSUBKRE=var_fld
   endif
   var_prm=mDict(acco_scr[4],pSUBKRE,acco_cod[4],var_file,'')
   if .not.is_find
    var_prm=space(3)
   endif
  else
   if ridraw
    pSUBKRE=0
   endif
   var_prm=space(3)
  endif
  @ Y_min+acco_row[4],X_min+acco_col[4] say var_prm
  if ridraw
   repl I->FORMAT with formula(translate(B->SUMMA))
   pSUMMA=I->FORMAT
  endif
  @ Y_min+acco_row[5],X_min+acco_col[5] say pSUMMA
  if ridraw
   pNOTES=dNOTES
  endif
  @ Y_min+acco_row[6],X_min+acco_col[6] say pNOTES
  ridraw=.F.
  set color to &cfr
  @ Y_min+21,X_min+20 say ' èêéÇéÑäì? '
  otwet=1
  @ row(),col() prompt ' ëÑÖãÄíú '
  @ row(),col() prompt ' éíåÖHàíú '
  @ row(),col() prompt ' èêÄÇàíú '
  menu to otwet
  do case
  case otwet=1
   select 1
   Nz=recno()
   go bottom
   n=A->NUMBER
   go Nz
   append blank
   repl A->NUMBER with n+1
   commit
   repl A->DEBET  with pDEBET
   repl A->SUBDEB with pSUBDEB
   repl A->KREDIT with pKREDIT
   repl A->SUBKRE with pSUBKRE
   repl A->SUMMA  with pSUMMA
   repl A->NOTES  with pNOTES
   if .not.chetpravo()
     MsgWrite("è ê é Ç é Ñ ä Ä  Ä H ì ã à ê ì Ö í ë ü ...")
     repl A->DEBET with 0
     repl A->SUBDEB with 0
     repl A->KREDIT with 0
     repl A->SUBKRE with 0
     repl A->SUMMA  with 0
     repl A->NOTES  with ""
   else
    if is_divis.and.(dDEBKRE=pDEBET.or.dDEBKRE=pKREDIT)
     select 7
     use_dbf=use_path+OborotDBF
     if is_setj
      use &use_dbf SHARED
     else
      use &use_dbf
     endif
     append blank
     repl NUMBER with A->NUMBER
     commit
     repl G->DEBKRE with dDEBKRE
     repl NUMB with dNUMB
     repl NAME with dNAME
     repl MEAS with dMEAS
     repl QUAN with 0
     repl EXPENSES with 0
     repl RECEIPTS with dEXPENSES
     repl DATBEG with dDATBEG
     repl DATEKS with dDATEKS
     repl DATEND with dDATEND
     repl ASHIFR with dASHIFR
     repl ANORMA with dANORMA
     repl KOEF with dKOEF
     repl NOTES with dNOTEP
     use
     select 1
    endif
   endif
   if I->ISDEB
   if pDEBET=I->DEB51
    if I->ISSUB
     if pSUBDEB=I->SUB51
      repl I->RECSUM with I->RECSUM+pSUMMA
      repl I->SALEND with I->SALBEG+I->RECSUM-I->EXPSUM
     endif
    else
     repl I->RECSUM with I->RECSUM+pSUMMA
     repl I->SALEND with I->SALBEG+I->RECSUM-I->EXPSUM
    endif
   endif
   if pKREDIT=I->DEB51
    if I->ISSUB
     if pSUBKRE=I->SUB51
      repl I->EXPSUM with I->EXPSUM+pSUMMA
      repl I->SALEND with I->SALBEG+I->RECSUM-I->EXPSUM
     endif
    else
     repl I->EXPSUM with I->EXPSUM+pSUMMA
     repl I->SALEND with I->SALBEG+I->RECSUM-I->EXPSUM
    endif
   endif
   else
    repl I->SALEND with I->SALEND+pSUMMA
   endif
   ComProtoc()
   commit
  case otwet=3
   dbinput(SIZE_ACCO,acco_fld,acco_dbf,acco_iif,acco_row,acco_col,;
           acco_scr,acco_cod,acco_nam,acco_ntx,"P",".DIC",;
           "DrawcInp()","")
   loop
  endcase
  select 2
  skip
  locate rest for DEB=dDEBET.and.KRE=dKREDIT
  ridraw=.T.
 enddo
 select(old_select)
 InitFilter()
 select 2
endif
use
select(old_select)
go old_Nz
return
*---------------------------------------------------------*
procedure DrawcInp
DrawInp()
InpsMenu()
return
*---------------------------------------------------------*
procedure InpsMenu
setcolor(cl)
@ Y_min+21,X_min+19 say ' å Ö ç û: '
setcolor(cmenu)
MSG='Enter'
@ row(),col() get MSG
@ row(),col() say 'Ç¢Æ§'
MSG='ESC'
@ row(),col() get MSG
@ row(),col() say 'ÇÎÂÆ§'
clear gets
return
