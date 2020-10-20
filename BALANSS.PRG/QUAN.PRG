*---------------------------------------------------------*
function Quan
parameters Y1,X1,Y2,X2,is_found
private old_color,old_select,Inp1Page,otwet,is_done,Nz,is_dk,Nz3,Nz4,Nz1,iS,iQ,is_defil
is_data=1
is_defil=.F.
if SIZE_LIST=0
 return is_data
endif
old_select=select()
old_color = setcolor(cinp)
save screen to Inp1Page
select 7
Nz4=recno()
if is_found
 dDEBET=A->DEBET
 dSUBDEB=A->SUBDEB
 dKREDIT=A->KREDIT
 dSUBKRE=A->SUBKRE
 dSUMMA=A->SUMMA
 dNOTES=A->NOTES
 dFUTURE=A->FUTURE
 dDEBKRE=DEBKRE
 dNUMB=NUMB
 dNAME=NAME
 dMEAS=MEAS
 dQUAN=QUAN
 dEXPENSES=EXPENSES
 dRECEIPTS=RECEIPTS
 dDATBEG=DATBEG
 dDATEKS=DATEKS
 dDATPER=DATPER
 dDATEND=DATEND
 dPROBEG=PROBEG
 dPROTEK=PROTEK
 dASHIFR=ASHIFR
 dANORMA=ANORMA
 dKOEF=KOEF
 dNOTEP=NOTES
else
 dDEBET =C->EXPENS
 dSUBDEB=C->SUBEXP
 dKREDIT=C->RECEIP
 dSUBKRE=C->SUBREC
 dSUMMA=nulvar(A->SUMMA)
 dNOTES=nulvar(A->NOTES)
 dFUTURE=.F.
 dDEBKRE=lst_dbf
 dNUMB=C->NUMB
 dNAME=C->NAME
 dMEAS=C->MEAS
 dQUAN=nulvar(QUAN)
 dEXPENSES=nulvar(EXPENSES)
 dRECEIPTS=nulvar(RECEIPTS)
 dDATBEG=nulvar(DATBEG)
 dDATEKS=nulvar(DATEKS)
 dDATPER=nulvar(DATPER)
 dDATEND=nulvar(DATEND)
 dPROBEG=nulvar(PROBEG)
 dPROTEK=nulvar(PROTEK)
 dASHIFR=nulvar(ASHIFR)
 dANORMA=nulvar(ANORMA)
 dKOEF=nulvar(KOEF)
 dNOTEP=nulvar(NOTES)
endif
DldProtoc()
setcolor(cinp)
is_data=dbinput(SIZE_QUAN,quan_fld,quan_dbf,quan_iif,quan_row,quan_col,;
                       quan_scr,quan_cod,quan_nam,quan_ntx,"d",".DIC",;
                       "DrawList1()","ListUser1()")
if is_data=0
 if (debpravo(dDEBET).or.dDEBET=0).and.(krepravo(dKREDIT).or.dKREDIT=0)
 if Fragen('ˆ•Ž„/€‘•Ž„','‚ë¯®«­¨âì ®¯¥à æ¨î?')
  MsgWrite("‚ ›  Ž ‹  Ÿ ž  Ž  …  € – ˆ ž ...")
  is_divis = .T.
  iSUMMA = 0
  iQUAN = 0
  if .not.(dRECEIPTS = 0 .or. dQUAN = 0)        &&  QuanSumma()
   if dRECEIPTS=B->D1
    select 7
    if is_found
     sum A->SUMMA,G->QUAN to iS,iQ for EXPENSES=B->D1.and.recno()!=Nz4
     iSUMMA=iSUMMA+iS
     iQUAN=iQUAN+iQ
     sum A->SUMMA,G->QUAN to iS,iQ for RECEIPTS=B->D1.and.recno()!=Nz4
     iSUMMA=iSUMMA-iS
     iQUAN=iQUAN-iQ
    else
     sum A->SUMMA,G->QUAN to iS,iQ for EXPENSES=B->D1
     iSUMMA=iSUMMA+iS
     iQUAN=iQUAN+iQ
     sum A->SUMMA,G->QUAN to iS,iQ for RECEIPTS=B->D1
     iSUMMA=iSUMMA-iS
     iQUAN=iQUAN-iQ
    endif
    iSUMMA=iSUMMA+C->DEBET
    iQUAN=iQUAN+C->QUAN
    go Nz4
   endif
  endif
  is_done=.F.
  do case
  case dRECEIPTS = 0.or.iQUAN >= dQUAN
   is_done=.T.
  othe
   if .not.Fragen(' Ž ’ Œ …  ˆ ’ œ  Ž  …  € – ˆ ž ? ','Š®«¨ç¥áâ¢® á¯¨áë¢ ¥¬®£® ¡®«ìè¥ ç¥¬ ¥áâì ¢ ­ «¨ç¨¨ !')
    is_done=.T.
   endif
  endcase
  if C->NUMB!=dNUMB.or.C->NAME!=dNAME.or.C->MEAS!=dMEAS
   is_defil=.T.
  endif
  if B->D1!=dEXPENSES.or.B->D1!=dRECEIPTS
   is_defil=.T.
  endif
  if is_done
   MsgWrite("‚ ›  Ž ‹  Ÿ ž  Ž  …  € – ˆ ž ...")
   select 1
   if is_found
    OnSetj()
   else
    Nz1=recno()
    go reccount()
    n=NUMBER
    go Nz1
    append blank
    OnSetj()
    repl A->NUMBER with n+1
    commit
   endif
   repl A->DEBET  with dDEBET
   repl A->SUBDEB with dSUBDEB
   repl A->KREDIT with dKREDIT
   repl A->SUBKRE with dSUBKRE
   if dSUMMA = 0 .and. iQUAN != 0
    repl A->SUMMA with dQUAN*iSUMMA/iQUAN
   else
    repl A->SUMMA with dSUMMA
   endif
   repl A->NOTES  with dNOTES
   UnSetj()
   select 7
   if is_found
    go Nz4
    OnSetj()
   else
    append blank
    OnSetj()
    repl NUMBER with n+1
    commit
   endif
   repl DEBKRE with dDEBKRE
   repl NUMB with dNUMB
   repl NAME with dNAME
   repl MEAS with dMEAS
   repl QUAN with dQUAN
   repl EXPENSES with dEXPENSES
   repl RECEIPTS with dRECEIPTS
   repl DATBEG with dDATBEG
   repl DATEKS with dDATEKS
   repl DATEND with dDATEND
   repl ASHIFR with dASHIFR
   repl ANORMA with dANORMA
   repl KOEF with dKOEF
   repl NOTES with dNOTEP
   UnSetj()
   name_dbf=use_path+strline(dEXPENSES)+"."+lst_ext        && ¥á«¨ ª àâ®çª¨ ­¥â
   if file(name_dbf)
    if name_dbf==name_dbf3
     select 3
     Nz3=recno()
    else
     select 8
     use &name_dbf SHARED
    endif
    locate for NUMB=dNUMB.and.NAME=dNAME.and.MEAS=dMEAS
    if .not.found()
     append blank
     repl NUMB with dNUMB
     repl NAME with dNAME
     repl MEAS with dMEAS
     repl EXPENS with A->DEBET
     repl SUBEXP with A->SUBDEB
     repl DATBEG with dDATBEG
     repl DATEKS with dDATEKS
     repl DATEND with dDATEND
     repl ASHIFR with dASHIFR
     repl ANORMA with dANORMA
     repl KOEF with dKOEF
    endif
    if name_dbf==name_dbf3
     go Nz3
    else
     use
    endif
   endif
   if is_defil
    select 7
    set filter to
    go top
    set filter to lst_dbf=G->DEBKRE.and.(B->D1=EXPENSES.or.B->D1=RECEIPTS).and.;
              C->NUMB=G->NUMB.and.C->NAME=G->NAME.and.C->MEAS=G->MEAS
    go top
   endif
   commit
   InpProtoc()
  endif
 endif
 else
  WarnWrite(PMSG8)
 endif
endif
restore screen from Inp1Page
setcolor(old_color)
select(old_select)
return is_data
*---------------------------------------------------------*
procedure DrawList1
setcolor(cinp)
BoxTenj(Y_min+1,X_min+3,Y_max-4,X_max-2,ibox)
setcolor(cl)
MSG=' Š €  ’ Ž — Š €  „ ‚ ˆ † …  ˆ Ÿ '
@ Y_min+1,int((X_min+X_max)/2-len(MSG)/2) say MSG
setcolor(cinp)
@ Y_min+ 2,X_min+4 say "ÍÍÍÍÍÍÍÍÍÍÍÍ‘ — … ’ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ‘ “  ‘ — … ’ÍÍÍÍÍÍÍÍÍÍÍÍ"
@ Y_min+ 3,X_min+4 say " „……’                                                                   "
@ Y_min+ 4,X_min+4 say " Š…„ˆ’                                                                  "
@ Y_min+ 5,X_min+4 say " -ˆ…                                                                  "
@ Y_min+ 6,X_min+4 say "ÍÍÍˆ­¢.NÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ € ˆ Œ …  Ž ‚ €  ˆ …ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ…„.ˆ‡Œ.ÍÍÍ"
@ Y_min+ 7,X_min+4 say "                                                                         "
@ Y_min+ 8,X_min+4 say "ÍÍÍÍÍÍÍÍŠ Ž ‹ ˆ — … ‘ ’ ‚ ŽÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ‘ “ Œ Œ €ÍÍÍÍÍÍÍÍÍÍÍÍÍÍ"
@ Y_min+ 9,X_min+4 say "                                                                         "
@ Y_min+10,X_min+4 say "ÍÍÍÍÍÍÍÍÍÍÍÍ  ˆ • Ž „ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ € ‘ • Ž „ÍÍÍÍÍÍÍÍÍÍÍÍ"
@ Y_min+11,X_min+4 say "                                                                         "
@ Y_min+12,X_min+4 say "ÍÍÍÍ„€’€ €—€‹€ÍÍÍÍÍ„€’€ Š‘‹“€’€–ˆˆÍÍÍÍÍ„€’€ ……„€—ˆÍÍÍÍÍ„€’€ ŠŽ–€ÍÍÍ"
@ Y_min+13,X_min+4 say "                                                                         "
@ Y_min+14,X_min+4 say "ÍÍÍÍŽ™ˆ‰ Ž…ƒÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ’…Š“™ˆ‰ Ž…ƒÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ"
@ Y_min+15,X_min+4 say "                                                                         "
@ Y_min+16,X_min+4 say "ÍÍÍÍ˜ˆ” €ŒŽ’ˆ‡€–ˆˆÍÍÍÍÍŽŒ€ €ŒŽ’ˆ‡€–ˆˆÍÍÍÍŠŽ””ˆ–ˆ…’ ……Ž–…ŠˆÍÍÍÍÍ"
@ Y_min+17,X_min+4 say "                                                                         "
@ Y_min+18,X_min+4 say "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ  ˆ Œ … — € H ˆ …ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ"
@ Y_min+19,X_min+4 say "                                                                         "
setcolor(cl)
@ Y_min+20,X_min+20 say 'Œ…ž:'
setcolor(cmenu)
MSG='Enter'
@ row(),col() get MSG
@ row(),col() say '‚¢®¤'
MSG='ESC'
@ row(),col() get MSG
@ row(),col() say '‚ëå®¤'
clear gets
setcolor(cinp)
return
*---------------------------------------------------------*
function ListUser1
return 1
*---------------------------------------------------------*
function Quad
parameters Y1,X1,Y2,X2
private old_color,old_select,Inp1Page,otwet,is_done,Nz,is_dk,Nz3,Nz4,Nz1,iS,iQ
is_data=1
if SIZE_LIST=0
 return is_data
endif
old_select=select()
old_color = setcolor(cinp)
save screen to Inp1Page
select 7
dDEBET=A->DEBET
dSUBDEB=A->SUBDEB
dKREDIT=A->KREDIT
dSUBKRE=A->SUBKRE
dSUMMA=A->SUMMA
dNOTES=A->NOTES
dFUTURE=A->FUTURE
dDEBKRE=DEBKRE
dNUMB=NUMB
dNAME=NAME
dMEAS=MEAS
dQUAN=QUAN
dEXPENSES=EXPENSES
dRECEIPTS=RECEIPTS
dDATBEG=DATBEG
dDATEKS=DATEKS
dDATPER=DATPER
dDATEND=DATEND
dPROBEG=PROBEG
dPROTEK=PROTEK
dASHIFR=ASHIFR
dANORMA=ANORMA
dKOEF=KOEF
dNOTEP=NOTES
DldProtoc()
setcolor(cinp)
is_data=dbinput(SIZE_QUAN,quan_fld,quan_dbf,quan_iif,quan_row,quan_col,;
                       quan_scr,quan_cod,quan_nam,quan_ntx,"d",".DIC",;
                       "DrawList1()","ListUser1()")
if is_data=0
 if (debpravo(dDEBET).or.dDEBET=0).and.(krepravo(dKREDIT).or.dKREDIT=0)
 if Fragen('ˆ•Ž„/€‘•Ž„','‚ë¯®«­¨âì ®¯¥à æ¨î?')
  is_divis = .T.
  MsgWrite("‚ ›  Ž ‹  Ÿ ž  Ž  …  € – ˆ ž ...")
  select 1
  OnSetj()
  repl A->DEBET  with dDEBET
  repl A->SUBDEB with dSUBDEB
  repl A->KREDIT with dKREDIT
  repl A->SUBKRE with dSUBKRE
  repl A->SUMMA with dSUMMA
  repl A->NOTES  with dNOTES
  UnSetj()
  select 7
  OnSetj()
  repl DEBKRE with dDEBKRE
  repl NUMB with dNUMB
  repl NAME with dNAME
  repl MEAS with dMEAS
  repl QUAN with dQUAN
  repl EXPENSES with dEXPENSES
  repl RECEIPTS with dRECEIPTS
  repl DATBEG with dDATBEG
  repl DATEKS with dDATEKS
  repl DATEND with dDATEND
  repl ASHIFR with dASHIFR
  repl ANORMA with dANORMA
  repl KOEF with dKOEF
  repl NOTES with dNOTEP
  UnSetj()
  commit
  InpProtoc()
 endif
 else
  WarnWrite(PMSG8)
 endif
endif
restore screen from Inp1Page
setcolor(old_color)
select(old_select)
return is_data
