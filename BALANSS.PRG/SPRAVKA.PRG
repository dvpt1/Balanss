*---------------------------------------------------------*
procedure SpravPrint
parameters Y1,X1,Y2,X2
private FivePage,otwet,old_color
Save Screen to FivePage
old_color=setcolor(cmenu)
BoxTenj(Y1,X1,Y2,X2,ibox)
setcolor(cl)
MSG='  … — € ’ œ '
@ Y1,int((X1+X2)/2-len(MSG)/2) say MSG
otwet=1
setcolor(cmenu)
@ Y1+1,   X1+1 prompt " ‘¯¨á®ª                    "
@ row()+1,X1+1 prompt " ¡®à®â ¬ â¥à¨ «           "
@ row()+1,X1+1 prompt " ¡®à®â ®á­®¢­®£® áà¥¤áâ¢  "
@ row()+1,X1+1 prompt " ¡®à®â á¢¥à­ãâë©          "
@ row()+1,X1+1 prompt " ¡®à®â à §¢¥à­ãâë©        "
menu to otwet
do case
case otwet=1
 dblpt1(dict_fld,dict_nam,TITL2)
case otwet=2
 Srav1(Y_srm-7,X_srm-37,Y_srm+6,X_srm+37)
case otwet=3
 Srav4(Y_srm-10,X_srm-37,Y_srm+ 9,X_srm+37)
case otwet=4
 Srav3(Y_srm-2,X_srm-20,Y_srm+ 1,X_srm+20)
case otwet=5
 Srav2(Y_srm-2,X_srm-20,Y_srm+ 1,X_srm+20)
endcase
setcolor(old_color)
Restore Screen from FivePage
return
*---------------------------------------------------------*
procedure Srav1
parameters Y1,X1,Y2,X2
MsgWrite("‘ç¨â î "+'"'+alltrim(NAME)+'"')
NUMEXP=0
SUMEXP=0
NUMREC=0
SUMREC=0
select 7
index on str(NUMB)+NAME+MEAS to obor.ntx
set filter to lst_dbf=DEBKRE.and.(B->D1=EXPENSES.or.B->D1=RECEIPTS)
go top
seek str(C->NUMB)+C->NAME+C->MEAS
do while  str(NUMB)+NAME+MEAS==str(C->NUMB)+C->NAME+C->MEAS
 if B->D1=EXPENSES
  NUMEXP=NUMEXP+QUAN
  SUMEXP=SUMEXP+A->SUMMA
 else
  NUMREC=NUMREC+QUAN
  SUMREC=SUMREC+A->SUMMA
 endif
 skip
enddo
select 3
***
cpfopen("spravka.txt")
***
cpfout("ÄÄ ˆ‚.N ÄÄÄÄÄÄÄÄÄÄÄÄÄÄ  € ˆ Œ …   ‚ €  ˆ … ÄÄÄÄÄÄÄÄÄÄÄÄÄÄ …„.ˆ‡Œ. ÄÄ"+chr(13)+chr(10))
cpfout(space(2)+str(NUMB)+space(3)+NAME+space(3)+MEAS+chr(13)+chr(10))
cpfout("ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ – …  € ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ"+chr(13)+chr(10))
repl I->FORMAT with (DEBET-KREDIT+SUMEXP-SUMREC)/(QUAN+NUMEXP-NUMREC)
cpfout(space(30)+str(I->FORMAT)+chr(13)+chr(10))
cpfout("ÄÄÄÄÄÄÄÄÄÄÄ   ˆ •  „ ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ  € ‘ •  „ ÄÄÄÄÄÄÄÄÄÄ"+chr(13)+chr(10))
cpfout("ÄÄ Š‹ˆ—…‘’‚ ÄÄÄÄÄÄÄ ‘ “ Œ Œ € ÄÄÄÄÄÄÄÄÄ Š‹ˆ—…‘’‚ ÄÄÄÄÄÄÄ ‘ “ Œ Œ € ÄÄ"+chr(13)+chr(10))
repl I->QORMAT with NUMEXP
cpfout(space(2)+str(I->QORMAT))
repl I->FORMAT with SUMEXP
cpfout(space(2)+str(I->FORMAT))
repl I->QORMAT with NUMREC
cpfout(space(9)+str(I->QORMAT))
repl I->FORMAT with SUMREC
cpfout(space(2)+str(I->FORMAT)+chr(13)+chr(10))
cpfout("ÄÄÄÄÄÄÄÄÄ ‘€‹œ„ € €—€‹ ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ ‘€‹œ„ € Š…– ÄÄÄÄÄÄÄÄ"+chr(13)+chr(10))
cpfout("ÄÄ Š‹ˆ—…‘’‚ ÄÄÄÄÄÄÄ ‘ “ Œ Œ € ÄÄÄÄÄÄÄÄÄ Š‹ˆ—…‘’‚ ÄÄÄÄÄÄÄ ‘ “ Œ Œ € ÄÄ"+chr(13)+chr(10))
cpfout(space(2)+str(QUAN))
repl I->FORMAT with  DEBET-KREDIT
cpfout(space(2)+str(I->FORMAT))
repl I->QORMAT with QUAN+NUMEXP-NUMREC
cpfout(space(9)+str(I->QORMAT))
repl I->FORMAT with  DEBET-KREDIT+SUMEXP-SUMREC
cpfout(space(2)+str(I->FORMAT)+chr(13)+chr(10))
***
cpfclose()
***
select 7
name_ntx=use_path+OborotNTX
set index to &name_ntx
set filter to
go top
dele file obor.ntx
select 3
return
*---------------------------------------------------------*
procedure Srav2
parameters Y1,X1,Y2,X2
private old_select,old_Nz,Nz1
MsgWrite(PMSG10)
old_select=select()
old_Nz=recno()
select 7
set filter to lst_dbf=DEBKRE.and.(B->D1=EXPENSES.or.B->D1=RECEIPTS)
go top
select(old_select)
sum DEBET,KREDIT to sDEBET,sKREDIT
select 7
sum A->SUMMA to oDEBET  for B->D1=EXPENSES
sum A->SUMMA to oKREDIT for B->D1=RECEIPTS
setcolor(cinp)
@ Y_srm-6,X_min+25 say "ÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛ"
@ Y_srm-5,X_min+25 say "Û ‘ € ‹ œ „   €  € — € ‹  Û"
@ Y_srm-4,X_min+25 say "Û „ …  … ’   ÛÛÛ Š  … „ ˆ ’ Û"
@ Y_srm-3,X_min+25 say "Û              Û              Û"
@ Y_srm-2,X_min+25 say "ÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛ"
@ Y_srm-1,X_min+25 say "Û             ’          Û"
@ Y_srm+0,X_min+25 say "Û „ …  … ’   ÛÛÛ Š  … „ ˆ ’ Û"
@ Y_srm+1,X_min+25 say "Û              Û              Û"
@ Y_srm+2,X_min+25 say "ÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛ"
@ Y_srm+3,X_min+25 say "Û ‘ € ‹ œ „    €  Š   … – Û"
@ Y_srm+4,X_min+25 say "Û „ …  … ’   ÛÛÛ Š  … „ ˆ ’ Û"
@ Y_srm+5,X_min+25 say "Û              Û              Û"
@ Y_srm+6,X_min+25 say "ÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛ"
repl I->FORMAT with sDEBET
@ Y_srm-3,X_min+26 say I->FORMAT
repl I->FORMAT with sKREDIT
@ row(),col()+1 say I->FORMAT
repl I->FORMAT with oDEBET
@ Y_srm+1,X_min+26 say I->FORMAT
repl I->FORMAT with oKREDIT
@ row(),col()+1 say I->FORMAT
repl I->FORMAT with sDEBET+oDEBET
@ Y_srm+5,X_min+26 say I->FORMAT
repl I->FORMAT with sKREDIT+oKREDIT
@ row(),col()+1 say I->FORMAT
inkey(0)
select 7
set filter to
go top
select(old_select)
go old_Nz
return
*---------------------------------------------------------*
procedure Srav3
parameters Y1,X1,Y2,X2
private old_select,old_Nz,Nz1
MsgWrite(PMSG10)
old_select=select()
old_Nz=recno()
select 7
set filter to lst_dbf=DEBKRE.and.(B->D1=EXPENSES.or.B->D1=RECEIPTS)
go top
select(old_select)
sum DEBET,KREDIT to sDEBET,sKREDIT
select 7
sum A->SUMMA to oDEBET  for B->D1=EXPENSES
sum A->SUMMA to oKREDIT for B->D1=RECEIPTS
setcolor(cinp)
@ Y_srm-2,X_min+9 say "ÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛ"
@ Y_srm-1,X_min+9 say "Û ‘ € ‹ œ „   Û             ’          Û ‘ € ‹ œ „   Û"
@ Y_srm+0,X_min+9 say "Û € €—€‹    Û  „ …  … ’  ÛÛÛ Š  … „ ˆ ’ Û € Š…–     Û"
@ Y_srm+1,X_min+9 say "ÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛ"
@ Y_srm+2,X_min+9 say "Û              Û              Û              Û              Û"
@ Y_srm+3,X_min+9 say "ÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛ"
repl I->FORMAT with sDEBET-sKREDIT
@ Y_srm+2,X_min+10 say I->FORMAT
repl I->FORMAT with oDEBET
@ row(),col()+1 say I->FORMAT
repl I->FORMAT with oKREDIT
@ row(),col()+1 say I->FORMAT
repl I->FORMAT with sDEBET+oDEBET-oKREDIT-sKREDIT
@ row(),col()+1 say I->FORMAT
inkey(0)
select 7
set filter to
go top
select(old_select)
go old_Nz
return
*---------------------------------------------------------*
procedure Srav4
parameters Y1,X1,Y2,X2
MsgWrite("‘ç¨â î "+'"'+alltrim(NAME)+'"')
NUMEXP=0
SUMEXP=0
NUMREC=0
SUMREC=0
select 7
index on str(NUMB)+NAME+MEAS to obor.ntx
set filter to lst_dbf=DEBKRE.and.(B->D1=EXPENSES.or.B->D1=RECEIPTS)
go top
seek str(C->NUMB)+C->NAME+C->MEAS
do while  str(NUMB)+NAME+MEAS==str(C->NUMB)+C->NAME+C->MEAS
 if A->DEBET=lst_dbf
  if B->D1=EXPENSES
   NUMEXP=NUMEXP+QUAN
   SUMEXP=SUMEXP+A->SUMMA
  else
   NUMEXP=NUMEXP-QUAN
   SUMEXP=SUMEXP-A->SUMMA
  endif
 else
  if B->D1=RECEIPTS
   NUMREC=NUMREC+QUAN
   SUMREC=SUMREC+A->SUMMA
  else
   NUMREC=NUMREC-QUAN
   SUMREC=SUMREC-A->SUMMA
  endif
 endif
 skip
enddo
select 3
***
cpfopen("spravka.txt")
***
cpfout("ÄÄ ˆ‚.N ÄÄÄÄÄÄÄÄÄÄÄÄÄÄ  € ˆ Œ …   ‚ €  ˆ … ÄÄÄÄÄÄÄÄÄÄÄÄÄÄ …„.ˆ‡Œ. ÄÄ"+chr(13)+chr(10))
cpfout(space(2)+str(NUMB)+space(3)+NAME+space(3)+MEAS+chr(13)+chr(10))
cpfout("ÄÄ ‘ ’  ˆ Œ  ‘ ’ œ ÄÄÄÄÄÄÄÄ ˆ ‡   ‘ ÄÄÄÄÄÄÄÄÄÄÄÄÄÄ  ‘ ’ € ’  Š ÄÄÄÄ"+chr(13)+chr(10))
repl I->FORMAT with DEBET+SUMEXP
cpfout(space(2)+str(I->FORMAT))
repl I->FORMAT with KREDIT+SUMREC
cpfout(space(9)+str(I->FORMAT))
repl I->FORMAT with DEBET+SUMEXP-KREDIT-SUMREC
cpfout(space(14)+str(I->FORMAT)+chr(13)+chr(10))
cpfout("ÄÄ Š‹ˆ—…‘’‚ ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ"+chr(13)+chr(10))
repl I->QORMAT with QUAN+NUMEXP-NUMREC
cpfout(space(2)+str(I->QORMAT)+chr(13)+chr(10))
cpfout("ÄÄÄÄ„€’€ €—€‹€ÄÄÄÄÄ„€’€ Š‘‹“€’€–ˆˆÄÄÄÄÄ„€’€ ……„€—ˆÄÄÄÄÄ„€’€ Š–€ÄÄÄ"+chr(13)+chr(10))
cpfout(space(2)+varstr(DATBEG))
cpfout(space(2)+varstr(DATEKS))
cpfout(space(2)+varstr(DATPER))
cpfout(space(2)+varstr(DATEND)+chr(13)+chr(10))
cpfout("ÄÄÄÄ™ˆ‰ …ƒÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ’…Š“™ˆ‰ …ƒÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ"+chr(13)+chr(10))
cpfout(space(2)+str(PROBEG))
cpfout(space(20)+str(PROTEK)+chr(13)+chr(10))
cpfout("ÄÄÄÄ˜ˆ” €Œ’ˆ‡€–ˆˆÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ"+chr(13)+chr(10))
cpfout(space(2)+str(ASHIFR)+chr(13)+chr(10))
cpfout("ÄÄÄÄŒ€ €Œ’ˆ‡€–ˆˆÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ"+chr(13)+chr(10))
cpfout(space(2)+str(ANORMA)+chr(13)+chr(10))
cpfout("ÄÄÄÄŠ””ˆ–ˆ…’ ……–…ŠˆÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ"+chr(13)+chr(10))
cpfout(space(2)+str(KOEF)+chr(13)+chr(10))
cpfout("ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ    — … … ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ"+chr(13)+chr(10))
cpfout(space(2)+NOTES+chr(13)+chr(10))
***
cpfclose()
***
select 7
name_ntx=use_path+OborotNTX
set index to &name_ntx
set filter to
go top
dele file obor.ntx
select 3
return
