*---------------------------------------------------------*
procedure InpMonth
external div_nul,sub_nul
private old_color,old_select,old_setj,old_date,old_filter
private ch,otwet,ptr,j,i,new_ptr,is_no,monotwet,is_new
if is_arhiv
 return
endif
if .not.is_adm
 WarnWrite(PMSG1)
 return
endif
if is_setj
 WarnWrite(PMSG6)
 return
endif
is_no=.F.
old_color=setcolor(cw)
save screen to InpPage
old_select=select()
DrawBox(Y_min,X_max-24,Y_min+2,X_max,ibox)
setcolor(cl)
MSG=' ’…Š“™€Ÿ „€’€ '
@ Y_min,int((X_max-24+X_max)/2-len(MSG)/2) say MSG
??chr(7)
old_date=I->T_DATE
otwet=1
do while otwet!=0
  setcolor(cw)
  @ Y_min+1,X_max-19 prompt Month[month(I->T_DATE)]
  @ row(),col()+1 prompt str(year(I->T_DATE),4)
  @ row(),col() say " £."
  menu to otwet
  if otwet != 0
    ptr=month(I->T_DATE)
    ch=str(year(I->T_DATE),4)
    do case
    case otwet=1
      save screen
      setcolor(cdict)
      BoxTenj(Y_min+1,X_max-20,Y_min+14,X_max-11,ibox)
      new_ptr=achoice(Y_min+2,X_max-19,Y_min+13,X_max-12,month,"","",ptr)
      if new_ptr != 0
        ptr=new_ptr
      endif
      rest screen
    case otwet=2
      set cursor on
      @ Y_min+1,X_max-19+len(Month[month(I->T_DATE)])+1 get ch picture '####'
      read
      set cursor off
      ch=iif(val(ch)<1980.or.val(ch)>2080,str(year(I->T_DATE),4),ch)
    endcase
    if lastkey() != 27
      replace I->T_DATE with ctod("01."+str(ptr,2)+"."+ch)
    endif
  endif
enddo
if old_date <> I->T_DATE
  setcolor(old_color)
  restore screen from InpPage
  DrawDate(Y_min,X_max-24,Y_min+2,X_max)
  save screen to InpPage
  restore screen from InpPage
  is_new=.not.file(cur_path+"\"+str(year(I->T_DATE),4)+"."+strtran(str(month(I->T_DATE),2)," ","0")+"\"+MainDBF)
  if is_new
   aOptions={" Ž’Œ…ˆ’œ "," ‘Ž‡„€’œ "}
  else
   aOptions={" Ž’Œ…ˆ’œ "," ‡€Œ…ˆ’œ "," „ŽŽ‹ˆ’œ "}
  endif
  monotwet=frags(" „ € ’ € "," Ž ‚ › ‰  Œ … ‘ Ÿ – ?",aOptions)
  if monotwet=2.or.monotwet=3
   old_path=bal_path
   bal_path=cur_path+"\"+str(year(I->T_DATE),4)+"."+strtran(str(month(I->T_DATE),2)," ","0")
   use_path=bal_path+"\"
   if monotwet=3
* use - ¨áâ®ç­¨ª
* tmp - ¯à¨¥¬­¨ª
    bal_temp=bal_path
    use_temp=use_path
    bal_path=cur_path+"\TEMP"
    use_path=bal_path+"\"
   endif
   use_dbf=use_path+MainDBF
   if ismono(use_dbf)
    nm=month(old_date)
    old_setj=is_setj
    is_setj=.F.
    restore screen from InpPage
    CloseDBF()  && § ªàë¢ î áâ àë© ¬¥áïæ
    set path to &bal_path
    mkdir(bal_path)
    deles("*.*",bal_path)
    copys("*.*",old_path,bal_path)
    OpenDBF()   && ®âªpë¢ î ­®¢ë© ¬¥áïæ
    select 1
    set index to
    select 4
    use_dic=use_path+MainDIC
    use &use_dic
    store reccount() to count_rec,count_exp
    setcolor(cmsg)
    BoxTenj(Y_srm-10,X_srm-33,Y_srm+10,X_srm+33,ibox)
    setcolor(cl)
    MSG=' ……‚Ž„ ‘—…’Ž‚ € Ž‚›‰ Œ…‘Ÿ– '
    @ Y_srm-10,int(X_srm-len(MSG)/2)+1 say MSG
    do while .not.eof()
      old_Nz=recno()
      var_exp=D->D1
      WinSDB(Y_srm-9,X_srm-32,Y_srm+9,X_srm,"strroun(D1)+D2",;
             "'‘ç¥â®¢: '+alltrim(str(reccount()))+' ‘ç¥â: '+alltrim(str(recno()))")
      name_sub=use_path+strline(var_exp)+".DIC"
      if file(name_sub)
        select 1
        index on str(DEBET)+str(SUBDEB)+str(KREDIT) to debet.ntx
        total on str(DEBET)+str(SUBDEB)+str(KREDIT) fields SUMMA to debet.dbf for DEBET=var_exp.and.(!FUTURE)
        set index to
        select 2
        use debet.dbf
        index on str(DEBET)+str(SUBDEB)+str(KREDIT) to debet.ntx
        select 1
        index on str(KREDIT)+str(SUBKRE)+str(DEBET) to kredit.ntx
        total on str(KREDIT)+str(SUBKRE)+str(DEBET) fields SUMMA to kredit.dbf for KREDIT=var_exp.and.(!FUTURE)
        set index to
        select 3
        use kredit.dbf
        index on str(KREDIT)+str(SUBKRE)+str(DEBET) to kredit.ntx
        select 5
        use &name_sub
***
        decl iREC[count_rec]
        afill(iREC,0)
        decl iEXP[count_exp]
        afill(iEXP,0)
        iSALBEGd=0
        iSALENDd=0
        iSALBEGk=0
        iSALENDk=0
        do while (.not.eof())
          WinSDB(Y_srm-9,X_srm,Y_srm+9,X_srm+32,"strroun(D1)+' '+D2",;
                 "'‘ã¡‘ç¥â®¢: '+alltrim(str(reccount()))+' ‘ã¡‘ç¥â: '+alltrim(str(recno()))")
          iSALBEGd=iSALBEGd+SALBED
          iSALBEGk=iSALBEGk+SALBEK
          select 4
          go top
          dRECSUM=0
          i=1
          do while .not.eof()
            select 2
            seek str(var_exp)+str(E->D1)+str(D->D1)
            dRECSUM=dRECSUM+SUMMA
            iREC[i]=iREC[i]+SUMMA
            select 4
            i=i+1
            skip
          enddo
          select 4
          go top
          dEXPSUM=0
          i=1
          do while .not.eof()
            select 3
            seek str(var_exp)+str(E->D1)+str(D->D1)
            dEXPSUM=dEXPSUM+SUMMA
            iEXP[i]=iEXP[i]+SUMMA
            select 4
            i=i+1
            skip
          enddo
          select 5
          dSALEND=SALBED-SALBEK+dRECSUM-dEXPSUM
          iSALENDd=iSALENDd+iif(dSALEND>0,dSALEND,0)
          iSALENDk=iSALENDk+iif(dSALEND<0,dSALEND,0)
          repl SALBED with iif(dSALEND>0,dSALEND,0)
          repl SALBEK with iif(dSALEND<0,-dSALEND,0)
          skip
        enddo
* "ˆâ®£®"
        select 2
        use
        select 3
        use
        select 5
        use
        rele iREC
        rele iEXP
      endif
      select 4
      go old_Nz
      skip
    enddo
    select 1
    use_ntx=use_path+MainNTX
    set index to &use_ntx
    select 2                                    && Oborot
    use_dbf=use_path+OborotDBF
    use &use_dbf
    use_ntx=use_path+OborotNTX
    set index to &use_ntx
    set relation to NUMBER into BALANS
    if SIZE_LIST!=0
     setcolor(cmsg)
     BoxTenj(Y_srm-10,X_srm-33,Y_srm+10,X_srm+33,ibox)
     setcolor(cl)
     MSG=' ……‚Ž„ Ž„€‡„…‹…ˆ‰ € Ž‚›‰ Œ…‘Ÿ– '
     @ Y_srm-10,int(X_srm-len(MSG)/2)+1 say MSG
     for i=1 to SIZE_LIST
      select 3
      name_dbf=use_path+strline(list_dbf[i])+".DIV"
      use &name_dbf
      do while .not.eof()
       WinSDB(Y_srm-9,X_srm-32,Y_srm+9,X_srm,"strroun(D1)+'   '+D2",;
             "'®¤à-­¨©: '+alltrim(str(reccount()))+' ®¤à-­¨¥: '+alltrim(str(recno()))")
       name_dbf=use_path+strline(C->D1)+"."+list_ext[i]
       dbtotal(name_dbf,"str(NUMB)+NAME+MEAS",".T.","QUAN","DEBET","KREDIT")
       select 5
       use &name_dbf
       index on str(NUMB)+NAME+MEAS to tmp.ntx
       ReExpRec("list_dbf[i]=DEBKRE.and.C->D1=EXPENSES",.T.,B->QUAN)  && á­ ç «  ¯à¨å®¤
       ReExpRec("list_dbf[i]=DEBKRE.and.C->D1=RECEIPTS",.F.,-(B->QUAN)) && ¯®â®¬ à áå®¤
       select 5
       use
       select 2
       set filter to
       go top
       select 3
       skip
      enddo
      select 3
      use
     next
    endif
    select 2
    set relation to
    zap
    use
    select 4
    use
    dele file temp.ntx
    dele file tmp.ntx
    dele file debet.dbf
    dele file kredit.dbf
    dele file debet.ntx
    dele file kredit.ntx
    setcolor(cmsg)
    BoxTenj(Y_srm-10,X_srm-33,Y_srm+10,X_srm+33,ibox)
    setcolor(cl)
    MSG=aOptions[monotwet]+'Œ…‘Ÿ– '
    @ Y_srm-10,int(X_srm-len(MSG)/2)+1 say MSG
    if monotwet=2
     MsgWrite("— ˆ ™ “  Ž‚Ž„Šˆ ˆ „ŽŠ“Œ…’› ...")
     select 1                                    && —¨éã balans
     zap
     use_dbf=use_path+DocumDIC
     if file(use_dbf)
      select 2                                   && —¨éã ¤®ªã¬¥­âë
      use &use_dbf
      do while .not.eof()
       name_dbf=use_path+alltrim(DOC_DOC)+'.DBF'
       if file(name_dbf)
        select 3
        use &name_dbf
        go bottom
        n=DOCUMER
        zap
        append blank
        repl DOCUMER with n
        use
        select 2
       endif
       skip
      enddo
      use
     endif
     use_dbf=use_path+ProtocDBF
     if file(use_dbf)
      select 2
      use &use_dbf
      zap
      use
     endif
     if SIZE_LIST!=0                     && —¨éã oborot
      MsgWrite("— ˆ ™ “  “‹…‚›… Ž‘’€’Šˆ ‚ Ž„€‡„…‹…ˆŸ• ...")
      for i=1 to SIZE_LIST
       select 3
       name_dbf=use_path+strline(list_dbf[i])+".DIV"
       use &name_dbf
       do while .not.eof()
        name_dbf=use_path+strline(C->D1)+"."+list_ext[i]
        select 5
        use &name_dbf
        delete for QUAN=0
        pack                              && ®ç¨áâ¨âì ­ã«¥¢ë¥ § ¯¨á¨
        use
        select 3
        skip
       enddo
       select 3
       use
      next
     endif
    endif
    CloseDBF(.T.)
    if monotwet=3
     dbequ(MainDIS,use_temp+MainDIC,use_path+MainDIC,;
           "source->D1=dest->D1","","‘Ž…„ˆŸž  ‘ — … ’ €...")
     select 4
     use_dic=use_temp+MainDIC
     use &use_dic
     do while .not.eof()
      dbequ(SubDIS,use_temp+strline(D->D1)+".DIC",;
            use_path+strline(D->D1)+".DIC","source->D1=dest->D1","sub_nul",;
            "‘Ž…„ˆŸž Ž‘’€’Šˆ ‚ ‘“‘—…’€•...")
      skip
     enddo
     use
     if SIZE_LIST!=0
      for i=1 to SIZE_LIST
       dbequ(DivisionSTR,use_temp+strline(list_dbf[i])+".DIV",;
             use_path+strline(list_dbf[i])+".DIV","source->D1=dest->D1","",;
             "‘ Ž … „ ˆ  Ÿ ž  Ž„€‡„…‹…ˆŸ...")
       select 4        && á®¥¤¨­ïî ¯®¤-­¨ï á ¯à®è«®£® ¬-æ 
       use_dic=use_path+strline(list_dbf[i])+".DIV"
       use &use_dic
       do while .not.eof()
        dbequ(QuanSTR,use_temp+strline(D->D1)+"."+list_ext[i],;
             use_path+strline(D->D1)+"."+list_ext[i],;
             "str(source->NUMB)+source->NAME+source->MEAS==str(dest->NUMB)+dest->NAME+dest->MEAS","div_nul",;
             "‘Ž…„ˆŸž Ž‘’€’Šˆ ‚ Ž„€‡„…‹…ˆŸ• ...")
        name_dbf=use_temp+strline(D->D1)+"."+list_ext[i]
        select 5
        use &name_dbf
        delete for QUAN=0
        pack                              && ®ç¨áâ¨âì ­ã«¥¢ë¥ § ¯¨á¨
        use
        select 4
        skip
       enddo
       use
      next
      select 7        && „Ž€‚‹Ÿž …„Ž‘’€ž™ˆ… Š€’Ž—Šˆ ‚ Ž„€‡„…‹…ˆŸ
      use_dbf=use_temp+OborotDBF
      use &use_dbf
      for i=1 to SIZE_LIST
       select 2
       name_dbf=use_temp+strline(list_dbf[i])+".DIV"
       use &name_dbf
       do while .not.eof()
        name_dbf=use_temp+strline(B->D1)+"."+list_ext[i]
        select 3
        use &name_dbf SHARED
        select 7
        set filter to list_dbf[i]=G->DEBKRE
        go top
        do while .not.eof()
         if G->EXPENSES=B->D1.or.G->RECEIPTS=B->D1
          select 3
          locate for G->NUMB=C->NUMB.and.G->NAME==C->NAME.and.G->MEAS==C->MEAS
          if .not.found()
           append blank
           repl C->NUMB with G->NUMB
           repl C->NAME with G->NAME
           repl C->MEAS with G->MEAS
          endif
          select 7
         endif
         skip
        enddo
        select 3
        use
        select 2
        skip
       enddo
       select 2
       use
      next
      select 7
      use
     endif
     deles("*.*",bal_path)
     bal_path=bal_temp
     use_path=use_temp
    endif
    is_setj=old_setj
    OpenDBF(.T.)
    redraw=.T.
    RETR=0
    WarnWrite(PMSG13)
   else
    WarnWrite(PMSG2_2)
    is_no=.T.
   endif
  else
   is_no=.T.
  endif
endif
if is_no
 repl I->T_DATE with old_date
 restore screen from InpPage
 DrawDate(Y_min,X_max-24,Y_min+2,X_max)
 save screen to InpPage
endif
restore screen from InpPage
setcolor(old_color)
return
*---------------------------------------------------------*
procedure sub_nul
repl SALBED with 0
repl SALBEK with 0
return
*---------------------------------------------------------*
procedure div_nul
repl QUAN with 0
repl DEBET with 0
repl KREDIT with 0
return
*---------------------------------------------------------*
procedure ReExpRec
parameters re_loc,proc1,proc2
select 2
set filter to &re_loc
go top
do while .not.eof()
 WinSDB(Y_srm-9,X_srm,Y_srm+9,X_srm+32,"substr(B->NAME,1,31)",;
        "' ¨¬-­¨©: '+alltrim(str(reccount()))+'  ¨¬-­¨¥: '+alltrim(str(recno()))")
 if .not.A->FUTURE
  select 5
  seek str(B->NUMB)+B->NAME+B->MEAS
  if found()
   if proc1
    repl QUAN with QUAN+B->QUAN
    if B->DEBKRE=A->DEBET
     repl DEBET with DEBET+A->SUMMA
    else
     repl KREDIT with KREDIT-A->SUMMA
    endif
   else
    repl QUAN with QUAN-B->QUAN
    if B->DEBKRE=A->KREDIT
     repl DEBET with DEBET-A->SUMMA
    else
     repl KREDIT with KREDIT+A->SUMMA
    endif
   endif
   if empty(EXPENS)
    repl EXPENS with A->DEBET
   endif
   if empty(SUBEXP)
    repl SUBEXP with A->SUBDEB
   endif
   if empty(RECEIP)
    repl RECEIP with A->KREDIT
   endif
   if empty(SUBREC)
    repl SUBREC with A->SUBKRE
   endif
   if empty(DATBEG)
    repl DATBEG with B->DATBEG
   endif
   if empty(DATEKS)
    repl DATEKS with B->DATEKS
   endif
   if empty(DATPER)
    repl DATPER with B->DATPER
   endif
   if empty(DATEND)
    repl DATEND with B->DATEND
   endif
   if empty(PROBEG)
    repl PROBEG with B->PROBEG
   endif
   if empty(PROTEK)
    repl PROTEK with B->PROTEK
   endif
   if .not.empty(PROTEK)
    repl PROBEG with PROBEG+PROTEK
    repl PROTEK with 0
   endif
   if empty(ASHIFR)
    repl ASHIFR with B->ASHIFR
   endif
   if empty(ANORMA)
    repl ANORMA with B->ANORMA
   endif
   if empty(KOEF)
    repl KOEF with B->KOEF
   endif
   if empty(NOTES)
    repl NOTES with B->NOTES
   endif
  else
   if .not.empty(B->NAME)
    append blank
    repl NUMB with B->NUMB
    repl NAME with B->NAME
    repl MEAS with B->MEAS
    repl QUAN with proc2
    repl DEBET with A->SUMMA
    repl EXPENS with A->DEBET
    repl RECEIP with A->KREDIT
    repl SUBREC with A->SUBKRE
    repl SUBEXP with A->SUBDEB
    repl DATBEG with B->DATBEG
    repl DATEKS with B->DATEKS
    repl DATPER with B->DATPER
    repl DATEND with B->DATEND
    repl PROBEG with B->PROBEG
    repl PROTEK with B->PROTEK
    repl ASHIFR with B->ASHIFR
    repl ANORMA with B->ANORMA
    repl KOEF   with B->KOEF
    repl NOTES  with B->NOTES
   endif
  endif
  select 2
 endif
 skip
enddo
return
