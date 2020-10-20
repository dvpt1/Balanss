*---------------------------------------------------------*
function Musors
if .not.fragen("ˆŽŠ€","Ž—ˆ‘’ˆ’œ “‹…‚›… Ž‚Ž„ŽŠˆ???")
 return .F.
endif
save screen
select 1
set filter to
go top
select 2
name_dbf=use_path+MainDIC
use &name_dbf
select 1
do while .not.eof()
 if SUMMA=0
  select 2
  locate for A->DEBET=D1
  if .not.empty(D_DOC)
   DelRecord(use_path+alltrim(D_DOC)+".DBF")
  endif
  select 2
  locate for A->KREDIT=D1
  if .not.empty(K_DOC)
   DelRecord(use_path+alltrim(K_DOC)+".DBF")
  endif
  DelRecord(use_path+OborotDBF,.F.)
  select 1
  delete
 endif
 skip
enddo
pack
select 2
go top
do while .not.eof()
 if .not.empty(D_DOC)
  file_dbf=use_path+alltrim(D_DOC)+".DBF"
  if file(file_dbf)
   select 3
   use &file_dbf
   delete for NUMBER=0
   pack
   use
   select 2
  endif
 endif
 if .not.empty(K_DOC)
  file_dbf=use_path+alltrim(K_DOC)+".DBF"
  if file(file_dbf)
   select 3
   use &file_dbf
   delete for NUMBER=0
   pack
   use
   select 2
  endif
 endif
 skip
enddo
use
file_dbf=use_path+OborotDBF
if file(file_dbf)
 select 3
 use &file_dbf
 delete for NUMBER=0
 pack
 use
endif
select 1
InitFilter()
restore screen
RETR=2
return .T.
