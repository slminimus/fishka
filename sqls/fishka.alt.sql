﻿
create or alter procedure GET_ENTITIES_INFO(
   ENTITY type of column ENTITIES.ID
  ,OPER   type of column SYS_OPERS.NAME = null
)returns(
   OPNAME type of column SYS_OPERS.NAME
  ,OPTYPE type of column SYS_OPERS.OPERTYPE
  ,SQL    type of column ENTITY_OPERS.SQL
)as
begin
  for
    select p.NAME,p.OPERTYPE,t.SQL
    from ENTITY_OPERS t
    join SYS_OPERS p on p.ID = t.OPER
    where t.ENTITY = :ENTITY
      and (:OPER is null or upper(p.NAME) = upper(:OPER))
    into :OPNAME,:OPTYPE,:SQL
  do
    suspend;
end;

create or alter procedure ENTITIES$EDIT(
   OPER$  XINT
  ,ID     type of column  ENTITIES.ID    = null
  ,NAME   type of column  ENTITIES.NAME  = null
  ,DESCR  type of column  ENTITIES.DESCR = null
)returns(
  ROW_ID  XGUID
)as
begin
  if (:OPER$ = 0) then
  begin
    :ID = coalesce(:ID, gen_uuid());
    insert into ENTITIES(ID,NAME,DESCR) values(:ID,:NAME,:DESCR);
    if (row_count = 0) then
      :ROW_ID = :ID;
  end else
  if (:OPER$ = 1) then
  begin
    update ENTITIES t set
       NAME  = coalesce(:NAME, t.NAME)
      ,DESCR = coalesce(:DESCR, t.DESCR)
    where t.ID = :ID;
    if (row_count > 0) then
      :ROW_ID = :ID;
  end else
  if (:OPER$ = 2) then
  begin
    :ROW_ID = :ID;
    delete from ENTITIES where ID = :ID;
  end else
    exception E_INVALID_OPER;
  suspend;
end;
grant execute on procedure ENTITIES$EDIT to ARTIX, USAGE_ROLE;
GRANT USAGE ON EXCEPTION E_INVALID_OPER TO PROCEDURE ENTITIES$EDIT;
GRANT SELECT,INSERT,DELETE,UPDATE ON ENTITIES TO PROCEDURE ENTITIES$EDIT;
-------------------------

create or alter procedure SYS_OPERS$EDIT(
     OPER$    XINT
    ,ID       type of column SYS_OPERS.ID
    ,OPERTYPE type of column SYS_OPERS.OPERTYPE = null
    ,NAME     type of column SYS_OPERS.NAME     = null
    ,DESCR    type of column SYS_OPERS.DESCR    = null
)returns(
  ROW_ID  XGUID
)as
begin
  if (:OPER$ = 0) then
  begin
    :ID = coalesce(:ID, gen_uuid());
    insert into SYS_OPERS(ID,OPERTYPE,NAME,DESCR) values(:ID,:OPERTYPE,:NAME,:DESCR);
    if (row_count = 0) then
      :ROW_ID = :ID;
  end else
  if (:OPER$ = 1) then
  begin
    update SYS_OPERS t set
       OPERTYPE = coalesce(:OPERTYPE, t.OPERTYPE)
      ,NAME  = coalesce(:NAME, t.NAME)
      ,DESCR = coalesce(:DESCR, t.DESCR)
    where t.ID = :ID;
    if (row_count > 0) then
      :ROW_ID = :ID;
  end else
  if (:OPER$ = 2) then
  begin
    :ROW_ID = :ID;
    delete from SYS_OPERS where ID = :ID;
  end else
    exception E_INVALID_OPER;
  suspend;
end;
grant execute on procedure SYS_OPERS$EDIT to ARTIX, USAGE_ROLE;
GRANT USAGE ON EXCEPTION E_INVALID_OPER TO PROCEDURE SYS_OPERS$EDIT;
GRANT SELECT,INSERT,DELETE,UPDATE ON SYS_OPERS TO PROCEDURE SYS_OPERS$EDIT;
------------------------

create or alter procedure ENTITY_OPERS$EDIT(
     OPER$    XINT
    ,ID     type of column ENTITY_OPERS.ID     = null
    ,ENTITY type of column ENTITY_OPERS.ENTITY = null
    ,OPER   type of column ENTITY_OPERS.OPER   = null
    ,SQL    type of column ENTITY_OPERS.SQL    = null
)returns(
  ROW_ID  XGUID
)as
begin
  if (:OPER$ = 0) then
  begin
    :ID = coalesce(:ID, gen_uuid());
    insert into ENTITY_OPERS(ID,ENTITY,OPER,SQL) values(:ID,:ENTITY,:OPER,:SQL);
    if (row_count = 0) then
      :ROW_ID = :ID;
  end else
  if (:OPER$ = 1) then
  begin
    update ENTITY_OPERS t set
       ENTITY = coalesce(:ENTITY, t.ENTITY)
      ,OPER = coalesce(:OPER, t.OPER)
      ,SQL = coalesce(:SQL, t.SQL)
    where t.ID = :ID;
    if (row_count > 0) then
      :ROW_ID = :ID;
  end else
  if (:OPER$ = 2) then
  begin
    :ROW_ID = :ID;
    delete from ENTITY_OPERS where ID = :ID;
  end else
    exception E_INVALID_OPER;
  suspend;
end;
grant execute on procedure ENTITY_OPERS$EDIT to ARTIX, USAGE_ROLE;
GRANT USAGE ON EXCEPTION E_INVALID_OPER TO PROCEDURE ENTITY_OPERS$EDIT;
GRANT SELECT,INSERT,DELETE,UPDATE ON ENTITY_OPERS TO PROCEDURE ENTITY_OPERS$EDIT;
------------------------

create or alter procedure MAINTREE$EDIT(
     OPER$    XINT
    ,ID      type of column MAINTREE.ID
    ,NAME    type of column MAINTREE.NAME
    ,PARENT  type of column MAINTREE.PARENT
    ,ENTITY  type of column MAINTREE.ENTITY
)returns(
  ROW_ID  XGUID
)as
begin
  if (:OPER$ = 0) then
  begin
    :ID = coalesce(:ID, gen_uuid());
    insert into MAINTREE(ID,NAME,PARENT,ENTITY) values(:ID,:NAME,:PARENT,:ENTITY);
    if (row_count = 0) then
      :ROW_ID = :ID;
  end else
  if (:OPER$ = 1) then
  begin
    update MAINTREE t set
       NAME = coalesce(:NAME, t.NAME)
      ,PARENT = coalesce(:PARENT, t.PARENT)
      ,ENTITY = coalesce(:ENTITY, t.ENTITY)
    where t.ID = :ID;
    if (row_count > 0) then
      :ROW_ID = :ID;
  end else
  if (:OPER$ = 2) then
  begin
    :ROW_ID = :ID;
    delete from MAINTREE where ID = :ID;
  end else
    exception E_INVALID_OPER;
  suspend;
end;
grant execute on procedure MAINTREE$EDIT to ARTIX, USAGE_ROLE;
GRANT USAGE ON EXCEPTION E_INVALID_OPER TO PROCEDURE MAINTREE$EDIT;
GRANT SELECT,INSERT,DELETE,UPDATE ON MAINTREE TO PROCEDURE MAINTREE$EDIT;
------------------------

create or alter procedure SYSPRIV$EDIT(
     OPER$   XINT
    ,ID      type of column SYSPRIV.ID
    ,SUBJ    type of column SYSPRIV.SUBJ
    ,ENTOPER type of column SYSPRIV.ENTOPER
)returns(
  ROW_ID  XGUID
)as
begin
  if (:OPER$ = 0) then
  begin
    :ID = coalesce(:ID, gen_uuid());
    insert into SYSPRIV(ID,SUBJ,ENTOPER) values(:ID,:SUBJ,:ENTOPER);
    if (row_count = 0) then
      :ROW_ID = :ID;
  end else
  if (:OPER$ = 1) then
  begin
    update SYSPRIV t set
       SUBJ = coalesce(:SUBJ, t.SUBJ)
      ,ENTOPER = coalesce(:ENTOPER, t.ENTOPER)
    where t.ID = :ID;
    if (row_count > 0) then
      :ROW_ID = :ID;
  end else
  if (:OPER$ = 2) then
  begin
    :ROW_ID = :ID;
    delete from SYSPRIV where ID = :ID;
  end else
    exception E_INVALID_OPER;
  suspend;
end;
grant execute on procedure SYSPRIV$EDIT to ARTIX, USAGE_ROLE;
GRANT USAGE ON EXCEPTION E_INVALID_OPER TO PROCEDURE SYSPRIV$EDIT;
GRANT SELECT,INSERT,DELETE,UPDATE ON SYSPRIV TO PROCEDURE SYSPRIV$EDIT;
------------------------

