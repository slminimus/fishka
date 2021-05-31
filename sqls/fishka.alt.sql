
create or alter function ASUUID(
    STR XGUID_STR
)returns XGUID deterministic
as
begin
  if(:Str = '' or :Str is null) then
    return null;
  if (char_length(:Str) = 36) then
    return char_to_uuid(:Str);
  if (char_length(:Str) = 16) then
    return :Str;
  exception E_INVALID_VALUE 'ASUUID: недопустимое значение параметра: "'|| :STR ||'"';
end;
GRANT USAGE ON EXCEPTION E_INVALID_VALUE TO FUNCTION ASUUID;
grant execute on function ASUUID to ARTIX, USAGE_ROLE;
---------------------------------------------------------

create or alter function BITGET(
    AVALUE XINT,
    BIT XINT
)returns XINT deterministic
as
begin
  return sign(bin_and(:AVALUE,bin_shl(1,:BIT)));
end;
grant execute on function BITGET to PUBLIC;
----------------------------------------------

create or alter function BITSET(
    AVALUE XINT,
    BIT XINT,
    STATE XINT
)returns XINT deterministic
as
begin
  if (:STATE = 0) then
    return bin_and(:AVALUE,bin_not(bin_shl(1,:BIT)));
  else
    return bin_or(:AVALUE,bin_shl(1,:BIT));
end;
grant execute on function BITSET to public;
---------------------------------------------------------

create or alter procedure GET_ENTITIES_INFO(
   ENTITY  XGUID_STR
  ,SYSOPER type of column SYS_OPERS.OPER = null
)returns(
   OPER   type of column SYS_OPERS.OPER
  ,OPTYPE type of column SYS_OPERS.OPERTYPE
  ,SQL    type of column ENTITY_OPERS.SQL
)as
begin
  :ENTITY = ASUUID(:ENTITY);
  for
    select p.OPER,p.OPERTYPE,t.SQL
    from ENTITY_OPERS t
    join SYS_OPERS p on p.OPER = t.OPER
    where t.ENTITY = :ENTITY
      and (:SYSOPER is null or upper(p.OPER) = upper(:SYSOPER))
    into :OPER,:OPTYPE,:SQL
  do
    suspend;
end;
grant execute on procedure GET_ENTITIES_INFO to ARTIX, USAGE_ROLE;
GRANT EXECUTE ON FUNCTION ASUUID TO PROCEDURE GET_ENTITIES_INFO;
GRANT SELECT ON ENTITY_OPERS TO PROCEDURE GET_ENTITIES_INFO;
GRANT SELECT ON SYS_OPERS TO PROCEDURE GET_ENTITIES_INFO;
---------------------------------------------------------

create or alter view ENTITIES$VW
as
  select t.ID,t.NAME,t.DESCR
  from ENTITIES t
;
grant select on ENTITIES$VW to ARTIX, USAGE_ROLE;
grant select on ENTITIES to view ENTITIES$VW;
---------------------------------------------------------

create or alter procedure ENTITIES$EDIT(
   OPER$  XINT
  ,IID    type of column  ENTITIES.ID    = null
  ,NAME   type of column  ENTITIES.NAME  = null
  ,DESCR  type of column  ENTITIES.DESCR = null
)returns(
  ID  XGUID
)as
begin
  if (:OPER$ = 0) then
  begin
    :IID = coalesce(:IID, gen_uuid());
    insert into ENTITIES(ID,NAME,DESCR) values(:IID,:NAME,:DESCR);
    if (row_count > 0) then
      :ID = :IID;
  end else
  if (:OPER$ = 1) then
  begin
    update ENTITIES t set
       NAME  = coalesce(:NAME, t.NAME)
      ,DESCR = coalesce(:DESCR, t.DESCR)
    where t.ID = :IID;
    if (row_count > 0) then
      :ID = :IID;
  end else
  if (:OPER$ = 2) then
  begin
    :ID = :IID;
    delete from ENTITIES where ID = :IID;
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
    ,SYSOPER  type of column SYS_OPERS.OPER
    ,OPERTYPE type of column SYS_OPERS.OPERTYPE = null
    ,DESCR    type of column SYS_OPERS.DESCR    = null
)returns(
  OPER type of column SYS_OPERS.OPER
)as
begin
  if (:OPER$ = 0) then
  begin
    insert into SYS_OPERS(OPER,OPERTYPE,DESCR) values(:SYSOPER,:OPERTYPE,:DESCR);
    if (row_count > 0) then
      :OPER = :SYSOPER;
  end else
  if (:OPER$ = 1) then
  begin
    update SYS_OPERS t set
       OPERTYPE = coalesce(:OPERTYPE, t.OPERTYPE)
      ,DESCR = coalesce(:DESCR, t.DESCR)
    where t.OPER = :SYSOPER;
    if (row_count > 0) then
      :OPER = :SYSOPER;
  end else
  if (:OPER$ = 2) then
  begin
    :OPER = :SYSOPER;
    delete from SYS_OPERS t where t.OPER = :SYSOPER;
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
    ,IID    type of column ENTITY_OPERS.ID     = null
    ,ENTITY type of column ENTITY_OPERS.ENTITY = null
    ,OPER   type of column ENTITY_OPERS.OPER   = null
    ,SQL    type of column ENTITY_OPERS.SQL    = null
)returns(
  ID  XGUID
)as
begin
  if (:OPER$ = 0) then
  begin
    :IID = coalesce(:IID, gen_uuid());
    insert into ENTITY_OPERS(ID,ENTITY,OPER,SQL) values(:IID,:ENTITY,:OPER,:SQL);
    if (row_count > 0) then
      :ID = :IID;
  end else
  if (:OPER$ = 1) then
  begin
    update ENTITY_OPERS t set
       ENTITY = coalesce(:ENTITY, t.ENTITY)
      ,OPER = coalesce(:OPER, t.OPER)
      ,SQL = coalesce(:SQL, t.SQL)
    where t.ID = :IID;
    if (row_count > 0) then
      :ID = :IID;
  end else
  if (:OPER$ = 2) then
  begin
    :ID = :IID;
    delete from ENTITY_OPERS where ID = :IID;
  end else
    exception E_INVALID_OPER;
  suspend;
end;
grant execute on procedure ENTITY_OPERS$EDIT to ARTIX, USAGE_ROLE;
GRANT USAGE ON EXCEPTION E_INVALID_OPER TO PROCEDURE ENTITY_OPERS$EDIT;
GRANT SELECT,INSERT,DELETE,UPDATE ON ENTITY_OPERS TO PROCEDURE ENTITY_OPERS$EDIT;
------------------------

create or alter view MAINTREE$VW
as
  select t.ID,t.NAME,t.PARENT,t.TAG,t.ENTITY
  from MAINTREE t
;
grant select on MAINTREE$VW to ARTIX, USAGE_ROLE;
GRANT SELECT ON MAINTREE TO VIEW MAINTREE$VW;
------------------------

create or alter procedure MAINTREE$EDIT(
     OPER$    XINT
    ,IID     type of column MAINTREE.ID
    ,NAME    type of column MAINTREE.NAME
    ,PARENT  type of column MAINTREE.PARENT
    ,TAG     type of column MAINTREE.TAG
    ,ENTITY  type of column MAINTREE.ENTITY
)returns(
  ID  XGUID
)as
begin
  if (:OPER$ = 0) then
  begin
    :IID = coalesce(:IID, gen_uuid());
    insert into MAINTREE(ID,NAME,PARENT,TAG,ENTITY) values(:IID,:NAME,:PARENT,:TAG,:ENTITY);
    if (row_count > 0) then
      :ID = :IID;
  end else
  if (:OPER$ = 1) then
  begin
    update MAINTREE t set
       NAME = coalesce(:NAME, t.NAME)
      ,PARENT = coalesce(:PARENT, t.PARENT)
      ,TAG = coalesce(:TAG, t.TAG)
      ,ENTITY = coalesce(:ENTITY, t.ENTITY)
    where t.ID = :IID;
    if (row_count > 0) then
      :ID = :IID;
  end else
  if (:OPER$ = 2) then
  begin
    :ID = :IID;
    delete from MAINTREE where ID = :IID;
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
    ,IID     type of column SYSPRIV.ID
    ,SUBJ    type of column SYSPRIV.SUBJ
    ,ENTOPER type of column SYSPRIV.ENTOPER
)returns(
  ID  XGUID
)as
begin
  if (:OPER$ = 0) then
  begin
    :IID = coalesce(:IID, gen_uuid());
    insert into SYSPRIV(ID,SUBJ,ENTOPER) values(:IID,:SUBJ,:ENTOPER);
    if (row_count > 0) then
      :ID = :IID;
  end else
  if (:OPER$ = 1) then
  begin
    update SYSPRIV t set
       SUBJ = coalesce(:SUBJ, t.SUBJ)
      ,ENTOPER = coalesce(:ENTOPER, t.ENTOPER)
    where t.ID = :IID;
    if (row_count > 0) then
      :ID = :IID;
  end else
  if (:OPER$ = 2) then
  begin
    :ID = :IID;
    delete from SYSPRIV where ID = :IID;
  end else
    exception E_INVALID_OPER;
  suspend;
end;
grant execute on procedure SYSPRIV$EDIT to ARTIX, USAGE_ROLE;
GRANT USAGE ON EXCEPTION E_INVALID_OPER TO PROCEDURE SYSPRIV$EDIT;
GRANT SELECT,INSERT,DELETE,UPDATE ON SYSPRIV TO PROCEDURE SYSPRIV$EDIT;
------------------------

------------------------

