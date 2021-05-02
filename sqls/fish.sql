create or alter function GG(ID DGUID) returns DGUID deterministic
as
begin
  return :ID;
end;
grant execute on function GG to PUBLIC;

create table SQLS(
   ID         DGUID_NN
  ,OPER_TYPE  DINT_NN
  ,OPER       DVCHAR16_NN
  ,NAME       DVCHAR48
  ,SQL        DVCHAR512
);

create or alter view SQLS$VW
as
  select * from SQLS
;
grant select on SQLS$VW to IT,RWORKER;
GRANT SELECT ON SQLS TO VIEW SQLS$VW;


update SQLS t set
t.SQL =
  'select t.ID,t.CARDNUM,t.RELEASED,t.FIO,t.PHONES,t.EMAIL,t.DISCONT,
       t.STATE,t.MODDT,t.MODUSER from CARDS$VWE t where GG(:ID) is null or t.ID = :ID'
where t.OPER = 'Select';
------------------------------------------

delete from SQLS;
insert into SQLS(ID,OPER_TYPE,OPER,NAME,SQL) values(
  char_to_uuid('C798DB28-A30E-40D2-8720-DC475BD2B2A2'),
  0,
  'Select',
  'Чтение',
  'select t.ID,t.CARDNUM,t.RELEASED,t.FIO,t.PHONES,t.EMAIL,t.DISCONT,
       t.STATE,t.MODDT,t.MODUSER from CARDS$VWE t where t.ID = :ID or :ID is null'
);

insert into SQLS(ID,OPER_TYPE,OPER,NAME,SQL) values(
  char_to_uuid('C798DB28-A30E-40D2-8720-DC475BD2B2A2'),
  3,
  'Delete',
  'Удалить',
'select t.ID,t.CARDNUM,t.RELEASED,t.FIO,t.PHONES,t.EMAIL,t.DISCONT,t.STATE,t.MODDT,t.MODUSER
from CARDS$EDITS(2,:ID,:RELEASED,:FIO,:PHONES,:EMAIL,:DISCONT,:STATE) t'
);

insert into SQLS(ID,OPER_TYPE,OPER,NAME,SQL) values(
  char_to_uuid('C798DB28-A30E-40D2-8720-DC475BD2B2A2'),
  2,
  'Update',
  'Исправить',
'select t.ID,t.CARDNUM,t.RELEASED,t.FIO,t.PHONES,t.EMAIL,t.DISCONT,t.STATE,t.MODDT,t.MODUSER
from CARDS$EDITS(1,:ID,:RELEASED,:FIO,:PHONES,:EMAIL,:DISCONT,:STATE) t'
);

insert into SQLS(ID,OPER_TYPE,OPER,NAME,SQL) values(
  char_to_uuid('C798DB28-A30E-40D2-8720-DC475BD2B2A2'),
  1,
  'Insert',
  'Исправить',
'select t.ID,t.CARDNUM,t.RELEASED,t.FIO,t.PHONES,t.EMAIL,t.DISCONT,t.STATE,t.MODDT,t.MODUSER
from CARDS$EDITS(0,:ID,:RELEASED,:FIO,:PHONES,:EMAIL,:DISCONT,:STATE) t'
);

commit work;

