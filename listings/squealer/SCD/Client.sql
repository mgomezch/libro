begin;
/*{{{*//*{{{*//* "Client" schema */
/*}}}*/
/*{{{*/create schema "Client";
/*}}}*/
/*{{{*//*{{{*//* Row versioning backend */
/*}}}*/
/*{{{*//*{{{*//* Row identification */
/*}}}*/
create table "Client"."identity"
  ( "identity" bigserial not null primary key
  , "code" text not null
  , unique ("code")
  )
;/*}}}*/
/*{{{*//*{{{*//* Row version journal */
/*}}}*/
create table "Client"."journal"
  ( "entry"     bigserial                not null primary key
  , "identity"  bigint                   not null references "Client"."identity"
  , "timestamp" timestamp with time zone not null default now()

  , unique ("entry", "timestamp")
  , unique ("entry", "identity" )
  )
;/*}}}*/
/*{{{*//*{{{*//* Row version revocation */
/*}}}*/
create table "Client"."revocation"
  ( "entry"           bigint                   not null primary key references "Client"."journal"
  , "start timestamp" timestamp with time zone not null -- redundant but required for time-efficient integrity
  , "end timestamp"   timestamp with time zone not null default now()

  , check ("start timestamp" <= "end timestamp")
  , unique ("entry", "end timestamp")
  , foreign key ("entry", "start timestamp") references "Client"."journal" ("entry", "timestamp")
  )
;/*}}}*/
/*{{{*//*{{{*//* Row version succession */
/*}}}*/
create table "Client"."succession"
  ( "entry"     bigint                   not null primary key references "Client"."revocation"
  , "successor" bigint                   not null unique      references "Client"."journal"
  , "timestamp" timestamp with time zone not null -- redundant but required for time-efficient integrity

  -- succession timestamp equals successor journal entry timestamp
  , unique      ("successor", "timestamp") -- implicit index may make foreign key checks more efficient
  , foreign key ("successor", "timestamp") references "Client"."journal" ("entry", "timestamp")

  -- revocation end timestamp equals successor journal entry creation timestamp
  , unique      ("entry", "timestamp") -- implicit index may make foreign key checks more efficient
  , foreign key ("entry", "timestamp") references "Client"."revocation" ("entry", "end timestamp")

  )
;/*}}}*/
/*{{{*//*{{{*//* Active row version tracking */
/*}}}*/
create table "Client"."active"
  ( "identity" bigint not null primary key references "Client"."identity"
  , "entry"    bigint not null unique      references "Client"."journal"

  , unique      ("identity", "entry") -- implicit index may make foreign key checks more efficient
  , foreign key ("identity", "entry") references "Client"."journal" ("identity", "entry")
  )
;/*}}}*//*}}}*/
/*{{{*//*{{{*//* Attributes */
/*}}}*/
/*{{{*//*{{{*//* "name" */
/*}}}*/
/*{{{*/create table "Client"."name state"
  ( "name state" bigserial not null primary key
  , "name" text not null
  )
;
/*}}}*/
/*{{{*/create table "Client"."name proxy"
  ( "entry" bigint not null primary key references "Client"."journal"
  , "name state" bigint not null references "Client"."name state"
  )
;
/*}}}*//*}}}*/
/*{{{*//*{{{*//* "date of birth" */
/*}}}*/
/*{{{*/create table "Client"."date of birth state"
  ( "date of birth state" bigserial not null primary key
  , "date of birth" date not null
  )
;
/*}}}*/
/*{{{*/create table "Client"."date of birth proxy"
  ( "entry" bigint not null primary key references "Client"."journal"
  , "date of birth state" bigint not null references "Client"."date of birth state"
  )
;
/*}}}*//*}}}*//*}}}*/
/*{{{*//*{{{*//* Frontend */
/*}}}*/
/*{{{*//*{{{*//* Version view */
/*}}}*/
create view "Client"."version" as
  select
    "Client"."journal"."entry",
    "Client"."journal"."timestamp" as "journal timestamp",
    "Client"."revocation"."end timestamp",
    "Client"."succession"."successor",
    "Client"."identity"."code",
    "Client"."name state"."name",
    "Client"."date of birth state"."date of birth"
  from "Client"."identity" natural join "Client"."journal"
  left outer join "Client"."revocation" on ("Client"."journal"."entry" = "Client"."revocation"."entry")
  left outer join "Client"."succession" on ("Client"."journal"."entry" = "Client"."succession"."entry")
  left outer join (
    "Client"."name proxy" natural join "Client"."name state"
  ) on ("Client"."journal"."entry" = "Client"."name proxy"."entry")
  left outer join (
    "Client"."date of birth proxy" natural join "Client"."date of birth state"
  ) on ("Client"."journal"."entry" = "Client"."date of birth proxy"."entry")
;/*}}}*/
/*{{{*//*{{{*//* Transactional view */
/*}}}*/
/*{{{*/create view public."Client" as
  select
    "Client"."identity"."code",
    "Client"."name state"."name",
    "Client"."date of birth state"."date of birth"
  from "Client"."active" natural join "Client"."identity" natural join "Client"."journal"
  left outer join (
    "Client"."name proxy" natural join "Client"."name state"
  ) on ("Client"."journal"."entry" = "Client"."name proxy"."entry")
  left outer join (
    "Client"."date of birth proxy" natural join "Client"."date of birth state"
  ) on ("Client"."journal"."entry" = "Client"."date of birth proxy"."entry")
;
/*}}}*/
/*{{{*//*{{{*//* Row version tracking triggers */
/*}}}*/
/*{{{*//*{{{*//* Insert into view */
/*}}}*/
/*{{{*//*{{{*//* Function */
/*}}}*/
create function "Client"."view insert"
  ()
returns trigger
  language 'plpgsql'
  security definer
as
  $body$
    declare
      "new identity" bigint;
      "new entry"    bigint;
    begin
      select     "Client"."identity"."identity"
      into       "new identity"
      from       "Client"."identity"
      where      "Client"."identity"."code" = new."code"
      ;

      if not found then
        insert into "Client"."identity"
          ("code") values
          (new."code")
        returning "Client"."identity"."identity"
        into "new identity"
        ;
      end if;

      insert into "Client"."journal"
        (    "identity") values
        ("new identity")
      returning "Client"."journal"."entry" into "new entry"
      ;

      insert into "Client"."active"
        (    "identity",     "entry") values
        ("new identity", "new entry")
      ;

      return new;
    end;
  $body$
;/*}}}*/
/*{{{*//*{{{*//* Trigger */
/*}}}*/
create trigger "00 insert"
instead of insert on public."Client"
for each row execute procedure "Client"."view insert"();/*}}}*//*}}}*/
/*{{{*//*{{{*//* Delete from view */
/*}}}*/
/*{{{*//*{{{*//* Function */
/*}}}*/
create function "Client"."delete function"
  ()
returns trigger
  language 'plpgsql'
  security definer
as
  $body$
    begin
      insert into  "Client"."revocation" ("entry", "start timestamp")
      select       "Client"."journal"."entry", "Client"."journal"."timestamp"
      from         "Client"."active"
      natural join "Client"."identity"
      natural join "Client"."journal"
      where        "Client"."identity"."code" = old."code"
      ;

      delete from "Client"."active"
      using       "Client"."identity" natural join "Client"."journal"
      where       "Client"."active"."entry" = "Client"."journal"."entry"
      and         "Client"."identity"."code" = old."code"
      ;

      return old;
    end;
  $body$
;/*}}}*/
/*{{{*//*{{{*//* Trigger */
/*}}}*/
create trigger "00 delete"
instead of delete on public."Client"
for each row execute procedure "Client"."delete function"();/*}}}*//*}}}*/
/*{{{*//*{{{*//* Update view */
/*}}}*/
/*{{{*//*{{{*//* Function */
/*}}}*/
create function "Client"."update function"
  ()
returns trigger
  language 'plpgsql'
  security definer
as
  $body$
    declare
      "old entry" bigint;
      "new identity" bigint;
      "new entry" bigint;
    begin
      if new."code" is null then
        raise exception 'null value in column % violates not-null constraint', 'code';
      end if;

      select "Client"."active"."entry"
      into   "old entry"
      from   "Client"."active" natural join "Client"."identity"
      where  "Client"."identity"."code" = old."code"
      ;

      delete from public."Client"
      where       public."Client"."code" = old."code"
      ;

      select "Client"."identity"."identity"
      into   "new identity"
      from   "Client"."identity"
      where  "Client"."identity"."code" = new."code"
      ;
      if not found then
        insert into "Client"."identity"
          ("code") values
          (new."code")
        returning "Client"."identity"."identity"
        into "new identity"
        ;
      end if;

      insert into "Client"."journal"
        (    "identity") values
        ("new identity")
      returning "Client"."journal"."entry"
      into "new entry"
      ;

      insert into "Client"."active"
        (    "identity",     "entry") values
        ("new identity", "new entry")
      ;

      insert into "Client"."succession" ("entry", "successor", "timestamp")
      select      "old entry", "new entry", "Client"."revocation"."end timestamp"
      from        "Client"."revocation"
      where       "Client"."revocation"."entry" = "old entry"
      ;

      return new;
    end;
  $body$
;/*}}}*/
/*{{{*//*{{{*//* Trigger */
/*}}}*/
create trigger "00 update"
instead of update on public."Client"
for each row execute procedure "Client"."update function"();/*}}}*//*}}}*//*}}}*/
/*{{{*//*{{{*//* Column triggers */
/*}}}*/
/*{{{*//*{{{*//* "name" */
/*}}}*/
/*{{{*//*{{{*//* Function */
/*}}}*/
create function "Client"."insert or update name function"
  ()
returns trigger
  language 'plpgsql'
  security definer
as
  $body$
    declare
      "new name state" bigint;
    begin
      if
        new."name" is not null
      then
        if
          tg_op = 'INSERT'
          or not (old."name" is not null and old."name" = new."name")
        then
          insert into "Client"."name state"
            (    "name") values
            (new."name")
          returning   "Client"."name state"."name state"
          into        "new name state"
          ;
        else
          select     "Client"."name proxy"."name state"
          into       "new name state"
          from       "Client"."identity" natural join "Client"."active" natural join "Client"."journal"
          inner join "Client"."succession" on ("Client"."journal"."entry" = "Client"."succession"."successor")
          inner join "Client"."name proxy" on ("Client"."succession"."entry" = "Client"."name proxy"."entry")
          where      "Client"."identity"."code" = new."code"
          ;
        end if;

        insert into  "Client"."name proxy" ("entry", "name state")
        select       "Client"."active"."entry", "new name state"
        from         "Client"."identity" inner join "Client"."active" using ("identity")
        where        "Client"."identity"."code" = new."code"
        ;
      end if;

      return new;
    end;
  $body$
;/*}}}*/
/*{{{*//*{{{*//* Trigger */
/*}}}*/
create trigger "10 insert or update name"
instead of insert or update on public."Client"
for each row execute procedure "Client"."insert or update name function"();/*}}}*//*}}}*/
/*{{{*//*{{{*//* "date of birth" */
/*}}}*/
/*{{{*//*{{{*//* Function */
/*}}}*/
create function "Client"."insert or update date of birth function"
  ()
returns trigger
  language 'plpgsql'
  security definer
as
  $body$
    declare
      "new date of birth state" bigint;
    begin
      if
        new."date of birth" is not null
      then
        if
          tg_op = 'INSERT'
          or not (old."date of birth" is not null and old."date of birth" = new."date of birth")
        then
          insert into "Client"."date of birth state"
            (    "date of birth") values
            (new."date of birth")
          returning   "Client"."date of birth state"."date of birth state"
          into        "new date of birth state"
          ;
        else
          select     "Client"."date of birth proxy"."date of birth state"
          into       "new date of birth state"
          from       "Client"."identity" natural join "Client"."active" natural join "Client"."journal"
          inner join "Client"."succession" on ("Client"."journal"."entry" = "Client"."succession"."successor")
          inner join "Client"."date of birth proxy" on ("Client"."succession"."entry" = "Client"."date of birth proxy"."entry")
          where      "Client"."identity"."code" = new."code"
          ;
        end if;

        insert into  "Client"."date of birth proxy" ("entry", "date of birth state")
        select       "Client"."active"."entry", "new date of birth state"
        from         "Client"."identity" inner join "Client"."active" using ("identity")
        where        "Client"."identity"."code" = new."code"
        ;
      end if;

      return new;
    end;
  $body$
;/*}}}*/
/*{{{*//*{{{*//* Trigger */
/*}}}*/
create trigger "10 insert or update date of birth"
instead of insert or update on public."Client"
for each row execute procedure "Client"."insert or update date of birth function"();/*}}}*//*}}}*//*}}}*/
/*{{{*//* Reference triggers */
/*}}}*//*}}}*//*}}}*//*}}}*/
commit;
