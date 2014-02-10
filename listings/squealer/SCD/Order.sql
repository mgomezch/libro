begin;
/*{{{*//*{{{*//* "Order" schema */
/*}}}*/
/*{{{*/create schema "Order";
/*}}}*/
/*{{{*//*{{{*//* Row versioning backend */
/*}}}*/
/*{{{*//*{{{*//* Row identification */
/*}}}*/
create table "Order"."identity"
  ( "identity" bigserial not null primary key
  , "code" text not null
  , unique ("code")
  )
;/*}}}*/
/*{{{*//*{{{*//* Row version journal */
/*}}}*/
create table "Order"."journal"
  ( "entry"     bigserial                not null primary key
  , "identity"  bigint                   not null references "Order"."identity"
  , "timestamp" timestamp with time zone not null default now()

  , unique ("entry", "timestamp")
  , unique ("entry", "identity" )
  )
;/*}}}*/
/*{{{*//*{{{*//* Row version revocation */
/*}}}*/
create table "Order"."revocation"
  ( "entry"           bigint                   not null primary key references "Order"."journal"
  , "start timestamp" timestamp with time zone not null -- redundant but required for time-efficient integrity
  , "end timestamp"   timestamp with time zone not null default now()

  , check ("start timestamp" <= "end timestamp")
  , unique ("entry", "end timestamp")
  , foreign key ("entry", "start timestamp") references "Order"."journal" ("entry", "timestamp")
  )
;/*}}}*/
/*{{{*//*{{{*//* Row version succession */
/*}}}*/
create table "Order"."succession"
  ( "entry"     bigint                   not null primary key references "Order"."revocation"
  , "successor" bigint                   not null unique      references "Order"."journal"
  , "timestamp" timestamp with time zone not null -- redundant but required for time-efficient integrity

  -- succession timestamp equals successor journal entry timestamp
  , unique      ("successor", "timestamp") -- implicit index may make foreign key checks more efficient
  , foreign key ("successor", "timestamp") references "Order"."journal" ("entry", "timestamp")

  -- revocation end timestamp equals successor journal entry creation timestamp
  , unique      ("entry", "timestamp") -- implicit index may make foreign key checks more efficient
  , foreign key ("entry", "timestamp") references "Order"."revocation" ("entry", "end timestamp")

  )
;/*}}}*/
/*{{{*//*{{{*//* Active row version tracking */
/*}}}*/
create table "Order"."active"
  ( "identity" bigint not null primary key references "Order"."identity"
  , "entry"    bigint not null unique      references "Order"."journal"

  , unique      ("identity", "entry") -- implicit index may make foreign key checks more efficient
  , foreign key ("identity", "entry") references "Order"."journal" ("identity", "entry")
  )
;/*}}}*//*}}}*/
/*{{{*//*{{{*//* Attributes */
/*}}}*/
/*{{{*//*{{{*//* "timestamp" */
/*}}}*/
/*{{{*/create table "Order"."timestamp state"
  ( "timestamp state" bigserial not null primary key
  , "timestamp" timestamp with time zone not null
  )
;
/*}}}*/
/*{{{*/create table "Order"."timestamp proxy"
  ( "entry" bigint not null primary key references "Order"."journal"
  , "timestamp state" bigint not null references "Order"."timestamp state"
  )
;
/*}}}*//*}}}*/
/*{{{*//*{{{*//* "status" */
/*}}}*/
/*{{{*/create table "Order"."status state"
  ( "status state" bigserial not null primary key
  , "status" text not null
  )
;
/*}}}*/
/*{{{*/create table "Order"."status proxy"
  ( "entry" bigint not null primary key references "Order"."journal"
  , "status state" bigint not null references "Order"."status state"
  )
;
/*}}}*//*}}}*/
/*{{{*//*{{{*//* "client" */
/*}}}*/
/*{{{*/create table "Order"."client reference"
  ( "entry" bigint not null primary key references "Order"."journal"
  , "client reference" bigint not null references "Client"."journal" ("entry") deferrable initially deferred
  )
;
/*}}}*//*}}}*//*}}}*/
/*{{{*//*{{{*//* Frontend */
/*}}}*/
/*{{{*//*{{{*//* Version view */
/*}}}*/
create view "Order"."version" as
  select
    "Order"."journal"."entry",
    "Order"."journal"."timestamp" as "journal timestamp",
    "Order"."revocation"."end timestamp",
    "Order"."succession"."successor",
    "Order"."identity"."code",
    "Order"."timestamp state"."timestamp",
    "Order"."status state"."status",
    "Order"."client reference"."client reference" as "client version",
    "client version view"."code" as "client -> code"
  from "Order"."identity" natural join "Order"."journal"
  left outer join "Order"."revocation" on ("Order"."journal"."entry" = "Order"."revocation"."entry")
  left outer join "Order"."succession" on ("Order"."journal"."entry" = "Order"."succession"."entry")
  left outer join (
    "Order"."timestamp proxy" natural join "Order"."timestamp state"
  ) on ("Order"."journal"."entry" = "Order"."timestamp proxy"."entry")
  left outer join (
    "Order"."status proxy" natural join "Order"."status state"
  ) on ("Order"."journal"."entry" = "Order"."status proxy"."entry")
  left outer join (
    "Order"."client reference" inner join "Client"."version" as "client version view"
    on ("Order"."client reference"."client reference" = "client version view"."entry")
  ) on ("Order"."journal"."entry" = "Order"."client reference"."entry")
;/*}}}*/
/*{{{*//*{{{*//* Transactional view */
/*}}}*/
/*{{{*/create view public."Order" as
  select
    "Order"."identity"."code",
    "Order"."timestamp state"."timestamp",
    "Order"."status state"."status",
    "Order"."client reference"."client reference" as "client version",
    "client version view"."code" as "client -> code"
  from "Order"."active" natural join "Order"."identity" natural join "Order"."journal"
  left outer join (
    "Order"."timestamp proxy" natural join "Order"."timestamp state"
  ) on ("Order"."journal"."entry" = "Order"."timestamp proxy"."entry")
  left outer join (
    "Order"."status proxy" natural join "Order"."status state"
  ) on ("Order"."journal"."entry" = "Order"."status proxy"."entry")
  left outer join (
    "Order"."client reference" inner join "Client"."version" as "client version view"
    on ("Order"."client reference"."client reference" = "client version view"."entry")
  ) on ("Order"."journal"."entry" = "Order"."client reference"."entry")
;
/*}}}*/
/*{{{*//*{{{*//* Row version tracking triggers */
/*}}}*/
/*{{{*//*{{{*//* Insert into view */
/*}}}*/
/*{{{*//*{{{*//* Function */
/*}}}*/
create function "Order"."view insert"
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
      select     "Order"."identity"."identity"
      into       "new identity"
      from       "Order"."identity"
      where      "Order"."identity"."code" = new."code"
      ;

      if not found then
        insert into "Order"."identity"
          ("code") values
          (new."code")
        returning "Order"."identity"."identity"
        into "new identity"
        ;
      end if;

      insert into "Order"."journal"
        (    "identity") values
        ("new identity")
      returning "Order"."journal"."entry" into "new entry"
      ;

      insert into "Order"."active"
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
instead of insert on public."Order"
for each row execute procedure "Order"."view insert"();/*}}}*//*}}}*/
/*{{{*//*{{{*//* Delete from view */
/*}}}*/
/*{{{*//*{{{*//* Function */
/*}}}*/
create function "Order"."delete function"
  ()
returns trigger
  language 'plpgsql'
  security definer
as
  $body$
    begin
      insert into  "Order"."revocation" ("entry", "start timestamp")
      select       "Order"."journal"."entry", "Order"."journal"."timestamp"
      from         "Order"."active"
      natural join "Order"."identity"
      natural join "Order"."journal"
      where        "Order"."identity"."code" = old."code"
      ;

      delete from "Order"."active"
      using       "Order"."identity" natural join "Order"."journal"
      where       "Order"."active"."entry" = "Order"."journal"."entry"
      and         "Order"."identity"."code" = old."code"
      ;

      return old;
    end;
  $body$
;/*}}}*/
/*{{{*//*{{{*//* Trigger */
/*}}}*/
create trigger "00 delete"
instead of delete on public."Order"
for each row execute procedure "Order"."delete function"();/*}}}*//*}}}*/
/*{{{*//*{{{*//* Update view */
/*}}}*/
/*{{{*//*{{{*//* Function */
/*}}}*/
create function "Order"."update function"
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

      select "Order"."active"."entry"
      into   "old entry"
      from   "Order"."active" natural join "Order"."identity"
      where  "Order"."identity"."code" = old."code"
      ;

      delete from public."Order"
      where       public."Order"."code" = old."code"
      ;

      select "Order"."identity"."identity"
      into   "new identity"
      from   "Order"."identity"
      where  "Order"."identity"."code" = new."code"
      ;
      if not found then
        insert into "Order"."identity"
          ("code") values
          (new."code")
        returning "Order"."identity"."identity"
        into "new identity"
        ;
      end if;

      insert into "Order"."journal"
        (    "identity") values
        ("new identity")
      returning "Order"."journal"."entry"
      into "new entry"
      ;

      insert into "Order"."active"
        (    "identity",     "entry") values
        ("new identity", "new entry")
      ;

      insert into "Order"."succession" ("entry", "successor", "timestamp")
      select      "old entry", "new entry", "Order"."revocation"."end timestamp"
      from        "Order"."revocation"
      where       "Order"."revocation"."entry" = "old entry"
      ;

      return new;
    end;
  $body$
;/*}}}*/
/*{{{*//*{{{*//* Trigger */
/*}}}*/
create trigger "00 update"
instead of update on public."Order"
for each row execute procedure "Order"."update function"();/*}}}*//*}}}*//*}}}*/
/*{{{*//*{{{*//* Column triggers */
/*}}}*/
/*{{{*//*{{{*//* "timestamp" */
/*}}}*/
/*{{{*//*{{{*//* Function */
/*}}}*/
create function "Order"."insert or update timestamp function"
  ()
returns trigger
  language 'plpgsql'
  security definer
as
  $body$
    declare
      "new timestamp state" bigint;
    begin
      if
        new."timestamp" is not null
      then
        if
          tg_op = 'INSERT'
          or not (old."timestamp" is not null and old."timestamp" = new."timestamp")
        then
          insert into "Order"."timestamp state"
            (    "timestamp") values
            (new."timestamp")
          returning   "Order"."timestamp state"."timestamp state"
          into        "new timestamp state"
          ;
        else
          select     "Order"."timestamp proxy"."timestamp state"
          into       "new timestamp state"
          from       "Order"."identity" natural join "Order"."active" natural join "Order"."journal"
          inner join "Order"."succession" on ("Order"."journal"."entry" = "Order"."succession"."successor")
          inner join "Order"."timestamp proxy" on ("Order"."succession"."entry" = "Order"."timestamp proxy"."entry")
          where      "Order"."identity"."code" = new."code"
          ;
        end if;

        insert into  "Order"."timestamp proxy" ("entry", "timestamp state")
        select       "Order"."active"."entry", "new timestamp state"
        from         "Order"."identity" inner join "Order"."active" using ("identity")
        where        "Order"."identity"."code" = new."code"
        ;
      end if;

      return new;
    end;
  $body$
;/*}}}*/
/*{{{*//*{{{*//* Trigger */
/*}}}*/
create trigger "10 insert or update timestamp"
instead of insert or update on public."Order"
for each row execute procedure "Order"."insert or update timestamp function"();/*}}}*//*}}}*/
/*{{{*//*{{{*//* "status" */
/*}}}*/
/*{{{*//*{{{*//* Function */
/*}}}*/
create function "Order"."insert or update status function"
  ()
returns trigger
  language 'plpgsql'
  security definer
as
  $body$
    declare
      "new status state" bigint;
    begin
      if
        new."status" is not null
      then
        if
          tg_op = 'INSERT'
          or not (old."status" is not null and old."status" = new."status")
        then
          insert into "Order"."status state"
            (    "status") values
            (new."status")
          returning   "Order"."status state"."status state"
          into        "new status state"
          ;
        else
          select     "Order"."status proxy"."status state"
          into       "new status state"
          from       "Order"."identity" natural join "Order"."active" natural join "Order"."journal"
          inner join "Order"."succession" on ("Order"."journal"."entry" = "Order"."succession"."successor")
          inner join "Order"."status proxy" on ("Order"."succession"."entry" = "Order"."status proxy"."entry")
          where      "Order"."identity"."code" = new."code"
          ;
        end if;

        insert into  "Order"."status proxy" ("entry", "status state")
        select       "Order"."active"."entry", "new status state"
        from         "Order"."identity" inner join "Order"."active" using ("identity")
        where        "Order"."identity"."code" = new."code"
        ;
      end if;

      return new;
    end;
  $body$
;/*}}}*/
/*{{{*//*{{{*//* Trigger */
/*}}}*/
create trigger "10 insert or update status"
instead of insert or update on public."Order"
for each row execute procedure "Order"."insert or update status function"();/*}}}*//*}}}*/
/*{{{*//*{{{*//* "client" */
/*}}}*/
/*{{{*//*{{{*//* Insert into view */
/*}}}*/
/*{{{*//*{{{*//* Function */
/*}}}*/
create function "Order"."insert client function"
  ()
returns trigger
  language 'plpgsql'
  security definer
as
  $body$
    begin
      if new."client version" is not null
      then
        raise exception 'insertions into % view must not specify % version', 'Order', 'client';
      end if;

      if new."client -> code" is not null then
        insert into "Order"."client reference" ("entry", "client reference")
        select      "Order"."active"."entry", "Client"."active"."entry"
        from        "Order"."identity" natural join "Order"."active",
                    "Client"."identity" natural join "Client"."active"
        where       "Order"."identity"."code" = new."code"
      and         "Client"."identity"."code" = new."client -> code"
        ;
        if not found then
          raise exception 'no active % row matches insert into % table % reference', 'Client', 'Order', 'client';
        end if;
      end if;

      return new;
    end;
  $body$
;/*}}}*/
/*{{{*//*{{{*//* Trigger */
/*}}}*/
create trigger "10 insert client"
instead of insert on public."Order"
for each row execute procedure "Order"."insert client function"();/*}}}*//*}}}*/
/*{{{*//*{{{*//* Update view */
/*}}}*/
/*{{{*//*{{{*//* Function */
/*}}}*/
create function "Order"."update client function"
  ()
returns trigger
  language 'plpgsql'
  security definer
as
  $body$
    begin
      if
        new."client version" is not null
        and (
          not (old."client version" is not null)
          or old."client version" <> new."client version"
        )
      then
        raise exception 'updates to % view must not set % version to non-null values', 'Order', 'client';

      elsif (
        -- If the referred identity did not change, and the referred version was set to null, the user requested updating the reference to the currently active version of the same row (“same” by identity).
        old."client version" is not null
        and not (new."client version" is not null)
        and new."client -> code" is not null and old."client -> code" = new."client -> code"

        -- If the referred version did not change, but the referred identity did, the user requested making the reference point to the currently active version of another row (“another” by identity).
      ) or (new."client -> code" is not null
        and (
          not (old."client -> code" is not null) or old."client -> code" <> new."client -> code"
        )
      ) then
        -- In either case, find the currently active version of the requested row and establish the reference.
        insert into "Order"."client reference" ("entry", "client reference")
        select      "Order"."active"."entry", "Client"."active"."entry"
        from        "Order"."identity" natural join "Order"."active",
                    "Client"."identity" natural join "Client"."active"
        where       "Order"."identity"."code" = new."code"
        and         "Client"."identity"."code" = new."client -> code"
        ;
        if not found then
          raise exception 'no active % row matches update to % table % reference', 'Client', 'Order', 'client';
        end if;

      -- If the reference was unchanged in this update, and a reference actually existed (it was not null), then the new referrer version should refer to the same referred version as the old version.  This works just like regular attributes: the proxy pointer is copied in the new version if it exists.
      elsif
        old."client version" is not null and new."client version" is not null
        and old."client version" = new."client version"
        and new."client -> code" is not null
        and old."client -> code" = new."client -> code"
      then
        insert into "Order"."client reference" ("entry", "client reference")
        select      "Order"."active"."entry", new."client version"
        from        "Order"."identity" natural join "Order"."active"
        where       "Order"."identity"."code" = new."code"
        ;
        -- FIXME: what if the referenced entity version is no longer active?  should this restrict, leave the reference as-is, or try to update it?
        -- FIXME: is it possible for the referenced entity version to no longer be active if this is a proper covariant reference with on delete/update cascade/restrict triggers?
      end if;

      return new;
    end;
  $body$
;/*}}}*/
/*{{{*//*{{{*//* Trigger */
/*}}}*/
create trigger "10 update client"
instead of update on public."Order"
for each row execute procedure "Order"."update client function"();/*}}}*//*}}}*//*}}}*//*}}}*/
/*{{{*//*{{{*//* Reference triggers */
/*}}}*/
/*{{{*//*{{{*//* cascade on update to "client" */
/*}}}*/
/*{{{*//*{{{*//* Function */
/*}}}*/
create function "Order"."cascade update on Order view client reference"
  ()
returns trigger
  language 'plpgsql'
  security definer
as
  $body$
    begin
      update public."Order"
      set
        ( "client version"
        , "client -> code"
        )
      = ( null
        , "Client"."version"."code"
        )
      from  "Client"."version"
      where new."entry" = public."Order"."client version"
      and   new."entry" = "Client"."version"."entry"
      ;

      return new;
    end;
  $body$
;/*}}}*/
/*{{{*//*{{{*//* Trigger */
/*}}}*/
create trigger "20 cascade update on Order view client reference"
after insert on "Client"."succession"
for each row execute procedure "Order"."cascade update on Order view client reference"();/*}}}*//*}}}*/
/*{{{*//*{{{*//* restrict on delete to "client" */
/*}}}*/
/*{{{*//*{{{*//* Function */
/*}}}*/
create function "Order"."restrict delete on Order view client reference"
  ()
returns trigger
  language 'plpgsql'
  security definer
as
  $body$
    begin
      perform *
      from    public."Order"
      where   public."Order"."client version" = new."entry"
      limit   1
      ;
      if found then
        raise exception '% on % table breaks % table % reference'
        , 'delete'
        , 'Client'
        , 'Order'
        , 'client'
        ;
      end if;

      return new;
    end;
  $body$
;/*}}}*/
/*{{{*//*{{{*//* Trigger */
/*}}}*/
create constraint trigger "20 restrict delete on Order view client reference"
after insert on "Client"."revocation"
deferrable initially deferred
for each row execute procedure "Order"."restrict delete on Order view client reference"();/*}}}*//*}}}*//*}}}*//*}}}*//*}}}*//*}}}*/
commit;
