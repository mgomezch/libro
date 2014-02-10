#!/usr/bin/env bash

(
  createdb SCD &&
  for i in Client Order Product BillItem
  do
    psql SCD -f "$i".sql
  done
)
