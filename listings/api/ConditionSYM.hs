class ConditionSYM repr where
  false :: repr
  true  :: repr
  neg   :: repr   -> repr
  and   :: [repr] -> repr
  or    :: [repr] -> repr
