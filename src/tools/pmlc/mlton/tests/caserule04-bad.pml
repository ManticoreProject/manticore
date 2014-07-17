_primcode (
  (* this should error and complain that varpat : 'a must come last *)
  fun id1 () -> 'a = case nullVP of
      long.con.id => return ()
   |  varpat : 'a => return()
   |  nullVP => return ()
  end;
)
