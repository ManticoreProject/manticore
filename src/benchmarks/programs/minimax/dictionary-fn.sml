functor DictionaryFn (S : sig 
			  type key 
			  type value 
			  val key_eq : key * key -> bool
		      end) : DICTIONARY = struct

  type key = S.key
  type value = S.value

  type dictionary = (key * value) list

  val empty = []

  fun insert (d, k, v) = (k,v)::d

  fun lookup (d, k) = let
    fun g [] = NONE
      | g ((k',v)::t) = if S.key_eq(k,k') then SOME v else g t
    in
      g d
    end

end
