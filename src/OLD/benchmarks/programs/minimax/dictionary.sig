signature DICTIONARY = sig

  type dictionary
  type key
  type value

  val empty : dictionary
  val lookup : dictionary * key -> value option
  val insert : dictionary * key * value -> dictionary

end
