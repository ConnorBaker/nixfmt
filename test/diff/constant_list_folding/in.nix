# TODO: These rewrites could be broken by parenthesizing.
[
  # Empty list remains empty
  ([ ])
  # Single element list remains unchanged
  ([ a ])
  # Two element list remains unchanged
  ([
    a
    b
  ])
  # Left addition of empty list reduces to right value
  ([ ] ++ [ a ])
  ([ ] ++ a)
  # Right addition of empty list reduces to left value
  ([ a ] ++ [ ])
  (a ++ [ ])
  # Addition of two empty lists reduces to empty list
  ([ ] ++ [ ])
  # Addition of lists of values reduces to list of values
  (
    [ a ]
    ++ [ b ]
    ++ [
      c
      d
      e
    ]
    ++ [ ]
    ++ [ f ]
    ++ [ a ]
  )
  # Addition of lists of values and values reduces to concatLists
  ([ a ] ++ b ++ [ c ] ++ [ d ] ++ e ++ f ++ g)
]
