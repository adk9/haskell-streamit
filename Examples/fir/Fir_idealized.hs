

-- An example of what we might like to write:

loop strm =
  let window = take n strm in
  sum (zipWith (*) weights window)
   `scons`
  loop (tail strm)


