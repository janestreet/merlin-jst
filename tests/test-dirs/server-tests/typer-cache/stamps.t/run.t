The server might already be running, we kill it to make sure we start from a
clean slate:
  $ $MERLIN server stop-server

Then we can look at identifier stamps and whether they are being reset between
buffers, and different runs for the same buffer:

  $ echo "let f x = x" | \
  > $MERLIN server dump -what browse -filename test.ml | \
  > sed 's:\\n:\n:g' | grep Tpat_var
<<<<<<< HEAD
    Tpat_var \"f/275\"
    Tpat_var \"x/277\"

  $ echo "let f x = let () = () in x" | \
  > $MERLIN server dump -what browse -filename test.ml | \
  > sed 's:\\n:\n:g' | grep Tpat_var
    Tpat_var \"f/278\"
    Tpat_var \"x/280\"
||||||| 7b73c6aa3
    Tpat_var \"f/273\"
    Tpat_var \"x/275\"

  $ echo "let f x = let () = () in x" | \
  > $MERLIN server dump -what browse -filename test.ml | \
  > sed 's:\\n:\n:g' | grep Tpat_var
    Tpat_var \"f/276\"
    Tpat_var \"x/278\"
=======
    Tpat_var \"f/276\"
    Tpat_var \"x/278\"
>>>>>>> upstream/main

<<<<<<< HEAD
  $ echo "let f x = x" | \
  > $MERLIN server dump -what browse -filename other_test.ml | \
  > sed 's:\\n:\n:g' | grep Tpat_var
    Tpat_var \"f/275\"
    Tpat_var \"x/277\"

||||||| 7b73c6aa3
  $ echo "let f x = x" | \
  > $MERLIN server dump -what browse -filename other_test.ml | \
  > sed 's:\\n:\n:g' | grep Tpat_var
    Tpat_var \"f/273\"
    Tpat_var \"x/275\"

=======
>>>>>>> upstream/main
  $ echo "let f x = let () = () in x" | \
  > $MERLIN server dump -what browse -filename test.ml | \
  > sed 's:\\n:\n:g' | grep Tpat_var
<<<<<<< HEAD
    Tpat_var \"f/278\"
    Tpat_var \"x/280\"

  $ echo "let f x = x" | \
  > $MERLIN server dump -what browse -filename test.ml | \
  > sed 's:\\n:\n:g' | grep Tpat_var
    Tpat_var \"f/281\"
    Tpat_var \"x/283\"
||||||| 7b73c6aa3
    Tpat_var \"f/276\"
    Tpat_var \"x/278\"

  $ echo "let f x = x" | \
  > $MERLIN server dump -what browse -filename test.ml | \
  > sed 's:\\n:\n:g' | grep Tpat_var
    Tpat_var \"f/279\"
    Tpat_var \"x/281\"
=======
    Tpat_var \"f/279\"
    Tpat_var \"x/281\"
>>>>>>> upstream/main

  $ echo "let f x = x" | \
  > $MERLIN server dump -what browse -filename other_test.ml | \
  > sed 's:\\n:\n:g' | grep Tpat_var
    Tpat_var \"f/276\"
    Tpat_var \"x/278\"

  $ echo "let f x = let () = () in x" | \
  > $MERLIN server dump -what browse -filename test.ml | \
  > sed 's:\\n:\n:g' | grep Tpat_var
    Tpat_var \"f/279\"
    Tpat_var \"x/281\"

  $ echo "let f x = x" | \
  > $MERLIN server dump -what browse -filename test.ml | \
  > sed 's:\\n:\n:g' | grep Tpat_var
    Tpat_var \"f/282\"
    Tpat_var \"x/284\"

  $ $MERLIN server stop-server
