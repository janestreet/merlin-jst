#!/bin/bash 

if ! git diff --quiet; then
    echo "Working directory must be clean before using this script,"
    echo "but currently has the following changes:"
    git diff --stat
    exit 1
fi


# First, fetch the new ocaml-jst sources and copy into upstream/ocaml_jst
git fetch https://github.com/ocaml-flambda/ocaml-jst main
rev=$(git rev-parse FETCH_HEAD)
cd upstream/ocaml_jst
echo $rev > base-rev.txt
for file in $(git ls-tree --name-only -r HEAD . | grep -v base-rev.txt); do
  git show FETCH_HEAD:$file > $file;
done
git add -u .
cd ../..
git commit -m "Import ocaml_jst $(git describe --always $rev)"

# Then patch src/ocaml using the changes you just imported
for file in $(git diff --name-only HEAD^ HEAD); do
   base=${file#upstream/ocaml_jst/}
   case $base in
       parsing/lexer.mll) tgt=preprocess/lexer_raw.mll;;
       parsing/parser.mly) tgt=preprocess/parser_raw.mly;;
       utils/clflags.ml*) echo "Ignoring changes to $base"; continue;;
       *) tgt=$base;;
   esac
   tgt=src/ocaml/$tgt

   # Not all files are necessary
   if [ ! -e $tgt ]; then continue; fi

   err=$(patch --merge $tgt <(git diff HEAD^ HEAD -- $file))
   # ignore patch output if it worked
   if [ $? = 0 ]; then
      git add -u $tgt
   else
      echo "$err"
   fi
   rm -f $tgt.orig
done

