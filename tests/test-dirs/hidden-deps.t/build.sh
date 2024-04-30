#!/bin/bash
set -e
cd "$(dirname "$0")"

OCAMLC=/j/office/app/ocaml/prod/5.1.1minus/5.1.1minus-13-d49e4649/el8+fl2/bin/ocamlc
BUILD="$(realpath ./_build)"

rm -rf $BUILD
mkdir -p $BUILD/a
mkdir -p $BUILD/b
mkdir -p $BUILD/c

# cd a
# $OCAMLC -c a.mli -o $BUILD/a/a.cmi
# $OCAMLC -c -I $BUILD/a a.ml -o $BUILD/a/a.cmo
# $OCAMLC -a $BUILD/a/a.cmo -o $BUILD/a/a.cma

# cd ../b
# $OCAMLC -c b.mli -o $BUILD/b/b.cmi
# $OCAMLC -c -I $BUILD/a -I $BUILD/b b.ml -o $BUILD/b/b.cmo
# $OCAMLC -a -I $BUILD/a $BUILD/b/b.cmo -o $BUILD/b/b.cma

# cd ../c
# $OCAMLC -I $BUILD/a -H $BUILD/b $BUILD/a/a.cma $BUILD/b/b.cma c.ml -o $BUILD/c/c



cp a/*.ml a/*.mli $BUILD/a
cd $BUILD/a
$OCAMLC -c -bin-annot a.mli -o a.cmi
$OCAMLC -c -bin-annot a.ml -o a.cmo
$OCAMLC -a -bin-annot a.cmo -o a.cma
cd ../..

cp b/*.ml b/*.mli $BUILD/b
cd $BUILD/b
$OCAMLC -c -bin-annot b.mli -o b.cmi
$OCAMLC -c -bin-annot -I $BUILD/a b.ml -o b.cmo
$OCAMLC -a -bin-annot -I $BUILD/a b.cmo -o b.cma
cd ../..

# cd ../c
# $OCAMLC -I $BUILD/a -H $BUILD/b $BUILD/a/a.cma $BUILD/b/b.cma c.ml -o $BUILD/c/c
