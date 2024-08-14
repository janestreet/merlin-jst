<<<<<<< HEAD
#!/bin/sh

D_MERLIN=../src/ocaml

FROM=500
TO=501

D_FROM=ocaml_${FROM}
D_TO=ocaml_${TO}
D_PATCH=patches_${TO}

mkdir "${D_PATCH}"

for file in "${D_TO}"/*/*.ml*; do
  F_TO=${file}
  F_FROM=$(echo "${F_TO}" | sed "s/${D_TO}/${D_FROM}/g")
  F_MERLIN=$(echo "${F_TO}" | sed "s,${D_TO},${D_MERLIN},g")
  F_PATCH=$(echo "${F_TO}" | sed "s/${D_TO}/${D_PATCH}/g")
  mkdir "$(dirname "${F_PATCH}")" 2>/dev/null | true
  # Make diff
  RES=$(diff -u -N "${F_FROM}" "${F_TO}")
  if [ -n "${RES}" ]; then
    # Write the patch file if non-empty
    echo "${RES}" > "${F_PATCH}.patch"
    # Apply the patch file
    patch "${F_MERLIN}" "${F_PATCH}.patch"
  fi
done
||||||| 7b73c6aa3f
=======
#!/bin/sh

D_MERLIN=../src/ocaml

FROM=501
TO=502

D_FROM=ocaml_${FROM}
D_TO=ocaml_${TO}
D_PATCH=patches_${TO}

mkdir "${D_PATCH}"

for file in "${D_TO}"/*/*.ml*; do
  F_TO=${file}
  F_FROM=$(echo "${F_TO}" | sed "s/${D_TO}/${D_FROM}/g")
  F_MERLIN=$(echo "${F_TO}" | sed "s,${D_TO},${D_MERLIN},g")
  F_PATCH=$(echo "${F_TO}" | sed "s/${D_TO}/${D_PATCH}/g")
  mkdir "$(dirname "${F_PATCH}")" 2>/dev/null | true
  # Make diff
  diff -u -N "${F_FROM}" "${F_TO}" >"${F_PATCH}.patch"
  if [ -s "${F_PATCH}.patch" ]; then
    # Apply the patch file
    patch "${F_MERLIN}" "${F_PATCH}.patch"
    echo "patched ${F_MERLIN}"
  else
    rm "${F_PATCH}.patch"
  fi
done
>>>>>>> upstream/main
