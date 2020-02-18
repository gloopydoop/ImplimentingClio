genbox << EOF
mesh_fluid.box
EOF
#
mv box.re2 conj_ht.re2
#
genmap << EOF
conj_ht

EOF

./compile.sh --all

nekb conj_ht

visnek conj_ht
