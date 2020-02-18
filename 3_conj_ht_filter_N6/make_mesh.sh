cd mesh

genbox << EOF
mesh_fluid.box
EOF
#
mv box.rea meshfluid.rea
#
genbox << EOF
mesh_lower_solid.box
EOF
#
mv box.rea meshlowersolid.rea
#
genbox << EOF
mesh_upper_solid.box
EOF
#
mv box.rea meshuppersolid.rea

nekmerge << EOF
a
meshsolid
meshlowersolid
meshuppersolid

EOF
#

pretex << EOF
conjHT
3
meshfluid
meshsolid

EOF
#
reatore2 << EOF
conjHT
conj_ht
EOF

genmap << EOF
conj_ht

EOF

mv conj_ht.* ../


rm *.re2
#rm *.rea
rm *.dra
rm pretex*
rm session.name
rm fort*
rm *.tmp

mv ../conj_ht.rea .
