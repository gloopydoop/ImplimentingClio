rm conj_ht0.f*
./compile.sh --all
nek conj_ht > /dev/null
visnek conj_ht
