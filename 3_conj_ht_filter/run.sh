rm conj_ht0.f*
./compile.sh --all
nek conj_ht > logfile
visnek conj_ht
