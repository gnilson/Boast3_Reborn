#/bin/bash

file_suffix=${1%%.*}

if [ -z $1 ] 
then
    echo "You did not specify an input data file.........................."
    echo "..........Please try again."
    exit -1
fi

if [ ! -e $1 ]
then
    echo "ERROR:   Specified input file \"$1\" does not exist!"
    exit -1
fi

if [ ! -e boast3 ]
then
    echo "ERROR:   Simulator executable file boast3 does not exist!"
    exit -1
fi


echo "All previous ${file_suffix} output files (if they exist) will be"
echo "scratched if you continue !"
echo "Press ANY KEY to continue or Ctrl-c to exit"
read -n 1

if [ -e "${file_suffix}.out" ]
then
    rm "${file_suffix}.out"
fi

if [ -e "${file_suffix}.scr" ]
then
    rm "${file_suffix}.scr"
fi

if [ -e "${file_suffix}.wel" ]
then
    rm "${file_suffix}.wel"
fi

if [ -e "${file_suffix}.tab" ]
then
    rm "${file_suffix}.tab"
fi

if [ -e "${file_suffix}.gwn" ]
then
    rm "${file_suffix}.gwn"
fi

if [ -e "${file_suffix}.bpd" ]
then
    rm "${file_suffix}.bpd"
fi

if [ -e "${file_suffix}.cgd" ]
then
    rm "${file_suffix}.cgd"
fi

if [ -e "${file_suffix}.map" ]
then
    rm "${file_suffix}.map"
fi

cp $1 B.SIM

echo "        .........boast3 executing using $1 input file"

./boast3

echo "       ....................Simulation completed"
echo "        ....................Saving output files"

if [ -e "B.SIM" ]
then
    rm B.SIM
fi

if [ -e "B.OUT" ]
then
    mv B.OUT "${file_suffix}.out"
fi

if [ -e "B.SCR" ]
then
    mv B.SCR "${file_suffix}.scr"
fi

if [ -e "B.WEL" ]
then
    mv B.WEL "${file_suffix}.wel"
fi

if [ -e "B.TAB" ]
then
    mv B.TAB "${file_suffix}.tab"
fi

if [ -e "B.GWN" ]
then
    mv B.GWN "${file_suffix}.gwn"
fi

if [ -e "B.BPD" ]
then
    mv B.BPD "${file_suffix}.bpd"
fi

if [ -e "B.CGD" ]
then
    mv B.CGD "${file_suffix}.cgd"
fi

if [ -e "B.MAP" ]
then
    mv B.MAP "${file_suffix}.map"
fi








