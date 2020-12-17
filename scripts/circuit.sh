#!/usr/bin/bash
lines=100
pids=
for i in {0..2}; do
    for j in {0..3}; do
        for c in {0..2}; do
            # rf  = 0,30,60
            # nsf = 5,10,15,20
            # cap = 10,25,40
            rf=$(echo "$i*30" | bc)
            nsf=$(echo "5+$j*5" | bc)
            cap=$(echo "10+$c*15" | bc)
            logfile="circuit-${lines}L-$rf-$nsf-$cap.out"
            echo $logfile
            ./scripts/race.sh -q -A $rf,$nsf,30,$cap $lines > $logfile &
            pid=$!
            pids="$pid $pids"
        done
    done
done
echo $pids
for pid in $pids; do
    wait $pid
done
