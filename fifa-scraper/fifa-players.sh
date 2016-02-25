#for i in `seq -w 1 320`; do
  #curl "http://www.futhead.com/16/career-mode/players/?page=$i" > players_$i.html
for i in `seq -w 1 40`; do
  curl "http://www.futhead.com/16/career-mode/players/?group=gk&page=$i" > keepers_$i.html
  sleep 0.5
done
