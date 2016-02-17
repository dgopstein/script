for i in `seq -w 1 320`; do
  curl "http://www.futhead.com/16/career-mode/players/?page=$i" > players_$i.html
  sleep 0.5
done
