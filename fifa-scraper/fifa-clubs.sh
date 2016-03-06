#for i in `seq -w 1 320`; do
  #curl "http://www.futhead.com/16/career-mode/players/?page=$i" > players_$i.html
for i in `seq -w 1 25`; do
  curl "http://www.futhead.com/16/clubs/?page=$i" > players_html/clubs_$i.html
  sleep 0.5
done

#//td[@class='text-left']//text()
