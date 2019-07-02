
################################

folder_ht_newsenses="data/HT/newsenses/scrape"
folder_ht_oldsenses="data/HT/oldsenses/scrape"
folder_ht_stablesenses="data/HT/stablesenses/scrape"

#baseurl="https://ht.ac.uk/category-selection/?word=&pos[]=aj&pos[]=av&pos[]=v&pos[]=vi&pos[]=vm&pos[]=vp&pos[]=vr&pos[]=vt&label=&category=&year=&startl=1850&endl=1959" 



#old senses: 1850-1959. OK!
for i in {1900..1959..1}
do 
  j=`expr $i + 1`
  echo $j
  url= "http://ht.ac.uk/category-selection/?word=&pos[]=aj&pos[]=av&pos[]=v&pos[]=vi&pos[]=vm&pos[]=vp&pos[]=vr&pos[]=vt&label=&category=&year=&startl=$i&endl=$j&page=1" 
  echo $url
  wget -r -k -l 1 "http://ht.ac.uk/category-selection/?word=&pos[]=aj&pos[]=av&pos[]=v&pos[]=vi&pos[]=vm&pos[]=vp&pos[]=vr&pos[]=vt&label=&category=&year=&startl=$i&endl=$j&page=1" -P $folder_ht_oldsenses 
done


###########################################


#new senses: 1900-1959. Replace l by f in URL. OK!
for i in {1900..1959..1}
do 
  j=`expr $i + 1`
  echo $j
  url= "http://ht.ac.uk/category-selection/?word=&pos[]=aj&pos[]=av&pos[]=v&pos[]=vi&pos[]=vm&pos[]=vp&pos[]=vr&pos[]=vt&label=&category=&year=&startf=$i&endf=$j&page=1" 
  echo $url
  wget -r -k -l 1 "http://ht.ac.uk/category-selection/?word=&pos[]=aj&pos[]=av&pos[]=v&pos[]=vi&pos[]=vm&pos[]=vp&pos[]=vr&pos[]=vt&label=&category=&year=&startf=$i&endf=$j&page=1" -P $folder_ht_newsenses 
done

###########################################

#stable senses: Date first cited: OE — 1800. Date last cited: 1900 — Current. FLAWED ASSUMPTION! WOrd-in-sense that is stable, not token

#https://ht.ac.uk/category-selection/?word=&pos%5B%5D=aj&pos%5B%5D=av&pos%5B%5D=allv&pos%5B%5D=v&pos%5B%5D=vi&pos%5B%5D=vm&pos%5B%5D=vp&pos%5B%5D=vr&pos%5B%5D=vt&label=&category=&year=&oef=Y&startf=&endf=1800&startl=1900&currentl=Y&endl=


