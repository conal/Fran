
unzip -o ../Fran1.$1.zip 
cp -rp * .
chmod -R u+w .
rm -rf Fran1/
git add .
git commit -m "Fran 1.$1"
