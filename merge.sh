
unzip ../rbmh0.$1.zip 
cp -rp Release0/* .
chmod -R u+w .
rm -rf Release0/
git add .
git commit -m "RBMH 0.$1"
