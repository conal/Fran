
unzip ../rbmh0.$1.zip 
cp -rp rbmh/* .
chmod -R u+w .
rm -rf rbmh/
git add .
git commit -m "RBMH 0.$1"
