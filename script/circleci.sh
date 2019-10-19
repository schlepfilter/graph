#TODO rewrite this script in closh when closh becomes stable
#!/usr/bin/env bash
git clone -b develop https://github.com/schlepfilter/aid &&
cd aid &&
lein install &&
cd .. &&
git clone -b develop https://github.com/schlepfilter/frp &&
cd frp &&
lein install &&
cd .. &&
yarn &&
yarn webpack &&
lein cljsbuild once builder &&
lein cljsbuild once main-prod &&
lein cljsbuild once renderer
node target/main.js
