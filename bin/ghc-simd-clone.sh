#!/bin/sh
set -e

SRC=$1; shift
DST=$1; shift

git clone ${SRC} ${DST}
cd ${DST}
git checkout simd

./sync-all get

for LIB in primitive vector dph; do
    (cd libraries/${LIB} &&
	git remote add github git@github.com:mainland/${LIB}.git &&
	git fetch github)
done

cat >simd.fp <<EOF
.|30a669a96ec824b78a5cfa988c5784f5a6cc162a
ghc-tarballs|18e0c37f8023abf469af991e2fc2d3b024319c27
libraries/Cabal|24bdc9b8862d228656ea2b5b0bd6307cb985819e
libraries/Win32|1f9f7175e747aad7c424f5b12be5b95f15286f0b
libraries/array|95c4cbc7417eca42ebe1c1ac97f10a00251e5178
libraries/base|25b44d2e089e3aa030f146e2c52601146c8d126a
libraries/binary|4d890e4465a0494e5fd80fbcf1fb339d8bd5800d
libraries/bytestring|9692aaf0bf9b203f9249a1414637328fd31fc04a
libraries/containers|41bc140a140143fa517df4c1a08365474cde4d14
libraries/deepseq|420507ea418db8664a79aedaa6588b772e8c97c6
libraries/directory|2334e09412f51bf847f3e23710daa566e561817f
libraries/dph|148c2edb4bb4778b676410ded2a3ac668aee5d35
libraries/filepath|abf31a9aef45d2119a5757dafbe4adf611388ee8
libraries/ghc-prim|dbe66a7ea2e109d3c7744badf5e0b434f3d0f2a7
libraries/haskeline|3a92ddd63d4edc622ad4af044c5b664aa64c3dd4
libraries/haskell2010|c54b04f7c3ae16cafb7ddc2b357b9ed6c8b593b7
libraries/haskell98|1fed87b8540a561c14bfbb675b87f84fa63f14c6
libraries/hoopl|8e0ef3b7bf6d25919209f74d65c4a77c6689934d
libraries/hpc|a7231c6727de54d17ce14b1286cfe88c4db95783
libraries/integer-gmp|59f3587ce395446f6c3b18843c5b8c00a0b02da3
libraries/integer-simple|30c4af5165f181ef4f089b3d245371230f0aafad
libraries/old-locale|df98c76b078de507ba2f7f23d4473c0ea09d5686
libraries/old-time|7e0df2eb500ce4381725b868440fde04fa139956
libraries/pretty|0b8eada2d4d62dd09ee361d8b6ca9b13e6573202
libraries/primitive|21fd4d055a26552919d06fbf7433f72289969043
libraries/process|ab7e650c7a1301cc53f28356fbd84661bd435bb0
libraries/random|4b68afd3356674f12a67a4e381fa9becd704fab2
libraries/template-haskell|77a92240b257e8d7b35e7ceccb3e953782bfaa4a
libraries/terminfo|116d3ee6840d52bab69c880d775ae290a20d64bc
libraries/time|12ba4321d34d646cf9040ad12810c4257d26ade9
libraries/transformers|a59fb93860f84ccd44178dcbbb82cfea7e02cd07
libraries/unix|6e900d020129afef7b424a6cdef98f703821d4b9
libraries/vector|91f6804a0ee324f46d38f3a071c943c0eff82db8
libraries/xhtml|fb9e0bbb69e15873682a9f25d39652099a3ccac1
nofib|e55a9f0895d35dae77dcedf23de07bddc9b62330
testsuite|4c7888408916c14090247f537adb1c7e38a1cfbc
utils/haddock|478b606bb0d61c4fea2d72171e2e5bb881a366d2
utils/hsc2hs|2099476075dd50adbf43200284fff92de690e66b
EOF

./utils/fingerprint/fingerprint.py restore -n -f simd.fp

for LIB in primitive vector dph; do
    (cd libraries/${LIB} && git checkout -b simd github/simd)
done

sed -e 's/#BuildFlavour = quick-llvm/BuildFlavour = quick-llvm/' <mk/build.mk.sample >mk/build.mk
