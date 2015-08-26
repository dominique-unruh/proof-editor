cd ..\haskell
cabal install --only-dependencies
pause
cabal build --builddir .dist-buildwrapper\dist
pause
cd ..
set CHERE_INVOKING=1
c:\cygwin\bin\bash -lc "pwd; make cmathml.exe"
pause
cd scripts
python setup.py build_exe --build-exe E:\Dropbox\share\merily-salura\math-ed
pause
