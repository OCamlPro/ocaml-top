PATH=$PATH;"C:\Program Files (x86)\WiX Toolset v3.10\bin"

for %i in (data bin doc lib share) do heat.exe dir ocaml-top/%i -srd -dr %i -cg %i -gg -var var.%iDir -out %i.wxs

for %i in (data bin doc lib share) do candle.exe -d"%iDir=ocaml-top\\%i" -out %i.wixobj %i.wxs

candle.exe -out ocaml-top.wixobj ocaml-top.wxs

light.exe -ext WixUIExtension -ext WixUtilExtension -out ocaml-top.msi *.wixobj
