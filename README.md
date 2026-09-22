# Everybody Codes (2025)

**Language**:
OCaml 5.5


**Environment**

```bash
opam switch create dragons2025 ocaml-base-compiler.5.5.0
eval $(opam env --switch=dragons2025)
opam install -y utop ounit2 ocaml-lsp-server ocamlformat dune
```

**Build**
```bash
dune build
```

**Run test**

Run unit tests for a given quest (e.g. quest01):
```bash
dune runtest src/quest01
```

**Run**

Run executable on a given text input (e.g. first part of quest01)
```bash
cat resources/quest01_1.txt | dune exec src/quest01/main1.exe
```
