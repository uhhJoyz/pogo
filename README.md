# What is owl?

`owl` (Ocaml Website Launcher) is a terminal-centric application (written in Ocaml, btw) for managing and opening bookmarks in your favorite web browsers.

To get started, ensure dune is installed and run `opam install minttea` and then run `dune exec pogo -- <parameters>`.

# Usage
To use owl, there are four main verbs:
- `flap`: find (and follow) link access point (open bookmark)
- `roost`: redirect or otherwise store token (add bookmark)
- `yarp`: yoke adjacent bookmarks and remove point (delete bookmark)
- `preen`: preview related existing entries now (display / search bookmarks)

To use owl, enter the following:
```bash
owl <verb> <token>
```

For more information, type:
```bash
owl --help
```
