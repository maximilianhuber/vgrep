`vgrep` -- A pager for `grep`
=============================

![Screenshot](./vgrep.png)

## Usage

* As a pager:
```bash
grep -rn data /some/path | vgrep  # -n for line numbers
```

* As a drop-in replacement for `grep`:
```bash
vgrep data /some/path                  # recursive by default
vgrep data /some/path | vgrep default  # works with pipes, too
```

## Installation
Installation via [`stack`][1] is recommended:
```bash
git clone https://github.com/fmthoma/vgrep.git
cd vgrep
stack setup
stack install
```
This will install `vgrep` to your `~/.local/bin` directory.

[1]: https://github.com/commercialhaskell/stack/blob/master/doc/install_and_upgrade.md
