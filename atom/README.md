# Atom config - emacs style

## Simple setup

All files in this directory should be linked to the `~/.atom/` directory.

**Packages:**

Packages are *exported* using

```sh
$ apm list --installed --bare > package-list.txt
```

and can be *installed* using

```sh
$ apm install --packages-file package-list.txt
```

**Configuration:**

*Don't overwrite current settings*

```sh
$ ln -s *.cson *.coffee ~/.atom/
```

 *Will probably overwrite current settings*

```sh
$ ln -sf *.cson *.coffee ~/.atom/
```

That's it. Enjoy.
