## Byte Compile Packages in `elpa/` (Optional)

Compile `.el` into `.elc` may shorten startup time.

```shell
emacs --load ./byte-compile-packages-in-elpa.el
```

## Upgrading Packages (Optional)

>Notice: This action is not necessary. It's possible cause some compatibility problems or install a latest but broken package.

You may want to update and upgrade all packages:

```shell
emacs --load ./auto-install-packages.el
```

If you encounter some problems while installing packages, runs it manually again (or more times).
