# KeyBinding for OS X

Do you enjoy the occasional `bash:  less: command not found`?

**I do not!**

It's caused by the useless `Options-Space` combination that I happen to type waaay too often.

**Let's fix that**

Copy `DefaultKeyBinding.dict` to `~/Library/KeyBindings/DefaultKeyBinding.dict` and restart any running applications to apply the key binding.

### Result?

#### No more: command not found

```sh
root@host:~# echo "Hello" | less
-bash:  less: command not found
```

#### More of this
... or less.

```sh
root@host:~# echo "Hello" | less
```
```sh
Hello
(END)
```
