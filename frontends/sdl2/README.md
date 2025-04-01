## Install
### Ubuntu

```shell
$ sudo apt install libsdl2-dev libsdl2-image-dev libsdl2-ttf-dev
```

### MacOS

```shell
$ brew install sdl2
$ brew install sdl2_image
$ brew install sdl2_ttf
```

### Windows
#### Requirements
- msys2
- sbcl
- [OpenSSL v1.1.1](https://slproweb.com/products/Win32OpenSSL.html)

```shell
$ pacman -S mingw-w64-x86_64-roswell

$ pacman -S mingw-w64-x86_64-SDL2
$ pacman -S mingw-w64-x86_64-SDL2_image
$ pacman -S mingw-w64-x86_64-SDL2_ttf
```

## Launch
```shell
$ qlot install
$ qlot exec sbcl
```

```common-lisp
* (ql:quickload :lem-sdl2)
* (lem:lem)
```

#### Keyboard Layout (Windows / Mac OS)

If your keyboard is a JIS layout, you need to put the following settings in $HOME/.lem/init.lisp

```common-lisp
#+lem-sdl2
(lem-sdl2:set-keyboard-layout :jis)
```
