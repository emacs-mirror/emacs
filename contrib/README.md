# contribs for lem

Files in this directroy are libraries which aren't loaded by default.

## How to try contrib

```
M-x load-library 
```

you can see completion by tab key.

## If you want the contrib after restart lem

```
M-x site-init-add-dependency
```

And choose which library to install.

then

```
#ros install lem
```

for uninstall.

```
M-x site-init-remove-dependency
```

then

```
#ros install lem
```
