# google-translate

## setup API

in ``~/.lem/init.lisp``

```
(defparameter lem-user::*google-api-key* "your api key")
```

or setenv GOOGLEAPI

```
(defparameter lem-user::*google-api-key* (uiop:getenv "GOOGLEAPI"))
```

## load-library

```
M-x load-library
google-translate
```

## how to translate

select region then

```
M-x popup-google-translate
```
