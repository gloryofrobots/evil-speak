# evilspeak
Evilspeak provides Emacspeak voice support for basic Evil motions.
Library is young and unstable. 
## Instalation
* clone this repository
* add to .emacs something like
```
(when (featurep 'emacspeak)
  (add-to-list 'load-path <PATH_TO_EVILSPEAK>)
  (require 'evilspeak))
```

