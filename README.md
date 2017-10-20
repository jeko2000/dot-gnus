# dot-gnus

This is my current dot-gnus configuration file.

## Dependencies


* (offlineimap)[https://github.com/OfflineIMAP/offlineimap] to download my emails as Maildirs
* (dovecot)[https://www.dovecot.org/] to serve as a local IMAP server

Also, since gnus expects to find its init file under ~/.gnus, we should have the following line
somewhere in the emacs init file:

```emacs-lisp
(setq gnus-init-file "~/location_of_dot_gnus.el") ; where ~ stands for /home/$USER
```
