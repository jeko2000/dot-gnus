# Gnus Configuration

Here's my current gnus configuration rewritten as an org file for easy
reference.

Please add the following to your init.el file (or equivalent) to
ensure gnus loads the configuration..

```emacs-lisp
(setq gnus-init-file (car (org-babel-tangle-file "~/location_of_dot_gnus.org")))
```
