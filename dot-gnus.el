(setq user-mail-address "jeko2000@yandex.com"
      user-full-name "Johnny Ruiz")

(setq gnus-check-new-newsgroups nil
      gnus-save-killed-list     nil)

(setq gnus-subscribe-newsgroup-method 'gnus-subscribe-alphabetically
      gnus-subscribe-hierarchical-interactive nil)

(setq gnus-save-newsrc-file nil
      gnus-read-newsrc-file nil)

(setq gnus-use-dribble-file t
      gnus-always-read-dribble-file t)

(setq gnus-read-active-file nil)

(setq gnus-check-bogus-newsgroups nil)

(setq gnus-group-line-format "%S%p%P%M%5y: %(%B%g%B%)\n")

(defvar jr/gnus-unread-count-threshold 1000
  "Threshold count of the number of unread articles in a group.")

(defface jr/gnus-group-too-many-unread-face
  '((((class color) (background dark))
     (:foreground "red" :bold t))
    (((class color) (background light))
     (:foreground "pink" :bold t)))
  "Face used when a group has more than
  `jr/gnus-unread-count-threshold' unread articles.")

(push '((> unread jr/gnus-unread-count-threshold) . jr/gnus-group-too-many-unread-face)
      gnus-group-highlight)

(add-hook 'gnus-group-mode-hook 'hl-line-mode)

(setq gnus-large-newsgroup 5000
      gnus-large-ephemeral-newsgroup 5000)

(setq gnus-newsgroup-maximum-articles nil)

(setq gnus-auto-select-first t
      gnus-auto-select-subject 'first)

(defun jr/gnus-group-catchup-group-hook ()
  "Hook to run as part of `gnus-group-catchup-group-hook'."
  (message "Group successfully caught up."))

(add-hook 'gnus-group-catchup-group-hook 'jr/gnus-group-catchup-group-hook)

(setq gnus-group-default-list-level 3)

(setq gnus-group-use-permanent-levels nil)

(setq gnus-activate-level 3)

(add-hook 'gnus-summary-exit-hook 'gnus-summary-bubble-group)
(add-hook 'gnus-summary-exit-hook 'gnus-group-sort-groups-by-rank)

(setq gnus-activate-foreign-newsgroups 3)

(setq gnus-parameters
      '(("list"
         (subscribed t))
        ("list\\.emacs\\.devel"
         (to-address . "emacs-devel@gnu.org")
         (to-list . "emacs-devel@gnu.org"))
        ("list\\.emacs\\.devel$"
         (to-address . "emacs-devel@gnu.org")
         (to-list . "emacs-devel@gnu.org"))
        ("list\\.emacs\\.help$"
         (to-address . "help-gnu-emacs@gnu.org")
         (to-list . "help-gnu-emacs@gnu.org"))
        ("list\\.emacs\\.bugs$"
         (to-list . "bug-gnu-emacs@gnu.org"))
        ("list\\.emacs\\.bugs\\.tracker"
         (list-identifier . "\\[debbugs-tracker\\]"))

        ("list\\.emacs\\.orgmode"
         (to-address . "emacs-orgmode@gnu.org")
         (to-list . "emacs-orgmode@gnu.org"))

        ("list\\.savannah\\.announce"
         (to-address . "savannah-announce@gnu.org")
         (to-list . "savannah-announce@gnu.org"))

        ("list\\.arch\\.security"
         (to-address . "arch-security@archlinux.org")
         (to-list . "arch-security@archlinux.org"))
        ("list\\.arch\\.events"
         (to-address . "arch-events@archlinux.org")
         (to-list . "arch-events@archlinux.org"))
        ("list\\.arch\\.announce"
         (to-address . "arch-announce@archlinux.org")
         (to-list . "arch-announce@archlinux.org"))))

(setq gnus-list-groups-with-ticked-articles nil)

(setq gnus-group-sort-function '(gnus-group-sort-by-alphabet gnus-group-sort-by-rank))

(add-hook 'gnus-suspend-gnus-hook 'gnus-group-save-newsrc)

(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

(setq gnus-topic-line-format "%i[ %A: %(%{%n%}%) ]%v\n")

(setq gnus-topic-display-empty-topics nil)

(defun jr/gnus-goto-buffer-group ()
  (interactive)
  (gnus-group-jump-to-group "buffer"))

(defun jr/gnus-goto-sent-group ()
  (interactive)
  (gnus-group-jump-to-group "Sent"))

(defun jr/gnus-goto-references-group ()
  (interactive)
  (gnus-group-jump-to-group "references"))

(define-key gnus-group-mode-map "vb"
  'jr/gnus-goto-buffer-group)

(define-key gnus-group-mode-map "vs"
  'jr/gnus-goto-sent-group)

(define-key gnus-group-mode-map "vr"
  'jr/gnus-goto-references-group)

(setq gnus-summary-line-format
      "%U%R%z%&user-date;     %-40,40f    %-2,2t     %B%s\n"
      gnus-user-date-format-alist '((t . "%Y-%m-%d %H:%M")))

(setq gnus-sum-thread-tree-false-root      ""
      gnus-sum-thread-tree-single-indent   ""
      gnus-sum-thread-tree-root            ""
      gnus-sum-thread-tree-vertical        "| "
      gnus-sum-thread-tree-leaf-with-other "+-> "
      gnus-sum-thread-tree-single-leaf     "\\-> "
      gnus-sum-thread-tree-indent          " ")

(setq gnus-extra-headers '(To Cc Keywords Gcc Newsgroups X-GM-LABELS User-Agent))

(setq gnus-ignored-from-addresses "\\(jeko2000\\|kubb18\\)")

(setq nnmail-extra-headers gnus-extra-headers)

(defface jr/gnus-summary-expirable-face
  '((((class color) (background dark))
     (:foreground "grey30" :italic t :strike-through t))
    (((class color) (background light))
     (:foreground "grey55" :italic t :strike-through t)))
  "Face for articles marked as expirable."
  :group 'gnus-summary-visual)

(push '((eq mark gnus-expirable-mark) . jr/gnus-summary-expirable-face)
      gnus-summary-highlight)

(gnus-delay-initialize)

(setq gnus-delay-default-hour 7
      gnus-delay-default-delay "1d")

(add-hook 'gnus-started-hook 'gnus-delay-send-queue)

(setq message-draft-headers (remove 'Date message-draft-headers))

(setq gnus-summary-goto-unread nil)

(setq gnus-summary-make-false-root 'adopt)

(setq gnus-summary-gather-subject-limit 25)

(setq gnus-thread-hide-subtree t)

(setq gnus-thread-ignore-subject nil)

(setq gnus-asynchronous t)

(setq gnus-use-cache t
      gnus-cache-directory "~/mail/cache/"
      gnus-use-long-file-name t)

(setq gnus-uncacheable-groups "^nnml")

(setq gnus-default-article-saver 'gnus-summary-save-in-mail
      gnus-article-save-directory "~/mail/saved/"
      gnus-prompt-before-saving nil)

(setq mm-text-html-renderer 'gnus-w3m)

(setq gnus-signature-separator '("^-- $" "^-- *$"))

(setq gnus-summary-display-while-building 100)

(add-hook 'gnus-summary-mode-hook 'hl-line-mode)

(setq gnus-visible-headers "^From:\\|^Newsgroups:\\|^Subject:\\|^Date:\\|^Followup-To:\\|^Reply-To:\\|^Organization:\\|^Summary:\\|^Keywords:\\|^To:\\|^[BGF]?Cc:\\|^Posted-To:\\|^Mail-Copies-To:\\|^Mail-Followup-To:\\|^Apparently-To:\\|^Gnus-Warning:\\|^Resent-From:\\|^User-Agent: "
      gnus-sorted-header-list '("^Date:" "^From:" "^Summary:" "^Keywords:" "^Newsgroups:" "^Followup-To:" "^To:" "^Reply-To:" "^Cc:" "^Organization:" "^Subject:" "User-Agent:"))

(setq gnus-treat-date 'head
      gnus-treat-hide-citation-maybe t
      gnus-treat-strip-cr t
      gnus-treat-strip-leading-blank-lines t
      gnus-treat-strip-multiple-blank-lines t
      gnus-treat-strip-trailing-blank-lines t
      gnus-treat-unsplit-urls t)

(setq gnus-single-article-buffer nil)

(setq gnus-inhibit-images t)

(setq gnus-mailing-list-groups "\\`list\\.")

(add-hook 'message-send-hook 'ispell-message)

(defun jr/gnus-select-dispatch-dictionary-on-group ()
  "Run `ispell-change-dictionary' with a dictionary appropriate for the group."
  (cond
   ((string-match
     "^list\\.es\\." (gnus-group-real-name gnus-newsgroup-name))
    (ispell-change-dictionary "spanish"))
   ((string-match
     "^list\\.ru\\." (gnus-group-real-name gnus-newsgroup-name))
    (ispell-change-dictionary "russian"))
   ((string-match
     "^list\\.fr\\." (gnus-group-real-name gnus-newsgroup-name))
    (ispell-change-dictionary "francais"))
   (t
    (ispell-change-dictionary "english"))))

(add-hook 'gnus-select-group-hook 'jr/gnus-select-dispatch-dictionary-on-group)

(define-key gnus-summary-mode-map "F" 'gnus-summary-wide-reply-with-original)
(define-key gnus-article-mode-map "F" 'gnus-article-wide-reply-with-original)

(setq gnus-message-archive-group '((list "nnimap+local:Sent" (format-time-string "sent.%Y-%m")))
      gnus-gcc-mark-as-read t)

(setq gnus-posting-styles
      '((".*"
         (signature user-full-name))
        ((file-exists-p "~/.signature")
         (signature-file "~/.signature"))
        ((header "to" "kubb18@gmail.com")
         (address "kubb18@gmail.com")
         ("X-Message-SMTP-Method" "smtp smtp.gmail.com 465"))
        ((header "to" "jeko2000@yandex.com")
         ("X-Message-SMTP-Method" "smtp smtp.yandex.ru 465")
         (address "jeko2000@yandex.com"))
        ((header "to" "kubb18@me.com")
         ("X-Message-SMTP-Method" "smtp.mail.me.com")
         (address "kubb18@me.com"))
        ((header "to" "kubb18@icloud.com")
         ("X-Message-SMTP-Method" "smtp.mail.me.com")
         (address "kubb18@icloud.com"))))

(setq gnus-message-replysign t)

(setq gnus-select-method
      '(nnimap "local"
               (nnimap-stream plain)
               (nnimap-address "localhost")))

(setq nnmail-expiry-wait 30)

(setq gnus-novice-user nil)

(setq gnus-interactive-exit 'quiet)

(gnus-add-configuration
 '(article
   (horizontal 1.0
               (vertical 40 (group 1.0))
               (vertical 1.0
                         (summary 0.16 point)
                         (article 1.0)))))

(gnus-add-configuration
 '(summary
   (horizontal 1.0
               (vertical 40 (group 1.0))
               (vertical 1.0 (summary 1.0 point)))))
