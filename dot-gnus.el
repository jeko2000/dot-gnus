(setq user-mail-address "jeko2000@yandex.com"
      user-full-name "Johnny Ruiz")

(setq gnus-check-new-newsgroups nil
      gnus-save-killed-list nil)

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

        ("list\\.es\\.general"
         (to-address . "www-es-general@gnu.org")
         (to-list . "www-es-general@gnu.org"))

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

(defun jr/gnus-exit-gnus ()
  (and (fboundp 'gnus-group-exit)
       (gnus-alive-p)
       (with-current-buffer (get-buffer "*Group*")
         (let (gnus-interactive-exit)
           (gnus-group-exit)))))

(add-hook 'kill-emacs-hook 'jr/gnus-exit-gnus)

(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

(setq gnus-topic-line-format "%i[ %A: %(%{%n%}%) ]%v\n")

(setq gnus-topic-display-empty-topics nil)

;; Jump-to-group commands

(defun jr/gnus-jump-to-main-group ()
  "Jump point to \"main\" newsgroup line."
  (interactive)
  (gnus-group-jump-to-group "main"))

(defun jr/gnus-jump-to-sent-group ()
  "Jump point to \"Sent\" newsgroup line."
  (interactive)
  (gnus-group-jump-to-group "Sent"))

(defun jr/gnus-jump-to-references-group ()
  "Jump point to \"references\" newsgroup line."
  (interactive)
  (gnus-group-jump-to-group "references"))

(define-key gnus-group-mode-map "vb"
  'jr/gnus-jump-to-main-group)

(define-key gnus-group-mode-map "vs"
  'jr/gnus-jump-to-sent-group)

(define-key gnus-group-mode-map "vr"
  'jr/gnus-jump-to-references-group)

;; Email update command

(defun jr/gnus-update-mail ()
  "Run \"offlineimap\" command asynchronously to fetch new mail."
  (interactive)
  (async-shell-command "offlineimap"))

(define-key gnus-group-mode-map "vu"
  'jr/gnus-update-mail)

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

(setq gnus-summary-gather-subject-limit 'fuzzy)
(setq gnus-simplify-ignored-prefixes
      (concat
       "\\`\\[?\\("
       (mapconcat 'identity
        '("RE:" "re;" "FW:" "fw:") "\\|") "\\)?\\]?:?[ \t]*"))

(setq gnus-thread-hide-subtree t)

(setq gnus-thread-sort-functions
      '(gnus-thread-sort-by-subject
        gnus-thread-sort-by-number))

(setq gnus-thread-ignore-subject nil)

(setq gnus-asynchronous t)

(setq gnus-use-cache t
      gnus-cache-directory "~/mail/cache/"
      gnus-use-long-file-name t)

(setq gnus-uncacheable-groups "^nnml")

(setq gnus-default-article-saver 'gnus-summary-save-in-mail
      gnus-article-save-directory "~/mail/saved/"
      gnus-prompt-before-saving nil)

(setq mm-text-html-renderer 'gnus-w3m
      mm-discouraged-alternatives '("text/html" "text/richtext"))

(setq gnus-signature-separator '("^-- $" "^-- *$"))

(setq gnus-summary-display-while-building 100)

(add-hook 'gnus-summary-mode-hook 'hl-line-mode)

;; Util
(defvar jr/gnus-work-newsgroup-regex "^\\(tn\\|work\\)/*+"
  "Regular expression for work-related newsgroup names")

(defun jr/gnus-work-newsgroup-active-p ()
  "Return non-nil if the current newsgroup is a work newsgroup.
It checks if `gnus-newsgroup-name' matches the regex
`jr/gnus-work-email-header-regex'."
  (and (boundp 'gnus-newsgroup-name)
       gnus-newsgroup-name
       (string-match jr/gnus-work-newsgroup-regex gnus-newsgroup-name)))

(defun jr/gnus-get-target-newsgroup-for (type)
  "Return a newsgroup string to move an article of type TYPE."
  (if (jr/gnus-work-newsgroup-active-p)
      (pcase type
        ('junk "tn/Junk Email")
        ('references "tn/Archive")
        ('inbox "tn/INBOX"))
    (pcase type
      ('junk "junk")
      ('references "references")
      ('inbox "main"))))

(defun jr/gnus-summary-mark-processable-as-read (&optional arg)
  "Mark articles marked with the process mark as read.
This function does not clear the process mark from articles."
  (interactive "P")
  (let ((articles (gnus-summary-work-articles arg))
        article)
    (dolist (article articles)
      (gnus-summary-mark-article article))))

;; Move article commands
(defun jr/gnus-mark-as-read-and-move-to-junk (arg)
  "Move articles to a junk newsgroup.
The articles that get moved depend on ARG and thos articles
marked with the process mark."
  (interactive "P")
  (let* ((type 'junk)
         (target (jr/gnus-get-target-newsgroup-for type)))
    (when target
      (jr/gnus-summary-mark-processable-as-read arg)
      (gnus-summary-move-article arg target)
      (message "Moved article(s) to %s." target))))

(defun jr/gnus-mark-as-read-and-move-to-references (arg)
  "Move articles to reference newsgroup.
The articles that get moved depend on ARG and thos articles
marked with the process mark."
  (interactive "P")
  (let* ((type 'references)
         (target (jr/gnus-get-target-newsgroup-for type)))
    (when target
      (jr/gnus-summary-mark-processable-as-read arg)
      (gnus-summary-move-article arg target)
      (message "Moved article(s) to %s." target))))

(define-key gnus-summary-mode-map (kbd "v J") 'jr/gnus-mark-as-read-and-move-to-junk)
(define-key gnus-summary-mode-map (kbd "v R") 'jr/gnus-mark-as-read-and-move-to-references)

(define-key gnus-article-mode-map (kbd "v J") 'jr/gnus-mark-as-read-and-move-to-junk)
(define-key gnus-article-mode-map (kbd "v R") 'jr/gnus-mark-as-read-and-move-to-references)

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

(defun jr/message-dispatch-ispell-dictionary-on-group ()
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

(add-hook 'message-mode-hook 'jr/message-dispatch-ispell-dictionary-on-group)

(define-key gnus-summary-mode-map "F" 'gnus-summary-wide-reply-with-original)
(define-key gnus-article-mode-map "F" 'gnus-article-wide-reply-with-original)

(defvar jr/gnus-work-email-header-regex "Johnny\\.Ruiz@ticketnetwork\\.com"
  "Regular expression for work email headers")

(defun jr/gnus-posting-from-work-p ()
  "Return non-nil if `gnus-current-headers' reflect they are from a work email."
  (when gnus-current-headers
    (let* ((extra-headers (mail-header-extra gnus-current-headers))
           (to-header (alist-get 'To extra-headers))
           (cc-header (alist-get 'Cc extra-headers)))
      (or (and to-header (string-match jr/gnus-work-email-header-regex to-header))
          (and cc-header (string-match jr/gnus-work-email-header-regex cc-header))))))

(defun jr/gnus-message-get-archive-group ()
  "Return a string or list of string of groups where to save sent
     messages."
  (let ((imap-archive (if (jr/gnus-posting-from-work-p)
                          "nnimap+local:tn/Sent Items"
                        "nnimap+local:Sent")))
    (list imap-archive (format-time-string "sent.%Y-%m"))))

(setq gnus-message-archive-group  '((jr/gnus-message-get-archive-group))
      gnus-gcc-mark-as-read t)

(defconst jr/message-cite-style-english
  '((system-time-locale "en_US.UTF-8")
    (message-cite-function 'message-cite-original-without-signature)
    (message-citation-line-function 'message-insert-formatted-citation-line)
    (message-cite-reply-position 'traditional)
    (message-yank-prefix "> ")
    (message-yank-cited-prefix ">")
    (message-yank-empty-prefix ">")
    (message-citation-line-format "On %Y-%m-%d %R %z, %N wrote:"))
  "Custom message citation style for English-language emails.")

(defconst jr/message-cite-style-spanish
  '((system-time-locale "es_US.UTF-8")
    (message-cite-function 'message-cite-original-without-signature)
    (message-citation-line-function 'message-insert-formatted-citation-line)
    (message-cite-reply-position 'traditional)
    (message-yank-prefix "> ")
    (message-yank-cited-prefix ">")
    (message-yank-empty-prefix ">")
    (message-citation-line-format "El %a, %e-%m-%Y a las %R %z, %N escribió:"))
  "Custom message citation style for Spanish-language emails.")

(setq message-cite-style 'jr/message-cite-style-english)

(setq gnus-cite-attribution-suffix "\\(\\(wrote\\|writes\\|said\\|says\\|escribió\\|>\\)\\(:\\|\\.\\.\\.\\)\\|----- ?Original Message ?-----\\)[        ]*$")

(defun jr/message-from-spanish-mailing-list-p ()
  "Return non-nil if the current `gnus-newsgroup-name'
corresponds to a Spanish language mailing list."
  (string-match "^list\\.es\\." gnus-newsgroup-name))

(setq gnus-posting-styles
      '((".*"
         (signature user-full-name))
        ((file-exists-p "~/.signature")
         (signature-file "~/.signature"))
        ((header "to" "kubb18@me.com")
         ("X-Message-SMTP-Method" "smtp smtp.mail.me.com 587")
         (address "kubb18@me.com"))
        ((header "to" "kubb18@icloud.com")
         ("X-Message-SMTP-Method" "smtp smtp.mail.me.com 587")
         (address "kubb18@icloud.com"))
        ((header "to" "kubb18@gmail.com")
         (address "kubb18@gmail.com")
         ("X-Message-SMTP-Method" "smtp smtp.gmail.com 587"))
        ((header "to" "jeko2000@yandex.com")
         ("X-Message-SMTP-Method" "smtp smtp.yandex.com 587")
         (address "jeko2000@yandex.com"))

        ((jr/gnus-posting-from-work-p)
         ("X-Message-SMTP-Method" "smtp smtp.office365.com 587")
         (address "Johnny.Ruiz@ticketnetwork.com")
         (eval (set (make-local-variable 'message-cite-style)
                    message-cite-style-outlook))
         (signature nil)
         (Organization "TicketNetwork"))

        ((jr/message-from-spanish-mailing-list-p)
         (eval (set (make-local-variable 'message-cite-style)
                    jr/message-cite-style-spanish)))))

(setq gnus-message-replysign t)

(setq gnus-select-method
      '(nnimap "local"
               (nnimap-stream plain)
               (nnimap-address "localhost")))

(require 'smtpmail)

(setq send-mail-function 'smtpmail-send-it
      message-send-mail-function 'smtpmail-send-it
      smtpmail-default-smtp-server "smtp.yandex.com"
      smtpmail-smtp-service 587
      smtpmail-smtp-server "smtp.yandex.com"
      smtpmail-stream-type 'starttls
      smtpmail-debug-info t
      smtpmail-debug-verb t)

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

(require 'gnus-icalendar)
(gnus-icalendar-setup)
(setq gnus-icalendar-org-capture-file org-default-notes-file)
(setq gnus-icalendar-org-capture-headline '("Calendar"))
(gnus-icalendar-org-setup)
