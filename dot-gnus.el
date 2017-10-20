;;; gnus configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'nnir)

;;main
(setq user-mail-address "kubb18@gmail.com"
      user-full-name "Johnny R."
      imap-shell-program "dovecot -c ~/.dovecotrc --exec-mail imap"
      gnus-ignored-from-addresses "Johnny R"
      gnus-use-full-window t)

(setq gnus-select-method
      '(nnimap "Mail"
               (nnimap-address "localhost")
               (nnimap-stream network)
               (nnimap-authenticator login)))
(setq smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587)

(setq gnus-check-new-newsgroups nil
      gnus-save-newsrc-file nil
      gnus-subscribe-newsgroup-method 'gnus-subscribe-zombies
      gnus-save-newsrc-file t
      gnus-use-dribble-file t
      gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]"
      gnus-read-active-file t
      gnus-large-newsgroup 1000
      gnus-auto-select-first t
      gnus-auto-select-subject 'first
      gnus-group-default-list-level 5
      gnus-group-sort-function 'gnus-group-sort-by-unread)

;; cron hooks
(defun start-mail-cron ()
  (let ((start "~/rep/emacs-config/scripts/start-mail-cron-job.sh"))
    (if (file-exists-p start)
          (shell-command start "*cron*" nil))))

(defun stop-mail-cron ()
  (let ((stop "~/rep/emacs-config/scripts/stop-mail-cron-job.sh"))
    (if (file-exists-p stop)
          (shell-command stop "*cron*" nil))))

(add-hook 'gnus-started-hook 'start-mail-cron)
(add-hook 'gnus-exit-gnus-hook 'stop-mail-cron)

;; group buffer

(setq gnus-summary-next-group-on-exit t
      gnus-group-line-format "%M%S%p%P%5y|%-5t %(%-20,20g%)\n"
      gnus-topic-line-format "%i %(%{%n%}%) (%A)%v\n")

(add-hook 'gnus-select-group-hook 'gnus-group-set-timestamp)
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)
(add-hook 'gnus-group-mode-hook 'hl-line-mode)

;; summary

(setq
 gnus-summary-line-format
 ;;"%0{%U%R%z%}%3{│%}%1{%&user-date;%}%3{│%}  %4{%-40,40f%}  %3{│%} %1{%B%}%s\n"
 "%U%R%z%3{│%}%&user-date;%3{│%}  %-40,40f  %3{│%} %B%s\n"
 gnus-user-date-format-alist '((t . "%Y-%m-%d %H:%M"))
 gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references)

(add-hook 'gnus-summary-mode-hook 'hl-line-mode)

;;Threading
(setq gnus-thread-indent-level 1)

;; Articles
(setq gnus-posting-styles
      '((".*"
         (signature user-full-name))
        (and (eq system-type 'gnu/linux)
             (file-exists-p "~/.signature"))
        (signature-file "~/.signature")))

(setq gnus-visible-headers "^From:\\|^To:\\|^Subject:\\|^Date:")
(setq gnus-treat-hide-citation t)
(setq mm-discouraged-alternatives '("text/html" "text/richtext"))

;;Scoring
(setq gnus-summary-exit-hook 'gnus-summary-bubble-group)

;;bbdb
(require 'bbdb)
(bbdb-initialize 'message 'gnus 'sendmail)
(add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)
(setq bbdb/mail-auto-create-p t
      bbdb/news-auto-create-p t)
(setq bbdb-send-mail-style 'gnus)

(add-hook 'message-mode-hook
          '(lambda ()
             (flyspell-mode t)
             (local-set-key "<TAB>" 'bbdb-complete-name)))

;;Other
(setq gnus-always-read-dribble-file t)
(setq gnus-summary-display-while-building 10)

(defun exit-gnus-on-exit () 
  (if (and (fboundp 'gnus-group-exit)
           (gnus-alive-p))
      (with-current-buffer (get-buffer "*Group*")
        (let (gnus-interactive-exit)
          (gnus-group-exit)))))

(add-hook 'kill-emacs-hook 'exit-gnus-on-exit)

;;layout

(defface gnus-summary-expirable-face
  '((((class color) (background dark))
     (:foreground "grey50" :italic t :strike-through t))
    (((class color) (background light))
     (:foreground "grey55" :italic t :strike-through t)))
  "Face used to highlight articles marked as expirable."
  :group 'gnus-summary-visual)

(push '((eq mark gnus-expirable-mark) . gnus-summary-expirable-face)
      gnus-summary-highlight)

(if window-system
    (setq
     gnus-sum-thread-tree-false-root      ""
     gnus-sum-thread-tree-single-indent   ""
     gnus-sum-thread-tree-root            ""
     gnus-sum-thread-tree-vertical        "|"
     gnus-sum-thread-tree-leaf-with-other "+-> "
     gnus-sum-thread-tree-single-leaf     "\\-> "
     gnus-sum-thread-tree-indent          " "))

(gnus-add-configuration
 '(article
   (horizontal 1.0
               (vertical 30 (group 1.0))
               (vertical 1.0
                         (summary 0.40 point)
                         (article 1.0)))))

(gnus-add-configuration
 '(summary
   (horizontal 1.0
               (vertical 30 (group 1.0))
               (vertical 1.0 (summary 1.0 point)))))

;; send message

(defun jj-gnus-custom-message-send-hook ()
  (ispell-message))

(add-hook 'message-send-hook 'jj-gnus-custom-message-send-hook)

;;sorting

(setq gnus-thread-sort-functions
      '(gnus-thread-sort-by-number
        gnus-thread-sort-by-subject
        (not gnus-thread-sort-by-total-score)))

;;article backlog
(setq gnus-keep-backlog 25)

;;security
(setq mm-verify-option 'always)

