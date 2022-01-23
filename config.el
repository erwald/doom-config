;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(require 'dash)
(require 's)

;; identifying information, e.g. for gpg configuration, email clients, file
;; templates and snippets.
(setq user-full-name "Erich Grunewald"
      user-mail-address "erichgrunewald@gmail.com")

;; theme
(setq doom-font (font-spec :family "IBM Plex Mono" :size 16 :weight 'normal))
(setq doom-theme 'modus-vivendi)

;; modeline
(setq doom-modeline-buffer-file-name-style 'truncate-except-project)

;; email
(set-email-account!
 "gmail"
 '((mu4e-sent-folder       . "/[Gmail]/Sent Mail")
   (mu4e-trash-folder      . "/[Gmail]/Bin")
   (smtpmail-smtp-user     . "erichgrunewald@gmail.com"))
 t)
(setq mu4e-get-mail-command "mbsync gmail"
      ;; get emails and index every 5 minutes
      mu4e-update-interval 300
      ;; send emails with format=flowed
      mu4e-compose-format-flowed t
      ;; don't need to run cleanup after indexing for gmail
      mu4e-index-cleanup nil
      mu4e-index-lazy-check t
      ;; more sensible date format
      mu4e-headers-date-format "%d.%m.%y")
;; this fixes some macos keychain issue.
;; => https://github.com/zzamboni/dot-doom/blob/master/doom.org
(after! auth-source (setq auth-sources (nreverse auth-sources)))
(after! mm-decode
  (add-to-list 'mm-discouraged-alternatives "text/html")
  (add-to-list 'mm-discouraged-alternatives "text/richtext"))

;; flycheck
(setq flycheck-check-syntax-automatically '(save))

;; org
(setq org-directory "~/org/")
(map! "C-c C-g" 'org-forward-heading-same-level) ; C-c C-b is already back
(map! "C-c C-x l" '+org/close-all-folds)
(map! "C-c C-x L" '+org/open-all-folds)
(setq company-global-modes '(not org-mode eshell-mode)) ; disable word completion
(setq org-journal-date-prefix "#+TITLE: "
      org-journal-time-prefix "* "
      org-journal-date-format "%a, %Y-%m-%d"
      org-journal-file-format "%Y-%m-%d.org")
(after! org
  (setq org-capture-templates
        '(("t" "Todo" entry
           (file+headline +org-capture-todo-file "Inbox")
           "* [ ] %?" :prepend t)
          ("n" "Note" entry
           (file+headline +org-capture-notes-file "Inbox")
           "* %u %?\n%i" :prepend t)

          ("r" "Templates for reviews")
          ("rd" "Daily review" entry
           (file+headline "~/org/reviews.org" "Reviews")
           (file "~/org/templates/daily-review-template.org"))
          ("rw" "Weekly review" entry
           (file+headline "~/org/reviews.org" "Reviews")
           (file "~/org/templates/weekly-review-template.org"))
          ("rm" "Monthly review" entry
           (file+headline "~/org/reviews.org" "Reviews")
           (file "~/org/templates/monthly-review-template.org"))
          )))

(require 'cl-lib)
(require 'org-clock)
(defun org-dblock-write:work-report (params)
  "Calculate how many hours too many or too few I have worked.
PARAMS are defined in the template, they are :tstart for the
first day for which there's data (e.g. <2021-08-05>) and :tend
for the last date (e.g. <now>)."
  (cl-flet* ((format-time (time) (format-time-string (org-time-stamp-format t t) time))
             (get-minutes-from-log (t1 t2) (cl-second
                                            (org-clock-get-table-data
                                             (buffer-file-name)
                                             (list :maxlevel 0
                                                   :tstart (format-time t1)
                                                   :tend (format-time t2))))))
      (let* ((start (seconds-to-time (org-matcher-time (plist-get params :tstart))))
             (end (seconds-to-time (org-matcher-time (plist-get params :tend))))
             (total-days-worked 0))
        (progn
          (while (time-less-p start end)
            (let* ((next-day (time-add start (date-to-time "1970-01-02T00:00Z")))
                   (minutes-in-day (get-minutes-from-log start next-day)))
              (if (> minutes-in-day 0)
                  (cl-incf total-days-worked 1))
              (setq start next-day)))
          (let* ((total-minutes-worked (get-minutes-from-log start end))
                 (hours-worked (/ total-minutes-worked 60.0))
                 (hours-per-workday 8)
                 (hours-should-work (* total-days-worked hours-per-workday))
                 (hour-difference (- hours-worked hours-should-work)))
            (insert (format "%0.1f" hour-difference)))))))

;; org-roam
(setq org-roam-directory (file-truename "~/org-roam"))
(setq +org-roam-open-buffer-on-find-file nil)

;; projectile
(setq projectile-project-search-path '(("~/Documents/projects/programming/" . 1)
                                       ("~/Documents/projects/" . 1)))
(setq projectile-globally-ignored-file-suffixes
      '(".png" ".jpg" ".gif" ".pdf" ".ipynb"))

;; mit/gnu scheme
(setq geiser-active-implementations '(mit))
(setq geiser-mit-binary "/usr/local/bin/mit-scheme")
(setq geiser-repl-use-other-window nil)

;; julia
(setenv "JULIA_NUM_THREADS" "8") ; tell julia to use all 8 virtual cores
(setq lsp-julia-package-dir nil)
(setq lsp-enable-folding t)
(setq lsp-julia-default-environment "~/.julia/environments/v1.6")

;; python
(map! :desc "Show the Poetry menu." "C-c y" 'poetry)

;; editor
(setq-default delete-by-moving-to-trash t fill-column 120)
(setq display-line-numbers-type t scroll-margin 2)
(add-to-list 'default-frame-alist '(fullscreen . maximized)) ; start maximized
(global-display-fill-column-indicator-mode) ; show fill col by default
(+global-word-wrap-mode +1) ; turn on soft word wrap almost everywhere
(after! which-key (setq which-key-idle-delay 0.5)) ; open popup more quickly

;; shell
(setq vterm-timer-delay 0.01)

;; windows
(map! "M-o" 'ace-window)
(after! ace-window (setq aw-keys '(?a ?r ?s ?t ?q ?w ?f ?p))) ; for colemak

;; associate some file extensions with modes
(add-to-list 'auto-mode-alist '("\\.njk\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tidal\\'" . haskell-mode))

;; general utility keybinds
(map! "C-c b" 'pop-global-mark) ; return to last mark
(map! "C-S-k" 'kill-visual-line) ; note sure about this one, delete?
(map! "C-c C-w" 'fixup-whitespace) ; removes extra whitespace around cursor
(map! "C-M-q" 'indent-pp-sexp) ; correctly indents sexp starting after cursor
(map! "C-x C-a" 'ace-jump-mode) ; jump to char, line or word
(map! "C-x C-S-a" 'ace-jump-mode-pop-mark) ; return to previous ace position
(map! "C-c C-r C-f" 'rotate-text)
(map! "C-c C-r C-b" 'rotate-text-backward)

;; lisp editing
(map! :prefix "C-§"
      "k" 'sp-kill-sexp
      "b f" 'sp-forward-barf-sexp
      "b b" 'sp-backward-barf-sexp
      "s f" 'sp-forward-slurp-sexp
      "s b" 'sp-backward-slurp-sexp
      "r" 'sp-raise-sexp
      "c" 'sp-convolute-sexp
      "s" 'sp-split-sexp
      "j" 'sp-join-sexp
      "w r" 'sp-wrap-round
      "w c" 'sp-wrap-curly
      "w s" 'sp-wrap-square
      "u" 'sp-unwrap-sexp)

;; configure expand region
(use-package! expand-region
  :commands (er/expand-region er/contract-region er/mark-defun
  er/mark-symbol er/mark-next-accessor er/mark-method-call
  er/mark-word er/mark-sentence er/mark-paragraph er/mark-url
  er/mark-email er/mark-python-block
  er/mark-python-block-and-decorator er/mark-python-statement
  er/mark-outsid-python-string er/mark-outer-python-block))
(map! :prefix "C-c x"
      "f" 'er/mark-defun
      "v" 'er/mark-symbol
      "m" 'er/mark-next-accessor
      "c" 'er/mark-method-call
      "/" 'er/mark-comment
      "w" 'er/mark-word
      "s" 'er/mark-sentence
      "p" 'er/mark-paragraph
      "u" 'er/mark-url
      "e" 'er/mark-email
      "y b" 'er/mark-python-block
      "y d" 'er/mark-python-block-and-decorator
      "y e" 'er/mark-python-statement
      "y s" 'er/mark-outside-python-string
      "y o" 'er/mark-outer-python-block)

(defun ehg/split-windows ()
  "Splits windows my way.
=> https://www.simplify.ba/articles/2016/01/25/display-buffer-alist/"
  (interactive)
  ;; Create new window right of the current one. Current window is 80 characters
  ;; (columns) wide.
  (split-window-right 80)
  ;; Go to next window.
  (other-window 1)
  ;; Create new window below current one.
  (split-window-below)
  ;; In current window, either open correct REPL or start shell.
  (if (and (projectile-project-root)
           (--any? (s-contains? ".jl" it)
                   (projectile-project-files (projectile-project-root))))
      (+eval/open-repl-same-window)
    (vterm))
  ;; Never open any buffer in window with shell.
  (set-window-dedicated-p (nth 1 (window-list)) t)
  ;; Go to original window.
  (other-window -2))

;; half-page scrolling
(autoload 'View-scroll-half-page-forward "view")
(autoload 'View-scroll-half-page-backward "view")
(map! "M-p" 'View-scroll-half-page-backward)
(map! "M-n" 'View-scroll-half-page-forward)

;; editing

(defun ehg/snake-case ()
  "Snake-case current region."
  (interactive)
  (if (use-region-p)
      (progn (kill-region (region-beginning) (region-end))
             (insert-before-markers (s-snake-case (pop kill-ring)))
             (when kill-ring-yank-pointer
               (setq kill-ring-yank-pointer kill-ring)))
    (message "No region marked")))

(defun ehg/kebab-case ()
  "Kebab-case current region."
  (interactive)
  (if (use-region-p)
      (progn (kill-region (region-beginning) (region-end))
             (insert-before-markers (s-dashed-words (pop kill-ring)))
             (when kill-ring-yank-pointer
               (setq kill-ring-yank-pointer kill-ring)))
    (message "No region marked")))

(defun ehg/upper-camel-case ()
  "Upper-camel-case current region."
  (interactive)
  (if (use-region-p)
      (progn (kill-region (region-beginning) (region-end))
             (insert-before-markers (s-upper-camel-case (pop kill-ring)))
             (when kill-ring-yank-pointer
               (setq kill-ring-yank-pointer kill-ring)))
    (message "No region marked")))

(defun ehg/lower-camel-case ()
  "Lower-camel-case current region."
  (interactive)
  (if (use-region-p)
      (progn (kill-region (region-beginning) (region-end))
             (insert-before-markers (s-lower-camel-case (pop kill-ring)))
             (when kill-ring-yank-pointer
               (setq kill-ring-yank-pointer kill-ring)))
    (message "No region marked")))

(defun ehg/org-to-markdown ()
  "Convert the contents of the current `org-mode' buffer to markdown and export to clipboard."
  (interactive)
  (progn
    (org-md-export-as-markdown)
    (let* ((s (buffer-substring-no-properties (point-min) (point-max)))
           (lines (-drop-while (lambda (line) (or (not (s-starts-with? "# " line))
                                                  (s-equals? "# Table of Contents" line)))
                               (s-lines s)))
           (s (s-join "\n" lines))
           ;; convert escaped characters to ascii/unicode.
           (s (s-replace-all '(("&lsquo;" . "'")
                               ("&rsquo;" . "'")
                               ("&ldquo;" . "\"")
                               ("&rdquo;" . "\"")
                               ("&#x2013;" . "–"))
                             s))
           (s (replace-regexp-in-string "<a id=\"org[^>]*></a>" "" s))
           ;; fix double spaces after list bullets.
           (s (replace-regexp-in-string "\\(-\\|[[:digit:]]+\.\\)[[:blank:]]\\{2,\\}" "\\1 " s))
           ;; fix footnotes in text.
           (s (replace-regexp-in-string "<sup>[^>]*footref[^>]*>\\([[:digit:]]+\\)</a></sup>" "[^\\1]" s))
           ;; fix footnotes after text.
           (s (replace-regexp-in-string "<sup>[^>]*>\\([[:digit:]]+\\)</a></sup>" "[^\\1]:\t" s))
           ;; NOTE: multiline footnotes need tab indentation to be detected as such.
           (footnote-split (-split-on "# Footnotes" (s-lines s)))
           (footnote-lines (-map (lambda (line) (if (or (s-equals? "" line) (s-starts-with? "[" line))
                                                    line
                                                  (concat "\t" line)))
                                 (cl-second footnote-split)))
           (s (s-join "\n" (-concat (cl-first footnote-split) footnote-lines)))
           ;; compress multiple consecutive line breaks.
           (s (replace-regexp-in-string "\n\\{3,\\}" "\n\n" s))
           ;; remove leading/trailing whitespace.
           (s (s-trim s)))
      (progn
        ;; make sure we took care of all escaped characters.
        (cl-assert (not (s-match "&[[:alnum:]]\\{3,6\\};" s)))
        (kill-new s)
        (kill-buffer (current-buffer))))))

;; blog commands

(defun ehg/blog-post-create ()
  "Create a new empty blog post."
  (interactive)
  (let ((title (read-string "Post title: "))
        (date (read-string "Date (yyyy-mm-dd): ")))
    (ehg/blog-post-create-with-content title date "")))

(defun ehg/blog-post-create-with-content (title date content)
  "Create a new blog post with TITLE, DATE and CONTENT."
  (let* ((root-dir (--first (s-contains? "blog" it) projectile-known-projects))
         (kebab-title (s-dashed-words (s-replace-regexp "\\([[:nonascii:]]\\|'\\)" "" title)))
         (filepath (concat root-dir "posts/" kebab-title ".md"))
         (header (concat "---\n"
                         "layout: layouts/post.njk\n"
                         "title: " title "\n"
                         "date: " date "\n"
                         "tags: post\n"
                         "---\n")))
    (progn (write-region (concat header content) nil filepath)
           (find-file filepath)
           (ehg/blog-post-fix-urls)
           (goto-char (point-min))
           (message "Done!"))))

(defun ehg/blog-post-create-from-org-buffer ()
  "Create a new blog post from content of current `org-mode' buffer."
  (interactive)
  (with-current-buffer buffer
    (let* ((title (read-string "Post title: ")) ; TODO: get title from content
           (date (read-string "Date (yyyy-mm-dd): "))
           (content buffer)) ; TODO: convert content to markdown
      (ehg/blog-post-create-with-content title date content))))

(defun ehg/blog-post-fix-urls ()
  "Fixe all URLs, both to the blog itself & to images (points them to the /img dir) in a blog post."
  (interactive)
  (progn
    (goto-char (point-min))
    (while (re-search-forward "\\(\\[.*\\]+\\)\(https:\/\/www\.erichgrunewald\.com\\([^[:space:]]+\\)\)" nil t)
      (replace-match "\\1({{ '\\2' | url }})"))
    (goto-char (point-min))
    (while (re-search-forward "\\(!\\[.*\\]+\\)\(\\([^{].+\\)\)" nil t)
      (replace-match "\\1({{ '/img/\\2' | url }})"))
    (message "Done!")))
