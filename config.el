;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; identifying information, e.g. for gpg configuration, email clients, file
;; templates and snippets.
(setq user-full-name "Erich Grunewald"
      user-mail-address "erichgrunewald@gmail.com")

;; theme
(setq doom-font (font-spec :family "Apercu" :size 16 :weight 'normal))
(setq doom-theme 'doom-homage-black)

;; modeline
(setq doom-modeline-buffer-file-name-style 'truncate-except-project)

;; email
(set-email-account!
 "gmail"
 '((mu4e-sent-folder       . "/[Gmail]/Sent Mail")
   (mu4e-trash-folder      . "/[Gmail]/Bin")
   (smtpmail-smtp-user     . "erichgrunewald@gmail.com"))
 t)
(setq  mu4e-get-mail-command "mbsync gmail"
       mu4e-compose-format-flowed t
       ;; don't need to run cleanup after indexing for gmail
       mu4e-index-cleanup nil
       mu4e-index-lazy-check t)
;; this fixes some macos keychain issue.
;; => https://github.com/zzamboni/dot-doom/blob/master/doom.org
(after! auth-source (setq auth-sources (nreverse auth-sources)))

;; flycheck
(setq flycheck-check-syntax-automatically '(save))

;; org
(setq org-directory "~/org/")
(map! "C-c C-g" 'org-forward-heading-same-level) ; C-c C-b is already back
(map! "C-c C-x l" '+org/close-all-folds)
(map! "C-c C-x L" '+org/open-all-folds)
(setq company-global-modes '(not org-mode)) ; disable word completion
(setq org-journal-date-prefix "#+TITLE: "
      org-journal-time-prefix "* "
      org-journal-date-format "%a, %Y-%m-%d"
      org-journal-file-format "%Y-%m-%d.org")
(after! org
  (setq org-capture-templates
        '(("t" "Todo" entry
           (file+headline +org-capture-todo-file "Inbox")
           "* [ ] %?" :prepend t)
          ("s" "Someday todo" entry
           (file+headline "~/org/somedaymaybe.org" "Someday / Maybe")
           "* SOMEDAY %?")
          ("m" "Maybe todo" entry
           (file+headline "~/org/somedaymaybe.org" "Someday / Maybe")
           "* MAYBE %?")
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

          ;; Will use {project-root}/{todo,notes,changelog}.org, unless a
          ;; {todo,notes,changelog}.org file is found in a parent directory.
          ;; Uses the basename from `+org-capture-todo-file',
          ;; `+org-capture-changelog-file' and `+org-capture-notes-file'.
          ("p" "Templates for projects")
          ("pt" "Project-local todo" entry  ; {project-root}/todo.org
           (file+headline +org-capture-project-todo-file "Inbox")
           "* TODO %?" :prepend t)
          ("pn" "Project-local notes" entry  ; {project-root}/notes.org
           (file+headline +org-capture-project-notes-file "Inbox")
           "* %U %?" :prepend t)
          ("pc" "Project-local changelog" entry  ; {project-root}/changelog.org
           (file+headline +org-capture-project-changelog-file "Unreleased")
           "* %U %?" :prepend t)

          ;; Will use {org-directory}/{+org-capture-projects-file} and store
          ;; these under {ProjectName}/{Tasks,Notes,Changelog} headings. They
          ;; support `:parents' to specify what headings to put them under, e.g.
          ;; :parents ("Projects")
          ("o" "Centralized templates for projects")
          ("ot" "Project todo" entry
           (function +org-capture-central-project-todo-file)
           "* TODO %?" :heading "Tasks" :prepend nil)
          ("on" "Project notes" entry
           (function +org-capture-central-project-notes-file)
           "* %U %?" :heading "Notes" :prepend t)
          ("oc" "Project changelog" entry
           (function +org-capture-central-project-changelog-file)
           "* %U %?" :heading "Changelog" :prepend t)
          )))

(defun erich/org-to-clipboard-as-markdown ()
  "Export marked org text to Markdown and put it in clipboard.

I found this somewhere but cannot locate the source now."
  ;; TODO: don't convert unicode chars to ascii
  (interactive)
  ;; TODO: fix unused variable warning
  (save-window-excursion (let ((org-export-with-toc nil))
                           (with-current-buffer (org-md-export-as-markdown)
                             (with-no-warnings (mark-whole-buffer))
                             (clipboard-kill-region (point-min) (point-max))
                             (kill-buffer-and-window)))))

;; org-roam
(setq org-roam-directory (file-truename "~/org-roam"))
(setq +org-roam-open-buffer-on-find-file nil)

;; projectile
(setq projectile-project-search-path
      (append (cdddr (directory-files "~/Documents/projects/programming" t))
              (cdddr (directory-files "~/Documents/projects" t))))
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
(setq-default delete-by-moving-to-trash t
              fill-column 120)
(setq display-line-numbers-type t
      scroll-margin 2)
(add-to-list 'default-frame-alist '(fullscreen . maximized)) ; start maximized
(global-display-fill-column-indicator-mode) ; show fill col by default
(+global-word-wrap-mode +1) ; turn on soft word wrap almost everywhere
(after! which-key (setq which-key-idle-delay 1)) ; open popup more quickly

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
(map! "C-x C-M-a" 'ace-jump-mode-pop-mark) ; return to previous ace position

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

(defun erich/split-windows ()
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

(defun erich/snake-case ()
  "Snake-case current region."
  (interactive)
  (if (use-region-p)
      (progn (kill-region (region-beginning) (region-end))
             (insert-before-markers (s-snake-case (pop kill-ring)))
             (when kill-ring-yank-pointer
               (setq kill-ring-yank-pointer kill-ring)))
    (message "No region marked")))

(defun erich/kebab-case ()
  "Kebab-case current region."
  (interactive)
  (if (use-region-p)
      (progn (kill-region (region-beginning) (region-end))
             (insert-before-markers (s-dashed-words (pop kill-ring)))
             (when kill-ring-yank-pointer
               (setq kill-ring-yank-pointer kill-ring)))
    (message "No region marked")))

(defun erich/upper-camel-case ()
  "Upper-camel-case current region."
  (interactive)
  (if (use-region-p)
      (progn (kill-region (region-beginning) (region-end))
             (insert-before-markers (s-upper-camel-case (pop kill-ring)))
             (when kill-ring-yank-pointer
               (setq kill-ring-yank-pointer kill-ring)))
    (message "No region marked")))

(defun erich/lower-camel-case ()
  "Lower-camel-case current region."
  (interactive)
  (if (use-region-p)
      (progn (kill-region (region-beginning) (region-end))
             (insert-before-markers (s-lower-camel-case (pop kill-ring)))
             (when kill-ring-yank-pointer
               (setq kill-ring-yank-pointer kill-ring)))
    (message "No region marked")))

;; blog commands

(defun erich/blog-post-create ()
  "Creates a new empty blog post."
  (interactive)
  (let ((root-dir (projectile-project-root)))
    (if (and root-dir (s-contains? "/blog/" root-dir))
        (let ((title (read-string "Post title: "))
              (date (read-string "Date (yyyy-mm-dd): ")))
          (let ((file-path
                 (concat root-dir "posts/"
                         (s-dashed-words
                          (s-replace-regexp "[[:nonascii:]]" ""
                                            (s-replace "'" "" title)))
                         ".md")))
            (progn (write-region (concat
                                  "---\n"
                                  "layout: layouts/post.njk\n"
                                  "title: " title "\n"
                                  "date: " date "\n"
                                  "tags: post\n"
                                  "---\n")
                                 nil
                                 file-path)
                   (find-file file-path)
                   (goto-char (point-min))
                   (message "Done!"))))
      (message "Not in /blog directory."))))


(defun erich/blog-post-fix-urls ()
  "Fixes all URLs, both to the blog itself & to images (points them to the /img
dir) in a blog post."
  (interactive)
  (progn
    (goto-char (point-min))
    (while (re-search-forward "\\(\\[.*\\]+\\)\(https:\/\/www\.erichgrunewald\.com\\([^[:space:]]+\\)\)" nil t)
      (replace-match "\\1({{ '\\2' | url }})"))
    (goto-char (point-min))
    (while (re-search-forward "\\(!\\[.*\\]+\\)\(\\([^{].+\\)\)" nil t)
      (replace-match "\\1({{ '/img/\\2' | url }})"))
    (message "Done!")))
