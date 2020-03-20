(use-package exec-path-from-shell
    :ensure t
    :config
    (exec-path-from-shell-initialize))

(use-package recentf
  :init
  (setq-default recentf-max-saved-items 50)
  (setq-default recentf-max-menu-items 50)
  (setq recentf-exclude '((expand-file-name package-user-dir)
                          ".cache"
                          ".cask"
                          ".elfeed"
                          "bookmarks"
                          "cache"
                          "ido.*"
                          "persp-confs"
                          "recentf"
                          "undo-tree-hist"
                          "url"
                          "elpa/*"
                          "node_modules/*"
                          "COMMIT_EDITMSG\\'"))
  :config
  (recentf-mode 1))

(use-package all-the-icons
  :ensure t)

(use-package dired
  :bind ("C-x C-j" . dired-jump)
  :config
  (setq dired-dwim-target t))

(use-package which-key
  :ensure t
  :config
  (which-key-mode 1))

(use-package whitespace
  :hook (prog-mode . whitespace-mode)
  :config
  (setq whitespace-style '(face tabs tab-mark trailing empty)))

(use-package ag
  :ensure t
  :commands (ag ag-regexp ag-project))

(use-package projectile
  :ensure t
  :bind-keymap ("C-x p" . projectile-command-map)
  :config
  (setq projectile-file-exists-remote-cache-expire nil
        projectile-dynamic-mode-line t
        projectile-mode-line-function '(lambda () (format " [%s]" (projectile-project-name)))
        projectile-completion-system 'ivy
        projectile-sort-order 'recently-active
        projectile-indexing-method 'hybrid)
  (projectile-mode +1))

(use-package ibuffer
  :bind(("C-x C-b" . ibuffer))
  :config
  (setq ibuffer-display-summary nil)
  :hook ibuffer . (lambda ()
                    (ibuffer-projectile-set-filter-groups)
                    (unless (eq ibuffer-sorting-mode 'recency)
                      (ibuffer-do-sort-by-recency))))

(use-package company
  :ensure t
  :config
  (global-company-mode t))

(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode))

(use-package flyspell
  :hook ((prog-mode . flyspell-prog-mode)
         (text-mode . flyspell-mode)))

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package smartparens
  :ensure t
  :config
  (require 'smartparens-config))

(use-package markdown-mode
  :ensure t
  :config
  (add-hook 'markdown-mode-hook #'toggle-word-wrap)
  (setq markdown-command "/usr/bin/pandoc"))

(use-package elfeed
  :ensure t
  :bind ("C-x w" . elfeed)
  :config
  (setq elfeed-feeds
        '("https://planet.emacslife.com/atom.xml"
          "https://www.ruby-lang.org/en/feeds/news.rss"
          "https://rubyweekly.com/rss"
          "https://wilhelmbot.com/feed.xml"
          "https://emacsair.me/feed.xml"
          "https://mikrotik.com/download.rss")))

(use-package easy-jekyll
  :ensure t
  :init
  (setq easy-jekyll-basedir "~/projects/wkirschbaum/blog"
        easy-jekyll-url "https://wilhelmbot.com"
        easy-jekyll-root ""
        easy-jekyll-previewtime "300")
  :bind ("C-c C-e" . easy-jekyll))

(use-package tramp
  :config
  (setq tramp-default-method "ssh"
        remote-file-name-inhibit-cache nil
        tramp-completion-reread-directory-timeout nil
        vc-ignore-dir-regexp (format "%s\\|%s"
                vc-ignore-dir-regexp
                tramp-file-name-regexp)))

;; Replaces 'delete-blank-lines command
(use-package shrink-whitespace
  :ensure t
  :bind ("C-x C-o" . shrink-whitespace))

(use-package restclient
  :ensure t)

;;; Experimental
;; This is suppose to make predictions better, based on statistics
(use-package prescient
  :ensure t
  :config
  (prescient-persist-mode))

(use-package ivy-prescient
  :ensure t
  :config
  (ivy-prescient-mode))

(use-package company-prescient
  :ensure t
  :config
  (company-prescient-mode))

;;; extra.el ends here
