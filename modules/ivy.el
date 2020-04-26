(use-package counsel
  :ensure t
  :bind(("M-x" . counsel-M-x)
        ("C-x C-f" . counsel-find-file)
        ("C-c k" . counsel-ag)
        ("M-i" . counsel-imenu)))

(use-package counsel-projectile
  :ensure t
  :config
  (counsel-projectile-mode))

;; change files from C-o
(use-package wgrep
  :ensure t)

(use-package ivy
  :ensure t
  :bind(("C-c r" . ivy-resume))
  :config
  (setq ivy-count-format "(%d/%d) "
        enable-recursive-minibuffers t
        ivy-initial-inputs-alist nil ;; no default regex
        ivy-use-virtual-buffers t
        magit-completing-read-function 'ivy-completing-read)
  (ivy-mode 1))
