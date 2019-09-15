(use-package counsel
  :straight t
  :bind(("M-x" . counsel-M-x)
        ("C-x C-f" . counsel-find-file)
        ("C-c k" . counsel-ag)
        ("M-i" . counsel-imenu)))

(use-package counsel-projectile
  :straight t
  :config
  (counsel-projectile-mode))

(use-package ivy
  :straight t
  :bind(("C-c r" . ivy-resume))
  :config
  (setq ivy-count-format "(%d/%d) "
        enable-recursive-minibuffers t
        ivy-initial-inputs-alist nil ;; no default regex
        ivy-use-virtual-buffers t
        magit-completing-read-function 'ivy-completing-read)
  (ivy-mode 1))

(use-package ivy-rich
  :straight t
  :config
  (ivy-rich-mode 1))
