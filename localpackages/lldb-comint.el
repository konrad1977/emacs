;;; lldb-comint.el --- A small package for interacting with lldb -*- lexical-binding: t -*-
;;; Code:

(require 'comint)

(defvar lldb-comint:buffer-name "*LLDB*"
  "Name of the buffer to use to run `LLDB-comint'.")

(defvar lldb-comint:command "lldb"
  "Application.")

(defvar lldb-comint:arguments '()
  "Command line arguments passed to `lldb'.")

(defvar lldb-comint-mode-map
  (let ((map (nconc (make-sparse-keymap) comint-mode-map)) )
    (define-key map "\t" 'completion-at-point)
    map)
  "Basic mode map for `lldb-comint'.")

(defvar lldb-comint:prompt-regexp "^\\(?:\\[[^@]+@[^@]+\\]\\)"
  "Prompt for `lldb-comint'.")

(defconst lldb-keywords
  '("platform" "select" "connect" "set")
  "List of keywords to highlight in `lldb-font-lock-keywords'.")

(defconst lldb-verbs-keywords
  '("ios-simulator" "attach" "c" "call" "continue" "detach" "di" "dis" "display" "down"
    "env" "exit" "quit" "f" "file" "finish" "history" "iamge" "j" "jump"
    "kill" "l" "list" "n" "next" "nexti" "ni" "p" "parray" "poarray" "po"
    "p" "v" "b" "bt" "c" "s" "shell" "sif" "step" "stepi" "t" "tbreak" "undisplay"
    "up" "var" "vo" "x" "trace")
  "List of verbs to highlight in `lldb-font-lock-keywords'.")

(defvar lldb-font-lock-keywords
  (list
   ;; highlight all the reserved commands.
   `(,(concat "\\_<" (regexp-opt lldb-keywords) "\\_>") . font-lock-keyword-face)
   `(,(concat "\\_<" (regexp-opt lldb-verbs-keywords) "\\_>") . font-lock-constant-face))
  "Additional expressions to highlight in `lldb-comint-mode'.")

(define-derived-mode lldb-comint-mode comint-mode "lldb-comint"
  "Major mode for `lldb'."
  (setq comint-prompt-regexp lldb-comint:prompt-regexp)
  (setq comint-prompt-read-only t)
  (set (make-local-variable 'paragraph-separate) "\\'")
  (set (make-local-variable 'font-lock-defaults) '(lldb-font-lock-keywords t))
  (set (make-local-variable 'paragraph-start) lldb-comint:prompt-regexp))

(cl-defun lldb-comint:runWith (&key path &key args)
  "Run from a specific as (PATH)."
  (let* ((lldb lldb-comint:command)
         (default-directory path)
         (args args)
         (buffer (get-buffer-create lldb-comint:buffer-name))
         (proc-alive (comint-check-proc buffer)))
    (unless proc-alive
      (with-current-buffer buffer
        (apply 'make-comint-in-buffer "lldb" buffer
               lldb nil args)
        (lldb-comint-mode)))
    (when buffer
      (pop-to-buffer buffer))))

(defun lldb-comint:run ()
  "Run `'LLDB inside Emacs."
  (interactive)
  (let* ((lldb lldb-comint:command)
         (buffer (get-buffer-create lldb-comint:buffer-name))
         (proc-alive (comint-check-proc buffer)))
    (unless proc-alive
      (with-current-buffer buffer
        (apply 'make-comint-in-buffer "lldb" buffer
               lldb nil lldb-comint:arguments)
        (lldb-comint-mode)))
    (when buffer
      (pop-to-buffer buffer))))

(provide 'lldb-comint)
;;; lldb-comint.el ends here
