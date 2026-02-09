;;; quelpa-test.el --- Tests for quelpa  -*- lexical-binding: t; -*-

(require 'quelpa)
(require 'ert)

;;; Test infrastructure

(defvar quelpa-test--shared-melpa-dir nil
  "Path to a shared MELPA checkout, created once for all tests.
Avoids cloning MELPA for every individual test.")

(defun quelpa-test--ensure-shared-melpa ()
  "Ensure a shared MELPA checkout exists and return its path.
On first call, clones MELPA into a temp directory.  Subsequent
calls return the cached path immediately."
  (unless (and quelpa-test--shared-melpa-dir
               (file-directory-p
                (expand-file-name ".git" quelpa-test--shared-melpa-dir)))
    (let ((quelpa-melpa-dir (make-temp-file "quelpa-test-melpa-" t)))
      (quelpa-checkout-melpa t)
      (setq quelpa-test--shared-melpa-dir quelpa-melpa-dir)))
  quelpa-test--shared-melpa-dir)

(defmacro quelpa-test-sandbox (&rest body)
  "Run BODY with all quelpa/package paths redirected to a temp directory.
Creates a fresh temporary `user-emacs-directory' so that tests never
install packages into the real Emacs configuration.  MELPA is
checked out once and shared read-only across all sandboxes; each
sandbox gets its own writable recipes directory.  The sandbox
directory is deleted when BODY finishes (or signals)."
  (declare (indent 0) (debug t))
  `(let* ((shared-melpa (quelpa-test--ensure-shared-melpa))
          (sandbox (make-temp-file "quelpa-test-" t))
          (user-emacs-directory (file-name-as-directory sandbox))
          (quelpa-dir (expand-file-name "quelpa" user-emacs-directory))
          (quelpa-melpa-dir shared-melpa)
          (quelpa-build-dir (expand-file-name "build" quelpa-dir))
          (quelpa-packages-dir (expand-file-name "packages" quelpa-dir))
          (quelpa-persistent-cache-file (expand-file-name "cache" quelpa-dir))
          ;; Per-sandbox writable recipes dir + shared MELPA recipes.
          (sandbox-recipes-dir (expand-file-name "recipes" quelpa-dir))
          (quelpa-melpa-recipe-stores
           (list sandbox-recipes-dir
                 (expand-file-name "recipes" shared-melpa)))
          (package-user-dir (expand-file-name "elpa" user-emacs-directory))
          ;; Reset state so `quelpa-setup-p' re-initializes.
          (quelpa-initialized-p nil)
          (quelpa-cache nil)
          (package-alist nil)
          ;; Shared MELPA is already cloned; skip update attempts.
          (quelpa-update-melpa-p nil))
     ;; Ensure the per-sandbox recipes directory exists.
     (make-directory sandbox-recipes-dir t)
     (unwind-protect
         (progn ,@body)
       (delete-directory sandbox t))))

(defmacro quelpa-deftest (name arglist &rest body)
  "Define Quelpa ERT test.
Defines ERT test with `quelpa-' prepended to NAME and
`quelpa-setup-p' as a precondition to BODY.  ARGLIST is passed to
`ert-deftest', which see.

Each test runs inside `quelpa-test-sandbox', so packages are
installed into a disposable temporary directory."
  (declare (indent 2))
  (let ((name (intern (concat "quelpa-" (symbol-name name))))
        (name-async (intern (concat "quelpa-" (symbol-name name) "-async")))
        (ert-plist (cl-loop while (keywordp (car body))
                            collect (pop body)
                            collect (pop body))))
    `(progn
       (ert-deftest ,name ()
         ,@ert-plist
         (quelpa-test-sandbox
           (should (quelpa-setup-p))
           (cl-macrolet ((should-install (quelpa-args)
                           (let* ((name (pcase quelpa-args
                                          ((pred atom) quelpa-args)
                                          (`((,name . ,_) . ,_) name)
                                          (`(,(and name (pred atom)) . ,_) name))))
                             `(progn
                                (quelpa ',quelpa-args)
                                (should (package-installed-p ',name))))))
             ,@body)))

       (ert-deftest ,name-async ()
         ,@ert-plist
         (quelpa-test-sandbox
           (should (quelpa-setup-p))
           (cl-macrolet ((should-install (quelpa-args)
                           (let* ((name (pcase quelpa-args
                                          ((pred atom) quelpa-args)
                                          (`((,name . ,_) . ,_) name)
                                          (`(,(and name (pred atom)) . ,_) name))))
                             `(progn
                                (quelpa ',quelpa-args)
                                (should (package-installed-p ',name))))))
             (let ((quelpa-async-p t)
                   (quelpa--in-async-context t))
               ,@body)))))))

;;; Unit tests (mocked, no network)

(quelpa-deftest expand-recipe ()
  "Should be expanding correctly as return value and into buffer."
  (let ((package-build-rcp '(package-build :repo "melpa/package-build" :fetcher github)))
    ;; Write a mock recipe file so `quelpa-get-melpa-recipe' can find it.
    (with-temp-file (expand-file-name "package-build"
                                      (car quelpa-melpa-recipe-stores))
      (prin1 package-build-rcp (current-buffer)))
    (should
     (equal
      (quelpa-expand-recipe 'package-build)
      package-build-rcp))
    (should
     (equal
      (with-temp-buffer
        (cl-letf (((symbol-function 'quelpa-interactive-candidate)
                   (lambda ()
                     (interactive)
                     'package-build)))
          (call-interactively 'quelpa-expand-recipe))
        (buffer-string))
      (prin1-to-string package-build-rcp)))))

(quelpa-deftest arg-rcp ()
  "Ensure `quelpa-arg-rcp' always returns the correct RCP format."
  (let ((quelpa-rcp '(quelpa :repo "quelpa/quelpa" :fetcher github))
        (package-build-rcp '(package-build :repo "melpa/package-build" :fetcher github)))
    ;; Write a mock recipe file so bare-name lookups work.
    (with-temp-file (expand-file-name "package-build"
                                      (car quelpa-melpa-recipe-stores))
      (prin1 package-build-rcp (current-buffer)))
    (should (equal (quelpa-arg-rcp quelpa-rcp)
                   quelpa-rcp))
    (should (equal (quelpa-arg-rcp 'package-build)
                   package-build-rcp))
    (should (equal (quelpa-arg-rcp '(package-build))
                   package-build-rcp))
    (should (equal (quelpa-arg-rcp "package-build")
                   package-build-rcp))))

(quelpa-deftest version>-p ()
  "Passed version should correctly be tested against the mocked
`package-alist' and built-in packages."
  (let ((package-alist (cond ((functionp 'package-desc-vers)
                              ;; old package-alist format
                              '((quelpa . [(1 0 0 20140406 1613)
                                           ((package-build (0)))
                                           "Emacs Lisp packages built directly from source"])))
                             ((version< emacs-version "26.1")
                              ;; New package-alist format, but before Emacs 26.
                              '((quelpa
                                 [cl-struct-package-desc
                                  quelpa
                                  (1 0 0 20140406 1613)
                                  "Emacs Lisp packages built directly from source"
                                  ((package-build (0))) nil nil "test" nil nil])))
                             ((functionp 'record)
                              ;; Emacs 26+ records.
                              `((quelpa
                                 ,(record 'package-desc
                                          'quelpa
                                          '(1 0 0 20140406 1613)
                                          "Emacs Lisp packages built directly from source"
                                          '((package-build (0))) nil nil "test" nil nil)))))))
    (should (quelpa-version>-p 'quelpa nil))
    (should (quelpa-version>-p 'quelpa "0"))
    (should-not (quelpa-version>-p 'quelpa "1.0.0.20140406.1613"))
    (should (quelpa-version>-p 'quelpa "1.0.0.20140406.1612"))
    (should (quelpa-version=-p '(quelpa :version-type elpa) "20140406.1613"))
    (should (quelpa-version=-p '(quelpa :version-type elpa) "20140406.1613"))
    ;; For not installed package, it should always older than any package version
    (should-not (quelpa-version>-p 'foo "0snapshot0.0.1"))
    (let ((package--builtin-versions '((foobar . (20140406 1613)))))
      (should (quelpa-version>-p 'foobar "0"))
      (should-not (quelpa-version>-p 'foobar "20140406.1613"))
      (should (quelpa-version>-p 'foobar "20140406.1612")))))

(quelpa-deftest check-hash ()
  "Make sure that old file hash is correctly compared with the new one
  and only when it has changed the new time-stamp is returned."
  (cl-letf* ((stamp-info '("20140413.90742" . "7e4c099e65d254f62e64b581c42ddeb3c487064b"))
             (hash "4935a306e358cbd0d9bd200e13ceb1e44942b323")
             ((symbol-function 'quelpa-build--read-from-file) (lambda (file) stamp-info))
             ((symbol-function 'quelpa-build--dump) (lambda (content file)))
             ((symbol-function 'quelpa-build--expand-source-file-list) 'ignore)
             ((symbol-function 'secure-hash) (lambda (&rest args) hash))
             ((symbol-function 'delete-directory) 'ignore)
             ((symbol-function 'make-directory) 'ignore)
             ((symbol-function 'copy-directory) 'ignore)
             ((symbol-function 'copy-file) 'ignore))
    (should-not (equal (quelpa-check-hash 'foobar nil "/" "baz")
                       (car stamp-info)))
    (setq hash (cdr stamp-info))
    (should (equal (quelpa-check-hash 'foobar nil "/" "baz")
                   (car stamp-info)))))

(quelpa-deftest cache ()
  "Ensure that installing a package with a different recipe will
update an existing cache item."
  (cl-letf ((quelpa-cache nil)
            ((symbol-function 'quelpa-package-install) (lambda (&rest _) '(1 0)))
            ((symbol-function 'quelpa--delete-obsoleted-package) 'ignore))
    (quelpa '(makey :fetcher github :repo "mickeynp/makey"))
    ;; Re-install by bare name.  `quelpa-arg-rcp' resolves the name
    ;; from `quelpa-melpa-recipe-stores'; include the cache so the
    ;; test doesn't depend on a real MELPA checkout.
    (let ((quelpa-melpa-recipe-stores
           (append quelpa-melpa-recipe-stores (list quelpa-cache))))
      (quelpa 'makey))
    (should (equal quelpa-cache '((makey :fetcher github :repo "mickeynp/makey"))))
    (quelpa '(makey :fetcher github :repo "foo/makey"))
    (should (equal quelpa-cache '((makey :fetcher github :repo "foo/makey"))))
    ))

(quelpa-deftest cache-regressions ()
  (cl-letf ((quelpa-cache nil)
            ((symbol-function 'quelpa-package-install) (lambda (&rest _) '(1 0)))
            ((symbol-function 'quelpa--delete-obsoleted-package) 'ignore))
    (quelpa '(multiple-cursors :fetcher github :repo "magnars/multiple-cursors.el" :stable t))
    (should (equal quelpa-cache '((multiple-cursors :fetcher github
                                                    :repo "magnars/multiple-cursors.el"
                                                    :stable t))))
    (let ((quelpa-stable-p t))
      (quelpa '(multiple-cursors :fetcher github
                                 :repo "magnars/multiple-cursors.el")
              :stable nil))
    (should (equal quelpa-cache '((multiple-cursors :fetcher github
                                                    :repo "magnars/multiple-cursors.el"))))))

(quelpa-deftest stable ()
  (cl-letf ((quelpa-cache nil)
            ((symbol-function 'quelpa-package-install) (lambda (&rest _) "1.0.0"))
            ((symbol-function 'package-delete) #'ignore))
    (quelpa '(2048-game :fetcher hg :url "https://hg.sr.ht/~zck/game-2048" :stable t))
    (quelpa '(elx :fetcher github :repo "emacscollective/elx") :stable t)
    (let ((quelpa-stable-p t))
      (quelpa '(imgur :fetcher github :repo "myuhe/imgur.el")))
    (should (equal (mapcar (lambda (item) (plist-get (cdr item) :stable))
                           quelpa-cache)
                   '(t t t)))))

(quelpa-deftest stable-upgrade ()
  ;; FIXME: Fails due to: (void-function quelpa-build--checkout-nil)
  :expected-result :failed
  (should-install (anzu :stable t))
  (should-install ((company :repo "company-mode/company-mode" :fetcher github)
                   :stable t))
  (should-install ((scss-mode :repo "antonj/scss-mode" :fetcher github)
                   :stable t))
  ;; Upgrade to non-stable.
  ;; TODO: Probably need to compare versions before and after.
  (should-install (anzu :upgrade t))
  (should-install ((company :repo "company-mode/company-mode" :fetcher github)
                   :upgrade t))
  (should-install ((scss-mode :repo "antonj/scss-mode" :fetcher github)
                   :upgrade t)))

;;;; Installation tests

;; These tests test installing packages from various kinds of repos.
;; Copied from the old ci/.emacs file.

(quelpa-deftest add-recipe ()
  (should (add-to-list 'quelpa-melpa-recipe-stores
                       '((let-alist :fetcher url
                                    :url "http://git.savannah.gnu.org/cgit/emacs.git/plain/lisp/emacs-lisp/let-alist.el"
                                    :version original)))))

(quelpa-deftest github ()
  "Test installing from GitHub."
  (should-install (anaconda-mode
                   :fetcher github :repo "proofit404/anaconda-mode"
                   :files ("*.el" "*.py" "vendor/jedi/jedi"
                           ("jsonrpc" "vendor/jsonrpc/jsonrpc/*.py"))))
  (should-install ag))

(quelpa-deftest gitlab ()
  (should-install gcmh))

(quelpa-deftest codeberg ()
  (should-install spell-fu))

(quelpa-deftest sourcehut ()
  (should-install myrddin-mode))

(quelpa-deftest hg ()
  (should-install 2048-game)
  ;; [2020-01-26 Sun 00:07] Nose repo is uncloneable due to TLS error,
  ;; but probably a local problem.
  ;; There is no package nose
  ;; (should-install nose)
  (should-install minesweeper))

(quelpa-deftest svn ()
  (should-install (confluence
                   :fetcher svn :url "https://svn.code.sf.net/p/confluence-el/code/trunk/"
                   :files ("confluence*.el" "*.dtd" "*.xsl"))))

(quelpa-deftest bzr ()
  (should-install (weblogger :fetcher bzr :url "lp:weblogger-el")))

(quelpa-deftest wiki ()
  (should-install (key-chord :fetcher wiki))
  (should-install (buffer-move :fetcher wiki))
  (should-install move-text))

(quelpa-deftest url ()
  ;; FIXME: For some reason this test seems to fail in batch mode but works interactively.
  :expected-result :failed
  (should-install (ox-rss
                   :url "https://code.orgmode.org/bzg/org-mode/raw/master/contrib/lisp/ox-rss.el"
                   :fetcher url))
  (should-install (rainbow-mode
                   :url "http://git.savannah.gnu.org/cgit/emacs/elpa.git/plain/packages/rainbow-mode/rainbow-mode.el"
                   :fetcher url))
  (should-install (thingatpt+
                   :url "https://raw.githubusercontent.com/emacsmirror/emacswiki.org/master/thingatpt+.el"
                   :fetcher url)))

(quelpa-deftest file ()
  ;; FIXME: Helm isn't checked out to this path, of course.
  :expected-result :failed
  (should-install (helm
                   :fetcher file
                   :files ("*.el" "emacs-helm.sh"
                           (:exclude "helm.el" "helm-lib.el" "helm-source.el" "helm-match-plugin.el" "helm-core-pkg.el"))
                    :path "~/emacs-packages/helm")))

;;;; Parallel upgrade / defer tests

(ert-deftest quelpa-plist-remove ()
  "Test `quelpa--plist-remove'."
  (should (equal (quelpa--plist-remove '(:a 1 :b 2 :c 3) :b)
                 '(:a 1 :c 3)))
  (should (equal (quelpa--plist-remove '(:a 1 :b 2 :c 3) :a :c)
                 '(:b 2)))
  (should (equal (quelpa--plist-remove '(:a 1) :a)
                 nil))
  (should (equal (quelpa--plist-remove nil :a)
                 nil))
  (should (equal (quelpa--plist-remove '(:a 1 :b 2) :z)
                 '(:a 1 :b 2))))

(ert-deftest quelpa-defer ()
  "`:defer t' should queue the package, not install it."
  (quelpa-test-sandbox
    (should (quelpa-setup-p))
    (let ((quelpa--deferred-queue nil))
      ;; Mock quelpa-package-install to detect if it's called.
      (cl-letf (((symbol-function 'quelpa-package-install)
                 (lambda (&rest _) (error "should not be called"))))
        (quelpa '(foo :fetcher github :repo "user/foo") :defer t)
        (quelpa '(bar :fetcher github :repo "user/bar") :defer t :upgrade t))
      ;; Queue should have two entries, in push order (LIFO).
      (should (= (length quelpa--deferred-queue) 2))
      ;; First pushed = last in list (push prepends).
      (should (equal (caar quelpa--deferred-queue)
                     '(bar :fetcher github :repo "user/bar")))
      (should (equal (cdar quelpa--deferred-queue)
                     '(:upgrade t)))
      (should (equal (car (cadr quelpa--deferred-queue))
                     '(foo :fetcher github :repo "user/foo")))
      ;; :defer should be stripped from the stored plist.
      (should-not (plist-get (cdar quelpa--deferred-queue) :defer))
      (should-not (plist-get (cdr (cadr quelpa--deferred-queue)) :defer)))))

(ert-deftest quelpa-process-queue-basic ()
  "Integration test: defer two packages, then process the queue."
  (quelpa-test-sandbox
    (should (quelpa-setup-p))
    (let* ((quelpa--deferred-queue nil)
           (installed-order nil)
           ;; Mock: quelpa-build returns a fake file, quelpa-arg-rcp
           ;; returns the recipe, quelpa-get-package-desc returns a
           ;; pkg-desc.
           (desc-foo (record 'package-desc 'foo '(1 0) "foo" nil
                             nil nil "test" nil nil))
           (desc-bar (record 'package-desc 'bar '(1 0) "bar"
                             '((foo (1 0))) nil nil "test" nil nil)))
      (cl-letf (((symbol-function 'quelpa-build)
                 (lambda (rcp)
                   (let ((name (car rcp)))
                     (format "/tmp/%s.el" name))))
                ((symbol-function 'quelpa-get-package-desc)
                 (lambda (file)
                   (cond ((string-match "foo" file) desc-foo)
                         ((string-match "bar" file) desc-bar))))
                ((symbol-function 'quelpa-package-install-file)
                 (lambda (file)
                   (push (if (string-match "foo" file) 'foo 'bar)
                         installed-order)))
                ((symbol-function 'quelpa--delete-obsoleted-package)
                 #'ignore)
                ((symbol-function 'quelpa--package-installed-p)
                 (lambda (name &optional _min-version)
                   ;; After foo is "installed", report it as installed.
                   (memq name installed-order))))
        ;; Defer two packages.
        (quelpa '(foo :fetcher github :repo "u/foo") :defer t)
        (quelpa '(bar :fetcher github :repo "u/bar") :defer t)
        (should (= (length quelpa--deferred-queue) 2))
        ;; Process the queue.
        (quelpa-process-queue)
        ;; Queue should be drained.
        (should (null quelpa--deferred-queue))
        ;; foo should be installed before bar (bar depends on foo).
        (should (equal (nreverse installed-order) '(foo bar)))
        ;; Both should be in the cache.
        (should (assq 'foo quelpa-cache))
        (should (assq 'bar quelpa-cache))))))

(ert-deftest quelpa-process-queue-empty ()
  "Processing an empty queue should be a no-op."
  (let ((quelpa--deferred-queue nil))
    (quelpa-process-queue)
    (should (null quelpa--deferred-queue))))

;;;; Footer

(provide 'quelpa-tests)
